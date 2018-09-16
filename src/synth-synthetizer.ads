------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2018                          --
--                                                                          --
--  Authors: Patrice Freydiere                                              --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------

With System;
with Synth.Driver;
With Ada.Real_Time;
use Ada.Real_Time;

package Synth.Synthetizer is

   --  synth object, that embed the sound system connection
   type Synthetizer_Type is limited private;


   Not_Defined_Clock : Time := Time_Last;

   --  The voice represent a play of a sound sample
   --  at a given frequency, it also remember the state of the play
   --  (given position), and loop possibility
   --
   --  the stopped mention of the voice is stopped, and can be freed
   --  for reuse
   --
   type Voice_Structure_Type is record
      Note_Play_Frequency          : Frequency_Type; -- the played frequency
      Play_Sample             : SoundSample; -- the sound sample to play
      Start_Play_Sample : Time;
      Stop_Play_Sample : Time := Not_Defined_Clock;
      Current_Sample_Position : Play_Second := 0.0; -- the position in second
      Volume       : Float := 1.0; -- volume factor
      Stopped : Boolean := False;
      Channel : Positive := 1; -- used for getting associated voices
   end record;

   --  the voice type, index to the voice array structure
   type Voice is new Natural;

   --  array of voices
   type Voice_Array is array (Positive range <>) of Voice;

   --  voices structures, maintain the voice information
   --  associated to voice number , if voice structure type is Null_Voice_Structure, the voice is
   --  free to reuse
   type Voice_Structure_Array is array (Voice range <>) of aliased Voice_Structure_Type;

   type ReadOnly_Voice_Structure_Access is access constant Voice_Structure_Type;

   --  open the synth device
   procedure Open
     (D : Driver.Sound_Driver_Access;
      S :    out Synthetizer_Type;
      Buffer_Size : Natural := Natural (0.05 * 44_100.0 / 2.0);
      Buffers_Number : Positive := 1);


   --  close the synth
   procedure Close (S : in out Synthetizer_Type);

   ----------
   -- Play --
   ----------

   --
   --  play a sound , on a typical frequency
   --  return the associated played channel
   --

   procedure Play
     (Synt         : Synthetizer_Type;
      S            : SoundSample;
      Frequency    : Float;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice);

   ----------
   -- Stop --
   ----------

   --  stop the voice (or the sound associated to it)

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : Voice);

   Synthetizer_Not_Inited : exception;

private

   --  max voice
   MAX_VOICES : constant Natural := 400;
   MAX_VOICES_INDICE : constant Voice := Voice (MAX_VOICES);

   --
   --  Fill a buffer with the given voice
   --
   procedure Process_Buffer (VSA : Voice_Structure_Type;
                             Buffer : Frame_Array_Access;
                             Volume_Factor : Float := 1.0;
                             Driver_Play_Frequency : Frequency_Type := 44_100.0;
                             Start_Buffer_Time : Time;
                             ReachEndSample : out Boolean;
                             Returned_Current_Sample_Position : out Play_Second);

   type Buffer_Type is record
      BP : PCM_Frame_Array_Access;
      BF : Frame_Array_Access;
   end record;

   --  buffers for the play
   type Buffers_Type is array (Natural range <>) of Buffer_Type;

   type Boolean_Array is array (Natural range <>) of Boolean;

   type Voice_Boolean_Array is array (Voice range <>) of Boolean;

   -- protected type for the buffers preparation
   -- buffers are prepared using a Frame_Array and provided for
   -- sound card as PCM_Frame
   protected type Buffer_Ring (NBBuffer : Positive; Buffer_Length : Positive ) is

      procedure Init;

      entry Consume_Buffer (Buffer : out PCM_Frame_Array_Access);

      entry Freeze_New_Buffer (Buffer : out Frame_Array_Access);

      entry UnFreeze_New_Buffer (Buffer : Frame_Array_Access);

      function Available_Buffer_For_Consume return Boolean;

   private

      --  buffers (frame and compiled ones)
      Buffers : Buffers_Type (1 .. NBBuffer);

      --  buffers that can be consumed
      Available_For_Consume : Boolean_Array (1 .. NBBuffer) :=
        (others => False);

      --  available buffers
      Free_Buffers : Natural := NBBuffer;

      --  current index for consumption
      Current_Consume : Natural := 1;

      --  outed frames, mark are "taken" buffer (outed)
      --  multiple buffer can be "taken"
      Outed_Frame_Buffer : Boolean_Array (1 .. NBBuffer) :=
        (others => False);

   end Buffer_Ring;

   type Buffer_Ring_Access is access all Buffer_Ring;

   --  task that play all the buffers
   task type Buffer_Play_Task_Type is

      pragma Priority(System.Priority'Last);
      --  start the play task
      entry Start (TheDriver : Driver.Sound_Driver_Access;
                   BufferRing : Buffer_Ring_Access);

      entry Stop;

   end Buffer_Play_Task_Type;

   type Buffer_Play_Task_Access is access Buffer_Play_Task_Type;

   --  constant null Voice Structure
   Null_Voice_Structure : constant Voice_Structure_Type := Voice_Structure_Type'
     (Current_Sample_Position => 0.0,
      Stopped                 => True,
      Play_Sample             => Null_Sound_Sample,
      Stop_Play_Sample => Not_Defined_Clock,
      Start_Play_Sample => Time_First,
      Note_Play_Frequency => 440.0,
      Channel => 1, Volume => 1.0
     );

   type Voice_Play_Structure is record
      V : Voice;
      VSA : Voice_Structure_Type;
      UpdatedPosition : Play_Second;
      Closing : Boolean;
   end record;

   type Voice_Play_Structure_Array is array (Natural range <>) of Voice_Play_Structure;

   -----------------
   -- Voices_Type --
   -----------------

   protected type Voices_Type is

      procedure Allocate_New_Voice (Voice_Structure : Voice_Structure_Type; TheVoice : out Voice);
      function Get_Voice (V : Voice) return ReadOnly_Voice_Structure_Access;
      function Is_Voice_Opened (V : Voice) return Boolean;
      function Can_Be_Stopped (V : Voice) return Boolean;
      procedure Close_Voice (V : Voice);

      function Get_All_Opened_Voices return Voice_Array;
      procedure Update_Position (V : Voice;
                                 Current_Sample_Position : Play_Second);

      --  specialized play structures
      function Get_All_Opened_Voices_Play_Structure return Voice_Play_Structure_Array;
      procedure Update_Close_And_Positions_Status (VA : Voice_Play_Structure_Array);

   private

      --  voice control, for the voices
      All_Voices : aliased Voice_Structure_Array (1 .. MAX_VOICES_INDICE) :=
        (others =>
           Null_Voice_Structure);

      --  min / max indices in the opened voice
      Max_All_Opened_Voice_Indice : Natural := MAX_VOICES / 4;
      Min_All_Opened_Voice_Indice : Natural := 1;
      Search_Open_Voice_Indice : Voice := 0;

      --  list all opened voice indices,
      --  for a given voice say the voice is opened or not
      Opened_Voice : Voice_Boolean_Array (1 .. MAX_VOICES_INDICE) := (others => False);

   end Voices_Type;

   type Voices_Access is access Voices_Type;

   --  task handling the buffer_fill

   task type Buffer_Preparing_Task_Type is
      pragma Priority(System.Priority'Last);

      entry Start (BR : Buffer_Ring_Access;
                   VSA : Voices_Access;
                   Buffer_Number : Positive;
                   Buffer_Length : Natural;
                   Driver_Frequency : Frequency_Type);

      entry Stop;

   end Buffer_Preparing_Task_Type;

   type Buffer_Preparing_Task_Access is access all Buffer_Preparing_Task_Type;

   --  Synthetizer type as protected object
   protected type Synthetizer_Structure_Type is

      procedure Init (D : Synth.Driver.Sound_Driver_Access;
                      NBBuffer : Positive;
                      Buffer_Length : Positive);

      procedure Play
        (S            : SoundSample;
         Frequency    : Float;
         Volume : Float := 1.0;
         Channel : Natural := 1;
         Opened_Voice :    out Voice);

      procedure Stop (V : Voice);

      procedure Close;

   private

      Inited : boolean := False;

      BR : Buffer_Ring_Access; -- buffer ring

      --  associated driver
      D : Synth.Driver.Sound_Driver_Access;

      --  internal tasks

      Prepare_Task : Buffer_Preparing_Task_Access;

      Play_Task : Buffer_Play_Task_Access;

      Voices : Voices_Access;

   end Synthetizer_Structure_Type;

   type Synthetizer_Type is access Synthetizer_Structure_Type;

end Synth.Synthetizer;


