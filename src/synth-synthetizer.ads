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

with Synth.Driver;

package Synth.Synthetizer is


   -- synth object, that embed the sound system connection
   type Synthetizer_Type is limited private;

   -- The voice represent a play of a sound sample
   -- at a given frequency, it also remember the state of the play
   -- (given position), and loop possibility
   --
   -- the stopped mention of the voice is stopped, and can be freed
   -- for reuse
   --
   type Voice_Structure_Type is record
      Note_Play_Frequency          : Frequency_Type; -- the played frequency
      Play_Sample             : SoundSample; -- the sound sample to play
      Current_Sample_Position : Play_Second := 0.0; -- the position in second
      Stopped : Boolean := False;
      Channel : Positive := 1; -- used for getting associated voices
   end record;

   -- the voice type, index to the voice array structure
   type Voice is new Natural;

      -- array of voices
   type Voice_Array is array (Positive range <>) of Voice;

   -- array of voice that can be null, or don't have any voices
   type Opened_Voice_Array is array(Natural range <>) of Voice;


   -- voices structures
   type Voice_Structure_Array is array(Voice range <>) of aliased Voice_Structure_Type;

   -- access to a voice definition
   type Voice_Structure_Access is access Voice_Structure_Type;

   type ReadOnly_Voice_Structure_Access is access constant Voice_Structure_Type;

   -- internal functions

   --
   -- Fill a buffer with the given voice
   --
   procedure Process_Buffer(VSA : in ReadOnly_Voice_Structure_Access;
                            Buffer : in Frame_Array_Access;
                            Volume_Factor : in Float := 1.0;
                            ReachEndSample : out Boolean;
                            Returned_Current_Sample_Position : out Play_Second);



   -- open the synth device
   procedure Open
     (D : in     Driver.Sound_Driver_Access;
      S :    out Synthetizer_Type);

   -- close the synth
   procedure Close (S : in out Synthetizer_Type);

   ----------
   -- Play --
   ----------

   --
   -- play a sound , on a typical frequency
   -- return the associated played channel
   --

   procedure Play
     (Synt         : in     Synthetizer_Type;
      S            : in     SoundSample;
      Frequency    : in     Float;
      Channel : in Positive := 1;
      Opened_Voice :    out Voice);

   ----------
   -- Stop --
   ----------

   -- stop the voice (or the sound associated to it)

   procedure Stop (Opened_Voice : in Voice);

   Synthetizer_Not_Inited : exception;

private

   -- max voice
   MAX_VOICES : constant Natural := 100;
   MAX_VOICES_INDICE : constant Voice := Voice(MAX_VOICES);

   type Preallocated_Voices_Range is range 1..MAX_VOICES;


   type Buffer_Type is record
      BP : PCM_Frame_Array_Access;
      BF : Frame_Array_Access;
   end record;

   -- buffers for the play
   type Buffers_Type is array (Natural range <>) of Buffer_Type;

   type Boolean_Array is array (Natural range <>) of Boolean;

   type Voice_Boolean_Array is array (Voice range <>) of Boolean;

   protected type Buffer_Ring(NBBuffer : Positive; Buffer_Length : Positive ) is

      procedure Init;

      entry Consume_Buffer(Buffer : out PCM_Frame_Array_Access);

      entry Freeze_New_Buffer(Buffer : out Frame_Array_Access);

      entry UnFreeze_New_Buffer(Buffer : in Frame_Array_Access);

      function Available_Buffer_For_Consume return Boolean;

   private


      -- buffers (frame and compiled ones)
      Buffers : Buffers_Type(1 .. NBBuffer);

      -- buffers that can be consumed
      Available_For_Consume : Boolean_Array(1 .. NBBuffer) :=
        (others => False);

      -- available buffers
      Free_Buffers : Natural := NBBuffer;


      -- current index for consumption
      Current_Consume : Natural := 1;

      -- outed frames
      Outed_Frame_Buffer : Boolean_Array(1 .. NBBuffer) :=
        (others => False);

   end Buffer_Ring;



   type Buffer_Ring_Access is access all Buffer_Ring;


   -- task that play all the buffers
   task type Buffer_Play_Task_Type is

      -- start the play task
      entry Start (D : in Driver.Sound_Driver_Access; BR : Buffer_Ring_Access);

      entry Stop;

   end Buffer_Play_Task_Type;

   type Buffer_Play_Task_Access is access Buffer_Play_Task_Type;



   -- constant null Voice Structure
   Null_Voice_Structure : constant Voice_Structure_Type := Voice_Structure_Type'
     (Current_Sample_Position => 0.0,
      Stopped                 => True,
      Play_Sample             => Null_Sound_Sample,
      Note_Play_Frequency => 440.0
        ,Channel => 1
     );


   -----------------
   -- Voices_Type --
   -----------------

   protected type Voices_Type is

       procedure Allocate_New_Voice(Voice_Structure : in Voice_Structure_Type; TheVoice : out Voice);
       function Get_Voice(V : Voice) return ReadOnly_Voice_Structure_Access;
       function Is_Voice_Opened(V : Voice) return Boolean;
       procedure Close_Voice(V : Voice);
      function Get_All_Opened_Voices return Voice_Array;
      procedure Update_Position(V : Voice; Current_Sample_Position : Play_Second);

   private

          -- voice control
      All_Voices : aliased Voice_Structure_Array (1..MAX_VOICES_INDICE) :=
        (others =>
           Null_Voice_Structure);

      Max_All_Opened_Voice_Indice : Natural := 0;

      -- list all opened voice indices
      Opened_Voice : Voice_Boolean_Array (1..MAX_VOICES_INDICE) := (others => False);

   end Voices_Type;

   type Voices_Access is access Voices_Type;





   -- task handling the buffer_fill

   task type Buffer_Preparing_Task_Type is

      entry Start(BR : Buffer_Ring_Access; VSA : Voices_Access);

      entry Stop;

   end Buffer_Preparing_Task_Type;


   type Buffer_Preparing_Task_Access is access all Buffer_Preparing_Task_Type;




   -- Synthetizer type as protected object
   protected type Synthetizer_Structure_Type is

      procedure Init(D : Synth.Driver.Sound_Driver_Access;
                     NBBuffer : Positive;
                     Buffer_Length : Positive);

      procedure Play
        (S            : in     SoundSample;
         Frequency    : in     Float;
         Channel : in Natural := 1;
         Opened_Voice :    out Voice);

      procedure Stop (V : in Voice);

      function Get_All_Opened_Voices return Voice_Array;

      procedure Close;


   private

      Inited : boolean := False;


      BR : Buffer_Ring_Access; -- buffer ring

      -- associated driver
      D : Synth.Driver.Sound_Driver_Access;

      -- internal tasks

      Prepare_Task : Buffer_Preparing_Task_Access;

      Play_Task : Buffer_Play_Task_Access;

      Voices : Voices_Access;

   end Synthetizer_Structure_Type;

   type Synthetizer_Type is access Synthetizer_Structure_Type;


end Synth.Synthetizer;


