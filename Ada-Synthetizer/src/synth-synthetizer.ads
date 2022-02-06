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

with System;
with Synth.Driver;

package Synth.Synthetizer is

   Not_Defined_Clock : constant Synthetizer_Time :=
     Synthetizer_Time (Time_Span_Last);

   Synthetizer_Time_First : constant Synthetizer_Time :=
     Synthetizer_Time (Time_Span_First);

   type Voice is private;

   No_Voice : constant Voice;

   --  synth object, that embed the sound system connection
   type Synthetizer_Type is limited private;
   type Synthetizer_Access is access all Synthetizer_Type;

   ------------------------------
   --  synth audit interface
   ------------------------------

   type Synthetizer_Audit is abstract tagged record
      SynthAccess : Synthetizer_Access;
   end record;

   type Synthetizer_Audit_Access is access all Synthetizer_Audit'Class;

   --  procedure associated to Audit Interface
   procedure Ready_To_Prepare (Synth_Audit : in out Synthetizer_Audit;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synthetizer_Time) is abstract;

   ----------
   -- Open --
   ----------

   --  open the synth device
   procedure Open
     (Driver_Access : Driver.Sound_Driver_Access;
      Synt :    out Synthetizer_Type;
      Buffer_Size : Natural := Natural (0.05 * 44_100.0 / 2.0);
      Buffers_Number : Positive := 1;
      Audit : Synthetizer_Audit_Access := null);

   -----------
   -- Close --
   -----------

   --  close the synth
   procedure Close (Synt : in out Synthetizer_Type);

   ----------
   -- Play --
   ----------

   --
   --  play a sound , on a typical frequency
   --  return the associated voice
   --

   procedure Play
     (Synt         : Synthetizer_Type;
      S            : SoundSample;
      Frequency    : Float;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice);

   --
   --  play a sound, giving the internal timing,
   --  defining the time permit to create a ahead of time playing
   --  return the associated opened voice
   --
   procedure Play
     (Synt         : Synthetizer_Type;
      S            : SoundSample;
      Frequency    : Float;
      Play_Time    : Synthetizer_Time;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice);

   --  Get synth internal time
   function Get_Time
     (Synth : Synthetizer_Type) return Synthetizer_Time;

   function Get_Buffer_Time
     (Synth : Synthetizer_Type) return Synthetizer_Time;

   ----------
   -- Stop --
   ----------

   --  stop the voice (or the sound associated to it)

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : in out Voice);

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : in out Voice;
                   Stop_Time     : Synthetizer_Time);


   --------------
   -- Stop_All --
   --------------

   -- Stop All current opened voices

   procedure Stop_All(Synth: Synthetizer_Type);

   -----------------------
   -- Get_Opened_Voices --
   -----------------------

   function Get_Opened_Voices (Synt : Synthetizer_Type) return Natural;

   --  Accessor for getting underlying associated driver
   function Get_Driver_Access (Synt : Synthetizer_Type) return Synth.Driver.Sound_Driver_Access;

   Synthetizer_Not_Inited : exception;

   MAX_VOICES : constant Natural := 600;

private

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
      Start_Play_Sample : Synthetizer_Time;
      Stop_Play_Sample : Synthetizer_Time := Not_Defined_Clock;
      Current_Sample_Position : Play_Second := 0.0; -- the position in second
      Volume       : Float := 1.0; -- volume factor
      Stopped : Boolean := False;
      Channel : Positive := 1; -- used for getting associated voices
   end record;

   --  the voice type, index to the voice array structure
   type Voice is new Natural;

   No_Voice : constant Voice := 0;

   --  array of voices
   type Voice_Array is array (Positive range <>) of Voice;

   --  voices structures, maintain the voice information
   --  associated to voice number ,
   --  if voice structure type is Null_Voice_Structure, the voice is
   --  free to reuse
   type Voice_Structure_Array is
     array (Voice range <>) of aliased Voice_Structure_Type;

   type ReadOnly_Voice_Structure_Access is
     access constant Voice_Structure_Type;

   --  max voice

   MAX_VOICES_INDICE : constant Voice := Voice (MAX_VOICES);

   --  buffer for preparing the sound output
   type Buffer_Type is record
      Buffer_Start_Time : Synthetizer_Time; -- buffer start synthetizer time
      BP : PCM_Frame_Array_Access;
      BF : Frame_Array_Access;
   end record;

   --  buffers for the play
   type Buffers_Type is array (Natural range <>) of Buffer_Type;

   type Boolean_Array is array (Natural range <>) of Boolean;

   type Voice_Boolean_Array is array (Voice range <>) of Boolean;

   --  protected type for the buffers preparation
   --  buffers are prepared using a Frame_Array and provided for
   --  sound card as PCM_Frame
   protected type Buffer_Ring (NBBuffer : Positive;
                               Buffer_Length : Positive) is

      --  Init the Buffer Ring
      procedure Init;

      entry Consume_Buffer (T : out Synthetizer_Time; Buffer : out PCM_Frame_Array_Access);

      --  this allocate the buffer
      entry Freeze_New_Buffer (T : Synthetizer_Time; Buffer : out Frame_Array_Access);

      --  Release the buffer
      entry UnFreeze_New_Buffer (Buffer : Frame_Array_Access);

      --  Available buffers
      function Available_Buffer_For_Consume return Boolean;

   private

      --  buffers (frame and compiled ones)
      Buffers : Buffers_Type (1 .. NBBuffer) := (others => Buffer_Type'(To_Time_Span (Duration (0.0)), null,null));

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

      pragma Priority (System.Priority'Last);
      --  start the play task, with the given ring buffer
      entry Start (TheDriver : Driver.Sound_Driver_Access;
                   BufferRing : Buffer_Ring_Access);

      entry Stop;

   end Buffer_Play_Task_Type;

   --  pointer to the play task
   type Buffer_Play_Task_Access is access Buffer_Play_Task_Type;

   --  constant null Voice Structure
   Null_Voice_Structure : constant Voice_Structure_Type :=
     Voice_Structure_Type'
       (Current_Sample_Position => 0.0,
        Stopped                 => True,
        Play_Sample             => Null_Sound_Sample,
        Stop_Play_Sample => Not_Defined_Clock,
        Start_Play_Sample => Synthetizer_Time_First,
        Note_Play_Frequency => 440.0,
        Channel => 1, Volume => 1.0
       );

   type Voice_Play_Structure is record
      V : Voice;
      VSA : Voice_Structure_Type;
      UpdatedPosition : Play_Second;
      Closing : Boolean;
   end record;

   type Voice_Play_Structure_Array is
     array (Natural range <>) of Voice_Play_Structure;

   -----------------
   -- Voices_Type --
   -----------------

   protected type Voices_Type is

      procedure Allocate_New_Voice (Voice_Structure : Voice_Structure_Type;
                                    TheVoice : out Voice);
      function Get_Voice (V : Voice) return ReadOnly_Voice_Structure_Access;
      function Is_Voice_Opened (V : Voice) return Boolean;
      function Can_Be_Stopped (V : Voice) return Boolean;
      procedure Close_Voice (V : Voice);
      procedure Close_Voice (V : Voice; Stop_Time : Synthetizer_Time);
      procedure Close_All_Voices;

      function Get_All_Opened_Voices return Voice_Array;
      procedure Update_Position (V : Voice;
                                 Current_Sample_Position : Play_Second);

      --  specialized play structures
      function Get_All_Opened_Voices_Play_Structure
        return Voice_Play_Structure_Array;
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
      Opened_Voice : Voice_Boolean_Array (1 .. MAX_VOICES_INDICE) :=
        (others => False);

   end Voices_Type;

   type Voices_Access is access Voices_Type;

   --  task handling the buffer_fill

   task type Buffer_Preparing_Task_Type is
      pragma Priority (System.Priority'First);

      --  Start the prepepare of the buffer
      --  Return the Reference time for the next buffer
      entry Start (BR : Buffer_Ring_Access;
                   VSA : Voices_Access;
                   Driver_Frequency : Frequency_Type;
                   Audit_Interface_Access : Synthetizer_Audit_Access := null);
      entry Stop;

   end Buffer_Preparing_Task_Type;

   type Buffer_Preparing_Task_Access is access all Buffer_Preparing_Task_Type;

   --  Synthetizer type as protected object
   type Synthetizer_Structure_Type is record

      Inited : Boolean := False;

      BR : Buffer_Ring_Access; -- buffer ring

      --  associated driver
      D : Synth.Driver.Sound_Driver_Access;

      --  internal tasks
      Prepare_Task : Buffer_Preparing_Task_Access;

      --  task to send buffer to driver
      Play_Task : Buffer_Play_Task_Access;

      --  access to voices
      Voices : Voices_Access;

      --  Reference initial Time (start of the synthetizer)
      Ref_Time : Time;

      Next_Buffer_Time : Synthetizer_Time;

   end record;

   procedure Init (SST : in out Synthetizer_Structure_Type;

                   D : Synth.Driver.Sound_Driver_Access;
                   NBBuffer : Positive;
                   Buffer_Length : Positive;
                   Audit : Synthetizer_Audit_Access;
                   T : out Time);

   --  Play a sound sample, with the given frequency
   --  Return the allocated voice
   procedure Play
     (
      SST : Synthetizer_Structure_Type;
      S            : SoundSample;
      Frequency    : Float;
      Volume : Float := 1.0;
      Channel : Natural := 1;
      Opened_Voice :    out Voice);

   procedure Play
     (
      SST : Synthetizer_Structure_Type;
      S            : SoundSample;
      Frequency    : Float;
      Play_Time    : Synthetizer_Time;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice);

   procedure Stop (SST : Synthetizer_Structure_Type; V : in out Voice);
   procedure Stop (SST : Synthetizer_Structure_Type; V : in out Voice;
                   Stop_Time : Synthetizer_Time);

   procedure Stop_All (SST : Synthetizer_Structure_Type);

   procedure Close (SST : in out Synthetizer_Structure_Type);

   function Get_Synthetizer_Time (SST : Synthetizer_Structure_Type) return Synthetizer_Time;
   function Get_Allocated_Voices (SST : Synthetizer_Structure_Type) return Natural;

   type Synthetizer_Type is access Synthetizer_Structure_Type;

end Synth.Synthetizer;
