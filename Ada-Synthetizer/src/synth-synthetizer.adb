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
with Ada.Text_IO; use Ada.Text_IO;
package body Synth.Synthetizer is

   ----------
   -- Open --
   ----------

   --  open the synth device
   procedure Open
     (Driver_Access : Driver.Sound_Driver_Access;
      Synt :    out Synthetizer_Type;
      --  human perceived sound within 0.05 s
      Buffer_Size : Natural := Natural (0.05 * 44_100.0 / 2.0);
      Buffers_Number : Positive := 1;
      Audit : Synthetizer_Audit_Access := null)
   is
      Start_Time : Time;
   begin


      Synt := new Synthetizer_Structure_Type;
      Init (SST => Synt.all,
            D            => Driver_Access,
            NBBuffer => Buffers_Number,
            Buffer_Length => Buffer_Size,
            T => Start_Time,
            Audit => Audit);

      if Audit /= null then
         Audit.SynthAccess := Synt'Unrestricted_Access;
      end if;


   exception
      when E : others =>
         DumpException (E);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Synt : in out Synthetizer_Type) is
   begin
      Close (SST => Synt.all);
   exception
      when E : others =>
         DumpException (E);

   end Close;

   ----------
   -- Play --
   ----------

   procedure Play
     (Synt         : Synthetizer_Type;
      S            : SoundSample;
      Frequency    : Float;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice)
   is
      OutVoice : Voice;
   begin

      if Float (S.Note_Frequency) <= 1.0 then
         raise Program_Error
           with "Sound sample does not have a valid Note_Frequency";
      end if;

      Play (SST => Synt.all, S            => S,
            Frequency    => Frequency,
            Channel => Channel,
            Volume => Volume,
            Opened_Voice => OutVoice);
      Opened_Voice := OutVoice;
   exception
      when E : others =>
         DumpException (E);
         raise;
   end Play;

   procedure Play
     (Synt         : Synthetizer_Type;
      S            : SoundSample;
      Frequency    : Float;
      Play_Time    : Synthetizer_Time;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice)  is
      OutVoice : Voice;
   begin

      if Float (S.Note_Frequency) <= 1.0 then
         raise Program_Error
           with "Sound sample does not have a valid Note_Frequency";
      end if;

      Play (SST => Synt.all,
            S       => S,
            Frequency    => Frequency,
            Channel => Channel,
            Volume => Volume,
            Play_Time => Play_Time,
            Opened_Voice => OutVoice);
      Opened_Voice := OutVoice;
   exception
      when E : others =>
         DumpException (E);
         raise;

   end Play;

   --  Get synth internal time
   function Get_Time
     (Synth : Synthetizer_Type) return Synthetizer_Time is
   begin
      return Get_Synthetizer_Time (Synth.all);
   end Get_Time;

   --  Compute a buffer time in milliseconds
   function Get_Buffer_Time
     (Synth : Synthetizer_Type) return Synthetizer_Time is
      Drv : constant Driver.Sound_Driver_Access := Synth.D;
   begin
      return Milliseconds (Integer (1000.0 * Float(Synth.BR.Buffer_Length) *
                             Float (1.0) /
                             Float (Driver.Get_Frequency (Drv.all))));
   end Get_Buffer_Time;

   ----------
   -- Stop --
   ----------

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : in out Voice) is
   begin
      Stop (Synt.all, Opened_Voice);
   exception
      when E : others =>
         DumpException (E);
   end Stop;

   ----------
   -- Stop --
   ----------

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : in out Voice;
                   Stop_Time : Synthetizer_Time) is
   begin
      Stop (Synt.all, Opened_Voice, Stop_Time);
   exception
      when E : others =>
         DumpException (E);
   end Stop;

   --------------
   -- Stop_All --
   --------------

   procedure Stop_All(Synth: Synthetizer_Type) is
   begin
      Stop_All(Synth.all);
        exception
      when E : others =>
         DumpException (E);
   end;


   --  may be to remove
   function Get_Opened_Voices (Synt : Synthetizer_Type) return Natural is
   begin
      return Get_Allocated_Voices (Synt.all);
   end Get_Opened_Voices;

   --
   --  Fill a buffer with the given voice
   --
   procedure Process_Buffer (VSA : Voice_Structure_Type;
                             Buffer : Frame_Array_Access;
                             Volume_Factor : Float := 1.0;
                             Driver_Play_Frequency : Frequency_Type;
                             Start_Buffer_Time : Synthetizer_Time;
                             ReachEndSample : out Boolean;
                             Returned_Current_Sample_Position : out Play_Second);

   -----------------
   -- Buffer_Ring --
   -----------------

   protected body Buffer_Ring is

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         --  init the buffers
         for i in Buffers'Range loop
            Buffers (i).BP := new PCM_Frame_Array (1 .. Buffer_Length);
            Buffers (i).BP.all := (Buffers (i).BP'Range => 0);
            Buffers (i).BF := new Frame_Array (1 .. Buffer_Length);
            Buffers (i).BF.all := (Buffers (i).BF'Range => 0.0);
         end loop;

      end Init;

      function Available_Buffer_For_Consume return Boolean is
      begin
         return Available_For_Consume (Current_Consume);
      end Available_Buffer_For_Consume;

      --------------------
      -- Consume_Buffer --
      --------------------

      entry Consume_Buffer (T : out Synthetizer_Time;
                            Buffer : out PCM_Frame_Array_Access)
        when Available_For_Consume (Current_Consume) is
      begin

         Buffer := Buffers (Current_Consume).BP;
         T := Buffers (Current_Consume).Buffer_Start_Time;

         Available_For_Consume (Current_Consume) := False; -- has been consumed

         Free_Buffers := Free_Buffers + 1;
         pragma Assert (Free_Buffers <= Buffers'Length);

         Current_Consume := (Current_Consume mod NBBuffer) + Buffers'First;
         pragma Assert (Current_Consume in Buffers'Range);

      end Consume_Buffer;

      -----------------------
      -- Freeze_New_Buffer --
      -----------------------

      entry Freeze_New_Buffer (T : Synthetizer_Time; Buffer : out Frame_Array_Access)
        when Free_Buffers > 0
      is
         Search_Index : Natural := Current_Consume;
         Sanity_Check_Indice : Natural := NBBuffer;
      begin

         --  search for the next available buffer
         while Outed_Frame_Buffer (Search_Index)
           or else Available_For_Consume (Search_Index) loop
            Search_Index := (Search_Index mod NBBuffer) + Buffers'First;
            Sanity_Check_Indice := Natural'Pred (Sanity_Check_Indice);
            if Sanity_Check_Indice = 0 then
               raise Program_Error with "Implementation error";
            end if;
         end loop;

         pragma Assert (Outed_Frame_Buffer (Search_Index) = False
                        and then Available_For_Consume (Search_Index) = False);

         Buffer := Buffers (Search_Index).BF;
         Buffers (Search_Index).Buffer_Start_Time := T;
         Outed_Frame_Buffer (Search_Index) := True;
         Free_Buffers := Free_Buffers - 1;

         --  raz buffer
         Buffer.all := (others => 0.0);

      end Freeze_New_Buffer;

      -------------------------
      -- UnFreeze_New_Buffer --
      -------------------------

      entry UnFreeze_New_Buffer (Buffer : Frame_Array_Access) when True is
         Search_Index : Natural := 0;
      begin
         for i in Buffers'Range loop
            if Buffers (i).BF = Buffer then
               Search_Index := i;
               exit;
            end if;
         end loop;

         if Search_Index = 0 then
            raise Program_Error with "Fail to find Frame Array in buffers";
         end if;

         --  convert the buffer
         Buffers (Search_Index).BP.all := To_Frame_Array (Buffer.all);
         Outed_Frame_Buffer (Search_Index) := False;
         Available_For_Consume (Search_Index) := True;

      end UnFreeze_New_Buffer;

   end Buffer_Ring;

   ---------------------------
   -- Buffer_Play_Task_Type --
   ---------------------------

   --  this task continuously send buffer to the play
   task body Buffer_Play_Task_Type is
      Task_BR : Buffer_Ring_Access;
      Task_Driver : Driver.Sound_Driver_Access;
      Terminated : Boolean := False;

      --  time to send to the sound driver the buffer
      Task_Last_Play_Time_Span : Time_Span;
      pragma Unreferenced (Task_Last_Play_Time_Span);
   begin

      accept Start (TheDriver : Driver.Sound_Driver_Access;
                    BufferRing : Buffer_Ring_Access) do
         Task_BR := BufferRing;
         Task_Driver := TheDriver;
      end Start;
      --  Put_Line("Play Task Started");

      while not Terminated loop
         select
            accept Stop do
               Terminated := True;
            end Stop;
         else

            declare
               Buffer : PCM_Frame_Array_Access;
               Buffer_Start_Time : Synthetizer_Time;
               Start_Time : Time;
            begin
               if Task_BR.Available_Buffer_For_Consume then
                  --  Put_Line("Play Task Started Consumming buffer");
                  Task_BR.Consume_Buffer (Buffer_Start_Time, Buffer);
                  --           Put_Line("Play Task buffer consummed");

                  --  this transfert the sound to driver
                  --  might be blocking until the transfert is done, or
                  --  driver buffer is full
                  --
                  --  can be triky to have the play time
                  Start_Time := Clock;
                  Synth.Driver.Play (Driver => Task_Driver.all,
                                     Buffer => Buffer,
                                     Play_Reference_Buffer_Start_Time => Buffer_Start_Time
                                    );
                  Task_Last_Play_Time_Span := Clock - Start_Time;
                  --         Put_Line("Done");

               end if;
               delay 0.01;
            exception
               when E : others =>
                  DumpException (E);
            end;

         end select;
      end loop;

      --  Put_Line ("End of Playing Thread");

   end Buffer_Play_Task_Type;

   --------------------------------
   -- Buffer_Preparing_Task_Type --
   --------------------------------

   task body Buffer_Preparing_Task_Type is
      Task_Buffer_Ring : Buffer_Ring_Access;
      Task_VSA : Voices_Access;

      Terminated : Boolean := False;

      Reporting_Elapse_Time : Time_Span;
      Reporting_Max_Elapse_Time : Time_Span := To_Time_Span (Duration (0.0));

      Counter : Integer := 0;
      Processed_Voices : Natural := 0;
      pragma Unreferenced (Processed_Voices);

      Current_Buffer_Start_Time : Synthetizer_Time :=
        To_Time_Span (Duration (0.0));

      Task_Driver_Frequency : Frequency_Type;

      Driver_Frequency_Period : Synthetizer_Time;

      Task_Audit_Interface_Access : Synthetizer_Audit_Access;

      -- time correction
      Last_Cycle_Clock : Time := Clock;
      Last_Cycle_Correction_Count : Natural;
      Last_Buffer_Time_Evaluated : Synthetizer_Time;


   begin

      accept Start (BR : Buffer_Ring_Access;
                    VSA : Voices_Access;
                    Driver_Frequency : Frequency_Type;
                    Audit_Interface_Access : Synthetizer_Audit_Access := null) do

         Task_Buffer_Ring := BR;
         Task_VSA := VSA;
         Task_Driver_Frequency := Driver_Frequency;

         Driver_Frequency_Period :=
           Synthetizer_Time (Frequency_Period (Task_Driver_Frequency));

         Task_Audit_Interface_Access := Audit_Interface_Access;

         Last_Cycle_Correction_Count := Natural(Task_Buffer_Ring.NBBuffer) + 1;

      end Start;
      --  Put_Line("Preparing Task Started");

      while not Terminated loop

         select
            accept Stop  do
               Terminated := True;
            end Stop;

         else

            --  take buffer, process the voices
            --

            declare
               Preparing_Buffer : Frame_Array_Access;
               ReachEndSample : Boolean := False;

               Returned_Sample_Position : Play_Second;

               Next_Buffer_Last_Time : Synthetizer_Time;

            begin

               Task_Buffer_Ring.Freeze_New_Buffer (T => Current_Buffer_Start_Time,
                                                   Buffer => Preparing_Buffer);

               declare
                  Buffer_Time : Synthetizer_Time := Driver_Frequency_Period * (Preparing_Buffer.all'Length + 1);
               begin
                  if Last_Cycle_Correction_Count > 0 then
                     Last_Cycle_Correction_Count := Natural'Pred(Last_Cycle_Correction_Count);
                     Last_Cycle_Clock := Clock;
                     Last_Buffer_Time_Evaluated := Buffer_Time;
                  else
                     -- reevaluate the time,
                     -- using clock

                     -- cesari
                     Last_Buffer_Time_Evaluated := (Last_Buffer_Time_Evaluated  + (Clock - Last_Cycle_Clock) ) / 2;

                     Buffer_time := Last_Buffer_Time_Evaluated;

                     Last_Cycle_Clock := Clock;


                  end if;


                  --  compute the next buffer start time (inclusive)
                  Next_Buffer_Last_Time :=
                    Current_Buffer_Start_Time +
                      Buffer_Time;

               end;


               --  call back, to populate the new buffer, commit all events before

               if Task_Audit_Interface_Access /= null then
                  --  call the audit interface to fill the next buffer
                  begin
                     --  Ada.Text_IO.Put_Line("Call from Ada");

                     Ready_To_Prepare (Task_Audit_Interface_Access.all,
                                       --  give the next buffer to prepare start
                                       Current_Buffer_Start_Time,
                                       --  give the next buffer end time
                                       Next_Buffer_Last_Time);
                     --  Ada.Text_IO.Put_Line("End Call from Ada");
                  exception
                     when e : others =>
                        DumpException (E => e);
                  end;
               end if;

               declare
                  --  retrieve all opened voice
                  Opened_Voice : Voice_Play_Structure_Array :=
                    Task_VSA.Get_All_Opened_Voices_Play_Structure;

                  --  counter for having the number of voices currently processed
                  Number_Of_Processed_Voices : Natural := 0;

               begin

                  for I in Opened_Voice'Range loop
                     declare
                        Reporting_Start_Time : constant Time := Clock;
                        V : constant Voice_Play_Structure := Opened_Voice (I);
                     begin

                        --  Put_Line("Process Buffer for Voice " & Voice'Image(V));
                        --
                        if V.VSA = Null_Voice_Structure
                          or else not V.VSA.Stopped
                        then -- and not Terminated
                           Number_Of_Processed_Voices := Natural'Succ (Number_Of_Processed_Voices);

                           --  process the voice
                           Process_Buffer (VSA           => V.VSA,
                                           Buffer        =>  Preparing_Buffer,
                                           Start_Buffer_Time  => Current_Buffer_Start_Time,
                                           Driver_Play_Frequency => Task_Driver_Frequency,
                                           ReachEndSample => ReachEndSample,
                                           Returned_Current_Sample_Position => Returned_Sample_Position);

                           --  calling this line below is too costy in synchronization
                           --  Task_VSA.Update_Position(V, Returned_Sample_Position);

                           --  update the array for upward (by copy passing, for transaction)
                           Opened_Voice (I).UpdatedPosition := Returned_Sample_Position;
                           if ReachEndSample then
                              Opened_Voice (I).Closing := True;
                           end if;
                           Reporting_Elapse_Time := Clock - Reporting_Start_Time;
                           if Reporting_Elapse_Time > Reporting_Max_Elapse_Time then
                              Reporting_Max_Elapse_Time := Reporting_Elapse_Time;
                           end if;

                        end if;
                     end;
                  end loop;

                  Processed_Voices := Number_Of_Processed_Voices;

                  Counter := Integer'Succ (Counter);
                  if (Counter mod 100) = 0 then
                     --  report elements for statistics
                     --  Ada.Text_IO.Put_Line ("Max Elapse time :"
                     --                       & Duration'Image (To_Duration (Reporting_Max_Elapse_Time))
                     --                       & " Processed Voices :"
                     --                       & Natural'Image (Processed_Voices));
                     Counter := 0;
                     Reporting_Max_Elapse_Time := To_Time_Span (Duration (0.0));
                  end if;

                  Task_VSA.Update_Close_And_Positions_Status (Opened_Voice);
               end;

               Current_Buffer_Start_Time :=
                 Next_Buffer_Last_Time;

               Task_Buffer_Ring.UnFreeze_New_Buffer (Buffer => Preparing_Buffer);

            end;

         end select;

         delay 0.0001;
      end loop;

      --  Put_Line ("End of Preparing Thread");
   exception
      when E : others =>
         DumpException (E);

   end Buffer_Preparing_Task_Type;

   -----------------
   -- Voices_Type --
   -----------------

   protected body Voices_Type is

      ------------------------
      -- Allocate_New_Voice --
      ------------------------

      --  allocate a new voice, and cleaning if necessary
      procedure Allocate_New_Voice (Voice_Structure : Voice_Structure_Type;
                                    TheVoice : out Voice) is
         V : Voice;
         Search_Indice : Voice := Search_Open_Voice_Indice + 1;
      begin

         --  Ada.Text_IO.Put_Line(Voice'Image(Search_Indice)
         --                      & " " & Natural'Image(Max_All_Opened_Voice_Indice) );
         for I in 1 .. Max_All_Opened_Voice_Indice loop
            --  try close finished ones
            --  No, this will be done by the sequencer task

            if Search_Indice > Voice (Max_All_Opened_Voice_Indice) then
               Search_Indice := All_Voices'First;
            end if;

            if not Opened_Voice (Search_Indice) then
               --  open the voice
               Opened_Voice (Search_Indice) := True;
               TheVoice := Search_Indice;
               All_Voices (TheVoice) := Voice_Structure;

               if Natural (Search_Indice) < Min_All_Opened_Voice_Indice then
                  Min_All_Opened_Voice_Indice := Natural (Search_Indice);
               end if;
               if Natural (Search_Indice) > Max_All_Opened_Voice_Indice then
                  Max_All_Opened_Voice_Indice := Natural (Search_Indice);
               end if;

               Search_Open_Voice_Indice := Search_Indice;

               return;
            end if;

            Search_Indice := Search_Indice + 1;
         end loop;

         --  reach the Max_All_Opened_Voice_Indice, enlarge the process

         if Max_All_Opened_Voice_Indice < MAX_VOICES then

            Max_All_Opened_Voice_Indice :=
              Natural'Succ (Max_All_Opened_Voice_Indice);

            V := Voice (Max_All_Opened_Voice_Indice);

            Opened_Voice (V) := True;
            All_Voices (V) := Voice_Structure;
            TheVoice := V;
            Search_Open_Voice_Indice := 1; -- raz search
            return;

         end if;

         raise Program_Error with "Cannot allocate new voice";

      end Allocate_New_Voice;

      ---------------
      -- Get_Voice --
      ---------------

      --  retrieve the voice structure pointer
      function Get_Voice (V : Voice) return ReadOnly_Voice_Structure_Access is
      begin
         --  note PFR : not always true ? -> voice may be stopped in given time
         --  pragma Assert (Opened_Voice (V));
         return  All_Voices (V)'Unchecked_Access;
      end Get_Voice;

      ---------------------
      -- Is_Voice_Opened --
      ---------------------

      --  is the voice Opened
      function Is_Voice_Opened (V : Voice) return Boolean is
      begin
         return Opened_Voice (V);
      end Is_Voice_Opened;

      --------------------
      -- Can_Be_Stopped --
      --------------------

      function Can_Be_Stopped (V : Voice) return Boolean is
         VSA : constant Voice_Structure_Type := All_Voices (V);
      begin

         if VSA.Play_Sample.Cant_Stop then
            return False;
         end if;

         if VSA.Stop_Play_Sample /= Not_Defined_Clock then
            return False;
         end if;

         return True;
      end Can_Be_Stopped;

      -----------------
      -- Close_Voice --
      -----------------

      --  close an opened voice
      procedure Close_Voice (V : Voice) is
      begin
         pragma Assert (Opened_Voice (V));
         if not Opened_Voice (V) then
            --  already closed
            return;
         end if;

         All_Voices (V).Stop_Play_Sample := Synthetizer_Time_First;

      end Close_Voice;


      -----------------
      -- Close Voice --
      -----------------

      procedure Close_Voice (V : Voice; Stop_Time : Synthetizer_Time) is
      begin
         pragma Assert (Opened_Voice (V));
         if not Opened_Voice (V) then
            --  already closed
            return;
         end if;

         All_Voices (V).Stop_Play_Sample := Stop_Time;

      end Close_Voice;


      ----------------------
      -- Close All Voices --
      ----------------------

      procedure Close_All_Voices is
      begin
         for It in Min_All_Opened_Voice_Indice .. Max_All_Opened_Voice_Indice loop
            if Opened_Voice (Voice (It)) then
               Close_Voice(Voice (It));
            end if;
         end loop;
      end Close_All_Voices;

      ---------------------------
      -- Get_All_Opened_Voices --
      ---------------------------

      function Get_All_Opened_Voices return Voice_Array is
         I : Natural := 0;
         V : Voice_Array (Min_All_Opened_Voice_Indice .. Max_All_Opened_Voice_Indice);
      begin
         for It in Min_All_Opened_Voice_Indice .. Max_All_Opened_Voice_Indice loop
            if Opened_Voice (Voice (It)) then
               I := Natural'Succ (I);
               V (I) := Voice (I);
            end if;
         end loop;
         return V (1 .. I);
      end Get_All_Opened_Voices;

      ---------------------
      -- Update_Position --
      ---------------------

      procedure Update_Position (V : Voice; Current_Sample_Position : Play_Second)  is
      begin
         All_Voices (V).Current_Sample_Position := Current_Sample_Position;
      end Update_Position;

      ------------------------------------------
      -- Get_All_Opened_Voices_Play_Structure --
      ------------------------------------------

      function Get_All_Opened_Voices_Play_Structure return Voice_Play_Structure_Array is
         I : Natural := 0;
         V : Voice_Play_Structure_Array (1 .. Max_All_Opened_Voice_Indice);
      begin
         for It in Min_All_Opened_Voice_Indice .. Max_All_Opened_Voice_Indice loop
            if Opened_Voice (Voice (It)) then
               I := Natural'Succ (I); -- increment the index
               declare
                  TheVoice : constant Voice := Voice (It);
               begin
                  V (I) := Voice_Play_Structure'(V => TheVoice,
                                                 VSA =>  All_Voices (TheVoice),
                                                 UpdatedPosition => 0.0,
                                                 Closing => False);
               end;
            end if;
         end loop;
         return V (1 .. I);
      end Get_All_Opened_Voices_Play_Structure;

      ---------------------------------------
      -- Update_Close_And_Positions_Status --
      ---------------------------------------

      procedure Update_Close_And_Positions_Status (VA : Voice_Play_Structure_Array) is
      begin
         for V of VA loop
            declare
               TheVoice : constant Voice := V.V;
            begin
               if V.Closing then
                  Opened_Voice (TheVoice) := False;
                  All_Voices (TheVoice).Stopped := True;
               end if;
               All_Voices (TheVoice).Current_Sample_Position := V.UpdatedPosition;
            end;

         end loop;
      end Update_Close_And_Positions_Status;

   end Voices_Type;

   --------------------------------
   -- Synthetizer_Structure_Type --
   --------------------------------

   ----------
   -- Init --
   ----------

   procedure Init (SST : in out Synthetizer_Structure_Type;
                   D : Synth.Driver.Sound_Driver_Access;
                   NBBuffer : Positive;
                   Buffer_Length : Positive;
                   Audit : Synthetizer_Audit_Access;
                   T : out Time) is
   begin

      SST.Ref_Time := Clock;

      SST.D := D;
      --  init the buffers
      SST.BR := new Buffer_Ring (NBBuffer, Buffer_Length);
      SST.BR.Init;

      SST.Voices := new Voices_Type;

      SST.Play_Task := new Buffer_Play_Task_Type;
      SST.Play_Task.Start (TheDriver  => D,
                           BufferRing => SST.BR);

      SST.Prepare_Task := new Buffer_Preparing_Task_Type;
      SST.Prepare_Task.Start (BR => SST.BR,
                              VSA => SST.Voices,
                              Driver_Frequency => Synth.Driver.Get_Frequency (D.all),
                              Audit_Interface_Access =>  Audit);
      SST.Next_Buffer_Time := Synthetizer_Time (Time_Span_Zero);
      T := SST.Ref_Time; -- start of the Synthetizer clock
      SST.Inited := True;
   end Init;

   -----------
   -- Close --
   -----------

   procedure Close (SST : in out Synthetizer_Structure_Type) is
   begin
      SST.Play_Task.Stop;

      --  consume remaining buffers for not blocking the preparing task
      while SST.BR.Available_Buffer_For_Consume loop
         declare
            Buffer : PCM_Frame_Array_Access;
            T : Synthetizer_Time;
         begin
            SST.BR.Consume_Buffer (T, Buffer);
         end;
      end loop;

      --  stop the prepare buffer
      SST.Prepare_Task.Stop;

      --  consume remaining buffers for not blocking the preparing task
      while SST.BR.Available_Buffer_For_Consume loop
         declare
            Buffer : PCM_Frame_Array_Access;
            T : Synthetizer_Time;
         begin
            SST.BR.Consume_Buffer (T, Buffer);
         end;
      end loop;
   end Close;

   -------------------
   -- Internal_Play --
   -------------------

   procedure Internal_Play

     (SST : Synthetizer_Structure_Type;
      S            : SoundSample;
      Frequency    : Float;
      Volume       : Float := 1.0;
      Channel : Natural := 1;
      Play_Time : Synthetizer_Time;
      Opened_Voice :    out Voice)
   is
      Allocated_Voice : Voice;

   begin
      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      --  compute the clock
      --  push the sample to tasks
      --  Put_Line("Allocate_New_Voice");
      SST.Voices.Allocate_New_Voice (Voice_Structure_Type'(Note_Play_Frequency     => Frequency,
                                                           Play_Sample             => S,
                                                           Current_Sample_Position => 0.0,
                                                           Start_Play_Sample => Play_Time,
                                                           Stop_Play_Sample => Not_Defined_Clock,
                                                           Volume => Volume,
                                                           Channel => Channel,
                                                           Stopped                 => False),
                                     TheVoice => Allocated_Voice);

      --  Put_Line("Allocated_Voice :"  & Voice'Image(Allocated_Voice));
      Opened_Voice := Allocated_Voice;

   end Internal_Play;

   ----------
   -- Play --
   ----------

   procedure Play
     (SST : Synthetizer_Structure_Type;

      S            : SoundSample;
      Frequency    : Float;
      Volume       : Float := 1.0;
      Channel : Natural := 1;
      Opened_Voice :    out Voice)
   is
      Start_Play_Clock : constant Time := Clock;
      Span_Time_From_Ref : constant Synthetizer_Time :=
        Start_Play_Clock - SST.Ref_Time;

   begin

      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      Internal_Play (
                     SST => SST,
                     S            => S,
                     Frequency    => Frequency,
                     Volume       => Volume,
                     Channel      => Channel,
                     Play_Time    => Span_Time_From_Ref,
                     Opened_Voice => Opened_Voice);
   end Play;

   ----------
   -- Play --
   ----------

   procedure Play
     (SST : Synthetizer_Structure_Type;
      S            : SoundSample;
      Frequency    : Float;
      Play_Time    : Synthetizer_Time;
      Volume       : Float := 1.0;
      Channel : Positive := 1;
      Opened_Voice :    out Voice) is
   begin

      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      Internal_Play (
                     SST => SST,
                     S            => S,
                     Frequency    => Frequency,
                     Volume       => Volume,
                     Channel      => Channel,
                     Play_Time    => Play_Time,
                     Opened_Voice => Opened_Voice);
   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop (SST : Synthetizer_Structure_Type; V : in out Voice) is
   begin
      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      if V = No_Voice then
         return;
      end if;

      if not SST.Voices.Can_Be_Stopped (V)
        and then (not SST.Voices.Get_Voice (V).Play_Sample.HasLoop)
      then
         return; -- wait for the end of sound
      end if;

      if SST.Voices.Is_Voice_Opened (V) then
         SST.Voices.Close_Voice (V);
      end if;

      V := No_Voice;
   end Stop;

   ----------
   -- Stop --
   ----------

   procedure Stop (SST : Synthetizer_Structure_Type; V : in out Voice; Stop_Time : Synthetizer_Time) is
   begin
      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      if V = No_Voice then
         return;
      end if;

      if not SST.Voices.Can_Be_Stopped (V)
        and then (not SST.Voices.Get_Voice (V).Play_Sample.HasLoop)
      then
         return; -- wait for the end of sound
      end if;

      if SST.Voices.Is_Voice_Opened (V) then
         SST.Voices.Close_Voice (V, Stop_Time);
      end if;

      V := No_Voice;

   end Stop;

   --------------
   -- Stop All --
   --------------

   procedure Stop_All (SST : Synthetizer_Structure_Type) is
   begin
      if not SST.Inited then
         raise Synthetizer_Not_Inited;
      end if;

      SST.Voices.Close_All_Voices;
   end Stop_All;


   ---------------------------
   --  Get_Synthetizer_Time --
   ---------------------------

   function Get_Synthetizer_Time (SST : Synthetizer_Structure_Type) return Synthetizer_Time is
      Current_Clock : constant Time := Clock;
   begin
      return Synthetizer_Time (Current_Clock - SST.Ref_Time);
   end Get_Synthetizer_Time;

   ------------------------
   --  Get_Driver_Access --
   ------------------------

   function Get_Driver_Access (Synt : Synthetizer_Type) return Synth.Driver.Sound_Driver_Access is
   begin
      return Synt.D;
   end Get_Driver_Access;

   ---------------------------
   --  Get_Allocated_Voices --
   ---------------------------

   function Get_Allocated_Voices (SST : Synthetizer_Structure_Type) return Natural is
      VPSA : constant Voice_Play_Structure_Array :=
        SST.Voices.Get_All_Opened_Voices_Play_Structure;
      N : Natural := 0;
   begin
      for I in VPSA'Range loop
         if not VPSA (I).Closing then
            N := Natural'Succ (N);
         end if;
      end loop;

      return N;
   end Get_Allocated_Voices;

   -----------------
   -- Fill_Buffer --
   -----------------

   function "/"(f : Play_Second; f2 : Float) return Play_Second is
   begin
      return f / Play_Second (f2);
   end "/";

   function "*"(f : Play_Second; f2 : Float) return Play_Second is
   begin
      return f * Play_Second (f2);
   end "*";

   --  forward
   procedure Internal_Process_Buffer (VSA : Voice_Structure_Type;
                                      Buffer : Frame_Array_Access;
                                      Volume_Factor : Float := 1.0;
                                      Driver_Play_Frequency : Frequency_Type;
                                      Start_Buffer_Time : Synthetizer_Time;
                                      ReachEndSample : out Boolean;
                                      Returned_Current_Sample_Position : out Play_Second);

   procedure Process_Buffer (VSA : Voice_Structure_Type;
                             Buffer : Frame_Array_Access;
                             Volume_Factor : Float := 1.0;
                             Driver_Play_Frequency : Frequency_Type;
                             Start_Buffer_Time : Synthetizer_Time;
                             ReachEndSample : out Boolean;
                             Returned_Current_Sample_Position : out Play_Second) is
   begin
      Internal_Process_Buffer (VSA, Buffer, Volume_Factor,
                               Driver_Play_Frequency,
                               Start_Buffer_Time, ReachEndSample,
                               Returned_Current_Sample_Position);

   end Process_Buffer;

   --------------------
   -- Process_Buffer --
   --------------------

   procedure Internal_Process_Buffer (VSA : Voice_Structure_Type;
                                      Buffer : Frame_Array_Access;
                                      Volume_Factor : Float := 1.0;
                                      Driver_Play_Frequency : Frequency_Type;
                                      Start_Buffer_Time : Synthetizer_Time;
                                      ReachEndSample : out Boolean;
                                      Returned_Current_Sample_Position : out Play_Second) is

      Current_Sample_Position : Play_Second :=
        VSA.Current_Sample_Position;

      Driver_Play_Period : constant Play_Second :=
        Play_Second (1) / Play_Second (Driver_Play_Frequency);

      One_Sample_Frame_Period : constant Play_Second :=
        Play_Second (1.0) /
        (Play_Second (Driver_Play_Frequency) *
             VSA.Play_Sample.Frequency /
               Driver_Play_Frequency);

      One_Played_Sample_Frame_Period : constant Play_Second :=
        One_Sample_Frame_Period *
          VSA.Play_Sample.Note_Frequency /
            VSA.Note_Play_Frequency;

      function To_Second (Frame_Pos : Natural) return Play_Second is
      begin
         return Play_Second (Frame_Pos - VSA.Play_Sample.Mono_Data'First) *
           One_Played_Sample_Frame_Period;
      end To_Second;

      SS : constant SoundSample := VSA.Play_Sample;

      Sample_Data_Seconds : constant Play_Second :=
        To_Second (SS.Mono_Data'Length);

      function To_Pos_InSample (Pos : Play_Second) return Natural is
         Pos_In_Array : constant Natural :=
           Natural (Pos / One_Played_Sample_Frame_Period);
      begin
         return SS.Mono_Data'First + Pos_In_Array;
      end To_Pos_InSample;

      procedure Move_Next
        (Pos_In_Sample       : in out Play_Second;
         Reach_End :    out Boolean)
      is
      begin
         --  increment

         Pos_In_Sample := Pos_In_Sample + Driver_Play_Period;

         Reach_End := False;

         case VSA.Play_Sample.HasLoop is
            when True =>
               if Pos_In_Sample > To_Second (SS.Loop_End) then
                  Pos_In_Sample := Pos_In_Sample - To_Second (SS.Loop_Start);
               end if;
            when False =>
               begin
                  if Pos_In_Sample > Sample_Data_Seconds then
                     Reach_End := True;
                  end if;
               end;

         end case;
      end Move_Next;

   begin

      --  if VSA.Stopped and then VSA.Stop_Play_Sample = Not_Defined_Clock then
      --   ReachEndSample := True;
      --   return;
      --  end if;

      if VSA.Play_Sample = Null_Sound_Sample then
         Put_Line ("Null sample");
         ReachEndSample := True;
         return;
      end if;

      for i in Buffer'Range loop

         declare
            From_Start : constant Synthetizer_Time :=
              Synthetizer_Time (
                                Microseconds (Natural (Driver_Play_Period * Play_Second (i - Buffer'First)
                                  * Play_Second (1_000_000.0))));
            IsStarted : Boolean := True;
            CurrentTime : constant Synthetizer_Time := Start_Buffer_Time + From_Start;
         begin

            if Current_Sample_Position = 0.0 then -- sample has not started yet

               --  check if sample has started
               if CurrentTime < VSA.Start_Play_Sample then
                  --  continue loop
                  IsStarted := False;
               end if;
            end if;

            if  VSA.Stop_Play_Sample /= Not_Defined_Clock
              and then VSA.Stop_Play_Sample < CurrentTime then
               IsStarted := False;
               ReachEndSample := True;
               return;
            end if;

            if IsStarted then

               --  interpolate ?
               declare
                  Pos_In_Sample  : constant Natural :=
                    To_Pos_InSample (Current_Sample_Position);
               begin

                  if Pos_In_Sample > VSA.Play_Sample.Mono_Data'Last then
                     --  debug info,
                     Ada.Text_IO.Put_Line (" pos : " & Natural'Image (Pos_In_Sample));
                     Ada.Text_IO.Put_Line (" index pos : "
                                           & Natural'Image (SS.Mono_Data'Last));

                  end if;

                  pragma Assert (Pos_In_Sample >= SS.Mono_Data'First);
                  pragma Assert (Pos_In_Sample <= SS.Mono_Data'Last);

                  pragma Assert (Pos_In_Sample in SS.Mono_Data'Range);

                  declare
                     V1 : constant Float := SS.Mono_Data (Pos_In_Sample);
                     R  : Boolean;
                     F : Float :=  Buffer (i) + V1 * Volume_Factor * VSA.Volume;
                  begin

                     if F > 1.0 then
                        F := 1.0;  -- saturation
                     end if;

                     if F < -1.0 then
                        F := -1.0;  -- saturation
                     end if;

                     Buffer (i) := F;

                     Move_Next (Pos_In_Sample => Current_Sample_Position,
                                Reach_End => R);
                     if R then
                        ReachEndSample := True;
                        Returned_Current_Sample_Position := Current_Sample_Position;
                        return;
                     end if;
                  end;
               end;

            end if;

         end;

      end loop;
      ReachEndSample := False;
      Returned_Current_Sample_Position := Current_Sample_Position;

   end Internal_Process_Buffer;

end Synth.Synthetizer;
