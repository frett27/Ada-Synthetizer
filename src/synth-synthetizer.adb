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

   procedure Open
     (D : Driver.Sound_Driver_Access;
      S :    out Synthetizer_Type;
      Buffer_Size : Natural := Natural (0.05 * 44_100.0 / 2.0);
      Buffers_Number : Positive := 1)
   is
   begin

      --  human perceived sound within 0.05 s

      S := new Synthetizer_Structure_Type;
      S.Init (D             => D,
              NBBuffer => Buffers_Number,
              Buffer_Length => Buffer_Size);

   exception
      when E : others =>
         DumpException (E);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Synthetizer_Type) is
   begin
      S.Close;
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
         raise Program_Error with "Sound sample does not have a valid Note_Frequency";
      end if;

      Synt.Play (S            => S,
                 Frequency    => Frequency,
                 Channel => Channel,
                 Volume => Volume,
                 Opened_Voice => OutVoice);
      Opened_Voice := OutVoice;
   exception
      when E : others =>
         DumpException (E);

   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop (Synt         : Synthetizer_Type;
                   Opened_Voice : Voice) is
   begin
      Synt.Stop (Opened_Voice);
   exception
      when E : others =>
         DumpException (E);
   end Stop;

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
            Buffers (i).BP := new PCM_Frame_Array (1..Buffer_Length);
            Buffers (i).BP.all := (Buffers (i).BP'Range => 0);
            Buffers (i).BF := new Frame_Array (1..Buffer_Length);
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

      entry Consume_Buffer (Buffer : out PCM_Frame_Array_Access)
        when Available_For_Consume (Current_Consume) is
      begin

         Buffer := Buffers (Current_Consume).BP;

         Available_For_Consume (Current_Consume) := False; -- has been consumed

         Free_Buffers := Free_Buffers + 1;
         pragma Assert (Free_Buffers <= Buffers'Length);

         Current_Consume := (Current_Consume mod NBBuffer) + Buffers'First;

         pragma Assert (Current_Consume in Buffers'range);

      end Consume_Buffer;

      -----------------------
      -- Freeze_New_Buffer --
      -----------------------

      entry Freeze_New_Buffer (Buffer : out Frame_Array_Access)
        when Free_Buffers > 0
      is
         Search_Index : Natural := Current_Consume;
         Sanity_Check_Indice : Natural := NBBuffer;
      begin

         --  search for the next available buffer
         while Outed_Frame_Buffer (Search_Index) or Available_For_Consume (Search_Index) loop
            Search_Index := (Search_Index mod NBBuffer) + Buffers'First;
            Sanity_Check_Indice := Natural'Pred (Sanity_Check_Indice);
            if Sanity_Check_Indice = 0 then
               raise Program_Error with "Implementation error";
            end if;
         end loop;

         Pragma Assert(Outed_Frame_Buffer(Search_Index) = False
                       and Available_For_Consume (Search_Index) = False);

         Buffer := Buffers (Search_Index).BF;
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
      Terminated : Boolean := false;
   begin

      accept Start (TheDriver : Driver.Sound_Driver_Access; BufferRing : Buffer_Ring_Access) do
         Task_BR := BufferRing;
         Task_Driver := TheDriver;
      end Start;
      --  Put_Line("Play Task Started");

      while not Terminated loop
         select
            accept Stop do
               Terminated := True;
            end;
         else

            declare
               Buffer : PCM_Frame_Array_Access;
            begin
               if Task_BR.Available_Buffer_For_Consume then
                  --  Put_Line("Play Task Started Consumming buffer");
                  Task_BR.Consume_Buffer(Buffer);
                  --           Put_Line("Play Task buffer consummed");
                  Synth.Driver.Play(Driver => Task_Driver.all,
                                    Buffer => Buffer);
                  --         Put_Line("Done");
               end if;
               delay 0.01;
            exception
               when E : others =>
                  DumpException (E);
            end;

         end select;
      end loop;

      Put_Line ("End of Playing Thread");

   end Buffer_Play_Task_Type;

   --------------------------------
   -- Buffer_Preparing_Task_Type --
   --------------------------------

   task body Buffer_Preparing_Task_Type is
      Task_BR : Buffer_Ring_Access;
      Task_VSA : Voices_Access;

      Task_Buffer_Number : Positive;
      Task_Buffer_Length : Natural;

      Terminated : Boolean := False;

      Elapse_Time : Time_Span;
      Max_Elapse_Time : Time_Span := To_Time_Span(Duration(0.0));

      Counter : Integer := 0;
      Processed_Voices : Natural := 0;

      Last_Clock : Time := Clock;
      Init_Clock : Time := Last_Clock;

      Time_Jitter : Time_Span;

      Task_Driver_Frequency : Frequency_Type;

   begin


      accept Start (BR : Buffer_Ring_Access;
                    VSA : Voices_Access;
                    Buffer_Number : Positive;
                    Buffer_Length : Natural;
                    Driver_Frequency : Frequency_Type) do
         Task_BR := BR;
         Task_VSA := VSA;
         Task_Buffer_Length := Buffer_Length;
         Task_Buffer_Number := Buffer_Number;
         Task_Driver_Frequency := Driver_Frequency;

         Time_Jitter := Microseconds
           ( Natural( Long_Float(Task_Buffer_Length)
             * 1_000_000.0 / Long_Float(Task_Driver_Frequency) ) );

         -- must be late in the pipeline,
         -- TODO: change it
         Last_Clock := Clock - 10 * Task_Buffer_Number * Time_Jitter; -- attention

         Init_Clock := Last_Clock;
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
               Opened_Voice : Voice_Play_Structure_Array :=
                 Task_VSA.Get_All_Opened_Voices_Play_Structure;
               Returned_Sample_Position : Play_Second;

            begin

               Task_BR.Freeze_New_Buffer (Buffer => Preparing_Buffer);
               -- Put_Line("Buffer Acquired");



               declare

                  Start_Buffer_Time : Time;

                  Number_Of_Processed_Voices : Natural := 0;

                  Buffer_Allocation_End : constant Time := Clock;
               begin

                  Last_Clock := Last_Clock + Time_Jitter;
                  Start_Buffer_Time := Last_Clock;

                  for I in Opened_Voice'Range loop
                     declare
                        Start_Time : constant Time := Clock;
                        V : constant Voice_Play_Structure := Opened_Voice (I);
                     begin

                        Number_Of_Processed_Voices := Natural'Succ (Number_Of_Processed_Voices);
                        --  Put_Line("Process Buffer for Voice " & Voice'Image(V));
                        --
                        if V.VSA = Null_Voice_Structure or else
                          not V.VSA.Stopped then -- and not Terminated

                           Process_Buffer (VSA           => V.VSA,
                                           Buffer        =>  Preparing_Buffer,
                                           Start_Buffer_Time  => Start_Buffer_Time,
                                           Driver_Play_Frequency => Task_Driver_Frequency,
                                           ReachEndSample => ReachEndSample,
                                           Returned_Current_Sample_Position => Returned_Sample_Position);

                           -- calling this is too costy in synchronization
                           -- Task_VSA.Update_Position(V, Returned_Sample_Position);

                           -- update the array for upward (by copy passing)
                           Opened_Voice (I).updatedPosition := Returned_Sample_Position;
                           if ReachEndSample then
                              Opened_Voice (I).Closing := True;
                           end if;
                           Elapse_Time := Clock - Start_Time;
                           if Elapse_Time > Max_Elapse_Time then
                              Max_Elapse_Time := Elapse_Time;
                           end if;

                        end if;
                     end;
                  end loop;

                  Processed_Voices := Number_Of_Processed_Voices;

                  Counter := Integer'Succ(Counter);
                  if (Counter mod 100) = 0 then
                     -- report elements for statistics
                     Ada.Text_IO.Put_Line("Max Elapse time :"
                                          & Duration'Image (To_Duration (Max_Elapse_Time))
                                          & " Processed Voices :"
                                          & Natural'Image (Processed_Voices));
                     Counter := 0;
                     Max_Elapse_Time := To_Time_Span(Duration(0.0));
                  end if;

               end;


               Task_BR.UnFreeze_New_Buffer (Buffer => Preparing_Buffer);
               Task_VSA.Update_Close_And_Positions_Status (Opened_Voice);


            end;


         end select;

         delay 0.002;
      end loop;

      Put_Line ("End of Preparing Thread");
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

         -- Ada.Text_IO.Put_Line(Voice'Image(Search_Indice)
         --                      & " " & Natural'Image(Max_All_Opened_Voice_Indice) );
         for I in 1..Max_All_Opened_Voice_Indice loop
            -- try close finished ones
            -- No, this will be done by the sequencer task

            if Search_Indice > Voice(Max_All_Opened_Voice_Indice) then
               Search_Indice := All_Voices'First;
            end if;

            if not Opened_Voice(Search_Indice) then
               -- open the voice
               Opened_Voice(Search_Indice) := true;
               TheVoice := Search_Indice;
               All_Voices(TheVoice) := Voice_Structure;

               if Natural(Search_Indice) < Min_All_Opened_Voice_Indice then
                  Min_All_Opened_Voice_Indice := Natural(Search_Indice);
               end if;
               if Natural(Search_Indice) > Max_All_Opened_Voice_Indice then
                  Max_All_Opened_Voice_Indice := Natural(Search_Indice);
               end if;

               Search_Open_Voice_Indice := Search_Indice;

               return;
            end if;

            Search_Indice := Search_Indice + 1;
         end loop;

         --  reach the Max_All_Opened_Voice_Indice, enlarge the process

         if Max_all_opened_Voice_Indice < MAX_VOICES then

            Max_All_Opened_Voice_Indice := Natural'Succ (Max_All_Opened_Voice_Indice);
            V := Voice (Max_All_Opened_Voice_Indice);

            Opened_Voice (V) := true;
            All_Voices (V) := Voice_Structure;
            TheVoice := V;
            Search_Open_Voice_Indice := 1; --raz search
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
         -- note PFR : not always true ? -> voice may be stopped in given time
         -- pragma Assert (Opened_Voice (V));
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
         VSA : Voice_Structure_Type := All_Voices (V);
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

         All_Voices (V).Stop_Play_Sample := Clock;

      end Close_Voice;


      ---------------------------
      -- Get_All_Opened_Voices --
      ---------------------------

      function Get_All_Opened_Voices return Voice_Array is
         I : Natural := 0;
         V : Voice_Array (Min_All_Opened_Voice_Indice ..Max_All_Opened_Voice_Indice);
      begin
         for It in Min_All_Opened_Voice_Indice .. Max_All_Opened_Voice_Indice loop
            if Opened_Voice (Voice (It)) then
               I := Natural'Succ (I);
               V (I) := Voice (I);
            end if;
         end loop;
         return V (1 ..I);
      end;


      ---------------------
      -- Update_Position --
      ---------------------

      procedure Update_Position (V : Voice; Current_Sample_Position : Play_Second)  is
      begin
         All_Voices (V).Current_Sample_Position := Current_Sample_Position;
      end;

      ------------------------------------------
      -- Get_All_Opened_Voices_Play_Structure --
      ------------------------------------------

      function Get_All_Opened_Voices_Play_Structure return Voice_Play_Structure_Array is
         I : Natural := 0;
         V : Voice_Play_Structure_Array (1 ..Max_All_Opened_Voice_Indice);
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
         return V (1 ..I);
      end;

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
                  Opened_Voice (TheVoice) := false;
                  All_Voices (TheVoice).Stopped := True;
               end if;
               All_Voices (TheVoice).Current_Sample_Position := V.UpdatedPosition;
            end;

         end loop;
      end;

   end Voices_Type;

   --------------------------------
   -- Synthetizer_Structure_Type --
   --------------------------------

   protected body Synthetizer_Structure_Type is

      ----------
      -- Init --
      ----------

      procedure Init (D : Synth.Driver.Sound_Driver_Access;
                      NBBuffer : Positive;
                      Buffer_Length : Positive) is
      begin
         --  init the buffers
         BR := new Buffer_Ring (NBBuffer, Buffer_Length);
         BR.Init;

         Voices := new Voices_Type;

         Play_Task := new Buffer_Play_Task_Type;
         Play_Task.Start (TheDriver  => D,
                          BufferRing => BR);

         Prepare_Task := new Buffer_Preparing_Task_Type;
         Prepare_Task.Start (BR => BR,
                             VSA => Voices,
                             Buffer_Number => NBBuffer,
                             Buffer_Length => Buffer_Length,
                             Driver_Frequency => Synth.Driver.Get_Frequency(D.all));

         Inited := true;
      end;

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         Play_Task.Stop;

         --  consume remaining buffers for not blocking the preparing task
         while BR.Available_Buffer_For_Consume loop
            declare
               Buffer : PCM_Frame_Array_Access;
            begin
               BR.Consume_Buffer (Buffer);
            end;
         end loop;

         Prepare_Task.Stop;

         --  consume remaining buffers for not blocking the preparing task
         while BR.Available_Buffer_For_Consume loop
            declare
               Buffer : PCM_Frame_Array_Access;
            begin
               BR.Consume_Buffer (Buffer);
            end;
         end loop;
      end;

      -----------------
      -- Test_Inited --
      -----------------

      --  sanity check for opened synthetizer
      procedure Test_Inited is
      begin
         if not Inited then
            raise Synthetizer_Not_Inited;
         end if;
      end;

      ----------
      -- Play --
      ----------

      procedure Play
        (S            : SoundSample;
         Frequency    : Float;
         Volume       : Float := 1.0;
         Channel : Natural := 1;
         Opened_Voice :    out Voice)
      is
         Allocated_Voice : Voice;
      begin
         Test_Inited;
         --  push the sample to tasks
         --  Put_Line("Allocate_New_Voice");
         Voices.Allocate_New_Voice ( Voice_Structure_Type'(Note_Play_Frequency     => Frequency,
                                                           Play_Sample             => S,
                                                           Current_Sample_Position => 0.0,
                                                           Start_Play_Sample => Clock,
                                                           Stop_Play_Sample => Not_Defined_Clock,
                                                           Volume => Volume,
                                                           Channel => Channel,
                                                           Stopped                 => False),
                                     TheVoice => Allocated_Voice );

         --  Put_Line("Allocated_Voice :"  & Voice'Image(Allocated_Voice));
         Opened_Voice := Allocated_Voice;
      end Play;

      ----------
      -- Stop --
      ----------

      procedure Stop (V : Voice) is
      begin

         if not Voices.Can_Be_Stopped (V)
           and (not Voices.Get_Voice (V).Play_Sample.HasLoop)
         then
            return; -- wait for the end of sound
         end if;

         if Voices.Is_Voice_Opened (V) then
            Voices.Close_Voice (V);
         end if;
      end Stop;


   end Synthetizer_Structure_Type;

   -----------------
   -- Fill_Buffer --
   -----------------

   function "/"(f : Play_Second; f2 : Float) return Play_Second is
   begin
      return f / Play_Second (f2);
   end;

   function "*"(f : Play_Second; f2 : Float) return Play_Second is
   begin
      return f * Play_Second (f2);
   end;

   -- forward
   procedure Internal_Process_Buffer (VSA : in Voice_Structure_Type;
                                      Buffer : Frame_Array_Access;
                                      Volume_Factor : Float := 1.0;
                                      Driver_Play_Frequency : Frequency_Type := 44_100.0;
                                      Start_Buffer_Time : Time;
                                      ReachEndSample : out Boolean;
                                      Returned_Current_Sample_Position : out Play_Second);

   procedure Process_Buffer (VSA : in Voice_Structure_Type;
                             Buffer : Frame_Array_Access;
                             Volume_Factor : Float := 1.0;
                             Driver_Play_Frequency : Frequency_Type := 44_100.0;
                             Start_Buffer_Time : Time;
                             ReachEndSample : out Boolean;
                             Returned_Current_Sample_Position : out Play_Second) is
   begin
      Internal_Process_Buffer (VSA, Buffer, Volume_Factor,
                               Driver_Play_Frequency,
                               Start_Buffer_Time, ReachEndSample,
                               Returned_Current_Sample_Position);

   end;


   --------------------
   -- Process_Buffer --
   --------------------

   procedure Internal_Process_Buffer (VSA : in Voice_Structure_Type;
                                      Buffer : Frame_Array_Access;
                                      Volume_Factor : Float := 1.0;
                                      Driver_Play_Frequency : Frequency_Type := 44_100.0;
                                      Start_Buffer_Time : Time;
                                      ReachEndSample : out Boolean;
                                      Returned_Current_Sample_Position : out Play_Second) is


      Current_Sample_Position : Play_Second :=
        VSA.Current_Sample_Position;

      Driver_Play_Period : constant Play_Second :=
        Play_Second (1) / Play_Second (Driver_Play_Frequency );

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

      --if VSA.Stopped and then VSA.Stop_Play_Sample = Not_Defined_Clock then
      --   ReachEndSample := True;
      --   return;
      --end if;

      if VSA.Play_Sample = Null_Sound_Sample then
         Put_Line ("Null sample");
         return;
      end if;

      for i in Buffer'Range loop

         declare
             From_Start : constant Time_Span :=
                    Microseconds( Natural(Driver_Play_Period * Play_Second(i - Buffer'First)
                                  * Play_Second(1_000_000.0)) );
            IsStarted : Boolean := True;
            CurrentTime : constant Time := Start_Buffer_Time + From_Start;
         begin

            if Current_Sample_Position = 0.0 then -- sample has not started yet


                  -- check if sample has started
                  if CurrentTime
                    < VSA.Start_Play_Sample then

                     -- continue loop
                     IsStarted := False;
                  end if;


            end if;

            if CurrentTime >
              VSA.Stop_Play_Sample then
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
                     --debug info,
                     Ada.Text_IO.Put_Line(" pos : " & Natural'Image(Pos_In_Sample));
                     Ada.Text_IO.Put_Line(" index pos : "
                                          & Natural'Image(SS.Mono_Data'Last));

                  end if;

                  Pragma Assert(Pos_In_Sample >= SS.Mono_Data'First);
                  Pragma Assert(Pos_In_Sample <= SS.Mono_Data'Last);

                  Pragma Assert(Pos_In_Sample in SS.Mono_Data'range);

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
