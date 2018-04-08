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
with Ada.Text_IO;use Ada.Text_IO;
package body Synth.Synthetizer is


   ----------
   -- Open --
   ----------

   procedure Open
     (D : in     Driver.Sound_Driver_Access;
      S :    out Synthetizer_Type)
   is
   begin

      S := new Synthetizer_Structure_Type;
      S.Init(D             => D,
             NBBuffer =>3,
             Buffer_Length => 5000);

   exception
            when E : others =>
                  DumpException(E);

   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Synthetizer_Type) is
   begin
     S.Close;
    exception
            when E : others =>
                  DumpException(E);

   end Close;

   ----------
   -- Play --
   ----------

   procedure Play
     (Synt         : in     Synthetizer_Type;
      S            : in     SoundSample;
      Frequency    : in     Float;
      Channel : in Positive := 1;
      Opened_Voice :    out Voice)
   is
      OutVoced : Voice;
   begin

      if Float(S.Note_Frequency) <= 1.0 then
         raise Program_Error with "Sound sample does not have a valid Note_Frequency";
      end if;


      Synt.Play(S            => S,
                Frequency    => Frequency,
                Channel => Channel,
                Opened_Voice => OutVoced);
      Opened_Voice := OutVoced;
   exception
            when E : others =>
                  DumpException(E);

   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop (Synt         : in     Synthetizer_Type;
                   Opened_Voice : in Voice) is
   begin
    Synt.Stop(Opened_Voice);
   exception
            when E : others =>
                  DumpException(E);
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
         -- init the buffers
         for i in Buffers'Range loop
            Buffers(i).BP := new PCM_Frame_Array(1..Buffer_Length);
            Buffers(i).BP.all := (Buffers(i).BP'Range => 0);
            Buffers(i).BF := new Frame_Array(1..Buffer_Length);
            Buffers(i).BF.all := (Buffers(i).BF'Range => 0.0);
         end loop;

      end Init;


      function Available_Buffer_For_Consume return Boolean is
      begin
         return Available_For_Consume(Current_Consume);
      end;



      --------------------
      -- Consume_Buffer --
      --------------------

      entry Consume_Buffer(Buffer : out PCM_Frame_Array_Access)
        when Available_For_Consume(Current_Consume) is
      begin

         Buffer := Buffers(Current_Consume).BP;

         Available_For_Consume(Current_Consume) := False; -- has been consumed

         Free_Buffers := Free_Buffers + 1;
         Pragma Assert(Free_Buffers <= Buffers'Length);

         Current_Consume := (Current_Consume mod NBBuffer) + Buffers'First;

         Pragma Assert(Current_Consume in Buffers'range);

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

         -- search for the next available buffer
         while Outed_Frame_Buffer(Search_Index) or Available_For_Consume(Search_Index) loop
            Search_Index := (Search_Index mod NBBuffer) + Buffers'First;
            Sanity_Check_Indice := Natural'Pred(Sanity_Check_Indice);
            if Sanity_Check_Indice = 0 then
               raise Program_Error with "Implementation error";
            end if;
         end loop;

         Pragma Assert(Outed_Frame_Buffer(Search_Index) = False
                       and Available_For_Consume(Search_Index) = False);

         Buffer := Buffers(Search_Index).BF;
         Outed_Frame_Buffer(Search_Index) := True;
         Free_Buffers := Free_Buffers - 1;

         -- raz buffer
         Buffer.all := (others => 0.0);

      end Freeze_New_Buffer;

      -------------------------
      -- UnFreeze_New_Buffer --
      -------------------------

      entry UnFreeze_New_Buffer(Buffer : in Frame_Array_Access) when True is
         Search_Index : Natural := 0;
      begin
         for i in Buffers'Range loop
            if Buffers(i).BF = Buffer then
               Search_Index := i;
               exit;
            end if;
         end loop;

         if Search_Index = 0 then
            raise Program_Error with "Fail to find Frame Array in buffers";
         end if;

         -- convert the buffer
         Buffers(Search_Index).BP.all := To_Frame_Array(Buffer.all);
         Outed_Frame_Buffer(Search_Index) := False;
         Available_For_Consume(Search_Index) := True;


      end UnFreeze_New_Buffer;

   end Buffer_Ring;

   ---------------------------
   -- Buffer_Play_Task_Type --
   ---------------------------
   -- this task continuously send buffer to the play
   task body Buffer_Play_Task_Type is
      Task_BR : Buffer_Ring_Access;
      Task_Driver : Driver.Sound_Driver_Access;
      Terminated : Boolean := false;
   begin

      accept Start (D : in Driver.Sound_Driver_Access; BR : Buffer_Ring_Access) do
         Task_BR := BR;
         Task_Driver := D;
      end Start;
      -- Put_Line("Play Task Started");
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
               -- Put_Line("Play Task Started Consumming buffer");
               Task_BR.Consume_Buffer(Buffer);
               --           Put_Line("Play Task buffer consummed");
               Synth.Driver.Play(Driver => Task_Driver.all,
                                 Buffer => Buffer);
                   --         Put_Line("Done");
                   end if;
                   delay 0.01;
            exception
               when E : others =>
                  DumpException(E);

            end;


         end select;
      end loop;

       Put_Line("End of Playing Thread");

   end Buffer_Play_Task_Type;

   --------------------------------
   -- Buffer_Preparing_Task_Type --
   --------------------------------

   task body Buffer_Preparing_Task_Type is
      Task_BR : Buffer_Ring_Access;
      Task_VSA : Voices_Access;
      Terminated : Boolean := False;
   begin

      accept Start (BR : in Buffer_Ring_Access;
                    VSA : Voices_Access) do
         Task_BR := BR;
         Task_VSA := VSA;
      end Start;
      -- Put_Line("Preparing Task Started");

      while not Terminated loop

         select
            accept Stop  do
               Terminated := True;
            end Stop;

         else

         -- Put_Line("Check Buffer");

         -- take buffer, process the voices
         --
         declare
            Preparing_Buffer : Frame_Array_Access;
            ReachEndSample : Boolean := False;
            Opened_Voice : Voice_Play_Structure_Array := Task_VSA.Get_All_Opened_Voices_Play_Structure;
            Returned_Sample_Position : Play_Second;
         begin

               if Opened_Voice'Length > 0 then

                  -- Put_Line("Prepare Buffer");


                  Task_BR.Freeze_New_Buffer(Buffer => Preparing_Buffer);

                  for I in Opened_Voice'Range loop
                     declare
                        V : constant Voice_Play_Structure := Opened_Voice(I);
                     begin

                        -- Put_Line("Process Buffer for Voice " & Voice'Image(V));
                        --
                        if not V.VSA.Stopped then -- and not Terminated

                           Process_Buffer(VSA           => V.VSA,
                                          Buffer        =>  Preparing_Buffer,
                                          ReachEndSample => ReachEndSample,
                                          Returned_Current_Sample_Position => Returned_Sample_Position);

                           --                        Task_VSA.Update_Position(V, Returned_Sample_Position);
                           Opened_Voice(I).updatedPosition := Returned_Sample_Position;
                           if ReachEndSample then

                              Opened_Voice(I).Closing := True;
                           end if;


                        end if;
                     end;
                  end loop;
                  -- Put_Line("End Of Preparation");
                  -- if (not Terminated) then
                  Task_BR.UnFreeze_New_Buffer(Buffer => Preparing_Buffer);
                  -- Put_Line("Done");
                  -- end if;
                  Task_VSA.Update_Close_And_Positions_Status(Opened_Voice);
               end if;

            end;

         end select;




         delay 0.01;
      end loop;

      Put_Line("End of Preparing Thread");
      exception
            when E : others =>
                  DumpException(E);


end Buffer_Preparing_Task_Type;

-----------------
-- Voices_Type --
-----------------

protected body Voices_Type is

   -- allocate a new voice, and cleaning if necessary
      procedure Allocate_New_Voice(Voice_Structure : in Voice_Structure_Type;
                                   TheVoice : out Voice) is
         V : Voice;
      begin

      --for I in 1..Max_All_Opened_Voice_Indice loop
      --   -- try close finished ones
      --   if Is_Voice_Opened(TheVoice) and then Get_Voice(TheVoice).Stopped then
      --      Close_Voice(TheVoice);
      --   end if;

      --   if not Opened_Voice(Voice(i)) then
      --      Opened_Voice(Voice(i)) := true;
      --      TheVoice := Voice(i);
      --      All_Voices(TheVoice) := Voice_Structure;
      --      return;
      --   end if;
      --end loop;

      -- reach the Max_All_Opened_Voice_Indice, enlarge the process

      if Max_all_opened_Voice_Indice < MAX_VOICES then

         Max_All_Opened_Voice_Indice := Natural'Succ(Max_All_Opened_Voice_Indice);
         V := Voice(Max_All_Opened_Voice_Indice);

         Opened_Voice(V) := true;
            All_Voices(V) := Voice_Structure;
            TheVoice := V;
         return;

      end if;

      raise Program_Error with "Cannot allocate new voice";

   end Allocate_New_Voice;

   -- retreive the voice structure pointer
   function Get_Voice(V : Voice) return ReadOnly_Voice_Structure_Access is
   begin
      pragma Assert (Opened_Voice(V));
      return  All_Voices(V)'Unchecked_Access;
   end Get_Voice;

   -- is the voice Opened
   function Is_Voice_Opened(V : Voice) return Boolean is
   begin
      return Opened_Voice(V);
   end Is_Voice_Opened;




   -- close an opened voice
   procedure Close_Voice(V : Voice) is
   begin
      pragma Assert (Opened_Voice(V));
         if not Opened_Voice(V) then
            -- already closed
         return;
      end if;

      Opened_Voice(V) := False;
      All_Voices(V) := Null_Voice_Structure;

      --if V = Voice(Max_All_Opened_Voice_Indice) then
      --   while Max_All_Opened_Voice_Indice > 0 and then
      --     not Is_Voice_Opened(Voice(Max_All_Opened_Voice_Indice)) loop
      --      Max_All_Opened_Voice_Indice := Natural'Pred(Max_All_Opened_Voice_Indice);
      --   end loop;
      --end if;

   end Close_Voice;

   function Get_All_Opened_Voices return Voice_Array is
      I : Natural := 0;
      V : Voice_Array(1..Max_All_Opened_Voice_Indice);
   begin
      for It in 1..Max_All_Opened_Voice_Indice loop
         if Opened_Voice(Voice(It)) then
            I := Natural'Succ(I);
            V(I) := Voice(I);
         end if;
      end loop;
      return V(1..I);
   end;



      procedure Update_Position(V : Voice; Current_Sample_Position : Play_Second)  is
      begin
         All_Voices(V).Current_Sample_Position := Current_Sample_Position;
      end;


      function Get_All_Opened_Voices_Play_Structure return Voice_Play_Structure_Array is
         I : Natural := 0;
         V : Voice_Play_Structure_Array(1..Max_All_Opened_Voice_Indice);
      begin
         for It in 1..Max_All_Opened_Voice_Indice loop
            if Opened_Voice(Voice(It)) then
               I := Natural'Succ(I); -- increment the index
               declare
                  TheVoice : constant Voice := Voice(It);
               begin
                  V(I) := Voice_Play_Structure'(V => TheVoice,
                                             VSA =>  All_Voices(TheVoice)'Unchecked_Access,
                                             UpdatedPosition => 0.0,
                                             Closing => False);
               end;
            end if;
         end loop;
         return V(1..I);
      end;

      procedure Update_Close_And_Positions_Status(VA : Voice_Play_Structure_Array) is
      begin
         for V of VA loop
            declare
               TheVoice : constant Voice := V.V;
            begin

               if V.Closing then
                  Opened_Voice(TheVoice) := false;
                  All_Voices(TheVoice).Stopped := True;

               end if;

               All_Voices(TheVoice).Current_Sample_Position := V.UpdatedPosition;
            end;

         end loop;
      end;


end Voices_Type;

--------------------------------
-- Synthetizer_Structure_Type --
--------------------------------


protected body Synthetizer_Structure_Type is

   procedure Init(D : Synth.Driver.Sound_Driver_Access;
                  NBBuffer : Positive;
                  Buffer_Length : Positive) is
   begin
      -- init the buffers
      BR := new Buffer_Ring(NBBuffer, Buffer_Length);
      BR.Init;

      Voices := new Voices_Type;

      Play_Task := new Buffer_Play_Task_Type;
      Play_Task.Start(D  => D,
                      BR => BR);

      Prepare_Task := new Buffer_Preparing_Task_Type;
      Prepare_Task.Start(BR => BR, VSA => Voices);

      Inited := true;
   end;

      procedure Close is
      begin
         Play_Task.Stop;

         -- consume remaining buffers for not blocking the preparing task
         while BR.Available_Buffer_For_Consume loop
            declare
               Buffer : PCM_Frame_Array_Access;
            begin
               BR.Consume_Buffer(Buffer);
            end;
         end loop;

         Prepare_Task.Stop;

          -- consume remaining buffers for not blocking the preparing task
         while BR.Available_Buffer_For_Consume loop
            declare
               Buffer : PCM_Frame_Array_Access;
            begin
               BR.Consume_Buffer(Buffer);
            end;
         end loop;
      end;

   -- sanity check for opened synthetizer
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
     (S            : in     SoundSample;
      Frequency    : in     Float;
      Channel : in Natural := 1;
      Opened_Voice :    out Voice)
   is
      Allocated_Voice : Voice;
   begin
      Test_Inited;
      -- push the sample to tasks
      Put_Line("Allocate_New_Voice");
      Voices.Allocate_New_Voice( Voice_Structure_Type'(Note_Play_Frequency     => Frequency,
                                                       Play_Sample             => S,
                                                       Current_Sample_Position => 0.0,
                                                       Channel => Channel,
                                                       Stopped                 => False),
                                 TheVoice => Allocated_Voice );

      Put_Line("Allocated_Voice :"  & Voice'Image(Allocated_Voice));
      Opened_Voice := Allocated_Voice;
   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop (V : in Voice) is
      begin
         if Voices.Is_Voice_Opened(V) then
            Voices.Close_Voice(V);
         end if;
   end Stop;

   ---------------------------
   -- Get_All_Opened_Voices --
   ---------------------------

   function Get_All_Opened_Voices return Voice_Array is
      Empty : constant Voice_Array(2..1) := (others => 0);
   begin
      if not Inited then
         raise Synthetizer_Not_Inited;
      end if;

      return Empty;

   end Get_All_Opened_Voices;


end Synthetizer_Structure_Type;

-----------------
-- Fill_Buffer --
-----------------

function "/"(f : Play_Second; f2 : Float) return Play_Second is
begin
   return f / Play_Second(f2);
end;

function "*"(f : Play_Second; f2 : Float) return Play_Second is
begin
   return f * Play_Second(f2);
end;


procedure Process_Buffer (VSA : in ReadOnly_Voice_Structure_Access;
                          Buffer : in Frame_Array_Access;
                          Volume_Factor : in Float := 1.0;
                          ReachEndSample : out Boolean;
                          Returned_Current_Sample_Position : out Play_Second) is

   Driver_Play_Frequency : constant Frequency_Type := 44_100.0;

   Current_Sample_Position : Play_Second := VSA.Current_Sample_Position;

   Play_Period : constant Play_Second :=
     Play_Second (1) / Play_Second( Driver_Play_Frequency );

   One_Sample_Frame_Period : constant Play_Second :=
     Play_Second(1.0) /
     (Play_Second(Driver_Play_Frequency) *
          VSA.Play_Sample.Frequency /
            Driver_Play_Frequency);

   One_Played_Sample_Frame_Period : constant Play_Second :=
     Play_Second(One_Sample_Frame_Period) *
     VSA.Play_Sample.Note_Frequency /
       VSA.Note_Play_Frequency;

   function To_Second (Frame_Pos : in Natural) return Play_Second is
   begin
      return Play_Second (Frame_Pos - VSA.Play_Sample.Mono_Data'First ) *
        One_Played_Sample_Frame_Period;
   end To_Second;

   function To_Pos (Pos : Play_Second) return Natural is
      Pos_In_Array : constant Natural :=
        Natural (Pos / One_Played_Sample_Frame_Period);
   begin
      return VSA.Play_Sample.Mono_Data'First + Pos_In_Array;
   end To_Pos;

   procedure Move_Next
     (Pos       : in out Play_Second;
      Reach_End :    out Boolean)
   is
   begin
      -- increment

      Pos := Pos + Play_Period;

      Reach_End := False;

      case VSA.Play_Sample.HasLoop is
         when True =>
            if Pos > To_Second (VSA.Play_Sample.Loop_End) then
               Pos := Pos - To_Second (VSA.Play_Sample.Loop_Start);
            end if;
         when False =>
            declare
               Sample_Data_Seconds : constant Play_Second :=
                 To_Second(VSA.Play_Sample.Mono_Data'Length);
            begin

               if Pos > Sample_Data_Seconds then
                  Reach_End := True;
               end if;
            end;

      end case;
   end Move_Next;

begin

   if VSA.Stopped then
      ReachEndSample := True;
      return;
   end if;

   if VSA.Play_Sample = Null_Sound_Sample then
      Put_Line("Null sample");
      return;
   end if;

   for i in Buffer'Range loop

      -- interpolate ?
      declare
         p  : constant Natural        := To_Pos (Current_Sample_Position);
         V1 : constant Float := VSA.Play_Sample.Mono_Data (p);
         R  : Boolean;
         F : Float :=  Buffer (i) + V1 * Volume_Factor;
      begin

         if F > 1.0 then
            F := 1.0;  -- saturation
         end if;

         if F < -1.0 then
            F := -1.0;  -- saturation
         end if;

         Buffer (i) := F;

         Move_Next (Pos => Current_Sample_Position, Reach_End => R);
         if R then
            ReachEndSample := True;
            Returned_Current_Sample_Position := Current_Sample_Position;
            return;
         end if;
      end;
   end loop;
   ReachEndSample := False;
   Returned_Current_Sample_Position := Current_Sample_Position;

end Process_Buffer;


end Synth.Synthetizer;
