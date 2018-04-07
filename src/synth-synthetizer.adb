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
             Buffer_Length => 10000);



   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Synthetizer_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Close unimplemented");
      raise Program_Error with "Unimplemented procedure Close";
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
   begin

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Play unimplemented");
      raise Program_Error with "Unimplemented procedure Play";

   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop (Opened_Voice : in Voice) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Stop unimplemented");
      raise Program_Error with "Unimplemented procedure Stop";
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

      --------------------
      -- Consume_Buffer --
      --------------------

      entry Consume_Buffer(Buffer : out PCM_Frame_Array_Access)
        when Available_For_Consume(Current_Consume)  is
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

      entry Freeze_New_Buffer (Buffer : out Frame_Array_Access) when Free_Buffers > 0
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

   task body Buffer_Play_Task_Type is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Buffer_Play_Task_Type unimplemented");
      raise Program_Error with "Unimplemented task Buffer_Play_Task_Type";
   end Buffer_Play_Task_Type;

   --------------------------------
   -- Buffer_Preparing_Task_Type --
   --------------------------------

   task body Buffer_Preparing_Task_Type is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Buffer_Preparing_Task_Type unimplemented");
      raise Program_Error with "Unimplemented task Buffer_Preparing_Task_Type";
   end Buffer_Preparing_Task_Type;

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

         Play_Task := new Buffer_Play_Task_Type;
         Play_Task.Start(D  => D,
                         BR => BR);

         Prepare_Task := new Buffer_Preparing_Task_Type;
         Prepare_Task.Start(BR => BR);

         Inited := true;
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
         Opened_Voice :    out Voice)
      is
      begin
         Test_Inited;


         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Play unimplemented");
         raise Program_Error with "Unimplemented procedure Play";
      end Play;

      ----------
      -- Stop --
      ----------

      procedure Stop (V : in Voice) is
      begin
         Test_Inited;

         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Stop unimplemented");
         raise Program_Error with "Unimplemented procedure Stop";
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

   procedure Process_Buffer (VSA : in Voice_Structure_Access;
                             Buffer : in Frame_Array_Access;
                             Volume_Factor : in Float := 1.0) is

      Driver_Play_Frequency : constant Frequency_Type := 44_100.0;

      Current_Sample_Position : Play_Second := 0.0;

      Play_Period : constant Play_Second :=
        Play_Second (1) / Float (Driver_Play_Frequency);

      One_Sample_Frame_Period : constant Play_Second :=
        1.0 /
          Float (Driver_Play_Frequency) *
        Float (VSA.Play_Sample.Frequency) /
        Float (Driver_Play_Frequency);

      One_Played_Sample_Frame_Period : constant Play_Second :=
        One_Sample_Frame_Period *
          Float (VSA.Play_Sample.Note_Frequency) /
        Float (VSA.Note_Play_Frequency);

      function To_Second (Frame_Pos : in Natural) return Play_Second is
      begin
         return Play_Second (Frame_Pos -  VSA.Play_Sample.Mono_Data'First) *
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
               if Pos > To_Second (VSA.Play_Sample.Mono_Data.all'Length) then
                  Reach_End := True;
               end if;
         end case;
      end Move_Next;

   begin

      if VSA.Stopped then
         return;
      end if;

      for i in Buffer'Range loop

         -- interpolate

         declare
            p  : constant Natural        := To_Pos (Current_Sample_Position);
            V1 : constant Float := VSA.Play_Sample.Mono_Data (p);
            R  : Boolean;
         begin

            Buffer (i) := Buffer (i) + V1 * Volume_Factor;

            if Buffer(i) > 1.0 then
               -- put_line ("saturation " & Float'Image(Buffer(i)));
               Buffer(i) := 1.0;  -- saturation
            end if;

            if Buffer(i) < -1.0 then
               -- put_line ("saturation " & Float'Image(Buffer(i)));
               Buffer(i) := -1.0;  -- saturation
            end if;

            Move_Next (Pos => Current_Sample_Position, Reach_End => R);
            if R then
               VSA.Stopped := True;
               return;
            end if;
         end;
      end loop;
   end Process_Buffer;


end Synth.Synthetizer;
