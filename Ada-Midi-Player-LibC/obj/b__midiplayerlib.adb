pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (midiplayerlibmain, Spec_File_Name => "b__midiplayerlib.ads");
pragma Source_File_Name (midiplayerlibmain, Body_File_Name => "b__midiplayerlib.adb");
pragma Suppress (Overflow_Check);

package body midiplayerlibmain is

   E76 : Short_Integer; pragma Import (Ada, E76, "system__os_lib_E");
   E14 : Short_Integer; pragma Import (Ada, E14, "system__soft_links_E");
   E26 : Short_Integer; pragma Import (Ada, E26, "system__exception_table_E");
   E71 : Short_Integer; pragma Import (Ada, E71, "ada__io_exceptions_E");
   E56 : Short_Integer; pragma Import (Ada, E56, "ada__strings_E");
   E41 : Short_Integer; pragma Import (Ada, E41, "ada__containers_E");
   E28 : Short_Integer; pragma Import (Ada, E28, "system__exceptions_E");
   E58 : Short_Integer; pragma Import (Ada, E58, "ada__strings__maps_E");
   E62 : Short_Integer; pragma Import (Ada, E62, "ada__strings__maps__constants_E");
   E46 : Short_Integer; pragma Import (Ada, E46, "interfaces__c_E");
   E22 : Short_Integer; pragma Import (Ada, E22, "system__soft_links__initialize_E");
   E82 : Short_Integer; pragma Import (Ada, E82, "system__object_reader_E");
   E51 : Short_Integer; pragma Import (Ada, E51, "system__dwarf_lines_E");
   E40 : Short_Integer; pragma Import (Ada, E40, "system__traceback__symbolic_E");
   E02 : Short_Integer; pragma Import (Ada, E02, "midiplayerlib_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure midiplayerlibfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      null;
   end midiplayerlibfinal;

   type No_Param_Proc is access procedure;

   procedure midiplayerlibinit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      midiplayerlibmain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E14 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E26 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E26 := E26 + 1;
      if E71 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E71 := E71 + 1;
      if E56 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E56 := E56 + 1;
      if E41 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E41 := E41 + 1;
      if E28 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E28 := E28 + 1;
      if E76 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E76 := E76 + 1;
      if E58 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      if E62 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E62 := E62 + 1;
      if E46 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      if E22 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E22 := E22 + 1;
      E14 := E14 + 1;
      E58 := E58 + 1;
      E46 := E46 + 1;
      if E82 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      if E51 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      E51 := E51 + 1;
      if E40 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E40 := E40 + 1;
      E82 := E82 + 1;
      E02 := E02 + 1;
   end midiplayerlibinit;

--  BEGIN Object file/option list
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player-Lib/obj/midiplayerlib.o
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player-Lib/obj/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/drivers/alsa-binding/obj/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player/obj/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/lib/
   --   -L/home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi/lib/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/9/adalib/
   --   -shared
   --   -lgnat-9
   --   -ldl
--  END Object file/option list   

end midiplayerlibmain;
