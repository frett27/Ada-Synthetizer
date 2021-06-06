pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (midiplayermain, Spec_File_Name => "b__midiplayer.ads");
pragma Source_File_Name (midiplayermain, Body_File_Name => "b__midiplayer.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body midiplayermain is

   E076 : Short_Integer; pragma Import (Ada, E076, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "system__soft_links_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "system__exception_table_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__io_exceptions_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__strings_E");
   E041 : Short_Integer; pragma Import (Ada, E041, "ada__containers_E");
   E028 : Short_Integer; pragma Import (Ada, E028, "system__exceptions_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__strings__maps_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__strings__maps__constants_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "interfaces__c_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__soft_links__initialize_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "system__object_reader_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__dwarf_lines_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "system__traceback__symbolic_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "ada__numerics_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__tags_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__streams_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "gnat_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "interfaces__c__strings_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "system__file_control_block_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "system__finalization_root_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "ada__finalization_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "system__file_io_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "ada__streams__stream_io_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "system__storage_pools_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "system__finalization_masters_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__storage_pools__subpools_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "ada__strings__unbounded_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "system__task_info_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "system__task_primitives__operations_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "system__regpat_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "ada__calendar_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "ada__calendar__delays_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "ada__real_time_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__text_io_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "gnat__directory_operations_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "system__assertions_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "system__direct_io_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__pool_global_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "system__regexp_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "gnat__command_line_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "system__sequential_io_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__tasking__initialization_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "system__tasking__protected_objects_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "system__tasking__protected_objects__entries_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "system__tasking__queuing_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "system__tasking__stages_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "bzip2_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "bzip2__decoding_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "lzma__decoding_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "midi_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "midi__file_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "sound__alsa_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "sound__mono_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "synth_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "midi__stream_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "synth__driver_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "synth__driver__alsa_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "synth__synthetizer_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "synth__wav_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "synth__driver__wav_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "zip_streams_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "zip_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "zip__headers_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "zip__crc_crypto_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "unzip_E");
   E292 : Short_Integer; pragma Import (Ada, E292, "unzip__decompress_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "unzip__decompress__huffman_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "unzip__streams_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "synth__soundbank_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "midi__player_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "midiplayerlib_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "midi__player__finalize_body");
      begin
         E146 := E146 - 1;
         if E146 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "synth__soundbank__finalize_body");
      begin
         E283 := E283 - 1;
         if E283 = 0 then
            F2;
         end if;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "synth__soundbank__finalize_spec");
      begin
         if E283 = 0 then
            F3;
         end if;
      end;
      E335 := E335 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "unzip__streams__finalize_spec");
      begin
         if E335 = 0 then
            F4;
         end if;
      end;
      E290 := E290 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "unzip__finalize_spec");
      begin
         if E290 = 0 then
            F5;
         end if;
      end;
      E331 := E331 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "zip_streams__finalize_spec");
      begin
         if E331 = 0 then
            F6;
         end if;
      end;
      E245 := E245 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "synth__driver__wav__finalize_spec");
      begin
         if E245 = 0 then
            F7;
         end if;
      end;
      E249 := E249 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "synth__wav__finalize_spec");
      begin
         if E249 = 0 then
            F8;
         end if;
      end;
      E251 := E251 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "synth__synthetizer__finalize_spec");
      begin
         if E251 = 0 then
            F9;
         end if;
      end;
      E233 := E233 - 1;
      E235 := E235 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "synth__driver__alsa__finalize_spec");
      begin
         if E235 = 0 then
            F10;
         end if;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "synth__driver__finalize_spec");
      begin
         if E233 = 0 then
            F11;
         end if;
      end;
      E219 := E219 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "midi__stream__finalize_spec");
      begin
         if E219 = 0 then
            F12;
         end if;
      end;
      E213 := E213 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "midi__file__finalize_spec");
      begin
         if E213 = 0 then
            F13;
         end if;
      end;
      E006 := E006 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "midi__finalize_spec");
      begin
         if E006 = 0 then
            F14;
         end if;
      end;
      E263 := E263 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         if E263 = 0 then
            F15;
         end if;
      end;
      E217 := E217 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__sequential_io__finalize_spec");
      begin
         if E217 = 0 then
            F16;
         end if;
      end;
      E207 := E207 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__regexp__finalize_spec");
      begin
         if E207 = 0 then
            F17;
         end if;
      end;
      E134 := E134 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "system__pool_global__finalize_spec");
      begin
         if E134 = 0 then
            F18;
         end if;
      end;
      E299 := E299 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__direct_io__finalize_spec");
      begin
         if E299 = 0 then
            F19;
         end if;
      end;
      E106 := E106 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "ada__text_io__finalize_spec");
      begin
         if E106 = 0 then
            F20;
         end if;
      end;
      E184 := E184 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "ada__strings__unbounded__finalize_spec");
      begin
         if E184 = 0 then
            F21;
         end if;
      end;
      E124 := E124 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__storage_pools__subpools__finalize_spec");
      begin
         if E124 = 0 then
            F22;
         end if;
      end;
      E126 := E126 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "system__finalization_masters__finalize_spec");
      begin
         if E126 = 0 then
            F23;
         end if;
      end;
      E142 := E142 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "ada__streams__stream_io__finalize_spec");
      begin
         if E142 = 0 then
            F24;
         end if;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "system__file_io__finalize_body");
      begin
         E112 := E112 - 1;
         if E112 = 0 then
            F25;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure midiplayerfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      finalize_library;
   end midiplayerfinal;

   type No_Param_Proc is access procedure;

   procedure midiplayerinit is
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
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, False, False, False, True, True, True, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, False, True, True, False, True, 
           True, False, True, True, True, True, False, True, 
           False, False, False, True, False, True, True, False, 
           True, True, True, True, False, True, False, True, 
           True, False, False, True, False, True, True, False, 
           False, False, False, True, True, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           False, True, False, False, False, True, True, True, 
           True, False, True, False),
         Count => (0, 0, 0, 3, 7, 6, 2, 0, 1, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
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

      midiplayermain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E014 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E026 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E026 := E026 + 1;
      if E071 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E071 := E071 + 1;
      if E056 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E056 := E056 + 1;
      if E041 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E041 := E041 + 1;
      if E028 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E028 := E028 + 1;
      if E076 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E076 := E076 + 1;
      if E058 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      if E062 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E062 := E062 + 1;
      if E046 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      if E022 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E022 := E022 + 1;
      E014 := E014 + 1;
      E058 := E058 + 1;
      E046 := E046 + 1;
      if E082 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      if E051 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      E051 := E051 + 1;
      if E040 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E040 := E040 + 1;
      E082 := E082 + 1;
      if E222 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E222 := E222 + 1;
      if E100 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E100 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E100 := E100 + 1;
      if E108 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E108 := E108 + 1;
      if E191 = 0 then
         Gnat'Elab_Spec;
      end if;
      E191 := E191 + 1;
      if E240 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E240 := E240 + 1;
      if E116 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E116 := E116 + 1;
      if E115 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E115 := E115 + 1;
      if E113 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E113 := E113 + 1;
      if E112 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E112 := E112 + 1;
      if E142 = 0 then
         Ada.Streams.Stream_Io'Elab_Spec;
      end if;
      E142 := E142 + 1;
      if E130 = 0 then
         System.Storage_Pools'Elab_Spec;
      end if;
      E130 := E130 + 1;
      if E126 = 0 then
         System.Finalization_Masters'Elab_Spec;
      end if;
      if E126 = 0 then
         System.Finalization_Masters'Elab_Body;
      end if;
      E126 := E126 + 1;
      if E124 = 0 then
         System.Storage_Pools.Subpools'Elab_Spec;
      end if;
      E124 := E124 + 1;
      if E184 = 0 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E184 := E184 + 1;
      if E172 = 0 then
         System.Task_Info'Elab_Spec;
      end if;
      E172 := E172 + 1;
      if E164 = 0 then
         System.Task_Primitives.Operations'Elab_Body;
      end if;
      E164 := E164 + 1;
      if E286 = 0 then
         System.Regpat'Elab_Spec;
      end if;
      E286 := E286 + 1;
      if E255 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E255 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E255 := E255 + 1;
      if E253 = 0 then
         Ada.Calendar.Delays'Elab_Body;
      end if;
      E253 := E253 + 1;
      if E155 = 0 then
         Ada.Real_Time'Elab_Spec;
      end if;
      if E155 = 0 then
         Ada.Real_Time'Elab_Body;
      end if;
      E155 := E155 + 1;
      if E106 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E106 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E106 := E106 + 1;
      if E195 = 0 then
         Gnat.Directory_Operations'Elab_Spec;
      end if;
      if E195 = 0 then
         Gnat.Directory_Operations'Elab_Body;
      end if;
      E195 := E195 + 1;
      if E228 = 0 then
         System.Assertions'Elab_Spec;
      end if;
      E228 := E228 + 1;
      if E299 = 0 then
         System.Direct_Io'Elab_Spec;
      end if;
      E299 := E299 + 1;
      if E134 = 0 then
         System.Pool_Global'Elab_Spec;
      end if;
      E134 := E134 + 1;
      if E207 = 0 then
         System.Regexp'Elab_Spec;
      end if;
      E207 := E207 + 1;
      if E193 = 0 then
         Gnat.Command_Line'Elab_Spec;
      end if;
      if E193 = 0 then
         Gnat.Command_Line'Elab_Body;
      end if;
      E193 := E193 + 1;
      if E217 = 0 then
         System.Sequential_Io'Elab_Spec;
      end if;
      E217 := E217 + 1;
      if E267 = 0 then
         System.Tasking.Initialization'Elab_Body;
      end if;
      E267 := E267 + 1;
      if E257 = 0 then
         System.Tasking.Protected_Objects'Elab_Body;
      end if;
      E257 := E257 + 1;
      if E263 = 0 then
         System.Tasking.Protected_Objects.Entries'Elab_Spec;
      end if;
      E263 := E263 + 1;
      if E275 = 0 then
         System.Tasking.Queuing'Elab_Body;
      end if;
      E275 := E275 + 1;
      if E281 = 0 then
         System.Tasking.Stages'Elab_Body;
      end if;
      E281 := E281 + 1;
      E294 := E294 + 1;
      E296 := E296 + 1;
      E301 := E301 + 1;
      if E006 = 0 then
         Midi'Elab_Spec;
      end if;
      if E006 = 0 then
         Midi'Elab_Body;
      end if;
      E006 := E006 + 1;
      if E213 = 0 then
         Midi.File'Elab_Spec;
      end if;
      if E213 = 0 then
         Midi.File'Elab_Body;
      end if;
      E213 := E213 + 1;
      E242 := E242 + 1;
      E238 := E238 + 1;
      if E221 = 0 then
         Synth'Elab_Spec;
      end if;
      E221 := E221 + 1;
      if E219 = 0 then
         Midi.Stream'Elab_Spec;
      end if;
      E219 := E219 + 1;
      if E233 = 0 then
         Synth.Driver'Elab_Spec;
      end if;
      if E235 = 0 then
         Synth.Driver.Alsa'Elab_Spec;
      end if;
      if E235 = 0 then
         Synth.Driver.Alsa'Elab_Body;
      end if;
      E235 := E235 + 1;
      E233 := E233 + 1;
      if E251 = 0 then
         Synth.Synthetizer'Elab_Spec;
      end if;
      if E251 = 0 then
         Synth.Synthetizer'Elab_Body;
      end if;
      E251 := E251 + 1;
      if E249 = 0 then
         Synth.Wav'Elab_Spec;
      end if;
      if E249 = 0 then
         Synth.Wav'Elab_Body;
      end if;
      E249 := E249 + 1;
      if E245 = 0 then
         Synth.Driver.Wav'Elab_Spec;
      end if;
      if E245 = 0 then
         Synth.Driver.Wav'Elab_Body;
      end if;
      E245 := E245 + 1;
      if E331 = 0 then
         Zip_Streams'Elab_Spec;
      end if;
      if E331 = 0 then
         Zip_Streams'Elab_Body;
      end if;
      E331 := E331 + 1;
      if E313 = 0 then
         Zip'Elab_Spec;
      end if;
      if E329 = 0 then
         Zip.Headers'Elab_Spec;
      end if;
      E329 := E329 + 1;
      E313 := E313 + 1;
      E333 := E333 + 1;
      if E290 = 0 then
         Unzip'Elab_Spec;
      end if;
      if E311 = 0 then
         Unzip.Decompress.Huffman'Elab_Spec;
      end if;
      E311 := E311 + 1;
      E290 := E290 + 1;
      E292 := E292 + 1;
      if E335 = 0 then
         Unzip.Streams'Elab_Spec;
      end if;
      if E335 = 0 then
         Unzip.Streams'Elab_Body;
      end if;
      E335 := E335 + 1;
      if E283 = 0 then
         Synth.Soundbank'Elab_Spec;
      end if;
      if E283 = 0 then
         Synth.Soundbank'Elab_Body;
      end if;
      E283 := E283 + 1;
      if E146 = 0 then
         Midi.Player'Elab_Body;
      end if;
      E146 := E146 + 1;
      E002 := E002 + 1;
   end midiplayerinit;

--  BEGIN Object file/option list
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/bzip2.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/bzip2-decoding.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/lzma.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/lzma-decoding.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/drivers/alsa-binding/obj/sound.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/drivers/alsa-binding/obj/sound-constants.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/drivers/alsa-binding/obj/sound-alsa.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/drivers/alsa-binding/obj/sound-mono.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player/obj/midi-stream.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth-driver-alsa.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth-driver.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth-synthetizer.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth-wav.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer/obj/synth-driver-wav.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/zip_streams.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/zip-headers.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/zip.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/zip-crc_crypto.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/unzip-decompress-huffman.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/unzip.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/unzip-decompress.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/unzip-streams.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Synthetizer-Bank/obj/synth-soundbank.o
   --   /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player/obj/midi-player.o
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
   --   -lgnarl-9
   --   -lgnat-9
   --   -lpthread
   --   -lrt
   --   -ldl
--  END Object file/option list   

end midiplayermain;
