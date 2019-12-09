pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Pointers;

package Soundio is

   SOUNDIO_MAX_CHANNELS : constant := 24;

   SoundIo_Exception : exception;

   type SoundIo_Error is
     (Error_None,
      Error_NoMem,
      Error_InitAudioBackend,
      Error_SystemResources,
      Error_OpeningDevice,
      Error_NoSuchDevice,
      Error_Invalid,
      Error_BackendUnavailable,
      Error_Streaming,
      Error_IncompatibleDevice,
      Error_NoSuchClient,
      Error_IncompatibleBackend,
      Error_BackendDisconnected,
      Error_Interrupted,
      Error_Underflow,
      Error_EncodingString);
   pragma Convention (C, SoundIo_Error);  -- /home/amiard/build/libsoundio/soundio/soundio.h:72

   type SoundIo_Channel_Id is
     (Channel_Id_Invalid,
      Channel_Id_FrontLeft,
      Channel_Id_FrontRight,
      Channel_Id_FrontCenter,
      Channel_Id_Lfe,
      Channel_Id_BackLeft,
      Channel_Id_BackRight,
      Channel_Id_FrontLeftCenter,
      Channel_Id_FrontRightCenter,
      Channel_Id_BackCenter,
      Channel_Id_SideLeft,
      Channel_Id_SideRight,
      Channel_Id_TopCenter,
      Channel_Id_TopFrontLeft,
      Channel_Id_TopFrontCenter,
      Channel_Id_TopFrontRight,
      Channel_Id_TopBackLeft,
      Channel_Id_TopBackCenter,
      Channel_Id_TopBackRight,
      Channel_Id_BackLeftCenter,
      Channel_Id_BackRightCenter,
      Channel_Id_FrontLeftWide,
      Channel_Id_FrontRightWide,
      Channel_Id_FrontLeftHigh,
      Channel_Id_FrontCenterHigh,
      Channel_Id_FrontRightHigh,
      Channel_Id_TopFrontLeftCenter,
      Channel_Id_TopFrontRightCenter,
      Channel_Id_TopSideLeft,
      Channel_Id_TopSideRight,
      Channel_Id_LeftLfe,
      Channel_Id_RightLfe,
      Channel_Id_Lfe2,
      Channel_Id_BottomCenter,
      Channel_Id_BottomLeftCenter,
      Channel_Id_BottomRightCenter,
      Channel_Id_MsMid,
      Channel_Id_MsSide,
      Channel_Id_AmbisonicW,
      Channel_Id_AmbisonicX,
      Channel_Id_AmbisonicY,
      Channel_Id_AmbisonicZ,
      Channel_Id_XyX,
      Channel_Id_XyY,
      Channel_Id_HeadphonesLeft,
      Channel_Id_HeadphonesRight,
      Channel_Id_ClickTrack,
      Channel_Id_ForeignLanguage,
      Channel_Id_HearingImpaired,
      Channel_Id_Narration,
      Channel_Id_Haptic,
      Channel_Id_DialogCentricMix,
      Channel_Id_Aux,
      Channel_Id_Aux0,
      Channel_Id_Aux1,
      Channel_Id_Aux2,
      Channel_Id_Aux3,
      Channel_Id_Aux4,
      Channel_Id_Aux5,
      Channel_Id_Aux6,
      Channel_Id_Aux7,
      Channel_Id_Aux8,
      Channel_Id_Aux9,
      Channel_Id_Aux10,
      Channel_Id_Aux11,
      Channel_Id_Aux12,
      Channel_Id_Aux13,
      Channel_Id_Aux14,
      Channel_Id_Aux15);
   pragma Convention (C, SoundIo_Channel_Id);  -- /home/amiard/build/libsoundio/soundio/soundio.h:106

   type SoundIo_Channel_Layout_Id is
     (Channel_Layout_Id_Mono,
      Channel_Layout_Id_Stereo,
      Channel_Layout_Id_2Point1,
      Channel_Layout_Id_3Point0,
      Channel_Layout_Id_3Point0Back,
      Channel_Layout_Id_3Point1,
      Channel_Layout_Id_4Point0,
      Channel_Layout_Id_Quad,
      Channel_Layout_Id_QuadSide,
      Channel_Layout_Id_4Point1,
      Channel_Layout_Id_5Point0Back,
      Channel_Layout_Id_5Point0Side,
      Channel_Layout_Id_5Point1,
      Channel_Layout_Id_5Point1Back,
      Channel_Layout_Id_6Point0Side,
      Channel_Layout_Id_6Point0Front,
      Channel_Layout_Id_Hexagonal,
      Channel_Layout_Id_6Point1,
      Channel_Layout_Id_6Point1Back,
      Channel_Layout_Id_6Point1Front,
      Channel_Layout_Id_7Point0,
      Channel_Layout_Id_7Point0Front,
      Channel_Layout_Id_7Point1,
      Channel_Layout_Id_7Point1Wide,
      Channel_Layout_Id_7Point1WideBack,
      Channel_Layout_Id_Octagonal);
   pragma Convention (C, SoundIo_Channel_Layout_Id);  -- /home/amiard/build/libsoundio/soundio/soundio.h:189

   type SoundIo_Backend is
     (Backend_None,
      Backend_Jack,
      Backend_PulseAudio,
      Backend_Alsa,
      Backend_CoreAudio,
      Backend_Wasapi,
      Backend_Dummy);
   pragma Convention (C, SoundIo_Backend);  -- /home/amiard/build/libsoundio/soundio/soundio.h:218

   type SoundIo_Device_Aim is
     (Device_AimInput,
      Device_AimOutput);
   pragma Convention (C, SoundIo_Device_Aim);  -- /home/amiard/build/libsoundio/soundio/soundio.h:228

   type SoundIo_Format is
     (Format_Invalid,
      Format_S8,
      Format_U8,
      Format_S16LE,
      Format_S16BE,
      Format_U16LE,
      Format_U16BE,
      Format_S24LE,
      Format_S24BE,
      Format_U24LE,
      Format_U24BE,
      Format_S32LE,
      Format_S32BE,
      Format_U32LE,
      Format_U32BE,
      Format_Float32LE,
      Format_Float32BE,
      Format_Float64LE,
      Format_Float64BE);
   pragma Convention (C, SoundIo_Format);  -- /home/amiard/build/libsoundio/soundio/soundio.h:235

   use System;
   DFE : System.Bit_Order renames System.Default_Bit_Order;

   procedure Check_Error (Error : SoundIo_Error);

   function Format_S16NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S16LE else Format_S16BE);

   function Format_U16NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_U16LE else Format_U16BE);

   function Format_S24NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S24LE else Format_S24BE);

   function Format_U24NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_U24LE else Format_U24BE);

   function Format_S32NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S32LE else Format_S32BE);

   function Format_U32NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_U32LE else Format_U32BE);

   function Format_Float32NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_Float32LE else Format_Float32BE);

   function Format_Float64NE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_Float64LE else Format_Float64BE);

   function Format_S16FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S16BE else Format_S16LE);

   function Format_U16FE return SoundIo_Format is
     (if DFE   = Low_Order_First then Format_U16BE else Format_U16LE);

   function Format_S24FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S24BE else Format_S24LE);

   function Format_U24FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_U24BE else Format_U24LE);

   function Format_S32FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_S32BE else Format_S32LE);

   function Format_U32FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_U32BE else Format_U32LE);

   function Format_Float32FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_Float32BE else Format_Float32LE);

   function Format_Float64FE return SoundIo_Format is
     (if DFE = Low_Order_First then Format_Float64BE else Format_Float64LE);

   type Channels_Array is array (0 .. 23) of aliased SoundIo_Channel_Id;
   type SoundIo_Channel_Layout is record
      Name : Interfaces.C.Strings.chars_ptr;
      Channel_Count : aliased int;
      Channels : aliased Channels_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Channel_Layout);

   type SoundIo_Sample_Rate_Range is record
      min : aliased int;
      max : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Sample_Rate_Range);

   type Chars_Array is array (int range <>) of aliased Character;
   pragma Convention (C, Chars_Array);

   package Chars_Ptrs is new Interfaces.C.Pointers
     (int, Character, Chars_Array, ASCII.NUL);

   subtype Chars_Ptr is Chars_Ptrs.Pointer;

   type SoundIo_Channel_Area is record
      Ptr  : Chars_Ptr;
      Step : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Channel_Area);

   type Soundio_Channel_Area_Array
   is array (Natural range <>) of aliased SoundIo_Channel_Area;
   pragma Convention (C, Soundio_Channel_Area_Array);

   No_Area : SoundIo_Channel_Area := (Ptr => null, Step => 0);

   package Soundio_Channel_Area_Ptrs is new Interfaces.C.Pointers
     (Natural, Soundio_Channel_Area, Soundio_Channel_Area_Array, No_Area);

   subtype SoundIo_Channel_Area_Ptr is Soundio_Channel_Area_Ptrs.Pointer;

   function Get_Area
     (Areas : SoundIo_Channel_Area_Ptr;
      Index : int) return SoundIo_Channel_Area;

   generic
      type Sample_Type is private;
   procedure Write_Sample
     (Area    : SoundIo_Channel_Area;
      Index   : int;
      Sample  : Sample_Type);

   type SoundIo is record
      userdata : System.Address;  -- /home/amiard/build/libsoundio/soundio/soundio.h:326
      on_devices_change : access procedure (arg1 : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:329
      on_backend_disconnect : access procedure (arg1 : access SoundIo; arg2 : int);  -- /home/amiard/build/libsoundio/soundio/soundio.h:346
      on_events_signal : access procedure (arg1 : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:350
      current_backend : aliased SoundIo_Backend;  -- /home/amiard/build/libsoundio/soundio/soundio.h:354
      app_name : Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:360
      emit_rtprio_warning : access procedure;  -- /home/amiard/build/libsoundio/soundio/soundio.h:368
      jack_info_callback : access procedure (arg1 : Interfaces.C.Strings.chars_ptr);  -- /home/amiard/build/libsoundio/soundio/soundio.h:376
      jack_error_callback : access procedure (arg1 : Interfaces.C.Strings.chars_ptr);  -- /home/amiard/build/libsoundio/soundio/soundio.h:379
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:324

   type SoundIo_Device is record
      The_Soundio : access SoundIo;  -- /home/amiard/build/libsoundio/soundio/soundio.h:385
      id : Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:395
      name : Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:397
      aim : aliased SoundIo_Device_Aim;  -- /home/amiard/build/libsoundio/soundio/soundio.h:400
      layouts : access SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:407
      layout_count : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:408
      current_layout : aliased SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:410
      formats : access SoundIo_Format;  -- /home/amiard/build/libsoundio/soundio/soundio.h:414
      format_count : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:416
      current_format : aliased SoundIo_Format;  -- /home/amiard/build/libsoundio/soundio/soundio.h:435
      sample_rates : access SoundIo_Sample_Rate_Range;  -- /home/amiard/build/libsoundio/soundio/soundio.h:443
      sample_rate_count : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:447
      sample_rate_current : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:450
      software_latency_min : aliased double;  -- /home/amiard/build/libsoundio/soundio/soundio.h:456
      software_latency_max : aliased double;  -- /home/amiard/build/libsoundio/soundio/soundio.h:461
      software_latency_current : aliased double;  -- /home/amiard/build/libsoundio/soundio/soundio.h:467
      is_raw : aliased Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:474
      ref_count : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:478
      probe_error : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:489
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Device);  -- /home/amiard/build/libsoundio/soundio/soundio.h:383

   type SoundIo_Out_Stream is record
      Device             : access SoundIo_Device;
      Format             : aliased SoundIo_Format;
      Sample_Rate        : aliased int;
      Layout             : aliased SoundIo_Channel_Layout;
      Software_Latency   : aliased double;
      User_Data          : System.Address;

      Write_Callback     : access procedure
           (Out_Stream      : access SoundIo_Out_Stream;
            Frame_Count_Min : int;
            Frame_Count_Max : int);

      Underflow_Callback : access procedure (arg1 : access SoundIo_Out_Stream);
      Error_Callback     : access procedure (arg1 : access SoundIo_Out_Stream; arg2 : int);
      Name               : Interfaces.C.Strings.chars_ptr;
      Non_Terminal_Hint  : aliased Extensions.bool;
      Bytes_Per_Frame    : aliased int;
      Bytes_Per_Sample   : aliased int;
      Layout_Error       : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Out_Stream);  -- /home/amiard/build/libsoundio/soundio/soundio.h:493

   type SoundIo_In_Stream is record
      Device : access SoundIo_Device;  -- /home/amiard/build/libsoundio/soundio/soundio.h:597
      format : aliased SoundIo_Format;  -- /home/amiard/build/libsoundio/soundio/soundio.h:601
      sample_rate : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:605
      layout : aliased SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:609
      software_latency : aliased double;  -- /home/amiard/build/libsoundio/soundio/soundio.h:623
      userdata : System.Address;  -- /home/amiard/build/libsoundio/soundio/soundio.h:626
      read_callback : access procedure
           (arg1 : access SoundIo_In_Stream;
            arg2 : int;
            arg3 : int);  -- /home/amiard/build/libsoundio/soundio/soundio.h:639
      overflow_callback : access procedure (arg1 : access SoundIo_In_Stream);  -- /home/amiard/build/libsoundio/soundio/soundio.h:644
      error_callback : access procedure (arg1 : access SoundIo_In_Stream; arg2 : int);  -- /home/amiard/build/libsoundio/soundio/soundio.h:651
      name : Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:659
      non_terminal_hint : aliased Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:664
      bytes_per_frame : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:667
      bytes_per_sample : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:669
      layout_error : aliased int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:673
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo_In_Stream);  -- /home/amiard/build/libsoundio/soundio/soundio.h:595

   function Version_String return Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:677
   pragma Import (C, Version_String, "soundio_version_string");

   function Version_Major return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:679
   pragma Import (C, Version_Major, "soundio_version_major");

   function Version_Minor return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:681
   pragma Import (C, Version_Minor, "soundio_version_minor");

   function Version_Patch return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:683
   pragma Import (C, Version_Patch, "soundio_version_patch");

   function Create return access SoundIo;  -- /home/amiard/build/libsoundio/soundio/soundio.h:689
   pragma Import (C, Create, "soundio_create");

   procedure Destroy (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:690
   pragma Import (C, Destroy, "soundio_destroy");

   function Connect (The_Soundio : access SoundIo) return SoundIo_Error;
   pragma Import (C, Connect, "soundio_connect");

   function Connect_Backend (The_Soundio : access SoundIo; Backend : SoundIo_Backend) return SoundIo_Error;  -- /home/amiard/build/libsoundio/soundio/soundio.h:712
   pragma Import (C, Connect_Backend, "soundio_connect_backend");

   procedure Disconnect (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:713
   pragma Import (C, Disconnect, "soundio_disconnect");

   function Error_String (Error : int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Error_String, "soundio_strerror");

   function Backend_Name (Backend : SoundIo_Backend) return Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:718
   pragma Import (C, Backend_Name, "soundio_backend_name");

   function Backend_Count (The_Soundio : access SoundIo) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:721
   pragma Import (C, Backend_Count, "soundio_backend_count");

   function Get_Backend (The_Soundio : access SoundIo; index : int) return SoundIo_Backend;  -- /home/amiard/build/libsoundio/soundio/soundio.h:724
   pragma Import (C, Get_Backend, "soundio_get_backend");

   function Have_Backend (Backend : SoundIo_Backend) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:727
   pragma Import (C, Have_Backend, "soundio_have_backend");

   procedure Flush_Events (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:751
   pragma Import (C, Flush_Events, "soundio_flush_events");

   procedure Wait_Events (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:755
   pragma Import (C, Wait_Events, "soundio_wait_events");

   procedure Wake_Up (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:758
   pragma Import (C, Wake_Up, "soundio_wakeup");

   procedure Force_Device_Scan (The_Soundio : access SoundIo);  -- /home/amiard/build/libsoundio/soundio/soundio.h:775
   pragma Import (C, Force_Device_Scan, "soundio_force_device_scan");

   function Channel_Layout_Equal (L : access constant SoundIo_Channel_Layout; R : access constant SoundIo_Channel_Layout) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:782
   pragma Import (C, Channel_Layout_Equal, "soundio_channel_layout_equal");

   function Get_Channel_Name (id : SoundIo_Channel_Id) return Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:786
   pragma Import (C, Get_Channel_Name, "soundio_get_channel_name");

   function Parse_Channel_Id (str : Interfaces.C.Strings.chars_ptr; str_len : int) return SoundIo_Channel_Id;  -- /home/amiard/build/libsoundio/soundio/soundio.h:790
   pragma Import (C, Parse_Channel_Id, "soundio_parse_channel_id");

   function Channel_Layout_Builtin_Count return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:793
   pragma Import (C, Channel_Layout_Builtin_Count, "soundio_channel_layout_builtin_count");

   function Channel_Layout_Get_Builtin (index : int) return access constant SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:798
   pragma Import (C, Channel_Layout_Get_Builtin, "soundio_channel_layout_get_builtin");

   function Channel_Layout_Get_Default (channel_count : int) return access constant SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:801
   pragma Import (C, Channel_Layout_Get_Default, "soundio_channel_layout_get_default");

   function Channel_Layout_Find_Channel (layout : access constant SoundIo_Channel_Layout; channel : SoundIo_Channel_Id) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:804
   pragma Import (C, Channel_Layout_Find_Channel, "soundio_channel_layout_find_channel");

   function Channel_Layout_Detect_Builtin (layout : access SoundIo_Channel_Layout) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:809
   pragma Import (C, Channel_Layout_Detect_Builtin, "soundio_channel_layout_detect_builtin");

   function Best_Matching_Channel_Layout
     (preferred_layouts : access constant SoundIo_Channel_Layout;
      preferred_layout_count : int;
      available_layouts : access constant SoundIo_Channel_Layout;
      available_layout_count : int) return access constant SoundIo_Channel_Layout;  -- /home/amiard/build/libsoundio/soundio/soundio.h:814
   pragma Import (C, Best_Matching_Channel_Layout, "soundio_best_matching_channel_layout");

   procedure Sort_Channel_Layouts (layouts : access SoundIo_Channel_Layout; layout_count : int);  -- /home/amiard/build/libsoundio/soundio/soundio.h:819
   pragma Import (C, Sort_Channel_Layouts, "soundio_sort_channel_layouts");

   function Get_Bytes_Per_Sample (format : SoundIo_Format) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:825
   pragma Import (C, Get_Bytes_Per_Sample, "soundio_get_bytes_per_sample");

   function Get_Bytes_Per_Frame (format : SoundIo_Format; channel_count : int) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:828
   pragma Import (CPP, Get_Bytes_Per_Frame, "_ZL27soundio_get_bytes_per_frame13SoundIoFormati");

   function Get_Bytes_Per_Second
     (format : SoundIo_Format;
      channel_count : int;
      sample_rate : int) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:833
   pragma Import (CPP, Get_Bytes_Per_Second, "_ZL28soundio_get_bytes_per_second13SoundIoFormatii");

   function Format_String (format : SoundIo_Format) return Interfaces.C.Strings.chars_ptr;  -- /home/amiard/build/libsoundio/soundio/soundio.h:840
   pragma Import (C, Format_String, "soundio_format_string");

   function Input_Device_Count (The_Soundio : access SoundIo) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:856
   pragma Import (C, Input_Device_Count, "soundio_input_device_count");

   function Output_Device_Count (The_Soundio : access SoundIo) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:859
   pragma Import (C, Output_Device_Count, "soundio_output_device_count");

   function Get_Input_Device (The_Soundio : access SoundIo; index : int) return access SoundIo_Device;  -- /home/amiard/build/libsoundio/soundio/soundio.h:865
   pragma Import (C, Get_Input_Device, "soundio_get_input_device");

   function Get_Output_Device (The_Soundio : access SoundIo; index : int) return access SoundIo_Device;  -- /home/amiard/build/libsoundio/soundio/soundio.h:870
   pragma Import (C, Get_Output_Device, "soundio_get_output_device");

   function Default_Onput_Device_Index (The_Soundio : access SoundIo) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:875
   pragma Import (C, Default_Onput_Device_Index, "soundio_default_input_device_index");

   function Default_Output_Device_Index (The_Soundio : access SoundIo) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:880
   pragma Import (C, Default_Output_Device_Index, "soundio_default_output_device_index");

   procedure Device_Ref (Device : access SoundIo_Device);  -- /home/amiard/build/libsoundio/soundio/soundio.h:883
   pragma Import (C, Device_Ref, "soundio_device_ref");

   procedure Device_Unref (Device : access SoundIo_Device);  -- /home/amiard/build/libsoundio/soundio/soundio.h:886
   pragma Import (C, Device_Unref, "soundio_device_unref");

   function Device_Equal (L : access constant SoundIo_Device; R : access constant SoundIo_Device) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:890
   pragma Import (C, Device_Equal, "soundio_device_equal");

   procedure Device_Sort_Channel_Layouts (Device : access SoundIo_Device);  -- /home/amiard/build/libsoundio/soundio/soundio.h:895
   pragma Import (C, Device_Sort_Channel_Layouts, "soundio_device_sort_channel_layouts");

   function Device_Supports_Format (Device : access SoundIo_Device; format : SoundIo_Format) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:899
   pragma Import (C, Device_Supports_Format, "soundio_device_supports_format");

   function Device_Supports_Layout (Device : access SoundIo_Device; layout : access constant SoundIo_Channel_Layout) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:904
   pragma Import (C, Device_Supports_Layout, "soundio_device_supports_layout");

   function Device_Supports_Sample_Rate (Device : access SoundIo_Device; sample_rate : int) return Extensions.bool;  -- /home/amiard/build/libsoundio/soundio/soundio.h:909
   pragma Import (C, Device_Supports_Sample_Rate, "soundio_device_supports_sample_rate");

   function Device_Nearest_Sample_Rate (Device : access SoundIo_Device; sample_rate : int) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:914
   pragma Import (C, Device_Nearest_Sample_Rate, "soundio_device_nearest_sample_rate");

   function Outstream_Create (Device : access SoundIo_Device) return access SoundIo_Out_Stream;  -- /home/amiard/build/libsoundio/soundio/soundio.h:924
   pragma Import (C, Outstream_Create, "soundio_outstream_create");

   procedure Outstream_Destroy (outstream : access SoundIo_Out_Stream);  -- /home/amiard/build/libsoundio/soundio/soundio.h:926
   pragma Import (C, Outstream_Destroy, "soundio_outstream_destroy");

   function Outstream_Open (outstream : access SoundIo_Out_Stream) return SoundIo_Error;  -- /home/amiard/build/libsoundio/soundio/soundio.h:950
   pragma Import (C, Outstream_Open, "soundio_outstream_open");

   function Outstream_Start (outstream : access SoundIo_Out_Stream) return SoundIo_Error;
   pragma Import (C, Outstream_Start, "soundio_outstream_start");

   function Outstream_Begin_Write
     (outstream : access SoundIo_Out_Stream;
      Areas : out SoundIo_Channel_Area_Ptr;
      Frame_Count : out int) return SoundIo_Error;
   pragma Import (C, Outstream_Begin_Write, "soundio_outstream_begin_write");

   function Outstream_End_Write (outstream : access SoundIo_Out_Stream) return SoundIo_Error;
   pragma Import (C, Outstream_End_Write, "soundio_outstream_end_write");

   function Outstream_Clear_Buffer (outstream : access SoundIo_Out_Stream) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1020
   pragma Import (C, Outstream_Clear_Buffer, "soundio_outstream_clear_buffer");

   function Outstream_Pause (outstream : access SoundIo_Out_Stream; pause : Extensions.bool) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1041
   pragma Import (C, Outstream_Pause, "soundio_outstream_pause");

   function Outstream_Get_Latency (outstream : access SoundIo_Out_Stream; out_latency : access double) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1054
   pragma Import (C, Outstream_Get_Latency, "soundio_outstream_get_latency");

   function Instream_Create (Device : access SoundIo_Device) return access SoundIo_In_Stream;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1064
   pragma Import (C, Instream_Create, "soundio_instream_create");

   procedure Instream_Destroy (Instream : access SoundIo_In_Stream);  -- /home/amiard/build/libsoundio/soundio/soundio.h:1066
   pragma Import (C, Instream_Destroy, "soundio_instream_destroy");

   function Instream_Open (Instream : access SoundIo_In_Stream) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1086
   pragma Import (C, Instream_Open, "soundio_instream_open");

   function Instream_Start (Instream : access SoundIo_In_Stream) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1095
   pragma Import (C, Instream_Start, "soundio_instream_start");

   function Instream_Begin_Read
     (Instream : access SoundIo_In_Stream;
      Areas : out SoundIo_Channel_Area_Ptr;
      Frame_Count : access int) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1126
   pragma Import (C, Instream_Begin_Read, "soundio_instream_begin_read");

   function Instream_End_Read (Instream : access SoundIo_In_Stream) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1136
   pragma Import (C, Instream_End_Read, "soundio_instream_end_read");

   function Instream_Pause (Instream : access SoundIo_In_Stream; pause : Extensions.bool) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1149
   pragma Import (C, Instream_Pause, "soundio_instream_pause");

   function Instream_Get_Latency (Instream : access SoundIo_In_Stream; out_latency : access double) return int;  -- /home/amiard/build/libsoundio/soundio/soundio.h:1159
   pragma Import (C, Instream_Get_Latency, "soundio_instream_get_latency");


   type SoundIo_Ring_Buffer is null record;
   pragma Convention (C_Pass_By_Copy, SoundIo_Ring_Buffer);

   function Ring_Buffer_Create
     (The_Soundio        : access SoundIo;
      Requested_Capacity : int) return access SoundIo_Ring_Buffer;
   pragma Import (C, Ring_Buffer_Create, "soundio_ring_buffer_create");

   procedure Ring_Buffer_Destroy (Ring_Buffer : access SoundIo_Ring_Buffer);
   pragma Import (C, Ring_Buffer_Destroy, "soundio_ring_buffer_destroy");

   function Ring_Buffer_Capacity
     (Ring_Buffer : access SoundIo_Ring_Buffer) return int;
   pragma Import (C, Ring_Buffer_Capacity, "soundio_ring_buffer_capacity");

   function Ring_Buffer_Write_Ptr
     (Ring_Buffer : access SoundIo_Ring_Buffer) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Ring_Buffer_Write_Ptr, "soundio_ring_buffer_write_ptr");

   procedure Ring_Buffer_Advance_Write_Ptr
     (Ring_Buffer : access SoundIo_Ring_Buffer; count : int);
   pragma Import (C, Ring_Buffer_Advance_Write_Ptr, "soundio_ring_buffer_advance_write_ptr");

   function Ring_Buffer_Read_Ptr
     (Ring_Buffer : access SoundIo_Ring_Buffer)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Ring_Buffer_Read_Ptr, "soundio_ring_buffer_read_ptr");

   procedure Ring_Buffer_Advance_Read_Ptr
     (Ring_Buffer : access SoundIo_Ring_Buffer; count : int);
   pragma Import (C, Ring_Buffer_Advance_Read_Ptr, "soundio_ring_buffer_advance_read_ptr");

   function Ring_Buffer_Fill_Count
     (Ring_Buffer : access SoundIo_Ring_Buffer) return int;
   pragma Import (C, Ring_Buffer_Fill_Count, "soundio_ring_buffer_fill_count");

   function Ring_Buffer_Free_Count
     (Ring_Buffer : access SoundIo_Ring_Buffer) return int;
   pragma Import (C, Ring_Buffer_Free_Count, "soundio_ring_buffer_free_count");

   procedure Ring_Buffer_Clear
     (Ring_Buffer : access SoundIo_Ring_Buffer);
   pragma Import (C, Ring_Buffer_Clear, "soundio_ring_buffer_clear");

end Soundio;
