with Soundio; use Soundio;
with Interfaces.C; use Interfaces.C;

package Main_Support is
   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Callback);
end Main_Support;
