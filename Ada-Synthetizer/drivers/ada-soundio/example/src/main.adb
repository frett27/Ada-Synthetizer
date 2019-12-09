with Soundio; use Soundio;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Main_Support;

procedure Main is

   IO                   : access Soundio.SoundIo := Create;
   Default_Device_Index : int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Err                  : SoundIo_Error;
begin
   Err := Connect (IO);
   Put_Line (Err'Img);

   Flush_Events (IO);
   Default_Device_Index := Default_Output_Device_Index (IO);
   Device := Get_Output_Device (IO, Default_Device_Index);
   Out_Stream := Outstream_Create (Device);
   Out_Stream.Format := Format_Float32NE;
   Out_Stream.Write_Callback := Main_Support.Write_Callback'Access;

   Err := Outstream_Open (Out_Stream);
   Put_Line (Err'Img);

   Err := Outstream_Start (Out_Stream);
   Put_Line (Err'Img);

   loop
      Wait_Events (IO);
   end loop;

   pragma Warnings (Off, "Unreachable");
   Outstream_Destroy (Out_Stream);
   Device_Unref (Device);
   Destroy (IO);
end Main;
