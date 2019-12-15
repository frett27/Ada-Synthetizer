with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Main_Support is

   Pitch : constant := 4400.0;
   Radians_S : constant := Pitch * 2.0 / Pi;
   Seconds_Offset : Float := 0.0;
   Err            : SoundIo_Error;

   procedure Write_Float_Sample is new Write_Sample (Float);

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int)
   is
      Layout            : SoundIo_Channel_Layout renames Out_Stream.Layout;
      Sample_Rate       : Float := Float (Out_Stream.Sample_Rate);
      Seconds_Per_Frame : Float := 1.0 / Sample_Rate;
      Areas             : SoundIo_Channel_Area_Ptr;
      Frames_Left       : int := Frame_Count_Max;
      Sample            : Float;
      use Soundio_Channel_Area_Ptrs;
   begin

      while Frames_Left > 0 loop
         declare
            Frame_Count : int := Frames_Left;
         begin
            Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);
            exit when Frame_Count = 0;

            for Frame in 0 .. Frame_Count - 1 loop
               Sample := Sin
                 ((Seconds_Offset + Float (Frame) * Seconds_Per_Frame)
                  * Radians_S);

               --  TODO: Write Get_Area function that returns the Area at index
               --  Channel
               for Channel in 0 .. Layout.Channel_Count - 1 loop
                  Write_Float_Sample
                    (Get_Area (Areas, Channel), Frame, Sample);
               end loop;
            end loop;

            Seconds_Offset :=
              Seconds_Offset + (Seconds_Per_Frame * Float (Frame_Count));

            Err := Outstream_End_Write (Out_Stream);

            Frames_Left := Frames_Left - Frame_Count;
         end;
      end loop;
   end Write_Callback;

end Main_Support;
