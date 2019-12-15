--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with Ada.Text_IO;
with Sound.Mono;

procedure Test_ALSA_Binding is
   Microphone  : Sound.Mono.Line_Type;
   Resolution  : Sound.Sample_Frequency := 44_100;
   Buffer_Size : Duration := 0.5;
   Period      : Duration := 0.1;
   Recording   : Sound.Mono.Frame_Array (1 .. 44_100 * 10);
   Filled_To   : Natural;
begin
   Sound.Mono.Open (Line        => Microphone,
                    Mode        => Sound.Input,
                    Resolution  => Resolution,
                    Buffer_Size => Buffer_Size,
                    Period      => Period);
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Resolution [samples/s]: " &
              Sound.Sample_Frequency'Image (Resolution));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Buffer size [s]: " & Duration'Image (Buffer_Size));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Period [s]: " & Duration'Image (Period));
   Sound.Mono.Read (Line => Microphone,
                    Item => Recording,
                    Last => Filled_To);
   Sound.Mono.Close (Line => Microphone);

   for Index in Recording'First .. Filled_To loop
      Ada.Text_IO.Put_Line (Recording (Index)'Img);
   end loop;
end Test_ALSA_Binding;
