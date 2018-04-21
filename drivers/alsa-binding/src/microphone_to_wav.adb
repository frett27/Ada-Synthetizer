--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with Ada.Text_IO, Ada.Text_IO.Text_Streams;
with Sound.Mono;

procedure Microphone_To_WAV is
   type Double_Word is mod 2 ** 32;
   for Double_Word'Size use 32;

   type Word is mod 2 ** 16;
   for Word'Size use 16;

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   function Little_Endian return Boolean;
   --  Checks if we are running on a little-endian architecture.

   function Little_Endian return Boolean is
      type Word_As_Bytes is array (1 .. 2) of Byte;
      for Word_As_Bytes'Size use 16;

      As_Word  : Word := 11 * 256 + 42;
      As_Bytes : Word_As_Bytes;
      for As_Bytes'Address use As_Word'Address;
   begin
      As_Word := 42 + 256 * 11;
      return As_Bytes = (42, 11);
   end Little_Endian;

   Microphone  : Sound.Mono.Line_Type;
   Resolution  : Sound.Sample_Frequency := 44_100;
   Buffer_Size : Duration := 0.5;
   Period      : Duration := 0.1;
   Recording   : Sound.Mono.Frame_Array (1 .. 44_100 * 10);
   Filled_To   : Natural;
   Target      : Ada.Text_IO.Text_Streams.Stream_Access;
begin
   if not Little_Endian then
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Big endian is too messy.");
      return;
   end if;

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

   Target :=  Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output);

   --  RIFF Chunk
   String'Write (Target, "RIFF");
   Double_Word'Write (Target, 4 + 24 + 8 + 2 * Double_Word (Recording'Length));
   String'Write (Target, "WAVE");

   --  FORMAT Chunk
   String'Write (Target, "fmt ");
   Double_Word'Write (Target, 16);
   Word'Write (Target, 1);
   Word'Write (Target, 1);
   Double_Word'Write (Target, Double_Word (Resolution));
   Double_Word'Write (Target, 2 * Double_Word (Resolution));
   Word'Write (Target, 2);
   Word'Write (Target, 16);

   --  DATA Chunk
   String'Write (Target, "data");
   Double_Word'Write (Target, 2 * Double_Word (Recording'Length));

   Sound.Mono.Read (Line => Microphone,
                              Item => Recording,
                              Last => Filled_To);

   Sound.Mono.Frame_Array'Write
     (Target, Recording (Recording'First .. Filled_To));

   Sound.Mono.Close (Line => Microphone);
end Microphone_To_WAV;
