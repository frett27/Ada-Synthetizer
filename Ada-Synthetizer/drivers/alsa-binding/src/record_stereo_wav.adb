--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with Ada.Text_IO, Ada.Text_IO.Text_Streams;
with Sound.Stereo_Recording;

procedure Record_Stereo_WAV is
   type Double_Word is mod 2 ** 32;
   for Double_Word'Size use 32;

   type Word is mod 2 ** 16;
   for Word'Size use 16;

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   function Little_Endian return Boolean;
   --  Checks if we are running on a little-endian architecture.

   procedure Write_RIFF
     (Target : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Data   : in     Sound.Stereo_Recording.Frame_Array);

   procedure Write_Format
     (Target           : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Sample_Frequency : in     Sound.Sample_Frequency);

   procedure Write_Data
     (Target : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Data : in     Sound.Stereo_Recording.Frame_Array);

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

   Bits_Per_Sample    : constant      := Sound.Stereo_Recording.Level'Size;
   Number_Of_Channels : constant Word :=
                          Sound.Stereo_Recording.Frame'Size / Bits_Per_Sample;
   Block_Alignment    : constant Word :=
                          Number_Of_Channels * Bits_Per_Sample / 8;

   procedure Write_Data
     (Target : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Data : in     Sound.Stereo_Recording.Frame_Array) is
      Number_Of_Samples : constant Double_Word := Data'Length;
   begin
      String'Write
        (Target,
         "data");
      Double_Word'Write
        (Target,
         Number_Of_Samples * Double_Word (Number_Of_Channels)
           * Bits_Per_Sample / 8);
      Sound.Stereo_Recording.Frame_Array'Write
        (Target,
         Data);
   end Write_Data;

   procedure Write_Format
     (Target           : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Sample_Frequency : in     Sound.Sample_Frequency) is
      Sample_Rate        : constant Double_Word := Double_Word
                                                     (Sample_Frequency);
      Byte_Rate          : constant Double_Word :=
        Sample_Rate * Double_Word (Number_Of_Channels) * Bits_Per_Sample / 8;
   begin
      String'Write      (Target, "fmt ");
      Double_Word'Write (Target, 16);
      Word'Write        (Target, 1);
      Word'Write        (Target, Number_Of_Channels);
      Double_Word'Write (Target, Sample_Rate);
      Double_Word'Write (Target, Byte_Rate);
      Word'Write        (Target, Block_Alignment);
      Word'Write        (Target, Bits_Per_Sample);
   end Write_Format;

   procedure Write_RIFF
     (Target : in     Ada.Text_IO.Text_Streams.Stream_Access;
      Data   : in     Sound.Stereo_Recording.Frame_Array) is
   begin
      String'Write (Target, "RIFF");
      Double_Word'Write (Target, 4 + 24 + 8 + Data'Size / 8);
      String'Write (Target, "WAVE");
   end Write_RIFF;

   Microphone  : Sound.Stereo_Recording.Line_Type;
   Resolution  : Sound.Sample_Frequency := 48_000;
   Buffer_Size : Duration := 0.5;
   Period      : Duration := 0.1;
   Recording   : Sound.Stereo_Recording.Frame_Array (1 .. 48_000 * 30);
   Filled_To   : Natural := Recording'First - 1;
   Target      : Ada.Text_IO.Text_Streams.Stream_Access;
begin
   if not Little_Endian then
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Big endian is too messy.");
      return;
   end if;

   Sound.Stereo_Recording.Open (Line        => Microphone,
                                Resolution  => Resolution,
                                Buffer_Size => Buffer_Size,
                                Period      => Period);

   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Resolution [samples/s]:" &
              Sound.Sample_Frequency'Image (Resolution));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Buffer size [s]:" & Duration'Image (Buffer_Size));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Period [s]:" & Duration'Image (Period));

   while Filled_To < Recording'Last loop
      Sound.Stereo_Recording.Read
        (Line => Microphone,
         Item => Recording (Filled_To + 1 ..
                            Positive'Min (Recording'Last, Filled_To + 3000)),
         Last => Filled_To);
   end loop;

   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Recorded [s]:" &
              Duration'Image (Duration (Filled_To) / Duration (Resolution)));

   Sound.Stereo_Recording.Close (Line => Microphone);

   Target :=  Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output);

   Write_RIFF (Target => Target,
               Data   => Recording (Recording'First .. Filled_To));
   Write_Format (Target           => Target,
                 Sample_Frequency => Resolution);
   Write_Data (Target => Target,
               Data   => Recording (Recording'First .. Filled_To));
end Record_Stereo_WAV;
