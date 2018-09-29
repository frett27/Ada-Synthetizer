------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2018                          --
--                                                                          --
--  Authors: Patrice Freydiere                                              --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------

package body Synth.Wav is

   function "and"(Left, Right: Integer) return Integer is
      type Unsigned_Integer is mod 2**Integer'Size;
   begin
    return Integer(Unsigned_Integer(Left) and Unsigned_Integer(Right));
   end "and";

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (W : Word) return Integer is
   begin
      return Integer (W.A) + Integer (W.B) * 2**8;
   end To_Integer;

   function To_Word (I : Natural) return Word is
   begin
      return Word'(A => Byte(I and Natural(16#FF#)),
                   B => Byte(I / 2**8 and 16#FF#));
   end To_Word;

   ---------------------------------------------------------------------------

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (W : DWord) return Integer is
   begin
      return Integer (W.A) +
        Integer (W.B) * 2**8 +
        Integer (W.C) * 2**16 +
        Integer (W.D) * 2**24;
   end To_Integer;




   function To_DWord (I : Natural) return DWord is
   begin
      return DWord'( A => Byte(I and Natural(16#FF#)),
                     B => Byte(I / 2**8 and 16#FF#),
                     C => Byte(I / 2**16 and 16#FF#),
                     D => Byte(I / 2**24 and 16#FF#));

   end To_DWord;


   ---------------------------------------------------------------------------

   type Magic is new String (1 .. 4);

   function "=" (D : DWord; S : Magic) return Boolean is
   begin
      return S (1) = Character'Val (D.A)
        and then S (2) = Character'Val (D.B)
        and then S (3) = Character'Val (D.C)
        and then S (4) = Character'Val (D.D);
   end "=";


   ---------------------------------------------------------------------------

   BAD_FORMAT : exception;

   ---------------------------
   -- Read_Data_From_Header --
   ---------------------------

   function Read_Data_From_Header
     (File         : File_Type;
      File_Stream  : Stream_Access;
      Header_Block : Wav_Block) return Wav_Block
   is
      Bytes_To_Read : constant Natural :=
        Natural (To_Integer (Header_Block.Size));
      Index : Positive := 1;
      BA    : Byte_Array (1 .. Bytes_To_Read) := (others => 0);
   begin
      --  read the data block
      --  ada.text_io.put_line(" bytes to read " & Natural'image(bytes_to_read));
      while Bytes_To_Read > 0 and then not End_Of_File (File) loop
         Byte'Read (File_Stream, BA (Index));
         Index := Positive'Succ (Index);
      end loop;

      --  return the new block
      return Wav_Block'
        (Type_Block => DATA,
         DataSize   => To_Integer (Header_Block.Size),
         Size       => Header_Block.Size,
         Samples    => BA);
   end Read_Data_From_Header;

   ---------------------------------------------------------------------------

   ----------------------------------
   -- Convert_Bytes_To_Frame_Array --
   ----------------------------------

   function Convert_Bytes_To_Frame_Array
     (BA : Byte_Array) return Frame_Array
   is
      Samples : Frame_Array (1 .. BA'Length / 2);
   begin

      for i in 0 .. BA'Length / 2 - 1 loop
         declare
            first  : constant Byte := BA (BA'First + i * 2);
            second : constant Byte := BA (BA'First + i * 2 + 1);
            l      : Long_Integer  :=
              Long_Integer (first) + Long_Integer (second) * 2**8;
            f : Frame;
         begin
            if l >= 2**15 then
               l := l - 2**16;
            end if;

            f := Float (l) / Float (2**15);

            Samples (1 + i) := f;

         end;
      end loop;
      return Samples;
   end Convert_Bytes_To_Frame_Array;

   ----------------------------------
   -- Convert_Frame_Array_To_Bytes --
   ----------------------------------

   function Convert_Frame_Array_To_Bytes(F : Frame_Array)
                                         return Byte_Array
   is
       Bytes : Byte_Array (1 .. F'Length * 2);
   begin
      for i in F'First..f'Last loop
      declare
            fl : constant Float := Float(F(i)) * Float(2**15);
            l : Long_Integer := Long_Integer(fl);
            A,B : Byte;
         begin

            if l < 0 then
               -- 2 complement
               -- reverse + 1
               l :=  2** 16 + l;
            end if;

            B := Byte(l / 2**8 mod 2**8);
            A := Byte(l mod 2**8);
            Bytes( (i - F'First)  * 2  + 1) := A;
            Bytes( (i - F'First)  * 2 + 1 + 1 ) := B;

         end;
      end loop;
      return Bytes;
   end Convert_Frame_Array_To_Bytes;


   ----------
   -- Load --
   ----------

   procedure Load (FileName : String; Sample : out SoundSample)
   is

      File             : File_Type;
      File_Stream      : Stream_Access;
      Block_Type_Value : DWord;

      Frequence : Frequency_Type;
      Samples   : Frame_Array_Access;

      MUST_BE_PCM_FORMAT : exception;
      ONLY_MONO_SUPPORTED : exception;
      SAMPLING_16BITS_ONLY_ACCEPTED : exception;

      procedure Parse (B : Wav_Block) is
      begin
         case B.Type_Block is
            when HEADER =>
               null; -- not read
            when FMT =>
               if To_Integer (B.AudioFormat) /= 1 then
                  raise MUST_BE_PCM_FORMAT;
               end if;

               if To_Integer (B.ChannelsNb) /= 1 then
                  raise ONLY_MONO_SUPPORTED;
               end if;

               if To_Integer (B.BitsPerSamples) /= 16 then
                  raise SAMPLING_16BITS_ONLY_ACCEPTED;
               end if;

               Frequence := Frequency_Type (To_Integer (B.Frequency));

            when DATA_HEADER =>
               raise BAD_FORMAT;
            when DATA =>
               --  convert bits
               if Samples /= null then
                  --  multiple Data block
                  raise BAD_FORMAT;
               end if;

               declare
                  Converted_Samples : constant Frame_Array :=
                    Convert_Bytes_To_Frame_Array (B.Samples);
               begin
                  Samples := new Frame_Array (1 .. Converted_Samples'Length);
                  Samples.all := Converted_Samples;

               end;
         end case;

      end Parse;

   begin
      Open (File, In_File, FileName);
      File_Stream := Stream (File);

      while not End_Of_File (File) loop

         --  Read the block type
         DWord'Read (File_Stream, Block_Type_Value);

         declare
            BT : Block_Type;
         begin

            if Block_Type_Value = "data" then
               BT := DATA_HEADER;
            elsif Block_Type_Value = "RIFF" then
               BT := HEADER;
            elsif Block_Type_Value = "fmt " then
               BT := FMT;
            else
               raise BAD_FORMAT;
            end if;

            declare
               subtype TheBlock is Wav_Block (BT, 0);
               B : TheBlock;
            begin

               TheBlock'Read (File_Stream, B);  -- read the block

               if BT = DATA_HEADER then
                  --  in case we have a data_header
                  --  we convert it in data block
                  --  with samples
                  Parse (Read_Data_From_Header (File, File_Stream, B));
               else
                  Parse (B);
               end if;

            end;

         end;

      end loop;

      --  all block must have been read

      Sample.Frequency := Frequence;
      Sample.Mono_Data := Samples;

      Close (File);
   end Load;


   procedure Write(W : Wav_Block; File_Stream : Stream_Access) is
   begin
      case W.Type_Block is
         when HEADER =>

            Magic'Write (File_Stream, "RIFF");
            -- Size of the hole file
            DWord'Write(File_Stream, W.Size);
            --  wave
            Magic'Write (File_Stream, "WAVE");


         when FMT =>
            Magic'Write (File_Stream, "fmt ");
            DWord'Write(File_Stream, W.Size);


            Word'Write(File_Stream, W.AudioFormat);
            Word'Write(File_Stream, W.ChannelsNb);
            DWord'Write(File_Stream, W.Frequency);
            DWord'Write(File_Stream, W.BytePerSec);
            Word'Write(File_Stream, W.BytePerBloc);
            Word'Write(File_Stream, W.BitsPerSamples);


         when DATA_HEADER=>

            raise Program_Error with "Invalid block";


         when DATA =>
               Magic'Write (File_Stream, "data");
               DWord'Write(File_Stream, W.Size);
            declare
               subtype BArray is Byte_Array(W.Samples'Range);
         BA : constant BArray := W.Samples;
      begin
         BArray'Write(File_Stream, BA);
      end;


      end case;


   end;



   procedure WriteHeaders(WAV_File : in out WAV_Write_Type;
                          MainChunkSize : in Natural) is
        File_Stream : Stream_Access := Wav_File.FileStream;
   begin

      -- Rought Write the Header
      -- the header will be rewrite at the finalize stage

      declare
         BlockHeader : Wav_Block(Type_Block => HEADER,
                                 DataSize => 0);
         BlockFmt : Wav_Block := (Type_Block => FMT,
                                  Size => To_DWord(16),
                                  DataSize => 0,
                                  AudioFormat    => To_Word(1),
                                  ChannelsNb     => To_Word(1),
                                  Frequency      =>
                                    To_DWord (Natural (44100.0)),
                                  BytePerSec     => To_DWord(Natural (44100.0) * 2),
                                  BytePerBloc    => To_Word(1),
                                  BitsPerSamples => To_Word(16)
                                 );
         BlockData : Wav_Block := (Type_Block => DATA,
                                   Size => To_DWord(WAV_File.Bytes_Written),
                                   DataSize => 0,
                                   Samples => Byte_Array'(0..-1=>0));
      begin
         BlockHeader.Size := To_DWord(MainChunkSize);
         -- Write Header
         Write(BlockHeader, File_Stream);
         -- Write Format Description
         Write(BlockFmt, File_Stream);
         -- Write Data Chunk
         Write(BlockData, File_Stream);
      end;


   end;



   procedure Open_For_Write (FileName : String ;
                             WAV_File : out WAV_Write_Type) is
      TMP_WAV_File : WAV_Write_Type := new WAV_Write_Structure_Type;
      File : access File_Type := new File_Type;
      File_Stream : Stream_Access;
   begin

      TMP_WAV_File.File := File;

      Create (File => File.all,
              Name => FileName);
      TMP_WAV_File.Bytes_Written := 0;

      File_Stream := Stream (File.all);

      TMP_WAV_File.FileStream := File_Stream;
      WAV_File := TMP_WAV_File;

      WriteHeaders(WAV_File => WAV_File,
                  MainChunkSize => 0); -- will be rewritten at close

   end Open_For_Write;


   procedure Close_And_Finalize (WAV_File : in out WAV_Write_Type) is
      C : Count := Size(WAV_File.File.all);
      File_Stream : Stream_Access;
   begin

      Reset(File=> WAV_File.file.all,
            Mode => Out_File );



      -- rewind
      File_Stream := Stream (Wav_File.File.all);
      WriteHeaders(WAV_File, Integer(C) - 8);
      Close(Wav_File.File.all);

   end Close_And_Finalize;


   procedure Write_Data (WAV_File : in WAV_Write_Type; Datas : in Frame_Array_Access) is
      BDatas : Byte_Array := Convert_Frame_Array_To_Bytes(Datas.all);
      subtype BArray is Byte_Array(BDatas'Range);
   begin
      WAV_File.Bytes_Written := Wav_File.Bytes_Written + Datas.all'Length;

      BArray'Write(WAV_File.FileStream, BDatas);
   end Write_Data;



end Synth.Wav;
