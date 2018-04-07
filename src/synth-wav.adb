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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Text_IO;

package body Synth.Wav is

   -- FILE FORMAT SPECIFICATION

   -- [Bloc de déclaration d'un fichier au format WAVE]
   --    FileTypeBlocID  (4 octets) : Constante «RIFF»  (0x52,0x49,0x46,0x46)
   --    FileSize        (4 octets) : Taille du fichier moins 8 octets
   --    FileFormatID    (4 octets) : Format = «WAVE»  (0x57,0x41,0x56,0x45)
   --

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Byte_Array is array (Positive range <>) of Byte;

   type Word is record
      A : Byte;
      B : Byte;
   end record;

   for Word'Size use 16;

   type DWord is record
      A : Byte;
      B : Byte;
      C : Byte;
      D : Byte;
   end record;

   for DWord'Size use 32;

   -- [Bloc décrivant le format audio]
   --    FormatBlocID    (4 octets) : Identifiant «fmt »  (0x66,0x6D, 0x74,0x20)
   --    BlocSize        (4 octets) : Nombre d'octets du bloc - 16  (0x10)
   --
   --    AudioFormat     (2 octets) : Format du stockage dans le fichier (1: PCM, ...)
   --    NbrCanaux       (2 octets) : Nombre de canaux (de 1 à 6, cf. ci-dessous)
   --    Frequence       (4 octets) : Fréquence d'échantillonnage (en hertz) [Valeurs standardisées : 11025, 22050, 44100 et éventuellement 48000 et 96000]
   --    BytePerSec      (4 octets) : Nombre d'octets à lire par seconde (i.e., Frequence * BytePerBloc).
   --    BytePerBloc     (2 octets) : Nombre d'octets par bloc d'échantillonnage (i.e., tous canaux confondus : NbrCanaux * BitsPerSample/8).
   --    BitsPerSample   (2 octets) : Nombre de bits utilisés pour le codage de chaque échantillon (8, 16, 24)
   --
   -- [Bloc des données]
   --    DataBlocID      (4 octets) : Constante «data»  (0x64,0x61,0x74,0x61)
   --    DataSize        (4 octets) : Nombre d'octets des données (i.e. "Data[]", i.e. taille_du_fichier - taille_de_l'entête  (qui fait 44 octets normalement).
   --    DATAS[] : [Octets du Sample 1 du Canal 1] [Octets du Sample 1 du Canal 2] [Octets du Sample 2 du Canal 1] [Octets du Sample 2 du Canal 2]
   --
   --    * Les Canaux :
   --       1 pour mono,
   --       2 pour stéréo
   --       3 pour gauche, droit et centre
   --       4 pour face gauche, face droit, arrière gauche, arrière droit
   --       5 pour gauche, centre, droit, surround (ambiant)
   --       6 pour centre gauche, gauche, centre, centre droit, droit, surround (ambiant)
   --
   -- NOTES IMPORTANTES :  Les octets des mots sont stockés sous la forme  (i.e., en "little endian")
   -- [87654321][16..9][24..17] [8..1][16..9][24..17] [...

   type Block_Type is (HEADER, FMT, DATA_HEADER, DATA);

   type Wav_Block (Type_Block : Block_Type; DataSize : Integer) is record
      Size : DWord;
      case Type_Block is
         when HEADER =>
            FileFormatID : DWord;
         when FMT =>
            AudioFormat    : Word;
            ChannelsNb     : Word;
            Frequency      : DWord;
            BytePerSec     : DWord;
            BytePerBloc    : Word;
            BitsPerSamples : Word;
         when DATA_HEADER =>
            null;
         when DATA =>
            Samples : Byte_Array (1 .. DataSize);

      end case;
   end record;

   ---------------------------------------------------------------------------

   function To_Integer (W : Word) return Integer is
   begin
      return Integer (W.A) + Integer (W.B) * 2**8;
   end To_Integer;

   ---------------------------------------------------------------------------

   function To_Integer (W : DWord) return Integer is
   begin
      return Integer (W.A) +
        Integer (W.B) * 2**8 +
        Integer (W.C) * 2**16 +
        Integer (W.D) * 2**24;
   end To_Integer;

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

   function Read_Data_From_Header
     (File         : File_Type;
      File_Stream  : Stream_Access;
      Header_Block : Wav_Block) return Wav_Block
   is
      Bytes_To_Read : constant Natural :=
        Natural (To_Integer (Header_Block.Size) );
      Index : Positive := 1;
      BA    : Byte_Array (1 .. Bytes_To_Read) := (others => 0);
   begin
      -- read the data block
      -- ada.text_io.put_line(" bytes to read " & Natural'image(bytes_to_read));
      while Bytes_To_Read > 0 and not End_Of_File (File) loop
         Byte'Read (File_Stream, BA (Index));
         Index := Positive'Succ (Index);
      end loop;

      -- return the new block
      return Wav_Block'
        (Type_Block => DATA,
         DataSize   => To_Integer (Header_Block.Size),
         Size       => Header_Block.Size,
         Samples    => BA);
   end Read_Data_From_Header;

   ---------------------------------------------------------------------------

   function Convert_Bytes_To_Frame_Array
     (BA : Byte_Array) return Frame_Array
   is
      Samples : Frame_Array (1 .. BA'Length / 2);
   begin

--        ada.text_io.put_line(" array size " & Integer'Image(BA'Length));
--        ada.text_io.put_line(" future frame size " & Integer'Image(samples'Length));

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

--              if i < 10 then
--                 Ada.Text_IO.Put_Line
--                   (" " &
--                      Byte'Image (first) &
--                      ", " &
--                      Byte'Image (second) &
--                      " -> " &
--                      Frame'Image (f));
--              end if;
         end;
      end loop;
      return Samples;
   end Convert_Bytes_To_Frame_Array;

   procedure Load (FileName : in String; Sample : out SoundSample) is

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
         -- Ada.Text_IO.Put_Line("new Block of type :" & Block_Type'Image(B.Type_Block));
         case B.Type_Block is
            when HEADER =>
               null;
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
               -- convert bits
               if Samples /= null then
                  -- multiple Data block
                  raise BAD_FORMAT;
               end if;

               declare
                  Converted_Samples : constant Frame_Array :=
                    Convert_Bytes_To_Frame_Array (B.Samples);
               begin
                  Samples := new Frame_Array (1 .. Converted_Samples'Length);
                  Samples.all := Converted_Samples;

--                    Ada.Text_IO.Put_Line
--                      ("data read : " &
--                         Integer'Image (Samples.all'Length) &
--                         " frames read");
               end;
         end case;

      end Parse;

   begin
      Open (File, In_File, FileName);
      File_Stream := Stream (File);

      while not End_Of_File (File) loop

         -- Read the block type
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

               -- Ada.Text_IO.Put_Line("Read Size :" & Integer'Image(TheBlock'Size / System.Storage_Unit));
               -- Ada.Text_IO.Put_Line("Reading block type :" & Block_Type'Image(BT));

               TheBlock'Read (File_Stream, B);  -- read the block

               -- Ada.Text_IO.Put_Line("Block Size :" & Integer'Image( To_Integer(B.Size)));

               if BT = DATA_HEADER then
                  -- in case we have a data_header
                  -- we convert it in data block
                  -- with samples
                  Parse (Read_Data_From_Header (File, File_Stream, B));
               else
                  Parse (B);
               end if;

            end;

         end;

      end loop;

      -- all block must have been read

      Sample.Frequency := Frequence;
      Sample.Mono_Data := Samples;

      Close (File);
   end Load;

end Synth.Wav;
