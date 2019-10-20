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
with Ada.Streams; use Ada.Streams;

package Synth.Wav
  --  with SPARK_Mode => On
is

   type WAV_Read_Stream is abstract new Ada.Streams.Root_Stream_Type with
     null record;

   function End_Of_Stream (Stream : WAV_Read_Stream)
                           return Boolean is abstract;

   type WAV_Read_Stream_Access is access all WAV_Read_Stream'Class;

   ----------
   -- Load --
   ----------

   --  Load a Wav from a stream

   procedure Load (Wav_Stream_Access : WAV_Read_Stream_Access;
                   Sample : out SoundSample);

   type WAV_Read_Stream_File is new WAV_Read_Stream with private;
   type WAV_Read_Stream_File_Access is access all WAV_Read_Stream_File;

   function End_Of_Stream (Stream : WAV_Read_Stream_File) return Boolean;

   --  Read data from the stream.
   procedure Read
     (Stream : in out WAV_Read_Stream_File;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Write
     (Stream : in out WAV_Read_Stream_File;
      Item   : Stream_Element_Array);

   ----------
   -- Load --
   ----------

   --  Load a Wav File from the file
   procedure Load (FileName : String; Sample : out SoundSample);
   --  with Post =>
   --    (Sample.Mono_Data /= null) and (Sample.Frequency >= 1.0) and
   --    (if Sample.HasLoop then Sample.Loop_Start >= Sample.Loop_End),
   --       Depends => (Sample => (FileName));

   type WAV_Write_Structure_Type is private;
   type WAV_Write_Type is access all WAV_Write_Structure_Type;

   procedure Open_For_Write (FileName : String; WAV_File : out WAV_Write_Type);

   procedure Write_Data (WAV_File : WAV_Write_Type; Datas : Frame_Array_Access);

   procedure Close_And_Finalize (WAV_File : WAV_Write_Type);

private

   --  FILE FORMAT SPECIFICATION

   --  [Bloc de déclaration d'un fichier au format WAVE]
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

   --  [Bloc décrivant le format audio]
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
   --  [Bloc des données]
   --    DataBlocID      (4 octets) : Constante «data»  (0x64,0x61,0x74,0x61)
   --    DataSize        (4 octets) : Nombre d'octets des données (i.e. "Data[]",
   --                                i.e. taille_du_fichier - taille_de_l'entête
   --                                (qui fait 44 octets normalement).
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
   --  NOTES IMPORTANTES :  Les octets des mots sont stockés sous la forme  (i.e., en "little endian")
   --  [87654321][16..9][24..17] [8..1][16..9][24..17] [...

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

   type WAV_Write_Structure_Type is record
      File : access File_Type;
      FileStream : Stream_Access;
      Bytes_Written : Natural;

   end record;

   type WAV_Read_Stream_File is new WAV_Read_Stream with
      record
         File : File_Type;
      end record;

end Synth.Wav;
