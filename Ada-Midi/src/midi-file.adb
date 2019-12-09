------------------------------------------------------------------------------
--                                 Ada Midi                                 --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
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

--  $Id: ais.ads,v 1.4 2003/09/30 05:48:30 frett Exp $

with Text_IO;
with Ada.Unchecked_Deallocation;

package body Midi.File is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Midifile) is
      C : Chunk_Array_Access := new Chunk_Array (O.Chunks'Range);
   begin
      C (O.Chunks'Range) := O.Chunks (O.Chunks'Range);
      O.Chunks           := C;
   end Adjust;

   -------------------------
   -- Dispose_Chunk_Array --
   -------------------------
   --  Free Chunk array

   procedure Dispose_Chunk_Array is new Ada.Unchecked_Deallocation
     (Object => Chunk_Array,
      Name   => Chunk_Array_Access);

   ----------------
   -- Initialize --
   ----------------
   --  Midi File Tagged Type definition
   procedure Initialize (O : in out Midifile) is
   begin
      O.Chunks := null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Midifile) is
   begin
      if O.Chunks /= null then
         Dispose_Chunk_Array (O.Chunks);
         O.Chunks := null;
      end if;
   end Finalize;

   ----------------
   -- Read4Bytes --
   ----------------

   function Read4Bytes (F : SeqByte.File_Type) return Natural is
      Retval : Natural := 0;
      B      : Byte;
   begin
      SeqByte.Read (F, B);
      Retval := Natural (B);
      Retval := Retval * 256;
      SeqByte.Read (F, B);
      Retval := Retval + Natural (B);
      Retval := Retval * 256;
      SeqByte.Read (F, B);
      Retval := Retval + Natural (B);
      Retval := Retval * 256;
      SeqByte.Read (F, B);
      Retval := Retval + Natural (B);
      return Retval;
   end Read4Bytes;

   -----------------
   -- Write4Bytes --
   -----------------
   --  write the 4 bytes, natural number

   procedure Write4Bytes (F : SeqByte.File_Type; N : Natural) is

      Tab : Byte_Array (1 .. 4);
      Num : Natural := N;
   begin
      for I in 1 .. 4 loop
         Tab (I) := Byte (Num mod 256);
         Num     := Num / 256;
      end loop;
      for I in reverse 1 .. 4 loop
         SeqByte.Write (F, Tab (I));
      end loop;
   end Write4Bytes;

   ------------------
   -- ReadRawChunk --
   ------------------

   function ReadRawChunk (F : SeqByte.File_Type)
                          return Chunk is
      Retval : Chunk;
      B      : Byte;
      Size   : Natural;
   begin
      SeqByte.Read (F, B);
      Retval.ChunkType (1) := Character'Val (B);
      SeqByte.Read (F, B);
      Retval.ChunkType (2) := Character'Val (B);
      SeqByte.Read (F, B);
      Retval.ChunkType (3) := Character'Val (B);
      SeqByte.Read (F, B);
      Retval.ChunkType (4) := Character'Val (B);

      --  Read Size
      Size        := Read4Bytes (F);
      Retval.Data := new Byte_Array (1 .. Size);
      for I in 1 .. Size loop
         SeqByte.Read (F, B);
         Retval.Data (I) := B;
      end loop;
      Retval.Length := Size;

      return Retval;
   end ReadRawChunk;

   -------------------
   -- WriteRawChunk --
   -------------------

   procedure WriteRawChunk (F : SeqByte.File_Type; C : Chunk) is
   begin

      --  Write Chunk Type
      SeqByte.Write (F, Character'Pos (C.ChunkType (1)));
      SeqByte.Write (F, Character'Pos (C.ChunkType (2)));
      SeqByte.Write (F, Character'Pos (C.ChunkType (3)));
      SeqByte.Write (F, Character'Pos (C.ChunkType (4)));

      --  Write size
      Write4Bytes (F, C.Length);
      for I in 1 .. C.Length loop
         SeqByte.Write (F, C.Data (I));
      end loop;
   end WriteRawChunk;

   ----------
   -- Read --
   ----------
   --  Read a midifile from disk..
   function Read (FileName : String) return Midifile is
      M  : Midifile;
      F  : SeqByte.File_Type;
      C  : Chunk;
      AC : Chunk_Array_Access;
   begin
      SeqByte.Open (F, SeqByte.In_File, FileName);
      C    := ReadRawChunk (F);
      M.Hc := To_HeaderChunk (C);
      AC   := new Chunk_Array (1 .. M.Hc.Ntracks);
      for I in 1 .. M.Hc.Ntracks loop
         AC (I) := ReadRawChunk (F);
      end loop;
      SeqByte.Close (F);
      M.Chunks := AC;
      return M;
   end Read;

   --------------------
   -- Dump_To_Screen --
   --------------------
   --  Dump an event to the screen
   procedure Dump_To_Screen (E : Event) is

      function Hex (B : Byte) return String is
         subtype HexValue is Byte range 0 .. 15;

         function ReturnChar (H : HexValue) return Character is
         begin
            if H < 10 then
               return Character'Val (Character'Pos ('0') + H);
            else
               return Character'Val (Character'Pos ('A') + H - 10);
            end if;
         end ReturnChar;

      begin
         return ReturnChar (B / 16) & ReturnChar (B mod 16);
      end Hex;

      procedure Dump_Byte_Array (B : Byte_Array) is
      begin
         for I in B'Range loop
            Text_IO.Put (Hex (B (I)) & " ");
         end loop;
      end Dump_Byte_Array;

   begin
      F (" -- " & Natural'Image (E.Ticks) & ", ");
      case E.ET is
         when MIDIEvent =>
            F ("Midi Event " &
               Midi.MidiCmd'Image (E.Cmd) &
               " for channel " &
               Midi.ChannelType'Image (E.Channel) &
               " -> ");
            Dump_Byte_Array (ToByteArray (E));
         when MetaEvent =>
            F ("Meta Event " & MetaEventService'Image (E.MetaService)
               & "->" & Hex (E.Service) & "--");
            Dump_Byte_Array (ToByteArray (E));
         when SysEvent =>
            null;
      end case;

   end Dump_To_Screen;

   -----------
   -- Write --
   -----------
   --  Write a midi file to disk
   procedure Write (M : Midifile; FileName : String) is
      F : SeqByte.File_Type;
   begin

      SeqByte.Create (File => F, Name => FileName);
      --  Write Header Chunk

      WriteRawChunk (F, To_Chunk (M.Hc));
      for I in 1 .. M.Hc.Ntracks loop

         WriteRawChunk (F, M.Chunks (I));
      end loop;
      SeqByte.Close (F);
   end Write;

   --------------
   -- GetChunk --
   --------------
   --  get a chunk from the file
   function GetChunk (M : Midifile; I : Natural) return Chunk is
   begin
      return M.Chunks (I);
   end GetChunk;

   -------------------
   -- GetTrackCount --
   -------------------
   --  Return the number of tracks in midifile
   function GetTrackCount (M : Midifile) return Natural is
   begin
      return M.Chunks'Length;
   end GetTrackCount;

   --------------------
   -- To_HeaderChunk --
   --------------------
   --  convert a raw chunk to a header chunk
   function To_HeaderChunk (C : Chunk) return HeaderChunk is
      Hc : HeaderChunk;
   begin
      --  check chunk type
      if C.ChunkType /= "MThd" then
         raise Invalid_HeaderChunk;
      end if;
      --  get format
      Hc.Format := Word2Natural (C.Data (1), C.Data (2));
      --  get ntracks
      Hc.Ntracks := Word2Natural (C.Data (3), C.Data (4));
      --  get divisions
      Hc.Division := Word2Natural (C.Data (5), C.Data (6));
      return Hc;
   end To_HeaderChunk;

   function To_Chunk (H : HeaderChunk) return Chunk is
      C : Chunk;
   begin
      C.ChunkType := "MThd";
      C.Data      := new Byte_Array (1 .. 6);
      C.Length    := 6;

      C.Data (1) := Byte (H.Format / 256);
      C.Data (2) := Byte (H.Format mod 256);

      C.Data (3) := Byte (H.Ntracks / 256);
      C.Data (4) := Byte (H.Ntracks mod 256);

      C.Data (5) := Byte (H.Division / 256);
      C.Data (6) := Byte (H.Division mod 256);

      return C;
   end To_Chunk;

   --------------
   -- AddChunk --
   --------------
   --  Add a chunk to the current midifile
   procedure AddChunk (M : in out Midifile; C : Chunk) is
   begin
      if M.Chunks = null then
         M.Chunks     := new Chunk_Array (1 .. 1);
         M.Chunks (1) := C;
         M.Hc.Ntracks := 1;
      else
         declare
            N : Chunk_Array_Access := new Chunk_Array (1 .. M.Chunks'Last + 1);
         begin
            N (1 .. M.Chunks'Last) := M.Chunks (M.Chunks'Range);
            N (N'Last)             := C;
            Dispose_Chunk_Array (M.Chunks);
            M.Chunks     := N;
            M.Hc.Ntracks := N'Last;
         end;
      end if;
   end AddChunk;

   ------------------
   -- Get_Division --
   ------------------
   function Get_Division (M : Midifile) return Natural
   is
   begin
      return M.Hc.Division;
   end Get_Division;

end Midi.File;
