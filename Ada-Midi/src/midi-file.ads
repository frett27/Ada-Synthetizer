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

with Ada.Finalization;
with Sequential_IO;

package Midi.File is

   type Midifile is private;
   File_Not_Found : exception;

   type HeaderChunk is record
      Format   : Natural range 0 .. 2 := 0;
      Ntracks  : Natural              := 0;
      Division : Positive             := 96;
   end record;

   --  Read a midifile
   function Read (FileName : String) return Midifile;
   --  Write a midifile
   procedure Write (M : Midifile; FileName : String);

   --  Read the division for the file
   function Get_Division (M : Midifile) return Natural;

   --  Get a Chunk from a midifile (Track)
   function GetChunk (M : Midifile; I : Natural) return Chunk;

   --  Get the Chunk Nb
   function GetTrackCount (M : Midifile) return Natural;

   --  Add a chunk
   procedure AddChunk (M : in out Midifile; C : Chunk);

   type Log_Out_Function is access procedure (S : String);

   generic
      F : Log_Out_Function;
   procedure Dump_To_Screen (E : Event);

private

   type Chunk_Array is array (Natural range <>) of Chunk;
   type Chunk_Array_Access is access all Chunk_Array;

   package SeqByte is new Sequential_IO (Byte);

   type Midifile is new Ada.Finalization.Controlled with record
      Hc     : HeaderChunk;
      Chunks : Chunk_Array_Access;
   end record;

   procedure Initialize (O : in out Midifile);
   procedure Adjust (O : in out Midifile);
   procedure Finalize (O : in out Midifile);

   function To_HeaderChunk (C : Chunk) return HeaderChunk;
   function To_Chunk (H : HeaderChunk) return Chunk;

end Midi.File;
