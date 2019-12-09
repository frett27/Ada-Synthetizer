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
--                                                                           --
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

--- ada midi test program

with Midi;
with Midi.File;
with Text_IO; use Text_IO;

--  using Gnat Extension for file Reading
with GNAT.Directory_Operations;
with Ada.Exceptions;

--  Midi Parsing/Writing test procedure
procedure Testmidi is

   procedure Log_To_Screen (S : String) is
   begin
      Text_IO.Put (S);
   end Log_To_Screen;

   --  Reading Test Procedure
   procedure TestMidiFileReading (Verbose : Boolean) is
      use GNAT.Directory_Operations;
      Dir     : Dir_Type;
      DirName : String (1 .. 1000);
      Len     : Natural;
      M       : Midi.File.Midifile;
      C       : Midi.Chunk;

      Log_To_String_Function : Midi.File.Log_Out_Function :=
        Log_To_Screen'Unrestricted_Access;

      procedure Midi_Dump is new Midi.File.Dump_To_Screen
        (F => Log_To_String_Function);

   begin
      if Verbose then
         Text_IO.Put_Line ("Verbose Mode activated");
      end if;

      Open (Dir, ".");
      if Is_Open (Dir) then
         Put_Line ("The directory was opened");
      else
         Put_Line ("The directory was not opened");
      end if;

      loop
         Read (Dir, DirName, Len);

         exit when Len = 0;
         Put_Line ("File => " & DirName (1 .. Len));

         if Len > 4 then
            if DirName (Len - 3 .. Len) = ".mid" then
               Put_Line ("    Reading ..  ");

               begin

                  M := Midi.File.Read (DirName (1 .. Len));
                  Text_IO.Put
                    (" numbers of tracks " &
                     Natural'Image (Midi.File.GetTrackCount (M)));

                  Text_IO.New_Line;

                  for I in 1 .. Midi.File.GetTrackCount (M) loop
                     Text_IO.Put ("Track " & Natural'Image (I));
                     Text_IO.New_Line;
                     if Verbose then

                        --  Using default, Dump_to_Screen Procedure

                        Midi.Parse
                          (Midi.File.GetChunk (M, I),
                           Midi_Dump'Unrestricted_Access);

                     else
                        --  no parsing informations
                        Midi.Parse (Midi.File.GetChunk (M, I), null);

                     end if;

                  end loop;

                  --  OK, then write the midi file back
                  Text_IO.Put_Line
                    ("Writing  the midi file back ... " &
                     "BAK_" &
                     DirName (1 .. Len));
                  Midi.File.Write (M, "BAK_" & DirName (1 .. Len));

               exception
                  when Error : others =>
                     Put_Line ("Error while parsing " & DirName (1 .. Len));
                     Put_Line
                       ("Error :" &
                        Ada.Exceptions.Exception_Information (Error));
               end;

            end if;
         end if;

      end loop;
      Put_Line ("End of directory");

      Close (Dir);

   end TestMidiFileReading;

   procedure Print_HexEvent (E : in Midi.Event) is
      Ba : Midi.Byte_Array := Midi.ToByteArray (E);
   begin
      Put_Line ("Event => ");
      --  Affiche d'evenement
      for I in Ba'Range loop
         Text_IO.Put (Midi.Byte'Image (Ba (I)));
         Text_IO.New_Line;
      end loop;

   end Print_HexEvent;

   procedure TestMidiFileCreation is
      --  Création d'un chunk
      C   : Midi.Chunk;
      M2  : Midi.File.Midifile;
      EOF : Midi.Event (Midi.MetaEvent);
   begin
      Text_IO.Put_Line ("Adding a new Event ..");

      Midi.AddEvent
        (C,
         Midi.Create_Note_Event
           (Ticks    => 0,
            Channel  => 0,
            Note     => 60,
            Velocity => 127,
            Status   => True));

      Midi.AddEvent
        (C,
         Midi.Create_Note_Event
           (Ticks    => 1000,
            Channel  => 0,
            Note     => 60,
            Velocity => 127,
            Status   => False));

      Midi.AddEvent (C, Midi.Create_EOF_Track_Event);

      Text_IO.Put_Line ("Adding chunk ..");

      Midi.File.AddChunk (M2, C);

      Text_IO.Put_Line ("Write File ..");
      Midi.File.Write (M2, "Hello.mid");

   end TestMidiFileCreation;

--- The Midi test program read the directory and parse all midi files.
--- Then test the creation of midi events

begin

   Put_Line ("Midi Reading Regression Test ... ");
   TestMidiFileReading (Verbose => False);
   Text_IO.New_Line;

   Put_Line ("Midi Writing File ... ");
   TestMidiFileCreation;

end Testmidi;
