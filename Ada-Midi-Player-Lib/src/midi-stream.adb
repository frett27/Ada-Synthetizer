------------------------------------------------------------------------------
--                             Ada Midi Player                              --
--                                                                          --
--                         Copyright (C) 2018-2021                          --
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

with Midi;
use Midi;
with Midi.File;
use Midi.File;

with Ada.Text_IO;
with Synth;

package body Midi.Stream is

   function "=" (Left, Right : TimeStampedEvent) return Boolean is
   begin
      if Left.T /= Right.T then
         return False;
      end if;
      if Left.isOn /= Right.isOn then
         return False;
      end if;
      if Left.Note /= Right.Note then
         return False;
      end if;

      return True;
   end "=";

   function "<" (Left, Right : TimeStampedEvent) return Boolean is
   begin
      if Left.T < Right.T then
         return True;
      elsif Left.T > Right.T then
         return False;
      end if;

      if Left.Note < Right.Note then
         return True;
      elsif Left.Note > Right.Note then
         return False;
      end if;

      return Left.isOn < Right.isOn;
   end "<";


   procedure DisplayException (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Text_IO.Put_Line ("Error While Parsing :" &
                              Ada.Exceptions.Exception_Message (E));
   end DisplayException;


   procedure Print (S : String) is
      use Ada.Text_IO;
   begin
      Put_Line (S);
   end Print;

   procedure Dump is new Midi.File.Dump_To_Screen (F => Print'Access);

   procedure Read_Midi_File (FileName : String; MES : out Midi_Event_Stream) is
      Midi_File : Midifile := Read (FileName);
      Midi_Chunk : Chunk;
      --  get the division to adjust the tempo

      M : Midi_Event_Stream;

      Divisions : Natural;

      TicksPerBeat : Long_Long_Float;

      CurrentTime : Long_Long_Float := 0.0;

      DelayPerTick : Long_Long_Float := 1_000_000.0 / (24.0 * 100.0);


      QuarterNotePerMinutes : Long_Float := 120.0;

      --  Parsing events
      procedure Parse_File (Ev : Event) is
      begin
         --  Dump (Ev);

         CurrentTime := CurrentTime + DelayPerTick * Long_Long_Float (Ev.Ticks);

         case Ev.ET is

         when MetaEvent =>
            --  parse tempo events
            if Ev.MetaService = Tempo then
               declare
                  TempoValue : Natural;
                  Pos : Natural := 1;
               begin
                  ReadFixedNatural (Ab    => Ev.Data,
                                    Pos   => Pos,
                                    Size  => 3,
                                    Value => TempoValue);

                  --  Ada.Text_IO.Put_Line (Natural'Image (TempoValue));
                  QuarterNotePerMinutes :=
                    60_000_000.0 / Long_Float (TempoValue);

                  DelayPerTick := Long_Long_Float (TempoValue)
                    / Long_Long_Float (Divisions) / 1_000_000.0;

               end;

            elsif Ev.MetaService = TimeSignature then
               declare
                  Numerator : Natural := Natural (Ev.Data (1));
                  Denominator : Natural := Natural (Ev.Data (2));
               begin

                  TicksPerBeat := (
                                   Long_Long_Float (Ev.Data (3)) * 256.0 -- * 4.0
                                   *  Long_Long_Float (Ev.Data (4))) /
                    Long_Long_Float (Ev.Data (1)) / Long_Long_Float (2 ** Natural (Ev.Data (2)));
               end;
            end if;

         when MIDIEvent =>

            --  parse notes events
            if Ev.Cmd = NoteON or else Ev.Cmd = NoteOFF then
               declare
                  use Event_Vector;
                  Note : Natural := Natural (Ev.Data (1));
                  isOn : Boolean := Ev.Cmd = NoteON;
               begin
                  M.Events := M.Events &
                    TimeStampedEvent'(T  => CurrentTime,
                                      isOn => isOn,
                                      Note => Note);
               end;
            end if;
         when others =>
            --  ignored for playing
            Ada.Text_IO.Put_Line ("Ignore " & MidiCmd'Image (
                                 Ev.Cmd));
         end case;

      end Parse_File;


   begin
      Divisions :=  Get_Division (Midi_File);

      -- clear stream
      M.Events.Clear;

      --  Get Header Tempo
      --  Ada.Text_IO.Put_Line("Division " & Natural'Image(Divisions));

      --  parse all chunks
      for I in 1 .. GetTrackCount (Midi_File) loop
         Midi_Chunk := GetChunk (Midi_File, I);
         CurrentTime := 0.0;
         Parse (Midi_Chunk, Parse_File'Unrestricted_Access, DisplayException'Access);
      end loop;

      Event_Sorted.Sort (Container => M.Events);

      MES := M;

   end Read_Midi_File;




end Midi.Stream;
