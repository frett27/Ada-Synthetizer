------------------------------------------------------------------------------
--                             Ada Midi Player                              --
--                                                                          --
--                         Copyright (C) 2018-2019                          --
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

with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;

with GNAT.Strings; use GNAT.Strings;

with Midi.Player;
with Synth.SoundBank; use Synth.SoundBank;

with Ada.Exceptions;

procedure Main is
   p: Midi.Player.MidiPlayerParameters;
   S: SoundBank_Access;
begin

   Midi.Player.ReadCommandLineParameters(p);

   if p.FileName = null then
      Put_Line("Filename must be specified");
      return;
   end if;


   Put_Line("Start MusicBox Midi Player and play :" & p.FileName.all);
   Midi.Player.Init;
   if p.BankName /= null then
      Put_Line("Read Soundbank");
      S := Synth.SoundBank.Read(FileName => p.BankName.all);
   end if;

   Midi.Player.Play (Parameters => p,
                     Sounds => S);

   Put_Line("End Of Play");
exception
   when e : Program_Error =>
      Midi.Player.DumpException(e);
   when Gnat.Command_Line.Exit_From_Command_Line =>
      return;
end Main;
