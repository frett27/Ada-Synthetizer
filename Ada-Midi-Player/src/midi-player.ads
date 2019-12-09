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
with GNAT.Strings;use GNAT.Strings;
With Synth.SoundBank; use Synth.SoundBank;
package Midi.Player is

   type MidiPlayerParameters is record
      FileName : String_Access;
      TempoFactor : Float;
      BankName: String_Access;
      WavOutput: String_Access;
   end record;

   procedure Init;
   procedure ReadCommandLineParameters(Parameters : out MidiPlayerParameters);
   procedure Play (Parameters : MidiPlayerParameters;   Sounds: SoundBank_Access);


   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence);

end Midi.Player;
