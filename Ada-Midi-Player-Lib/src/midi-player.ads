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
with GNAT.Strings; use GNAT.Strings;
with Synth.Driver;
with Synth.SoundBank; use Synth.SoundBank;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Midi.Player is


   --  Init the bam_engine
   procedure Init (SoundDriver : Synth.Driver.Sound_Driver_Access);

   --  define the sound bank playing
   procedure Define_SoundBank (S : Synth.SoundBank.SoundBank_Access);

   --  play the given midi filename
   procedure Play (FileName : String);

--
--     -- get all file time
--     function Get_TimeLength() return ...
--
--     -- set current file time
--     procedure Set_Current_Time()
--
--

   --  change the tempo factor
   procedure Change_Tempo_Factor (Tempo_Factor : Float);

   --  activate a given bank name by its name
   procedure Activate_Bank (Bank_Name : String);

   --  deactivate a given bank name by its name
   procedure Deactivate_Bank (Bank_Name : String);

   --  is it playing ?
   function IsPlaying return Boolean;

   --  stop the play, release
   procedure Stop;

   --  dumping a raised exception
   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence);


end Midi.Player;
