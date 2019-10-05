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

with Synth.Wav;

package Synth.Driver.Wav is

   type WAV_Driver is new Sound_Driver with private;

   type WAV_Driver_Access is access all WAV_Driver;

   ----------
   -- Open --
   ----------
   --  factory
   procedure Open (Driver : out Sound_Driver_Access;
                   Frequency : Frequency_Type := 44100.0;
                   FileName : String);

   -----------
   -- Close --
   -----------

   overriding procedure Close (Driver : in out WAV_Driver);

   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out WAV_Driver;
      Buffer : PCM_Frame_Array_Access);

   -------------------
   -- Get_Frequency --
   -------------------
   overriding function Get_Frequency (Driver : WAV_Driver)
           return Frequency_Type;

private

   type WAV_Driver is new Sound_Driver with record
      Frequency : Frequency_Type;
      WAV_File : Synth.Wav.WAV_Write_Type;
   end record;

end Synth.Driver.Wav;
