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

with Ada.Unchecked_Conversion;
With Sound.Mono;

------------------------
-- Synth.Driver.Win32 --
------------------------

package Synth.Driver.Alsa is

   type ALSA_Driver is new Sound_Driver with private;

   type ALSA_Driver_Access is access all ALSA_Driver;

   ----------
   -- Open --
   ----------
   --  factory
   procedure Open (Driver : out Sound_Driver_Access;
	   Frequency: Frequency_Type := 48200.0);

   -----------
   -- Close --
   -----------
   overriding procedure Close (Driver : in out ALSA_Driver);

   -------------------
   -- Get_Frequency --
   -------------------
   overriding function Get_Frequency (Driver : ALSA_Driver)
                          return Frequency_Type;
   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out ALSA_Driver;
      Buffer : PCM_Frame_Array_Access;
      Play_Reference_Buffer_Start_Time : Synthetizer_Time);

   ---------------------------
   -- Get_Current_Play_Time --
   ---------------------------

   overriding function Get_Current_Play_Time(Driver: ALSA_Driver)
                                  return Synthetizer_Time;


private
   type ALSA_Driver is new Synth.Driver.Sound_Driver with record
      Speakers    : Sound.Mono.Line_Type;
      Frequency   : Frequency_Type;
      Current_Buffer_Start_Time : Synthetizer_Time;
      Buffer_Playing_Ref_Time : Time;
   end record;



end Synth.Driver.Alsa;
