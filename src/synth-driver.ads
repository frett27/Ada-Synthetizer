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

--  This package define the sound driver to use for the synthetizer
--  this can be either alsa, or an other embeded device

--  this driver play synchronousely a block of sound
--  constructor of the driver is done be the derived type

package Synth.Driver is

   type Sound_Driver is abstract tagged null record;

   type Sound_Driver_Access is access all Sound_Driver'Class;

   ----------
   -- Play --
   ----------

   procedure Play
     (Driver : in out Sound_Driver;
      Buffer : PCM_Frame_Array_Access) is abstract;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Sound_Driver) is abstract;

   -----------
   -- Open  --
   -----------

   procedure Open (Driver : out Sound_Driver_Access);

   -------------------
   -- Get_Frequency --
   -------------------

   function Get_Frequency (Driver : Sound_Driver)
   return Frequency_Type is abstract;

end Synth.Driver;
