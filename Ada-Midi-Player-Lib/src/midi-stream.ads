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


with Ada.Containers.Vectors;
use Ada.Containers;

package Midi.Stream is

   type TimeStampedEvent is record
      T : Long_Long_Float;
      isOn : Boolean;
      Note : Natural;
   end record;

   function "=" (Left, Right : TimeStampedEvent) return Boolean;
   function "<" (Left, Right : TimeStampedEvent) return Boolean;

   package Event_Vector is new Vectors (Index_Type   => Natural,
                                        Element_Type => TimeStampedEvent,
                                        "=" => "=");

   package Event_Sorted is new Event_Vector.Generic_Sorting ("<" => "<");

   type Midi_Event_Stream is record
      Events : Event_Vector.Vector;
   end record;

   procedure Read_Midi_File (FileName : String; MES : out Midi_Event_Stream);

end Midi.Stream;
