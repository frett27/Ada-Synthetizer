------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2016                          --
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



package body Synth is

   --------------------
   -- To_Frame_Array --
   --------------------

   function To_Frame_Array (FA : in Frame_Array) return PCM_Frame_Array is
      R : PCM_Frame_Array (FA'Range);
   begin

      for i in FA'Range loop

         R (i) := PCM_Frame (Float( (2**15 - 1)) * FA (i));


      end loop;

      return R;
   end To_Frame_Array;


   -- convert a midi code to frequency
   function MIDICode_To_Frequency (Midi_Code : Natural) return Frequency_Type is
      LA_440_MIDICODE : constant Natural := 69;
   begin
      return 2.0**((Midi_Code-LA_440_MIDICODE)/12)*Frequency_Type'(440.0);
   end;





end Synth;
