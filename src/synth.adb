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
with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;

package body Synth is

   --------------------
   -- To_Frame_Array --
   --------------------

   function To_Frame_Array (FA : Frame_Array) return PCM_Frame_Array is
      R : PCM_Frame_Array (FA'Range);
   begin
      for i in FA'Range loop
         pragma Assert (FA (i) <= 1.0);
         pragma Assert (FA (i) >= -1.0);
         declare
            F : constant Float := Float ((2**15 - 1)) * FA (i);
            P : PCM_Frame;
         begin
            P := PCM_Frame (F);
            R (i) := P;
         end;
      end loop;

      return R;
   end To_Frame_Array;

   package Float_Gen is
     new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Float);

   --  convert a midi code to frequency
   function MIDICode_To_Frequency (Midi_Code : Natural) return Frequency_Type is
      LA_440_MIDICODE : constant Natural := 69;
      use Float_Gen;
   begin
      return 2.0**(Float (Midi_Code - LA_440_MIDICODE) / 12.0)
        * Frequency_Type'(440.0);
   end MIDICode_To_Frequency;

   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence) is
   begin
        New_Line (Standard_Error);
               Put_Line
                 (Standard_Error,
                  "--------------------[ Unhandled exception ]------"
                  & "-----------");
               Put_Line
                 (Standard_Error,
                  " > Name of exception . . . . .: " &
                    Ada.Exceptions.Exception_Name (E));
               Put_Line
                 (Standard_Error,
                  " > Message for exception . . .: " &
                    Ada.Exceptions.Exception_Message (E));
               Put_Line (Standard_Error, " > Trace-back of call stack: ");
               Put_Line
                 (Standard_Error,
                  GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

   end DumpException;

end Synth;
