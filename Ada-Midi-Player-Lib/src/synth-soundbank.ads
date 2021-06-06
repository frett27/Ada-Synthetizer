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

with Synth;use Synth;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

package Synth.SoundBank is
   
   -- soundbank with a defined number of sounds
   type SoundBank_Type(Number: Natural) is private;

   type SoundBank_Access is access all SoundBank_Type;
   
   -- read instrument file, and return the soundbank access
   function Read(FileName: String;
                 Force_No_Stop_For_Sounds: Boolean := False) return SoundBank_Access;
   
   -- get sound sample associated to midi note, 
   -- return null_soundsample if no mapping associated
   function GetSoundSample(s: SoundBank_Type; Midi_Note: Natural) 
                           return SoundSample;
   
   function GetSoundSample(s: SoundBank_Type; Bank_Name: Unbounded_String; Midi_Note:Natural)
     return SoundSample;
   
  --- function GetSoundSample(s: SoundBank_Type; 
  ---                         Bank_Name: String; 
  ---                         Midi_Note:Natural)
  ---                         return SoundSample;
   
   -- function GetSoundSampleBanks(S: SoundBank_Type) 
   --                             return Array(<> of String_Access);
      
   No_Mapping: Natural := Natural'Last;
   
   Unsupported_Call: exception;
   
private
   
   type SoundSample_Array is array(Natural range <>) of SoundSample;
   
   -- reference by note, the soundsample indice in the array
   type Note_Sound_Sample_Mapping_Array is array(Natural range 0..127) of Natural;
   
   type Bank_Type;
   type Bank_Access is access Bank_Type;
   
   type Bank_Type is record
      -- linked list of the other sound organized by bank name
      Name: Unbounded_String;
      Note_Mapping : Note_Sound_Sample_Mapping_Array := (others => No_Mapping);
      Next : Bank_Access;
   end record;
   
   type SoundBank_Type(Number: Natural) is record
      -- Samples
      Samples: SoundSample_Array(1..Number) := (others => Null_Sound_Sample); 
      -- other banks
      Banks: Bank_Access := null; 
   end record; 
   

end Synth.SoundBank;
