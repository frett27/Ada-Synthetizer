------------------------------------------------------------------------------
--                             Ada Midi Player Lib                          --
--                                                                          --
--                         Copyright (C) 2020-2021                          --
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

with Interfaces.C;
with Interfaces.C.Strings;
use Interfaces;
use Interfaces.C;

with Synth.SoundBank;
use Synth.SoundBank;

package MidiPlayerLib is

   type SoundBankRef is private;
   subtype API_RETURN_CODE is Integer;

   API_OK : API_RETURN_CODE := 0;
   API_GENERIC_ERROR : API_RETURN_CODE := 1000;
   API_ERROR_NO_SOUNDBANK : API_RETURN_CODE := 1100;

   --  Init
   function Init return API_RETURN_CODE;

   -- activate some global features in the API
   function Activate_Global_Feature (Feature : C.Strings.chars_ptr;
                                    Activated : C.int) return API_RETURN_CODE;

   --  Load_SoundBank
   function Load_SoundBank (FileName : C.Strings.chars_ptr)
                            return SoundBankRef;

   --  define the sound bank playing
   function Define_SoundBank (SoundBank : SoundBankRef)
                              return API_RETURN_CODE;

   --  play the given midi filename
   function Play (FileName : C.Strings.chars_ptr)
                  return API_RETURN_CODE;

   --  change tempo factor
   function Change_Tempo_Factor (Tempo_Factor : C.double)
                                 return API_RETURN_CODE;

   --  activate bank (to change some instruments)
   function Activate_Bank (Bank_Name : C.Strings.chars_ptr)
                           return API_RETURN_CODE;

   --  deactivate bank
   function Deactivate_Bank (Bank_Name : C.Strings.chars_ptr)
                             return API_RETURN_CODE;

   --  is it playing ?
   function IsPlaying return Natural;

   --  return the current stream position
   function CurrentStreamPosition return C.C_float;

   --  return the current stream length
   function CurrentStreamLength return C.C_float;

   --  stop the play, release
   function Stop return API_RETURN_CODE;

   --  stream function
   --  function Load_Stream_Midi (FileName : C.Strings.chars_ptr)
   --   return API_RETURN_CODE;

private

   type SoundBankRef is new Synth.SoundBank.SoundBank_Access;

   pragma Export (Convention => C,
                 Entity => Init,
                  External_Name => "midiplayerlib_init");

   pragma Export (Convention => C,
                 Entity => Activate_Global_Feature,
                  External_Name => "midiplayerlib_activatefeature");

   pragma Export (Convention => C,
                 Entity => Load_SoundBank,
                 External_Name => "midiplayerlib_loadsoundbank");

   pragma Export (Convention => C,
                 Entity => Define_SoundBank,
                 External_Name => "midiplayerlib_definesoundbank");

   pragma Export (Convention => C,
                 Entity => Play,
                 External_Name => "midiplayerlib_play");

   pragma Export (Convention => C,
                 Entity => CurrentStreamPosition,
                 External_Name => "midiplayerlib_play_currentstreamposition");

   pragma Export (Convention => C,
                 Entity => CurrentStreamLength,
                 External_Name => "midiplayerlib_play_currentstreamlength");

   pragma Export (Convention => C,
                 Entity => Change_Tempo_Factor,
                 External_Name => "midiplayerlib_changetempofactor");

   pragma Export (Convention => C,
                 Entity => Activate_Bank,
                 External_Name => "midiplayerlib_activatebank");

   pragma Export (Convention => C,
                 Entity => Deactivate_Bank,
                 External_Name => "midiplayerlib_deactivatebank");

   pragma Export (Convention => C,
                 Entity => IsPlaying,
                 External_Name => "midiplayerlib_isplaying");

   pragma Export (Convention => C,
                 Entity => Stop,
                 External_Name => "midiplayerlib_stop");

end MidiPlayerLib;
