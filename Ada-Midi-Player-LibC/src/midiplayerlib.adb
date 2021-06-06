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

with Synth;
with Synth.SoundBank;

with Midi.Player;

with Synth.Driver;

use type Synth.SoundBank.SoundBank_Access;

with Ada.Text_IO;

package body MidiPlayerLib is


   CurrentSoundBank : SoundBankRef := null;


   function Init return API_RETURN_CODE is
      D : Synth.Driver.Sound_Driver_Access;
   begin

      -- open the sound driver
      Synth.Driver.Open (Driver    => D,
                         Frequency => Synth.Frequency_Type (48_000));

      Midi.Player.init(SoundDriver => D);

      Ada.text_io.Put_Line("Initied");

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end Init;



   function Load_SoundBank(FileName: C.Strings.chars_ptr) return SoundBankRef is
      S : SoundBankRef := null;
   begin

      S := SoundBankRef(
                        Synth.SoundBank.Read(FileName                 => C.Strings.Value(FileName),
                           Force_No_Stop_For_Sounds => False));
      CurrentSoundBank := S;

      if Define_SoundBank(SoundBank => S) /= API_OK then
         return null;
      end if;


      return S;
   exception
         when others => return null;
   end Load_SoundBank;

   --  define the sound bank playing
   function Define_SoundBank (SoundBank: SoundBankRef) return API_RETURN_CODE is
   begin
      Ada.Text_IO.Put_Line("Define SoundBank ");
      Midi.Player.Define_SoundBank(S => SoundBank_Access(SoundBank));

      return API_OK;
   exception
         when others => return API_GENERIC_ERROR;
   end Define_SoundBank;

   --  play the given midi filename
   function Play (FileName : C.Strings.chars_ptr) return API_RETURN_CODE is
      SFileName: String := C.Strings.Value(FileName
                      );
   begin
      if CurrentSoundBank = null then
         Ada.Text_IO.Put_Line("No current SoundBank");
         raise Program_Error with "No current SoundBank";
      end if;


      Ada.Text_IO.Put_Line("Play " & SFileName);

      Midi.Player.play(FileName => SFileName);

        Ada.Text_IO.Put_Line("Queued ");

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end Play;


   function Change_Tempo_Factor (Tempo_Factor : C.double) return API_RETURN_CODE
   is
   begin

      Midi.Player.Change_Tempo_Factor(Tempo_Factor => Float(Tempo_Factor));

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end Change_Tempo_Factor;

   function Activate_Bank (Bank_Name : C.Strings.chars_ptr) return API_RETURN_CODE is
      sBank: String := C.Strings.Value(Bank_Name);
   begin
      Ada.Text_IO.Put_Line("Activate Bank " & sBank);
      Midi.Player.Activate_Bank(Bank_Name => sBank);

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end Activate_Bank;


   function Deactivate_Bank (Bank_Name : C.Strings.chars_ptr) return API_RETURN_CODE is
       sBank: String := C.Strings.Value(Bank_Name);
   begin

      Midi.Player.Deactivate_Bank(sBank);

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end;


   --  is it playing ?
   function IsPlaying return Natural is
   begin

      if (Midi.Player.IsPlaying) then
         return 1;
      end if;

      return 0;
   end IsPlaying;

   --  stop the play, release
   function Stop return API_RETURN_CODE is
   begin

      Midi.Player.Stop;

      return API_OK;

   exception
         when others => return API_GENERIC_ERROR;
   end Stop;



end MidiPlayerLib;
