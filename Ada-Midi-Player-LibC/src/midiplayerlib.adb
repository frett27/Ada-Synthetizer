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

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

package body MidiPlayerLib is

   CurrentSoundBank : SoundBankRef := null;

   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence) is
   begin

      Ada.Text_IO.Put_Line
        (
         "--------------------[ Unhandled exception ]------"
         & "-----------");
      Ada.Text_IO.Put_Line (
                            " > Name of exception . . . . .: " &
                              Ada.Exceptions.Exception_Name (E));
      Ada.Text_IO.Put_Line (

                            " > Message for exception . . .: " &
                              Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (" > Trace-back of call stack: ");
      Ada.Text_IO.Put_Line (
                            GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

   end DumpException;

   --  init the player
   function Init return API_RETURN_CODE is
      D : Synth.Driver.Sound_Driver_Access;
   begin

      --  open the sound driver
      Synth.Driver.Open (Driver    => D,
                         Frequency => Synth.Frequency_Type (48_200));

      Midi.Player.Init (SoundDriver => D);

      Ada.Text_IO.Put_Line ("Initied");

      return API_OK;

   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Init;

   --  activate a midi player feature
   function Activate_Global_Feature (Feature : C.Strings.chars_ptr;
                                     Activated : C.int)
                                     return API_RETURN_CODE is
      FeatureName : String := C.Strings.Value (Feature);
   begin
      Midi.Player.ActivateGlobalFeature (FeatureName, Activated > 0);
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Activate_Global_Feature;

   --  load a sound bank
   function Load_SoundBank (FileName : C.Strings.chars_ptr)
                            return SoundBankRef is
      S : SoundBankRef := null;
   begin
      S := SoundBankRef (
                         Synth.SoundBank.Read (
                           FileName                 =>
                             C.Strings.Value (FileName),
                           Force_No_Stop_For_Sounds => False));
      CurrentSoundBank := S;
      if Define_SoundBank (SoundBank => S) /= API_OK then
         raise Program_Error with "Error in defining soundbank";
      end if;

      return S;
   exception
      when e : others   =>
         DumpException (E => e);
         return null;
   end Load_SoundBank;

   --  define the sound bank playing
   function Define_SoundBank (SoundBank : SoundBankRef)
                              return API_RETURN_CODE is
   begin
      Ada.Text_IO.Put_Line ("Define SoundBank ");
      Midi.Player.Define_SoundBank (S => SoundBank_Access (SoundBank));
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Define_SoundBank;

   --  play the given midi filename
   function Play (FileName : C.Strings.chars_ptr) return API_RETURN_CODE is
      SFileName : String := C.Strings.Value (FileName);
   begin
      if CurrentSoundBank = null then
         Ada.Text_IO.Put_Line ("No current SoundBank");
         return API_ERROR_NO_SOUNDBANK;
      end if;
      Midi.Player.Play (FileName => SFileName);
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Play;

   --  change play tempo of the file
   function Change_Tempo_Factor (Tempo_Factor : C.double)
                                 return API_RETURN_CODE
   is
   begin
      Midi.Player.Change_Tempo_Factor (Tempo_Factor => Float (Tempo_Factor));
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Change_Tempo_Factor;

   --  activate the play of the bank
   function Activate_Bank (Bank_Name : C.Strings.chars_ptr)
                           return API_RETURN_CODE is
      sBank : String := C.Strings.Value (Bank_Name);
   begin
      Midi.Player.Activate_Bank (Bank_Name => sBank);
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);

         return API_GENERIC_ERROR;
   end Activate_Bank;

   --  remove the active bank in the play
   function Deactivate_Bank (Bank_Name : C.Strings.chars_ptr)
                             return API_RETURN_CODE is
      sBank : String := C.Strings.Value (Bank_Name);
   begin
      Midi.Player.Deactivate_Bank (sBank);
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Deactivate_Bank;

   --  is it playing ?
   function IsPlaying return Natural is
   begin

      if Midi.Player.IsPlaying then
         return 1;
      end if;

      return 0;
   end IsPlaying;

   --  return the current stream position
   function CurrentStreamPosition return C_float is
   begin
      return C_float (Midi.Player.Get_Played_Stream_Time);
   exception
      when e : others   =>
         DumpException (E => e);
         return C_float (-1);
   end CurrentStreamPosition;

   --  return the current stream length
   function CurrentStreamLength return C_float is
   begin
      return C_float (Midi.Player.Get_Played_Stream_Length);
   exception
      when e : others   =>
         DumpException (E => e);
         return C_float (-1.0);
   end CurrentStreamLength;

   --  stop the play, release
   function Stop return API_RETURN_CODE is
   begin
      Midi.Player.Stop;
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end Stop;

   --  pause or resume the play on the midi player
   function PauseResume return C.int is
      Status : Boolean;
   begin
      Midi.Player.Pause_Resume (Status);
      if Status then
         return C.int (1);
      end if;
      return C.int (0);
   exception
      when e : others   =>
         DumpException (E => e);
         return -1;
   end PauseResume;

   --  stop all active voices
   function StopAllVoices return API_RETURN_CODE is
   begin
      Midi.Player.Stop_All;
      return API_OK;
   exception
      when e : others   =>
         DumpException (E => e);
         return API_GENERIC_ERROR;
   end StopAllVoices;

end MidiPlayerLib;
