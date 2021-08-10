------------------------------------------------------------------------------
--                             Ada Midi Player                              --
--                                                                          --
--                         Copyright (C) 2018-2021                        --
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

with Ada.Text_IO; use Ada.Text_IO;
with Log;
with GNAT.Command_Line; use GNAT.Command_Line;

with GNAT.Strings; use GNAT.Strings;

with Midi.Player;

with Ada.Exceptions;
with Synth;
with Synth.Driver;
with Synth.Driver.Wav;

with Synth.SoundBank;

procedure MidiPlayer is


   type MidiPlayerParameters is record
      FileName : String_Access;
      TempoFactor : Float := 1.0;
      BankName : String_Access;
      WavOutput : String_Access;
      Frequency : Synth.Frequency_Type := 44_100.0;
      MusicBoxBehaviour : Boolean := False;
   end record;


   Read_Parameters : MidiPlayerParameters :=
     MidiPlayerParameters'(FileName          => null,
                           TempoFactor       => 1.0,
                           BankName          => null,
                           WavOutput         => null,
                           Frequency         => 44_100.0,
                           MusicBoxBehaviour => False);

   s : Synth.SoundBank.SoundBank_Access := null;

   MustSpecifyASoundbank : exception;

   Config : Command_Line_Configuration;


   procedure Callback (Switch, Param, Section : String);
   procedure ReadCommandLineParameters;

   procedure Callback (Switch, Param, Section : String) is
   begin
      if Switch = "-f" then
         Log.Print ("Filename :" & Param);
         Read_Parameters.FileName := new String'(Param);
      elsif Switch = "-t" then
         Log.Print ("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "--tempo" then
         Log.Print ("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "-b" then
         Log.Print ("Sound Bank:" & Param);
         Read_Parameters.BankName := new String'(Param);
      elsif Switch = "-w" then
         Log.Print ("Will Output to :" & Param);
         Read_Parameters.WavOutput := new String'(Param);
      elsif Switch = "-r" then
         Log.Print ("Frequency rate " & Param);
         Read_Parameters.Frequency := Synth.Frequency_Type'Value (Param);
      elsif Switch = "-m" then
         Log.Print ("Music Box Behaviour");
         Read_Parameters.MusicBoxBehaviour := True;
      elsif Switch = "--help" then
         Display_Help (Config);
      end if;
   end Callback;


   procedure ReadCommandLineParameters is
   begin

      Define_Switch (Config, "-f:",
                     Help => "Specify the midi file to read");   -- 2
      Define_Switch (Config, "-t:",
                     Help => "Tempo factor");
      Define_Switch (Config, "-b:",
                     Long_Switch => "--bank=",
                     Help => "instrument filename");
      Define_Switch (Config, "-m",
                     Long_Switch => "--music-box",
                     Help => "music box behaviour");
      Define_Switch (Config, "-w:",
                     Help => "Output to Wav File");
      Define_Switch (Config, "-r:",
                     Help => "Frequency for playing");
      Define_Switch (Config,
                     Long_Switch => "--tempo=",
                     Help => "Enable long option. Arg is an integer");
      Define_Switch (Config,
                     Long_Switch => "--help",
                     Help => "Display help");

      Getopt (Config, Callback'Unrestricted_Access);   -- 3

   end ReadCommandLineParameters;

   D : Synth.Driver.Sound_Driver_Access;

begin

   -- Midi.Player.SetLog(Log.Print'Access);

   ReadCommandLineParameters;

   if Read_Parameters.FileName = null then
      Put_Line ("Filename must be specified, see --help to more informations");
      Midi.Player.Close;
      return;
   end if;

   Put_Line("Opening Audio Device");

   if Read_Parameters.WavOutput /= null then
      Synth.Driver.Wav.Open (Driver    => D,
                             Frequency => Read_Parameters.Frequency,
                             FileName  => Read_Parameters.WavOutput.all);
   else
      --
      --  open the driver (native plateform)
      --
      Synth.Driver.Open (Driver    => D,
                         Frequency => Read_Parameters.Frequency);
      Put_Line(Synth.Frequency_Type'Image(Synth.Driver.Get_Frequency(D.all)));
   end if;

   Put_Line ("Start MusicBox Midi Player and play :"
             & Read_Parameters.FileName.all);
   Midi.Player.Init (D);

   if Read_Parameters.BankName /= null then
      Log.Print ("Read SoundBank " & Read_Parameters.BankName.all);
      s := Synth.SoundBank.Read
        (FileName => Read_Parameters.BankName.all,
         Force_No_Stop_For_Sounds => Read_Parameters.MusicBoxBehaviour);

      Midi.Player.Define_SoundBank (S => s);
   else
      raise MustSpecifyASoundbank with "Must Specify a soundbank";
   end if;

   Midi.Player.Play (FileName => Read_Parameters.FileName.all);

   delay 1.0;

   Midi.Player.Change_Tempo_Factor
     (Tempo_Factor => Read_Parameters.TempoFactor);
   Midi.Player.Activate_Bank ("DEFAULT");
   while Midi.Player.IsPlaying loop
      delay 2.0;
      Put_Line ("Playing ... ");
   end loop;

   delay 3.0;

   Put_Line ("End Of Play");
   Midi.Player.Stop;


   Put_Line ("Close the Player");
   -- this terminate the player task
   Midi.Player.Close;

   Put_Line ("Close the Sound Device");
   D.Close;

exception
   when e : Program_Error =>
      Midi.Player.DumpException (e);
      Midi.Player.Close;
   when GNAT.Command_Line.Exit_From_Command_Line =>
      Midi.Player.Close;
      return;
   when others =>
      Midi.Player.Close;
end MidiPlayer;
