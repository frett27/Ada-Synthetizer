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
      TempoFactor : Float;
      BankName : String_Access;
      WavOutput : String_Access;
      MusicBoxBehaviour : Boolean := False;
   end record;


   Read_Parameters : MidiPlayerParameters;
   s : Synth.SoundBank.SoundBank_Access := null;

   MustSpecifyASoundbank : exception;

   Config : Command_Line_Configuration;


   procedure Print (S : String) is
      use Ada.Text_IO;
   begin
      Put_Line (S);
   end Print;

   procedure Callback (Switch, Param, Section : String) is
   begin
      if Switch = "-f" then
         Print ("Filename :" & Param);
         Read_Parameters.FileName := new String'(Param);
      elsif Switch = "-t" then
         Print ("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "--tempo" then
         Print ("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "-b" then
         Print ("Sound Bank:" & Param);
         Read_Parameters.BankName := new String'(Param);
      elsif Switch = "-w" then
         Print ("Will Output to :" & Param);
         Read_Parameters.WavOutput := new String'(Param);
      elsif Switch = "-m" then
         Print ("Music Box Behaviour");
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

   ReadCommandLineParameters;

   if Read_Parameters.FileName = null then
      Put_Line ("Filename must be specified");
      return;
   end if;

   if Read_Parameters.WavOutput /= null then
      Synth.Driver.Wav.Open (Driver    => D,
                            Frequency => Synth.Frequency_Type (44_100),
                            FileName  => Read_Parameters.WavOutput.all);
   else
      --
      --  open the driver (native plateform)
      --
      Synth.Driver.Open (Driver    => D,
                         Frequency => Synth.Frequency_Type (48_000));
   end if;

   Put_Line ("Start MusicBox Midi Player and play :"
            & Read_Parameters.FileName.all);
   Midi.Player.Init (D);
   if Read_Parameters.BankName /= null then
      Print ("Read SoundBank " & Read_Parameters.BankName.all);
      s := Synth.SoundBank.Read (FileName => Read_Parameters.BankName.all,
                                 Force_No_Stop_For_Sounds => Read_Parameters.MusicBoxBehaviour);

      Midi.Player.Define_SoundBank (S => s);
   else
      raise MustSpecifyASoundbank with "Must Specify a soundbank";
   end if;

   Midi.Player.Play (FileName => Read_Parameters.FileName.all);
   Midi.Player.Activate_Bank ("DEFAULT");
   while Midi.Player.IsPlaying loop
      delay 0.3;
   end loop;


   Midi.Player.Stop;

   Put_Line ("End Of Play");

exception
   when e : Program_Error =>
      Midi.Player.DumpException (e);
   when GNAT.Command_Line.Exit_From_Command_Line =>
      return;
end MidiPlayer;
