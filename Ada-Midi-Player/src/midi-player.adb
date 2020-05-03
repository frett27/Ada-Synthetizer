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

with Midi.File;

with Synth.Driver;
With Synth.Driver.Wav;
with Synth.Wav;
with Synth.Synthetizer;

use Midi;
with Ada.Text_IO;

with Ada.Real_Time;
use Ada.Real_Time;

with Ada.Exceptions;

with Ada.Containers.Vectors;
use Ada.Containers;

-- for command line reading
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings; use GNAT.Strings;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;


package body Midi.Player is

   --  synthetizer
   S : Synth.Synthetizer.Synthetizer_Type;

   --  the only sound sample for this midi player
   Sound : Synth.SoundSample;
   
   --  opened voices
   Opened_Voices : array (0 .. 127) of Synth.Synthetizer.Voice := 
     (others => Synth.Synthetizer.No_Voice);

   ----------
   -- Init --
   ----------
   
   procedure Init is
   begin
      Synth.Wav.Load (FileName =>  "DR2-0003N_BaM.wav", --"pling_plong_67.wav", --"odin_flute_la440.wav", -- "enregistrement_2.wav",
                      Sample   => Sound);
      Sound.Note_Frequency := Synth.MIDICode_To_Frequency (Midi_Code => 67); -- la 67
      Sound.Cant_Stop := True;
   end Init;

   procedure Print (S : String) is
      use Ada.Text_IO;
   begin
      Put_Line (S);
   end Print;
   
   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Text_IO;
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
   
   Read_Parameters : MidiPlayerParameters;
   Config : Command_Line_Configuration;
   
   procedure Callback (Switch, Param, Section : String) is
   begin
      if Switch = "-f" then
         Print("Filename :" & Param);
         Read_Parameters.FileName := new String'(Param);
      elsif Switch = "-t" then
         Print("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "--tempo" then
         Print("Tempo :" & Param);
         Read_Parameters.TempoFactor := Float'Value (Param);
      elsif Switch = "-b" then
         Print("Sound Bank:" & Param);
         Read_Parameters.BankName := new String'(Param);
      elsif Switch = "-w" then
         Print("Will Output to :" & Param);
         Read_Parameters.WavOutput := new String'(Param);
      elsif Switch = "--help" then
         Display_Help (Config); 
      end if;
   end Callback;
   
   
   procedure ReadCommandLineParameters(Parameters : out MidiPlayerParameters) is
   
   begin
      -- default values;
      Read_Parameters.TempoFactor := 1.0;
   
      Define_Switch (Config, "-f:", Help => "Specify the midi file to read");   -- 2
      Define_Switch (Config, "-t:", Help => "Tempo factor");
      Define_Switch (Config, "-b:", Long_Switch => "--bank=" ,Help => "instrument filename");
      Define_Switch (Config, "-w:", Help => "Output to Wav File");
      Define_Switch (Config, Long_Switch => "--tempo=",
                     Help => "Enable long option. Arg is an integer");
      Define_Switch (Config, Long_Switch => "--help",
                     Help => "Display help");

      Getopt (Config, Callback'Access);   -- 3
   
      Parameters := Read_Parameters;
      
   end;

   procedure Dump is new Midi.File.Dump_To_Screen (F => Print'Access);

   type TimeStampedEvent is record
      T : Long_Float;
      isOn : Boolean;
      Note : Natural;
   end record;

   function "=" (Left, Right : TimeStampedEvent) return Boolean is
   begin
      if Left.T /= Right.T then
         return False;
      end if;
      if Left.isOn /= Right.isOn then
         return False;
      end if;
      if Left.Note /= Right.Note then
         return False;
      end if;

      return True;
   end;

   function "<" (Left, Right : TimeStampedEvent) return Boolean is
   begin
      if Left.T < Right.T then
         return True;
      elsif Left.T > Right.T then
         return False;
      end if;

      if Left.Note < Right.Note then
         return True;
      elsif Left.Note > Right.Note then
         return False;
      end if;

      return Left.isOn < Right.isOn;
   end;

   package Event_Vector is new Vectors (Index_Type   => Natural,
                                        Element_Type => TimeStampedEvent,
                                        "=" => "=");

   package Event_Sorted is new Event_Vector.Generic_Sorting ("<" => "<");

   QuarterNotePerMinutes : Long_Float := 120.0;
   
   Divisions : Natural;
   
   TicksPerBeat : Long_Float; 
   
   DelayPerTick : Long_Float := 1_000_000.0 / (24.0 * 100.0) ;
   
   CurrentTime : Long_Float := 0.0;
   FileEvents : Event_Vector.Vector;

   --  Parsing events
   procedure Parse_File (Ev : Event) is
      use Synth.Synthetizer;
      use Synth;
   begin
      -- Dump (Ev);

      CurrentTime := CurrentTime + DelayPerTick * Long_Float (Ev.Ticks);

      case Ev.ET is

         when MetaEvent =>
            --  parse tempo events
            if Ev.MetaService = Tempo then
               declare
                  TempoValue : Natural;
                  Pos : Natural := 1;
               begin
                  ReadFixedNatural (Ab    => Ev.Data,
                                    Pos   => Pos,
                                    Size  => 3,
                                    Value => TempoValue);

                  --  Ada.Text_IO.Put_Line (Natural'Image (TempoValue));
                  QuarterNotePerMinutes := 60_000_000.0 / Long_Float (TempoValue); 
                  
                  DelayPerTick := Long_Float(TempoValue) / Long_Float(Divisions) / 1_000_000.0;
                  
               end;

            elsif Ev.MetaService = TimeSignature then
               declare
                  Numerator : Natural := Natural(Ev.Data(1));
                  Denominator : Natural := Natural(Ev.Data(2));
               begin
                  
                  TicksPerBeat := (
                     Long_Float (Ev.Data (3)) * 256.0 -- * 4.0
                     *  Long_Float (Ev.Data(4))) /
                    Long_Float (Ev.Data(1)) / Long_Float (2 ** Natural(Ev.Data(2))) ;
                 
                 
                  -- Ada.Text_IO.Put_Line ("Ticks per beat : " 
                  --                       & Long_Float'Image (TicksPerBeat));
               end;
               
            else
               --  Ada.Text_IO.Put_Line("uncovered event :" & MetaEventService'Image(Ev.MetaService));
               null;                       
                 
            end if;

         when MIDIEvent =>

            --  parse notes events
            if Ev.Cmd = NoteON or else Ev.Cmd = NoteOFF then
               declare
                  use Event_Vector;
                  Note : Natural := Natural (Ev.Data (1));
                  isOn : Boolean := Ev.Cmd = NoteON;
               begin
                  FileEvents := FileEvents &
                    TimeStampedEvent'(T  => CurrentTime,
                                      isOn => isOn,
                                      Note => Note);
               end;
            end if;
         when others =>
            -- ignored for playing
            Ada.Text_IO.Put_Line("Ignore " & MidiCmd'Image(
                                 Ev.Cmd));
      end case;

   end Parse_File;

   procedure DisplayException (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Text_IO.Put_Line ("Error While Parsing :" &
                              Ada.Exceptions.Exception_Message (E));
   end DisplayException;

   
   ----------------------------------------------------------------------------
   -- audit protected interface
   
   
   -- audit is used to populate voice --
   
   type Player_Synth_Audit is new Synth.Synthetizer.Synthetizer_Audit with record
      CT : Long_Float := 0.0;
      S_Time : Synth.Synthetizer_Time := 
        Synth.Synthetizer_Time(Microseconds(0));
      EventCursor : Event_Vector.Cursor;
      TempoFactor: Float := 1.0;
      Event_Counter : Natural := 0;
      Sounds : SoundBank_Access;
   end record;
   
   --
   -- audit procedure for filling the planned events
   -- this call back is called by synthetizer to fill the event buffer
   --
   procedure Ready_To_Prepare(Audit : in out Player_Synth_Audit;
                              Current_Buffer_Time,
                              Next_Buffer_Time : Synth.Synthetizer_Time) is 
      use Synth.Synthetizer;
   begin
         
      while Event_Vector.Has_Element (Audit.EventCursor) and then 
        Audit.S_Time < Next_Buffer_Time
      
      loop
         -- Ada.Text_IO.Put_Line ("Next Event, Current Time :" & Float'Image (CT));
         Audit.Event_Counter := Natural'Succ(Audit.Event_Counter);
         declare
            use Synth.Synthetizer;
            use Synth;
            E : TimeStampedEvent :=
              Event_Vector.Element (Audit.EventCursor);
            SelectedSound: SoundSample := Sound; -- default
            EventDuration : Duration :=  Duration ((E.T - Audit.CT) * Long_Float(Audit.TempoFactor));
            Duration_Time : Synthetizer_Time := Microseconds(Integer(Long_Float(E.T - Audit.CT) * Long_Float(Audit.TempoFactor) * Long_Float(1_000_000)));
         begin
            Audit.S_Time := Audit.S_Time + Duration_Time;
            
            if Audit.Sounds /= null then
               if E.Note > 127 or E.Note < 0 then
                  Ada.Text_IO.Put_Line("Invalid note :" & Natural'image(e.note));
               end if;
                  
            end if;
            
            begin 
               SelectedSound := SoundBank.GetSoundSample(Audit.Sounds.all, E.Note);
            exception
               when others =>
                  SelectedSound := Null_Sound_Sample;
                  Ada.Text_IO.Put_Line("error getting sound sample for note " & Natural'image(E.Note));
            end;
               
            if E.isOn then
               -- bank read ?
               
               if SelectedSound /= Null_Sound_Sample then
                  pragma Assert(SelectedSound.Mono_Data /= null);
                  Play (Synt         => S,
                        S            => SelectedSound,
                        Frequency    => Synth.MIDICode_To_Frequency (E.Note),
                        Channel      => 1,
                        Volume => 0.5,
                        Play_Time => Audit.S_Time,
                        Opened_Voice => Opened_Voices (E.Note));
               end if;
               
            else
               if SelectedSound /= Null_Sound_Sample then
                  Stop (Synt         => S,
                        Opened_Voice => Opened_Voices (E.Note),
                        Stop_Time => Audit.S_Time);
               end if;
            end if;
            Audit.CT := E.T;
              
                     
         end;
         Event_Vector.Next (Audit.EventCursor);
      end loop;

   end Ready_To_Prepare;
   
   
   procedure Play (Parameters : MidiPlayerParameters;
                   Sounds: SoundBank_Access) is
      use Midi.File;
      use Ada.Real_Time;

      D : Synth.Driver.Sound_Driver_Access;
      Midi_File : Midifile := Read (Parameters.FileName.all);
      Midi_Chunk : Chunk;
      

   begin
      
      -- get the division to adjust the tempo
      Divisions :=  Get_Division(Midi_File);
        
      -- Get Header Tempo
      -- Ada.Text_IO.Put_Line("Division " & Natural'Image(Divisions));
      
      --  parse all chunks
      for I in 1 .. GetTrackCount (Midi_File) loop
         Midi_Chunk := GetChunk (Midi_File, I);
         CurrentTime := 0.0;
         Parse (Midi_Chunk, Parse_File'Access, DisplayException'Access);
      end loop;

      Event_Sorted.Sort (Container => FileEvents);

      -- Ada.Text_IO.Put_Line ("Event Count : "
      --                       & Ada.Containers.Count_Type'Image (Event_Vector.Length (FileEvents)));

      if Parameters.WavOutput /= null then
         Synth.Driver.Wav.Open(Driver    => D,
                               Frequency => Synth.Frequency_Type(44_100),
                               FileName  => Parameters.WavOutput.all);
      else 
         --
         --  open the driver (native plateform)
         --
         Synth.Driver.Open (D, Synth.Frequency_Type(44_100));
      end if;
      
      
     
      --  play the events
      declare
         use Synth.Synthetizer;
        
         
         Audit : aliased Player_Synth_Audit := 
           Player_Synth_Audit'(
                               ct => 0.0,
                               S_Time => Synth.Synthetizer_Time(Seconds(3)),
                               TempoFactor => Parameters.TempoFactor,
                               Sounds => Sounds,
                               Event_Counter => 0,
                               EventCursor => Event_Vector.First (FileEvents)
                              );
      begin
         
         --  open synth
         Synth.Synthetizer.Open (Driver_Access => D,
                                 Synt => S,
                                 Buffers_Number =>  2,
                                 Buffer_Size => 10_000, -- 10_000
                                 Audit =>  Audit'Unchecked_Access
                                );
         
         while Event_Vector.Has_Element (Audit.EventCursor) loop
            -- active thread, 
            -- wait 
            delay 2.0;
         
         end loop;
         
         delay 10.0;
         
         Synth.Synthetizer.Close (Synt => S);

      end;
      

      Synth.Driver.Close(S => D.all);

   end Play;

end Midi.Player;
