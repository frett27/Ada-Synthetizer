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

with Ada.Real_Time;
use Ada.Real_Time;

with Ada.Exceptions;

with Synth; use Synth;
with Synth.Driver;
with Synth.Driver.Wav;
with Synth.Wav;
with Synth.Synthetizer;

with Midi.File;
use Midi;

with Midi.Stream;
use Midi.Stream;

--  for command line reading
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings; use GNAT.Strings;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps;
use Ada.Containers;

with Ada.Strings.Hash;

with Ada.Text_IO;

package body Midi.Player is

   package Maps is new Indefinite_Hashed_Maps (Key_Type => String,
                                               Element_Type => Boolean,
                                               Hash => Ada.Strings.Hash,
                                               Equivalent_Keys => "=");
   use Maps;

   --  synthetizer
   --  this is a global access because the synth audit is not directly
   --  associated in the design with the synth itself

   GlobalSynth : aliased Synth.Synthetizer.Synthetizer_Type;

   CurrentStreamLength : Float;

   MusicBoxBehaviour: Boolean := False;


   --  log function for infos or debug
   type Log_Function_Access is access
     procedure (S : String);

   --  default Print Log
   procedure P (S : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, S);
   end P;

   --  default log function
   LogFunction : Log_Function_Access := P'Access;

   procedure SetLog (L : Log_Function_Access) is
   begin
      LogFunction := L;
   end SetLog;


   procedure DebugPrint (S : String) is
   begin
      if LogFunction /= null then
         LogFunction (S);
      end if;
   end DebugPrint;

   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      if LogFunction = null then
         return;
      end if;

      LogFunction
        (
         "--------------------[ Unhandled exception ]------"
         & "-----------");
      LogFunction (
                   " > Name of exception . . . . .: " &
                     Ada.Exceptions.Exception_Name (E));
      LogFunction (

                   " > Message for exception . . .: " &
                     Ada.Exceptions.Exception_Message (E));
      LogFunction (" > Trace-back of call stack: ");
      LogFunction (
                   GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

   end DumpException;

   ----------------------------------------------------------------------------
   --  audit protected interface

   CurrentSounds : Synth.SoundBank.SoundBank_Access;

   procedure Define_SoundBank (S : Synth.SoundBank.SoundBank_Access) is
   begin
      CurrentSounds := S;
   end Define_SoundBank;

   type Player_Synth_Audit is
     new Synth.Synthetizer.Synthetizer_Audit with record

      --  committed played time in events
      StreamTime : Long_Float := 0.0;
      Associated_Synth_Time : Synthetizer_Time := Microseconds (0);

      EventCursor : Event_Vector.Cursor;

      --  play tempo factor
      TempoFactor : Float := 1.0;

      --  played event counter
      Event_Counter : Natural := 0;

      Sounds : SoundBank_Access := null;

      Activated_Banks : Map;

      --  stopped flag
      Stopped : Boolean := True;
   end record;

   type Opened_Voices_Type is array (0 .. 127) of Synth.Synthetizer.Voice;
   type Opened_Voices_Type_Access is access all Opened_Voices_Type;

   package Maps_Open_Voices is
     new Indefinite_Hashed_Maps (Key_Type => String,
                                 Element_Type => Opened_Voices_Type_Access,
                                 Hash => Ada.Strings.Hash,
                                 Equivalent_Keys => "=");
   use Maps_Open_Voices;

   --  Opened voices, by bank name
   Opened_Voices : Maps_Open_Voices.Map  := Maps_Open_Voices.Empty_Map;

   --  Sound
   TheSoundDriver : Synth.Driver.Sound_Driver_Access;

   --
   --  audit procedure for filling the planned events
   --  this call back is called by synthetizer to fill the event buffer
   --
   overriding
   procedure Ready_To_Prepare (Audit : in out Player_Synth_Audit;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synth.Synthetizer_Time) is
      use Synth.Synthetizer;

      Current_Tempo : Float := Audit.TempoFactor;

      D : Duration := To_Duration (Next_Buffer_Time - Current_Buffer_Time);

      --  compute the next frame time in Synth Space
      Stream_Projected_Synthetizer_Next_Time_Frame_In_Stream_Space : Long_Float :=
        Audit.StreamTime
          + Long_Float(Long_Long_Float (D)
        / Long_Long_Float (Current_Tempo));

      Current_Stream_Time : Long_Float := Audit.StreamTime;
      Current_Associated_SynthTime : Synthetizer_Time :=
        Audit.Associated_Synth_Time;

      C : Ada.Real_Time.Time := Clock;
   begin
      if not Audit.Stopped then
         begin

            -- DebugPrint (S =>
            --               "Handle Frame, tempo factor: "
            --             & Float'Image (Current_Tempo)
            --             & "- frame duration :" & Duration'Image (D)
            --             & "  stream time :" & Long_Float'Image (Audit.StreamTime)
            --             & " projected : " &   Long_Float'Image (Stream_Projected_Synthetizer_Next_Time_Frame_In_Stream_Space)
            --             & " diff : " &   Long_Float'Image (Stream_Projected_Synthetizer_Next_Time_Frame_In_Stream_Space - Audit.StreamTime)
            --             & " synth_time " & Duration'Image(To_Duration(Current_Associated_SynthTime)));

            while Event_Vector.Has_Element (Audit.EventCursor) and then
              Current_Stream_Time < Stream_Projected_Synthetizer_Next_Time_Frame_In_Stream_Space
            loop
               Audit.Event_Counter := Natural'Succ (Audit.Event_Counter);
               declare
                  E : TimeStampedEvent :=
                    Event_Vector.Element (Audit.EventCursor);
                  SelectedSound : SoundSample; -- default

                  Event_Start_Time_In_SynthSpace : Synthetizer_Time :=
                    Microseconds (Integer (
                                  (Long_Long_Float (E.T) - Long_Long_Float(Audit.StreamTime)) *
                                    Long_Long_Float (Current_Tempo) *
                                    Long_Long_Float (1_000_000)))
                    + Audit.Associated_Synth_Time;

               begin
                  Current_Stream_Time := Long_Float (E.T);
                  Current_Associated_SynthTime := Event_Start_Time_In_SynthSpace;

                  -- DebugPrint ("Current Stream Time "
                  --            & Long_Float'Image (Current_Stream_Time));
                  -- DebugPrint ("Time in the file "
                  --            & Duration'Image (
                  --              To_Duration (Current_Associated_SynthTime)));

                  declare
                     SBAccess : SoundBank_Access := Audit.Sounds;
                  begin

                     if SBAccess /= null then
                        if E.Note > 127 or else E.Note < 0 then
                           if LogFunction /= null then
                              LogFunction ("Invalid note :" & Natural'Image (E.Note));
                           end if;
                        else
                           --  for each activated sound bank
                           for Bank in Audit.Activated_Banks.Iterate loop

                              if Element (Bank) then
                                 declare
                                    BankName : String :=  Key (Bank);
                                    OVCur : Maps_Open_Voices.Cursor :=
                                      Opened_Voices.Find (Key => BankName);
                                    VA : Opened_Voices_Type_Access := null;

                                 begin
                                    if not  Has_Element (OVCur) then
                                       Opened_Voices.
                                         Insert (Key      => BankName,
                                                 New_Item => new Opened_Voices_Type'
                                                   (others => No_Voice));
                                    end if;
                                    VA := Opened_Voices.Element (BankName);
                                    begin
                                       SelectedSound :=
                                         SoundBank.GetSoundSample
                                           (SBAccess.all,
                                            To_Unbounded_String (Key (Bank)),
                                            E.Note);
                                    exception
                                       when others =>
                                          SelectedSound := Null_Sound_Sample;
                                          if LogFunction /= null then
                                             LogFunction
                                               ("error getting sound sample for note "
                                                & Natural'Image (E.Note));
                                          end if;

                                    end;
                                    if E.isOn then
                                       --  bank read ?
                                       if SelectedSound /= Null_Sound_Sample and then SelectedSound.Mono_Data /= null then
                                          pragma Assert (SelectedSound.Mono_Data /= null);
                                          declare
                                             VTA : Synth.Synthetizer.Voice;
                                          begin
                                             Play (Synt         =>  Audit.SynthAccess.all,
                                                   S            => SelectedSound,
                                                   Frequency    => Synth.MIDICode_To_Frequency (E.Note),
                                                   Channel      => 1,
                                                   Volume => 0.5,
                                                   Play_Time => Event_Start_Time_In_SynthSpace,
                                                   Opened_Voice => VTA);
                                             VA (E.Note) := VTA;
                                          exception
                                             when e: others =>
                                                DumpException(E => e);
                                          end;

                                       end if;
                                    else
                                       if SelectedSound /= Null_Sound_Sample and then not MusicBoxBehaviour then
                                          Stop (Synt         => Audit.SynthAccess.all,
                                                Opened_Voice => VA (E.Note),
                                                Stop_Time => Event_Start_Time_In_SynthSpace);
                                       end if;
                                    end if;
                                 end;
                              end if;
                           end loop;
                        end if;
                     end if;


                  end;

               end;
               Event_Vector.Next (Audit.EventCursor);
            end loop;

         exception
            when e : others =>
               DumpException (E => e);
         end;

      end if;

      Audit.StreamTime := Stream_Projected_Synthetizer_Next_Time_Frame_In_Stream_Space;
      Audit.Associated_Synth_Time := Audit.Associated_Synth_Time +
        (Next_Buffer_Time - Current_Buffer_Time);

      if (Next_Buffer_Time - Current_Buffer_Time) < (Clock - C) then
         P ("********** WARNING , time elapsed to construct the buffer is larger than the buffer ***************");
      end if;


   end Ready_To_Prepare;

   --  player task, handling the play / stop
   task Task_Player is
      entry Init(SoundDriver : Synth.Driver.Sound_Driver_Access);
      entry Play (MS : Midi_Event_Stream;
                  S : Synth.SoundBank.SoundBank_Access);
      entry isPlaying (result : out Boolean);
      entry Change_Tempo_Factor (Tempo_Factor : Float);
      entry Deactivate_Bank (Bank_Name : String);
      entry Activate_Bank (Bank_Name : String);
      entry Get_Played_Stream_Time (Stream_Time: out Long_Float);
      entry Stop;
      entry Close;
   end Task_Player;

   task body Task_Player is
      Player_Synth : Player_Synth_Audit;
      IsOpen : Boolean := False;
      IsInited : Boolean := False;
   begin

      while not IsInited loop
         select
            accept Init(SoundDriver : Synth.Driver.Sound_Driver_Access) do
               -- Initialize the Synth
               Player_Synth :=
                 Player_Synth_Audit'(
                                     StreamTime  => -2.0,
                                     Associated_Synth_Time => Microseconds (0),
                                     TempoFactor => 1.0,
                                     Sounds => null,
                                     Event_Counter => 0,
                                     EventCursor =>
                                       Event_Vector.First (Event_Vector.Empty_Vector),
                                     SynthAccess => null,
                                     Stopped => True,
                                     Activated_Banks => Maps.Empty_Map
                                    );
               --  open synth
               Synth.Synthetizer.Open (Driver_Access => SoundDriver,
                                       Synt => GlobalSynth,
                                       Buffers_Number =>  1,
                                       Buffer_Size => 10_000, -- 10_000
                                       Audit =>  Player_Synth'Unrestricted_Access
                                      );

               Player_Synth.SynthAccess := GlobalSynth'Access;
               IsOpen := True;

               isInited := True;

            end Init;
         or
            accept Close do
               -- for task termination, the normal way, freeing resources
               IsInited := True;

            end Close;
         end select;

      end loop;



      while IsOpen loop
         select
            accept Play (MS : Midi_Event_Stream;
                         S : Synth.SoundBank.SoundBank_Access) do

               declare
                  Cur : Maps.Cursor;
                  Inserted : Boolean;
               begin
                  Player_Synth.Sounds := S;
                  Player_Synth.EventCursor := Event_Vector.First (MS.Events);

                  --  activate the DEFAULT SOUNDBANK
                  Player_Synth.Activated_Banks := Maps.Empty_Map;
                  Player_Synth.Activated_Banks.Insert
                    ("DEFAULT", True, Cur, Inserted);

                  Player_Synth.StreamTime := 0.0;
                  -- should not be before the current time
                  Player_Synth.Associated_Synth_Time :=
                    Synth.Synthetizer.Get_Time(GlobalSynth) + Microseconds(2_000_000);

                  Player_Synth.Stopped := False;

               end;

            end Play;
         or
            accept isPlaying (result : out Boolean) do
               if not IsOpen then
                  result := False;
               else
                  result := not Player_Synth.Stopped and Event_Vector.Has_Element (Player_Synth.EventCursor);
               end if;

            end isPlaying;

         or
            accept Stop  do
               if IsOpen then
                  Player_Synth.Stopped := True;
                  Player_Synth.EventCursor := Event_Vector.No_Element;
                  delay 0.1;

               end if;
            end Stop;
         or
            accept Change_Tempo_Factor (Tempo_Factor : Float) do
               Player_Synth.TempoFactor := Tempo_Factor;
            end Change_Tempo_Factor;
         or
            accept Activate_Bank (Bank_Name : String)  do
               declare
                  Cur : Maps.Cursor;
                  Inserted : Boolean;
               begin
                  Player_Synth.Activated_Banks.Insert (Bank_Name, True, Cur, Inserted);
                  if not Inserted then
                     Player_Synth.Activated_Banks.Replace (Bank_Name, True);
                  end if;

               end;
            end Activate_Bank;
         or
            accept Deactivate_Bank (Bank_Name : String)  do

               --  close all voices
               declare
                  Cur : Maps_Open_Voices.Cursor := Opened_Voices.Find (Key => Bank_Name);
               begin
                  if Maps_Open_Voices. Has_Element (Cur) then
                     declare
                        VA : Opened_Voices_Type_Access := Opened_Voices. Element (Key => Bank_Name);
                     begin

                        for I in VA.all'Range loop
                           Synthetizer.Stop (Synt         => Player_Synth.SynthAccess.all,
                                             Opened_Voice => VA (I)
                                            );
                        end loop;
                     end;
                     Opened_Voices.Delete (Key => Bank_Name);
                  end if;
               end;

               Player_Synth.Activated_Banks.Replace (Bank_Name, False);

            end Deactivate_Bank;
         or
            accept Close do
               Synth.Synthetizer.Close (Synt => GlobalSynth);
               IsOpen := False;
            end Close;
         or
            accept Get_Played_Stream_Time (Stream_Time: out Long_Float)  do
               if not IsOpen then

                  Stream_Time := -1.0;
               else
                  Stream_Time := Player_Synth.StreamTime;
               end if;

            end Get_Played_Stream_Time;
         or
            terminate;
         end select;

      end loop;

   end Task_Player;



   MidiStream : Midi.Stream.Midi_Event_Stream;



   procedure Init (SoundDriver : Synth.Driver.Sound_Driver_Access) is
   begin
      TheSoundDriver := SoundDriver;
      Task_Player.Init(SoundDriver => SoundDriver);
   end Init;


   -- activate some feature on the player
   procedure ActivateGlobalFeature(ParameterName: String;
                                  Activated : Boolean) is
   begin
      if ParameterName = "MUSICBOX" then
         MusicBoxBehaviour := Activated;
      else
         raise Program_Error with "No Feature " & ParameterName;
      end if;

   end ActivateGlobalFeature;

   procedure Play (FileName : String) is
      File : aliased String := FileName;
   begin
      if CurrentSounds = null then
         raise Program_Error with "No current SoundBank";
      end if;

      Read_Midi_File (File, MidiStream);

      -- update the current stream time
      if Midi.Stream.Event_Vector.Is_Empty(MidiStream.Events) then
         CurrentStreamLength := -1.0;
      else
         declare
            Last_Event : Midi.Stream.TimeStampedEvent :=
              Midi.Stream.Event_Vector.Last_Element(Container => MidiStream.Events);
         begin
            CurrentStreamLength := Float(Last_Event.T);
         end;
      end if;

      Task_Player.Play (MS => MidiStream,
                        S => CurrentSounds);

   end Play;

   function IsPlaying return Boolean is
      Result : Boolean;
   begin
      Task_Player.isPlaying (result => Result);
      return Result;
   end IsPlaying;

   procedure Change_Tempo_Factor (Tempo_Factor : Float) is
   begin
      Task_Player.Change_Tempo_Factor (Tempo_Factor);
   end Change_Tempo_Factor;

   procedure Stop is
   begin
      Task_Player.Stop;
   end Stop;

   procedure Close is
   begin
      Task_Player.Close;
   end;

   procedure Activate_Bank (Bank_Name : String) is
   begin
      Task_Player.Activate_Bank (Bank_Name);
   end Activate_Bank;

   procedure Deactivate_Bank (Bank_Name : String)  is
   begin
      Task_Player.Deactivate_Bank (Bank_Name);
   end Deactivate_Bank;

   function Get_Played_Stream_Time return Float is
      Out_Stream_Time : Long_Float := -1.0;
   begin
      Task_Player.Get_Played_Stream_Time(Stream_Time => Out_Stream_Time);
      return Float(Out_Stream_Time);
   end Get_Played_Stream_Time;

   function Get_Played_Stream_Length return Float is
   begin
      return Float(CurrentStreamLength);
   end Get_Played_Stream_Length;

end Midi.Player;
