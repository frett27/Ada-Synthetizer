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

pragma Ada_2012;

with Synth;use Synth;
with Synth.Synthetizer;use Synth.Synthetizer;
with Synth.Wav;
with Synth.Driver; use Synth.Driver;
with ada.Text_IO;use ada.Text_IO;

package body SynthLib is

   ------------------
   -- ValidateCall --
   ------------------

   procedure ValidateCall is
   begin
      null;
   end ValidateCall;


   -- synthetizer initialization
   function Synthetizer_Init(S: in out API_Synth_Access;
                             Call_Back: in BufferPrepare_CallBack) return API_ERROR_CODE is

      D : Sound_Driver_Access;
   begin
      s := new API_Synth;
      s.S := new Synthetizer_Type;
      s.Callback := Call_Back;

      begin
         Synth.Driver.Open(Driver    => D,
                           Frequency => 44_100.0);
      exception
         when E: others =>
            return E_INIT_DRIVER;
      end;

      Synth.Synthetizer.Open(Driver_Access  => D,
                             Synt           => S.S.all,
                             Audit => Synthetizer_Audit_Access(S) );
      return SS_OK;
   exception
      when E: others =>
         return E_OPEN_SYNTH;
   end;



   -- procedure associated to Audit Interface
   procedure Ready_To_Prepare (S : in out API_Synth;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synthetizer_Time) is
   begin

      if S.Callback /= null then
         S.Callback(S'Unchecked_Access, Current_Buffer_Time, Next_Buffer_Time);
      end if;

   end;




   function Synthetizer_Play(S: in API_Synth_Access;
                             Sample_No: in Natural;
                             Frequency : in Interfaces.C.C_float;
                             Voice_Result : out API_Voice) return API_ERROR_CODE is
      Sound : SoundSample := Null_Sound_Sample;
   begin
      if Sample_No > 0 then
         Sound := SoundSample_Buffers(Sample_No);
      end if;
      if Sound = Null_Sound_Sample then
         Voice_Result := No_Voice;
         return E_NULLSOUNDSAMPLE;
      end if;
      begin
         Synth.Synthetizer.Play(Synt         => S.S.all,
                                S            => Sound,
                                Frequency    => Float(Frequency),
                                Volume       => 1.0,
                                Channel      => 1,
                                Opened_Voice => Voice_Result);
         return SS_OK;
      exception
         when E:others =>
            return E_PLAY;
      end Synthetizer_Play;


      function Synthetizer_Stop(S: in API_Synth_Access;
                                Voice_No : in API_Voice) return API_ERROR_CODE is
      begin
         Synth.Synthetizer.Stop(Synt         => S.S.all,
                                Opened_Voice => Voice_No);
         return SS_OK;
      exception
         when E:others =>
            return E_STOP;
      end;

      -- Timed functions
      function Synthetizer_Timed_Play (S : in API_Synth_Access;
                                       Sample_No : in     Natural;
                                       Frequency : in Interfaces.C.C_float;
                                       T : in     API_Synthetizer_Time;
                                       Voice : out API_Voice) return API_ERROR_CODE is
         Sound: SoundSample;
      begin
         if Sample_No > 0 then
            Sound := SoundSample_Buffers(Sample_No);
         end if;
         if Sound = Null_Sound_Sample then
            Voice := No_Voice;
            return E_NULLSOUNDSAMPLE;
         end if;

         Synth.Synthetizer.Play(Synt         => S.S.all,
                                S            => Sound,
                                Frequency    => Float(Frequency),
                                Volume       => 1.0,
                                Play_Time => T,
                                Channel      => 1,
                                Opened_Voice => Voice);

         return SS_OK;
      exception
         when E: others =>
            return E_PLAY;

      end Synthetizer_Timed_Play;

      function Synthetizer_Timed_Stop (S :     API_Synth_Access;
                                       Voice : in out API_Voice;
                                       T : API_Synthetizer_Time) return API_ERROR_CODE is
      begin
         Synth.Synthetizer.Stop(Synt         => S.S.all,
                                Opened_Voice => Voice);
         Voice := No_Voice;
         return SS_OK;
      exception
         when E: others =>
            return E_STOP;

      end Synthetizer_Timed_Stop;


      function Synthetizer_Load_Wav_Sample(Filename: Interfaces.C.Strings.chars_ptr;
                                           Sample_Frequency: in Interfaces.C.C_Float;
                                           Sample_No: out Natural) return API_ERROR_CODE is
         Sound : SoundSample;
         FN : String := Interfaces.C.Strings.Value(Filename);
      begin
         Ada.Text_IO.Put_Line("loading " & FN);
         begin
            Synth.Wav.load(FileName => FN,  Sample   => Sound);
         exception
            when E : others =>
               DumpException(E);
               return E_LOAD_SOUNDSAMPLE;
         end;


         Ada.Text_IO.Put_Line("loaded");
         Sound.Note_Frequency := Float(Sample_Frequency);
         CurrentSoundSample_Index := Natural'Succ(CurrentSoundSample_Index);
         SoundSample_Buffers(CurrentSoundSample_Index) := Sound;
         Sample_No := Positive(CurrentSoundSample_Index);
         return SS_OK;
      exception
         when E: others =>
            DumpException(E);
            return E_FAILED;
      end Synthetizer_Load_Wav_Sample;



      function Synthetizer_Close(S: in out API_Synth_Access) return API_ERROR_CODE is
      begin
         Synth.Synthetizer.Close(Synt => S.S.all);
         return SS_OK;
      exception
         when others =>
            return E_FAILED;
      end;


   end SynthLib;
