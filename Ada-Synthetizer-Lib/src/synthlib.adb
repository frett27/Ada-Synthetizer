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
with Ada.Real_Time;

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
                             Buffer_Size : in Interfaces.C.int;
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
                             Buffers_Number => 2,
                             Buffer_Size => Natural(Buffer_Size),
                             Audit => Synthetizer_Audit_Access(S) );
      return SS_OK;
   exception
      when E: others =>
         DumpException(E);
         return E_OPEN_SYNTH;
   end;



   function To_Synth_Time(T : API_Synthetizer_Time) return Synthetizer_Time is
      use Ada.Real_Time;
   begin
      return Synthetizer_Time(Microseconds(Natural(T)));
   end;

   function From_Synth_Time(T : Synthetizer_Time) return API_Synthetizer_Time is
      use Ada.Real_Time;
   begin
      return API_Synthetizer_Time(Natural(To_Duration(T) * 1_000_000));
   end;


   function Synthetizer_Get_Time(S : in out API_Synth_Access;
                                 Time: out API_Synthetizer_Time) return API_ERROR_CODE is
      use Ada.Real_Time;
   begin
      if S = null then
         return E_FAILED;
      end if;
      Time := From_Synth_Time( Synth.Synthetizer.Get_Time(Synth => S.S.all));
      return SS_OK;
   exception
      when E: others =>
         DumpException(E);
         return E_FAILED;
   end;





   -- procedure associated to Audit Interface
   procedure Ready_To_Prepare (S : in out API_Synth;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synthetizer_Time) is

   begin

      if S.Callback /= null then
         S.Callback(S'Unchecked_Access, From_Synth_Time(Current_Buffer_Time),
           From_Synth_Time(Next_Buffer_Time));
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
         Ada.Text_IO.Put_Line(Float'image(Float(Frequency)));
         Synth.Synthetizer.Play(Synt         => S.S.all,
                                S            => Sound,
                                Frequency    => Float(Frequency),
                                Volume       => 1.0,
                                Channel      => 1,
                                Opened_Voice => Voice_Result);
         return SS_OK;
      exception
         when E:others =>
            DumpException(E);
            return E_PLAY;
      end;

   end Synthetizer_Play;


   function Synthetizer_Stop(S: in API_Synth_Access;
                             Voice_No : in API_Voice) return API_ERROR_CODE is
   begin
      Synth.Synthetizer.Stop(Synt         => S.S.all,
                             Opened_Voice => Voice_No);
      return SS_OK;
   exception
      when E:others =>
         DumpException(E);
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
                             Play_Time => To_Synth_Time(T),
                             Channel      => 1,
                             Opened_Voice => Voice);

      return SS_OK;
   exception
      when E: others =>
         DumpException(E);
         return E_PLAY;

   end Synthetizer_Timed_Play;

   function Synthetizer_Timed_Stop (S :  in   API_Synth_Access;
                                    Voice : in out API_Voice;
                                    T : in API_Synthetizer_Time) return API_ERROR_CODE is
   begin
      Synth.Synthetizer.Stop(Synt         => S.S.all,
                             Opened_Voice => Voice,
                             Stop_Time    => To_Synth_Time(T)
                            );
      Voice := No_Voice;
      return SS_OK;
   exception
      when E: others =>
         DumpException(E);
         return E_STOP;

   end Synthetizer_Timed_Stop;

   -- allocate a new soundsample
   procedure Get_Next_Sample_No(Allocated_Sample_No: out Natural) is
      NextSNO : Natural := Natural'Succ(CurrentSoundSample_Index);
   begin
      while not (NextSNO in SoundSample_Buffers'Range)
        or else SoundSample_Buffers(NextSNO) /= Synth.Null_Sound_Sample loop

         if not (NextSNO in SoundSample_Buffers'Range) then
            NextSNO := SoundSample_Buffers'First;
         else
            NextSNO := Natural'Succ(NextSNO);
         end if;

         if NextSNO = CurrentSoundSample_Index then
            -- loop back
            Allocated_Sample_No := 0;
            return;
         end if;


      end loop;
      Allocated_Sample_No := NextSNO;
   end;



   -- load a sound sample from memory buffer
   function Synthetizer_Load_Sample(Buffer: C_Float_Array_Access;
                                    BufferSize: in Interfaces.c.int;

                                    Sample_Frequency: in Interfaces.C.C_float;
                                    Note_Frequency: in Interfaces.C.C_float;
                                    CantStop : in Interfaces.C.int;
                                    Has_Loop: in Interfaces.C.int;
                                    Loop_Start : in Interfaces.C.int;
                                    Loop_End : in Interfaces.C.int;
                                    Sample_No: out Natural) return API_ERROR_CODE is
      use Interfaces.C;

      NewSample_No : Natural;
      FA : Frame_Array_Access;
      Sound : SoundSample;

      f : C_float;
      fr : Synth.Frame;
   begin
      -- allocate new slot
      Get_Next_Sample_No(Allocated_Sample_No => NewSample_No);
      if NewSample_No = 0 then
         return E_NO_FREE_SOUNDSAMPLE;
      end if;


      FA := new Synth.Frame_Array (1..Integer(BufferSize));
      for i in 1..Integer(BufferSize) loop

         f := Buffer( Natural(I) - 1 );

         if Float(f) < -1.0 or float(f) > 1.0 then
            Ada.Text_IO.Put_Line("range failed for  :" &
                                   C_Float'image(f) & " in " &
                                   Integer'image(i));
            return E_FAILED;
         end if;

         fr := Synth.Frame(f);
         FA(i) := fr;

      end loop;

      ada.Text_IO.Put_Line(C_float'image(Note_Frequency));

      if Has_Loop = 1 then
         Sound := SoundSample'(HasLoop => True,
                               Frequency      => Float(Sample_Frequency),
                               Note_Frequency => Float(Note_Frequency),
                               Mono_Data      => FA,
                               Cant_Stop      => CantStop = 1,
                               Loop_Start     => Natural(Loop_Start),
                               Loop_End       => Natural(Loop_End));
      else
         Sound := SoundSample'(HasLoop        => False,
                               Frequency      => Float(Sample_Frequency),
                               Note_Frequency => Float(Note_Frequency),
                               Mono_Data      => FA,
                               Cant_Stop      => CantStop = 1);
      end if;


   SoundSample_Buffers(NewSample_No) := Sound;
   sample_No := NewSample_No;

      return SS_OK;

      exception
   when E: others =>
      DumpException(E);
      return E_FAILED;
end Synthetizer_Load_Sample;



-- load a sound sample from wav file
function Synthetizer_Load_Wav_Sample(Filename: Interfaces.C.Strings.chars_ptr;
                                     Note_Frequency: in Interfaces.C.C_Float;
                                     Sample_No: out Natural) return API_ERROR_CODE is
   Sound : SoundSample;
   FN : String := Interfaces.C.Strings.Value(Filename);
   NewSample_No : Natural;
begin

   Get_Next_Sample_No(Allocated_Sample_No => NewSample_No);
   if NewSample_No = 0 then
      return E_NO_FREE_SOUNDSAMPLE;
   end if;


   Ada.Text_IO.Put_Line("loading " & FN);
   begin
      Synth.Wav.load(FileName => FN,  Sample   => Sound);
   exception
      when E : others =>
         DumpException(E);
         return E_LOAD_SOUNDSAMPLE;
   end;


   Ada.Text_IO.Put_Line("loaded");
   Sound.Note_Frequency := Float(Note_Frequency);


   -- find next free slot
   CurrentSoundSample_Index := NewSample_No;

   SoundSample_Buffers(CurrentSoundSample_Index) := Sound;
   Sample_No := Positive(CurrentSoundSample_Index);
   return SS_OK;
exception
   when E: others =>
      DumpException(E);
      return E_FAILED;
end Synthetizer_Load_Wav_Sample;

procedure Synthetizer_Free_Sample(Sample_No: Natural) is
begin
   declare
      S : SoundSample := SoundSample_Buffers(Sample_No);
   begin
      if S /= Synth.Null_Sound_Sample then
         Synth.Free_Frame_Array(S.Mono_Data);
      end if;
      SoundSample_Buffers(Sample_No) := Synth.Null_Sound_Sample;
   end;
exception
   when E: others =>
      DumpException(E);
      raise;
end;


function Synthetizer_Close(S: in out API_Synth_Access) return API_ERROR_CODE is
begin
   Synth.Synthetizer.Close(Synt => S.S.all);
   return SS_OK;
exception
   when others =>
      return E_FAILED;
end;


end SynthLib;
