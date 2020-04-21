------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2019                          --
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

with Synth.Synthetizer;
with Synth.Driver;

with Interfaces.C;
with Interfaces.C.Strings;

package SynthLib is

   type API_Synth is private;
   type API_Synth_Access is private;


   type C_Float_Array is array(Natural range 0..Natural'Last) of Interfaces.C.C_float;
   pragma Convention(C, C_Float_Array);
   type C_Float_Array_Access is access all C_Float_Array;


   subtype API_ERROR_CODE is Natural;

   type API_Synthetizer_Time is new Natural;
   subtype API_Voice is Synth.Synthetizer.Voice;

   type BufferPrepare_CallBack is access procedure (S : API_Synth_Access;
                                                    Current_Buffer_Time,
                                                    Next_Buffer_Time : API_Synthetizer_Time);

   -- synthetizer initialization
   function Synthetizer_Init (S : in out API_Synth_Access;
                              Buffer_Size : in Interfaces.C.int;
                              Call_Back                  : in     BufferPrepare_CallBack)
                              return API_ERROR_CODE;

   function Synthetizer_Get_Time(S : in out API_Synth_Access;
                                 Time: out API_Synthetizer_Time) return API_ERROR_CODE;


   function Synthetizer_Estimate_Play_Time(S : in out API_Synth_Access;
                                           Time: out API_Synthetizer_Time) return API_ERROR_CODE;

   -- synthetize resource freeze
   function Synthetizer_Close (S : in out API_Synth_Access) return API_ERROR_CODE;

   -- sound loading from wav file
   function Synthetizer_Load_Wav_Sample(Filename: Interfaces.C.Strings.chars_ptr;
                                        Note_Frequency: in Interfaces.C.C_Float;
                                        Sample_No: out Natural) return API_ERROR_CODE;

   -- sound loading from memory frame array
   function Synthetizer_Load_Sample(Buffer: C_Float_Array_Access;
                                    BufferSize: in Interfaces.c.int;
                                    Sample_Frequency: in Interfaces.C.C_float;
                                    Note_Frequency: in Interfaces.C.C_float;
                                    CantStop : in Interfaces.C.int;
                                    Has_Loop: in Interfaces.C.int;
                                    Loop_Start : in Interfaces.C.int;
                                    Loop_End : in Interfaces.C.int;
                                    Sample_No: out Natural) return API_ERROR_CODE;


   -- free sample
   function Synthetizer_Free_Sample(Sample_No: Natural) return API_ERROR_CODE;

   -- realtime play
   function Synthetizer_Play (S : in API_Synth_Access;
                              Sample_No : in     Natural; Frequency : in Interfaces.C.C_float;
                              Voice_Result               :    out API_Voice) return API_ERROR_CODE;

   -- play with associated time
   function Synthetizer_Timed_Play (S : in API_Synth_Access;
                                    Sample_No : in     Natural; Frequency : in Interfaces.C.C_float;
                                    T : in     API_Synthetizer_Time; Voice : out API_Voice) return API_ERROR_CODE;

   --stop playing with time
   function Synthetizer_Stop (S : in API_Synth_Access;
                              Voice_No                   : in     API_Voice) return API_ERROR_CODE;

   --stop playing with time
   function Synthetizer_Timed_Stop (S : in API_Synth_Access;
                                    Voice : in out API_Voice; T : in API_Synthetizer_Time) return API_ERROR_CODE;

   -- test method for testing calling convention
   procedure ValidateCall;


   SS_OK: constant API_ERROR_CODE := 0;
   E_FAILED : constant API_ERROR_CODE := 1;
   E_INIT_DRIVER: constant API_ERROR_CODE := 10;
   E_OPEN_SYNTH: constant API_ERROR_CODE := 11;

   E_PLAY: constant API_ERROR_CODE := 20;
   E_STOP: constant API_ERROR_CODE := 30;

   E_NULLSOUNDSAMPLE: constant API_ERROR_CODE := 100;
   E_LOAD_SOUNDSAMPLE: constant API_ERROR_CODE := 101;

   E_NO_FREE_SOUNDSAMPLE : constant API_ERROR_CODE := 200;


private

   pragma Export (C, ValidateCall);

   pragma Export (C, Synthetizer_Init);
   pragma Export (C, Synthetizer_Play);
   pragma Export (C, Synthetizer_Stop);

   pragma Export (C, Synthetizer_Get_Time);
   pragma Export (C, Synthetizer_Estimate_Play_Time);

   pragma Export (C, Synthetizer_Timed_Play);
   pragma Export (C, Synthetizer_Timed_Stop);

   pragma Export (C, Synthetizer_Load_Wav_Sample);
   pragma Export (C, Synthetizer_Load_Sample);
   pragma Export (C, Synthetizer_Free_Sample);

   pragma Export (C, Synthetizer_Close);

   pragma Convention (C, BufferPrepare_CallBack);

   type API_Synth is new Synth.Synthetizer.Synthetizer_Audit with record
      S        : Synth.Synthetizer.Synthetizer_Access;
      Callback : BufferPrepare_CallBack;
   end record;

   -- procedure associated to Audit Interface
   procedure Ready_To_Prepare (S : in out API_Synth;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synth.Synthetizer_Time);

   type  API_Synth_Access is  access all API_Synth;
   pragma Convention(C, API_Synth_Access);


   type Sound_Sample_Array is array (Positive range <>) of Synth.SoundSample;
   MAX_SOUND_SAMPLE    : constant Positive := 100;

   SoundSample_Buffers : Sound_Sample_Array (1 .. MAX_SOUND_SAMPLE) :=
     (others => Synth.Null_Sound_Sample);
   CurrentSoundSample_Index: Natural := 0; --mean no initial sound samples

   API_No_Voice : constant Natural := 0;

end SynthLib;
