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

with Interfaces.C;
with Interfaces.C.Strings;

package SynthLib is

   type API_Synth is private;

   type API_Synth_Access is access all API_Synth;

   subtype API_ERROR_CODE is Natural;

   subtype API_Synthetizer_Time is Synth.Synthetizer.Synthetizer_Time;
   subtype API_Voice is Synth.Synthetizer.Voice;

   type BufferPrepare_CallBack is access procedure (S : API_Synth_Access;
      Current_Buffer_Time, Next_Buffer_Time           : API_Synthetizer_Time);

   -- synthetizer initialization
   function Synthetizer_Init (S : in out API_Synth_Access;
      Call_Back                  : in     BufferPrepare_CallBack) return API_ERROR_CODE;

   -- synthetize resource freeze
   function Synthetizer_Close (S : in out API_Synth_Access) return API_ERROR_CODE;

   -- sound handling

   function Synthetizer_Load_Wav_Sample(Filename: Interfaces.C.Strings.chars_ptr;
                                         Sample_Frequency: in Interfaces.C.C_Float;
                                         Sample_No: out Natural) return API_ERROR_CODE;
   -- procedure Synthetizer_Free_Sample(Sample_No: Natural);

   function Synthetizer_Play (S : in API_Synth_Access;
      Sample_No : in     Natural; Frequency : in Interfaces.C.C_float;
      Voice_Result               :    out API_Voice) return API_ERROR_CODE;

   function Synthetizer_Timed_Play (S : in API_Synth_Access;
      Sample_No : in     Natural; Frequency : in Interfaces.C.C_float;
      T : in     API_Synthetizer_Time; Voice : out API_Voice) return API_ERROR_CODE;

   function Synthetizer_Stop (S : in API_Synth_Access;
      Voice_No                   : in     API_Voice) return API_ERROR_CODE;

   function Synthetizer_Timed_Stop (S :     API_Synth_Access;
      Voice : in out API_Voice; T : API_Synthetizer_Time) return API_ERROR_CODE;

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


private

   pragma Export (C, ValidateCall);

   pragma Export (C, Synthetizer_Init);
   pragma Export (C, Synthetizer_Play);
   pragma Export (C, Synthetizer_Stop);

   pragma Export (C, Synthetizer_Timed_Play);
   pragma Export (C, Synthetizer_Timed_Stop);

   pragma Export (C, Synthetizer_Load_Wav_Sample);

   pragma Export (C, Synthetizer_Close);

   pragma Convention (C, BufferPrepare_CallBack);

   type API_Synth is new Synth.Synthetizer.Synthetizer_Audit with record
      S        : Synth.Synthetizer.Synthetizer_Access;
      Callback : BufferPrepare_CallBack;
   end record;

    -- procedure associated to Audit Interface
   procedure Ready_To_Prepare (S : in out API_Synth;
                               Current_Buffer_Time,
                               Next_Buffer_Time : Synth.Synthetizer.Synthetizer_Time);


   type Sound_Sample_Array is array (Positive range <>) of Synth.SoundSample;
   MAX_SOUND_SAMPLE    : constant Positive := 100;
   SoundSample_Buffers : Sound_Sample_Array (1 .. MAX_SOUND_SAMPLE);
   CurrentSoundSample_Index: Natural := 0; --mean no initial sound samples

   API_No_Voice : constant Natural := 0;

end SynthLib;
