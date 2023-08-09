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

with Synth;

package Synth.Oscillators is
    pragma Elaborate_Body;  

   -- Oscillator

   type Oscillator is abstract tagged null record;

   function Can_Be_Stopped(X : in Oscillator) return Boolean is abstract;
   function Is_EndLess(X : in Oscillator) return Boolean is abstract;

   procedure Stop(X: in out Oscillator) is abstract;
   procedure Stop(X: in out Oscillator; Stop: in Synthetizer_Time) is abstract;
   procedure Change_Current_Play_Position(X: in out Oscillator;
                                          Position: in Play_Second) is abstract;

    procedure Apply_Oscillator_To_Buffer (O : in out Oscillator;
                                         Buffer : Frame_Array_Access;
                                         Volume_Factor : Float := 1.0;
                                         Driver_Play_Frequency : Frequency_Type;
                                         Start_Buffer_Time : Synthetizer_Time;
                                         ReachEndOscillator : out Boolean;
                                          Returned_Current_Oscillator_Position : out Play_Second) is abstract;



   type Wav_Oscillator_Type is new Oscillator with record

      Note_Play_Frequency          : Frequency_Type; -- the played frequency
      Play_Sample             : SoundSample; -- the sound sample to play
      Start_Play_Sample : Synthetizer_Time;
      Stop_Play_Sample : Synthetizer_Time := Not_Defined_Clock;
      Current_Sample_Position : Play_Second := 0.0; -- the position in second

   end record;

   function Can_Be_Stopped(O : in Wav_Oscillator_Type) return Boolean;
   function Is_EndLess(X : in Wav_Oscillator_Type) return Boolean;

   procedure Stop(X: in out Wav_Oscillator_Type);
   procedure Stop(X: in out Wav_Oscillator_Type; Stop: in Synthetizer_Time);
   procedure Change_Current_Play_Position(X: in out Wav_Oscillator_Type;
                                          Position: in Play_Second);

   procedure Apply_Oscillator_To_Buffer (O : in out Wav_Oscillator_Type;
                                         Buffer : Frame_Array_Access;
                                         Volume_Factor : Float := 1.0;
                                         Driver_Play_Frequency : Frequency_Type;
                                         Start_Buffer_Time : Synthetizer_Time;
                                         ReachEndOscillator : out Boolean;
                                         Returned_Current_Oscillator_Position : out Play_Second);



   type Oscillator_Reference is access all Oscillator'Class;



end Synth.Oscillators;
