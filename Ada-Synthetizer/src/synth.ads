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

with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Unchecked_Deallocation;

package Synth is

   --  basic types used in this synthetizer
   subtype Synthetizer_Time is Time_Span;

   Not_Defined_Clock : constant Synthetizer_Time :=
     Synthetizer_Time (Time_Span_Last);

   Synthetizer_Time_First : constant Synthetizer_Time :=
     Synthetizer_Time (Time_Span_First);

   subtype Frame is Float range -1.0 .. 1.0;

   type Frame_Array is array (Natural range <>) of aliased Frame;
   type Frame_Array_Access is access all Frame_Array;
   subtype Frequency_Type is Float range 1.0 .. 100_000.0;

   function Frequency_Period (Frequency : Frequency_Type) return Time_Span;

   function MIDICode_To_Frequency (Midi_Code : Natural) return Frequency_Type;

   procedure Free_Frame_Array is
     new Ada.Unchecked_Deallocation (Object => Frame_Array,
                                     Name   => Frame_Array_Access);

   ---------------------------------------------------------------------------
   --  This is a recorded Sound

   type SoundSample (HasLoop : Boolean := False) is record
      --  File Frequency
      Frequency : Frequency_Type;

      Note_Frequency : Frequency_Type;

      Mono_Data : Frame_Array_Access;

      --  if can't stop is true, the sound will be played
      --  until the end (can't be stopped)
      Cant_Stop : Boolean;

      case HasLoop is
         when True =>
            Loop_Start : Natural;
            Loop_End   : Natural;
         when False =>
            null;
      end case;

   end record;

   Null_Sound_Sample : constant SoundSample :=
     SoundSample'(HasLoop    => False,
                  Frequency  => 44_100.0,
                  Note_Frequency => 440.0,
                  Mono_Data  => null,
                  Cant_Stop => False);

   --  16 bits PCM structure types
   type PCM_Frame is range  -(2**15) .. 2**15 - 1;

   for PCM_Frame'Size use 16;

   --  type PCM_Frame is private;
   type PCM_Frame_Access is access all PCM_Frame;
   type PCM_Frame_Array is array (Natural range <>) of aliased PCM_Frame;
   type PCM_Frame_Array_Access is access all PCM_Frame_Array;

   function To_Frame_Array (FA : Frame_Array) return PCM_Frame_Array;

   --  type representing a position in second in the sample
   subtype Play_Second is Long_Float;

   procedure DumpException (E : Ada.Exceptions.Exception_Occurrence);

end Synth;
