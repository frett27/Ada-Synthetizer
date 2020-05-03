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

with Ada;
with Ada.Real_Time; use Ada.Real_Time;
-- with Ada.Text_IO; use Ada.Text_IO;

With Sound;
with System;

------------------------
-- Synth.Driver.Win32 --
------------------------

package body Synth.Driver.Alsa is


   ----------
   -- Open --
   ----------

   procedure Open (Driver : out Sound_Driver_Access;
	   Frequency: Frequency_Type := 44100.0) is

       ALSADriver : ALSA_Driver_Access := new ALSA_Driver;
       Buffer_Size : Duration := 0.5;
       Period      : Duration := 0.1;
       Resolution  : Sound.Sample_Frequency := 
	       Sound.Sample_Frequency(Frequency);
   begin
       Sound.Mono.Open (Line        => ALSADriver.Speakers,
                        Mode        => Sound.Output,
                        Resolution  => Resolution,
                        Buffer_Size => Buffer_Size,
                        Period      => Period);

      --  open speakers

      Driver := ALSADriver.all'Access;
      ALSADriver.Frequency := Frequency;

   end Open;

   -------------------
   -- Get_Frequency --
   -------------------

   function Get_Frequency (Driver : ALSA_Driver)
                          return Frequency_Type is
   begin
      return Driver.Frequency;
   end Get_Frequency;




   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out ALSA_Driver;
      Buffer : PCM_Frame_Array_Access;
      Play_Reference_Buffer_Start_Time : Synthetizer_Time)
   is

      Timing : Time;
      pragma Unreferenced (Timing);

      AlsaBuffer : Sound.Mono.Frame_Array(Buffer'Range);
      Last : Integer;
      pragma Unreferenced (Last);
   begin

      Driver.Current_Buffer_Start_Time := Play_Reference_Buffer_Start_Time;
      Driver.Buffer_Playing_Ref_Time := Clock;

      for I in Buffer'Range loop
         AlsaBuffer(I) := Sound.Mono.Frame(Buffer(I) );
      end loop;

      

      Sound.Mono.Write(Line => Driver.Speakers,
                         Item => AlsaBuffer,
                       Last => Last);




   end;

   overriding function Get_Current_Play_Time(Driver: ALSA_Driver)
                                             return Synthetizer_Time is
      -- T : Time := Clock;
     --  Modifier_Time  :Time_Span := T - Driver.Buffer_Playing_Ref_Time;
   begin
      return Driver.Current_Buffer_Start_Time; -- + Synthetizer_Time(Modifier_Time);
   end Get_Current_Play_Time;


   overriding procedure Close (Driver : in out ALSA_Driver) is
   begin
      null;
   end Close;



end Synth.Driver.Alsa;
