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

with Ada;
with Ada.Real_Time; use Ada.Real_Time;
-- with Ada.Text_IO; use Ada.Text_IO;

With Sound;


------------------------
-- Synth.Driver.Win32 --
------------------------

package body Synth.Driver.Alsa is


   ----------
   -- Open --
   ----------

   procedure Open (Driver : out Sound_Driver_Access) is

       ALSADriver : ALSA_Driver_Access := new ALSA_Driver;
       Resolution  : Sound.Sample_Frequency := 44_100;
       Buffer_Size : Duration := 0.5;
       Period      : Duration := 0.1;
   begin
       Sound.Mono.Open (Line        => ALSADriver.Speakers,
                        Mode        => Sound.Output,
                        Resolution  => Resolution,
                        Buffer_Size => Buffer_Size,
                        Period      => Period);

      --  open speakers

      Driver := ALSADriver.all'Access;
      ALSADriver.Frequency := Frequency_Type(Resolution);

   end Open;

   -------------------
   -- Get_Frequency --
   -------------------

   function Get_Frequency (Driver : in ALSA_Driver)
                          return Frequency_Type is
   begin
      return Driver.Frequency;
   end Get_Frequency;




   ----------
   -- Play --
   ----------

   procedure Play
     (Driver : in out ALSA_Driver;
      Buffer : PCM_Frame_Array_Access)
   is

      Timing : Time;
      pragma Unreferenced (Timing);
      AlsaBuffer : Sound.Mono.Frame_Array(Buffer'Range);
      Last : Integer;
   begin

      for I in Buffer'Range loop
         AlsaBuffer(I) := Sound.Mono.Frame(Buffer(I) );
      end loop;

      

      Sound.Mono.Write(Line => Driver.Speakers,
                         Item => AlsaBuffer,
                       Last => Last);




   end;

   procedure Close (Driver : in out ALSA_Driver) is
   begin
      null;
   end;



end Synth.Driver.Alsa;
