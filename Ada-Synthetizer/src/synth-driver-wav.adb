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

with Synth.Wav;
use Synth.Wav;

package body Synth.Driver.Wav is

   ----------
   -- Open --
   ----------

   procedure Open
     (Driver : out Sound_Driver_Access;
      Frequency : Frequency_Type := 44100.0;
      FileName : String)
   is
      S : constant WAV_Driver_Access := new WAV_Driver;
   begin
      S.Frequency := Frequency;
      Open_For_Write (FileName => FileName,
                     WAV_File => S.WAV_File);
      Driver := Sound_Driver_Access (S);
   end Open;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Driver : in out WAV_Driver) is
   begin
      Close_And_Finalize (WAV_File => Driver.WAV_File);
   end Close;

   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out WAV_Driver;
      Buffer : PCM_Frame_Array_Access;
      Play_Reference_Buffer_Start_Time : Synthetizer_Time)
   is
      --  compute the end of play
      EndTime : constant Time := Clock +
        Microseconds
             (US =>
                    Integer (Float (1.0) / Float (Driver.Frequency)
              * Float (Buffer.all'Length * 1_000_000)));
      Frames : aliased Frame_Array := (Buffer.all'Range => 0.0);
   begin
      Driver.Current_Buffer_Start_Time := Play_Reference_Buffer_Start_Time;

      for I in Buffer.all'Range loop
         Frames (I) := Frame (Float (Buffer (I)) / Float (2**15));
      end loop;

      Write_Data (WAV_File => Driver.WAV_File,
                  Datas    => Frames'Unchecked_Access);
      --  block until end of play
      delay until EndTime;
   end Play;

   -------------------
   -- Get_Frequency --
   -------------------

   overriding function Get_Frequency
     (Driver : WAV_Driver)
      return Frequency_Type
   is
   begin
      return Driver.Frequency;
   end Get_Frequency;

   ---------------------------
   -- Get_Current_Play_Time --
   ---------------------------

   overriding function Get_Current_Play_Time (Driver: WAV_Driver)
                                  return Synthetizer_Time is
   begin
      return Driver.Current_Buffer_Start_Time;
   end Get_Current_Play_Time;

end Synth.Driver.Wav;
