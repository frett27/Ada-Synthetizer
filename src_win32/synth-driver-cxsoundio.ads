------------------------------------------------------------------------------
--                             Ada Synthetizer                              --
--                                                                          --
--                         Copyright (C) 2018-2019                          --
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

with Interfaces.C; use Interfaces.C;
with Soundio;

package Synth.Driver.CxSoundio is

   type Soundio_Driver is new Sound_Driver with private;

   type Soundio_Driver_Access is access all Soundio_Driver;

   ----------
   -- Open --
   ----------
   --  factory
   procedure Open (Driver : out Sound_Driver_Access;
                   Frequency : Frequency_Type := 44100.0);

   -----------
   -- Close --
   -----------

   overriding procedure Close (Driver : in out Soundio_Driver);

   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out Soundio_Driver;
      Buffer : PCM_Frame_Array_Access);

   -------------------
   -- Get_Frequency --
   -------------------

   overriding function Get_Frequency (Driver : Soundio_Driver)
           return Frequency_Type;

private

   type Soundio_Driver is new Synth.Driver.Sound_Driver with record

      IO : access Soundio.SoundIo;
      Device : access Soundio.SoundIo_Device;
      Out_Stream : access Soundio.SoundIo_Out_Stream;

      Frequency : Frequency_Type;

      CurrentPlayedBuffer : PCM_Frame_Array_Access := null;
      CurrentIndex : Natural := 0;

      IsPlaying : Boolean := False;
   end record;

   --  simple semaphone for synchronous play and buffers creation
   protected type Semaphore (N : Positive) is
      entry Passen;
      entry Verlassen;
      function Allocated return Natural;
   private
      Current : Natural := N;
   end Semaphore;

   SBuffer : Semaphore (1); -- for handling the buffers

    procedure Write_Device_Callback
     (Out_Stream       : access Soundio.SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Device_Callback);

end Synth.Driver.CxSoundio;
