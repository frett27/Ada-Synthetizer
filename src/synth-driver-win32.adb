------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2016                          --
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
with System;
with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;

with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Win32.Winbase; use Win32.Winbase;
with Win32.WinDef; use Win32.WinDef;

------------------------
-- Synth.Driver.Win32 --
------------------------

package body Synth.Driver.Win32 is


   M : Mutex;

   -- Call back for the sound playing, to avoid the gaps between plays

   WOM_OPEN : constant :=
     16#03BB#; -- Sent when the device is opened using the waveOutOpen function.
   WOM_CLOSE : constant :=
     16#03BC#; --  Sent when the device is closed using the waveOutClose function.
   WOM_DONE : constant :=
     16#03BD#; -- Sent when the device driver is finished with a data block sent using the waveOutWrite function.

   function to_Adress is new Ada.Unchecked_Conversion
     (Source => LPDWORD,
      Target => System.Address);

   procedure Sound_Driver_Call_Back
     (hwo                            : in HWAVEOUT;
      uMsg                           : in UINT;
      dwInstance, dwParam1, dwParam2 : in LPDWORD)
   is
   begin

      case uMsg is

         when WOM_OPEN =>
            Put_Line ("open the line");

         when WOM_DONE =>
            Put_Line ("play done, unallocate the buffer");
            declare
               H : HGLOBAL;
            begin
               H := GlobalFree (hMem => to_Adress (dwParam1));
            end;

            M.Release;

         when WOM_CLOSE =>
            Put_Line ("line close");

         when others =>
            Put_Line ("unknown message " & UINT'Image (uMsg));

      end case;

   exception
      when others =>
         Put_Line ("exception in call back");

   end Sound_Driver_Call_Back;

   function CB_To_DWORD is new Ada.Unchecked_Conversion
     (Source => Sound_Call_Back_Type,
      Target => DWORD);

   function To_Access is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => PWAVEFORMATEX);

   ----------
   -- Open --
   ----------

   procedure Open (Driver : out WIN32_Driver) is
      result : MMRESULT;
   begin

      Driver.wfx :=
        To_Access
          (GlobalAlloc
             (uFlags  => LMEM_FIXED,
              dwBytes => WAVEFORMATEX'Size / System.Storage_Unit));

      Driver.wfx.all :=
        (nSamplesPerSec  => 44100,
         wBitsPerSample  => 16,
         nChannels       => 1,
         cbSize          => 0,
         wFormatTag      => 1,
         nBlockAlign     => 2 * 1,
         nAvgBytesPerSec => 2 * 44100);

--        testFormat.nBlockAlign = (ushort)(testFormat.nChannels *
--                                          (testFormat.wBitsPerSample / 8));
--        testFormat.nAvgBytesPerSec = (testFormat.nSamplesPerSec *
--                                        testFormat.nBlockAlign);
--

      result :=
        waveOutOpen
          (lphwo      => Driver.hWo'Unchecked_Access,
           uDeviceID  => 16#FFFF#,  -- WAVE_MAPPER
           lpFormat   => Driver.wfx,
           dwCallback => CB_To_DWORD (Sound_Driver_Call_Back'Access),
           dwInstance => 0,
           dwFlags    => CALLBACK_FUNCTION); -- CALLBACK FUNCTION

      if result > 0 then
         raise Program_Error
           with "Error opening the waveOut device :" & MMRESULT'Image (result);
      end if;

   end Open;

   ----------
   -- Play --
   ----------

   procedure Play
     (Driver : in out WIN32_Driver;
      Buffer : in     PCM_Frame_Array_Access)
   is

      H : PWAVEHDR;

      function CPByte is new Ada.Unchecked_Conversion
        (Source => PCM_Frame_Access,
         Target => PBYTE);

      function To_PWAVEHDR is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => PWAVEHDR);

      result : MMRESULT;

      Timing : Time;

   begin

      M.Seize;

      H :=
        To_PWAVEHDR
          (GlobalAlloc
             (uFlags  => LMEM_FIXED,
              dwBytes => WAVEHDR'Size / System.Storage_Unit));

      H.dwBufferLength := Buffer'Length * PCM_Frame'Size / System.Storage_Unit;
      H.lpData         := CPByte (Buffer (Buffer'First)'Access);


      Timing := clock;

      result :=
        waveOutPrepareHeader
          (hwo  => Driver.hWo,
           pwh  => H,
           cbwh => H.all'Size / System.Storage_Unit);

      put_line("prepare in " & Duration'Image(To_Duration( Clock - Timing)));

      if result > 0 then
         raise Program_Error
           with "Error preparing the output waveOutPrepareHeader : " &
           MMRESULT'Image (result);
      end if;

      result :=
        waveOutWrite
          (hwo  => Driver.hWo,
           pwh  => H,
           cbwh => H.all'Size / System.Storage_Unit);

      if result > 0 then
         raise Program_Error
           with "Error calling the play procedure waveOutWrite :" &
           MMRESULT'Image (result);
      end if;


   exception
      when E : others =>
         m.Release;
         raise;

   end Play;

   -----------
   -- Close --
   -----------

   procedure Close (Driver : in out WIN32_Driver) is
      result : MMRESULT;
   begin

      m.Seize;
      m.Release;

      result := waveOutClose (Driver.hWo);

      if result > 0 then
         raise Program_Error
           with "Error closing the device :" & MMRESULT'Image (result);
      end if;

   end Close;

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

end Synth.Driver.Win32;
