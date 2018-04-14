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

with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

--  use windows api for sounds
with Win32.Winbase; use Win32.Winbase;
with Win32.Windef; use Win32.Windef;

------------------------
-- Synth.Driver.Win32 --
------------------------

package body Synth.Driver.Win32 is

   --  Call back for the sound playing, to avoid the gaps between plays

   WOM_OPEN : constant :=
     16#03BB#; -- Sent when the device is opened using
               -- the waveOutOpen function.
   WOM_CLOSE : constant :=
     16#03BC#; -- Sent when the device is closed using
               -- the waveOutClose function.
   WOM_DONE : constant :=
     16#03BD#; -- Sent when the device driver is finished with
               -- a data block sent using the waveOutWrite function.

   function to_Address is new Ada.Unchecked_Conversion
     (Source => LPDWORD,
      Target => System.Address);
   pragma Unreferenced (to_Address);

   function to_WIN32_Driver is new Ada.Unchecked_Conversion
     (Source => LPDWORD,
      Target => WIN32_Driver_Access);

   function to_DWORD is new Ada.Unchecked_Conversion
     (Source => WIN32_Driver_Access,
      Target => DWORD);

   function to_LPWAVEHDR is new Ada.Unchecked_Conversion
     (Source => LPVOID,
      Target => LPWAVEHDR);


   function To_Address is new Ada.Unchecked_Conversion
     (Source => PWAVEHDR,
      Target => HGLOBAL);

   function To_Address is new Ada.Unchecked_Conversion
     (Source => LPBYTE,
      Target => HGLOBAL);

   function To_System_Address is new Ada.Unchecked_Conversion
     (Source => HGLOBAL,
      Target => Integer);





   procedure Sound_Driver_Call_Back
     (hwo                            : HWAVEOUT;
      uMsg                           : UINT;
      dwInstance, dwParam1, dwParam2 : LPDWORD)
   is
      pragma Unreferenced (hwo, dwParam2, dwParam1);
      Driver : constant WIN32_Driver_Access := to_WIN32_Driver (dwInstance);
      Buffer_Empty : Boolean;
      result : MMRESULT;


      procedure FreeStructure(Buffer : in PWAVEHDR) is

         H : HGLOBAL;
         I : Integer;
         use System;
      begin

         -- free the buffer
         H := GlobalFree(hMem =>
                           to_Address(Buffer.lpData));

         I := To_System_Address(H);
         Put_Line("System I : " & Integer'Image(I));
         if I /= 0 then
            raise Program_Error with "Failed to release the windows wave data buffer";
         end if;

         result := waveOutUnprepareHeader(hwo  => Driver.hWo,
                                          pwh  => Buffer,
                                          cbwh => Buffer
                                          .all'Size / System.Storage_Unit);
         -- free the buffer
         -- H := GlobalFree (hMem => to_Address (dwParam1));
         I := To_System_Address(GlobalFree(hMem => to_Address(Buffer)));
         if I /= 0 then
            raise Program_Error with "Failed to release the windows WaveHeader structure";
         end if;
      end FreeStructure;



   begin

      case uMsg is
         when WOM_OPEN =>
            --  Put_Line ("open the line");
            null;
         when WOM_DONE =>
            --  Put_Line ("play done, unallocate the buffer");

            ada.Text_IO.Put_Line("Play Done Event..");

            SBuffer.Verlassen; -- block if there are no more buffers ?

            declare
               BufferToPlay : PWAVEHDR;
               PreviousPlayedBuffer : constant PWAVEHDR :=
                 Driver.Buffer (Driver.Buffer_Last);
            begin

               -- Driver.Buffer_Last has been consumed,
               -- we can remove it, but do it @ the end for preserving lattency

               -- consume or block

               SBufferCursor.Passen;
               Buffer_Empty := (Driver.Buffer_First = Driver.Buffer_Last);
               if Buffer_Empty then
                  ada.Text_IO.Put_Line("Empty buffer the next ..");
                  FreeStructure(Buffer => PreviousPlayedBuffer);

                  SBufferCursor.Verlassen;
                  return;
               end if;

               Driver.Buffer_Last :=
                 Play_Buffer_Cursor'Succ (Driver.Buffer_Last);

               BufferToPlay := Driver.Buffer (Driver.Buffer_Last);

               SBufferCursor.Verlassen;

                ada.Text_IO.Put_Line("Play the next ..");

               result :=
                 waveOutWrite
                   (hwo  => Driver.hWo,
                    pwh  => BufferToPlay,
                    cbwh => WAVEHDR'Size);

               if result > 0 then

                  case result is

                     when MMSYSERR_INVALHANDLE =>
                        raise Program_Error with
                          "Specified device handle is invalid";
                     when MMSYSERR_NODRIVER =>
                        raise Program_Error with
                          "No device driver is present";
                     when MMSYSERR_NOMEM =>
                        raise Program_Error with
                          "Unable to allocate or lock memory.";
                     when WAVERR_UNPREPARED =>
                        raise Program_Error with
                          "The data block pointed to by the pwh parameter " &
                          " hasn't been prepared";
                     when others =>
                        raise Program_Error with
                          "Error calling the play procedure waveOutWrite :" &
                          MMRESULT'Image (result);

                  end case;
               end if;

               -- clean up the buffer passed to out

               ada.Text_IO.Put_Line("clean");

               FreeStructure(Buffer => PreviousPlayedBuffer);


            end;



         when WOM_CLOSE =>
            Put_Line ("line close");

         when others =>
            Put_Line ("unknown message " & UINT'Image (uMsg));

      end case;

   exception
      when E : others =>
         DumpException (E);

   end Sound_Driver_Call_Back;

   function CB_To_DWORD is new Ada.Unchecked_Conversion
     (Source => Sound_Call_Back_Type,
      Target => DWORD);

   function To_Access is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => LPCWAVEFORMATEX);


   ----------
   -- Open --
   ----------

   procedure Open (Driver : out Sound_Driver_Access) is
      result : MMRESULT;
      WIN32_Driver_Ref : WIN32_Driver_Access;
   begin

      WIN32_Driver_Ref := new WIN32_Driver;

      WIN32_Driver_Ref.wfx :=
        To_Access
          (GlobalAlloc
             (uFlags  => LMEM_FIXED,
              dwBytes => WAVEFORMATEX'Size / System.Storage_Unit));

      WIN32_Driver_Ref.wfx.all :=
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
          (lphwo      => WIN32_Driver_Ref.hWo'Unchecked_Access,
           uDeviceID  => 16#FFFF#,  -- WAVE_MAPPER
           lpFormat   => WIN32_Driver_Ref.wfx,
           dwCallback => CB_To_DWORD (Sound_Driver_Call_Back'Access),
           dwInstance => to_DWORD (WIN32_Driver_Ref), -- pass the driver reference for the call back function
           dwFlags    => CALLBACK_FUNCTION); -- CALLBACK FUNCTION

      if result > 0 then
         raise Program_Error
           with "Error opening the waveOut device :" & MMRESULT'Image (result);
      end if;

      --        -- allocate buffers
      --        for i in WIN32_Driver_Ref.Buffer'Range loop
      --           WIN32_Driver_Ref.Buffer(i) := To_PWAVEHDR
      --             (GlobalAlloc
      --                (uFlags  => LMEM_FIXED,
      --                 dwBytes => WAVEHDR'Size / System.Storage_Unit));
      --        end loop;

      Driver := WIN32_Driver_Ref.all'Access;

   end Open;

   ----------
   -- Play --
   ----------

   procedure Play
     (Driver : in out WIN32_Driver;
      Buffer : PCM_Frame_Array_Access)
   is

      H : LPWAVEHDR;

      function To_LPBYTE is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => LPBYTE);

      function To_Address is new Ada.Unchecked_Conversion
        (Source => LPBYTE,
         Target => System.Address);

      type Buffer_Range is array (Natural range Buffer'Range) of PCM_Frame;

      result : MMRESULT;

      Timing : Time;
      pragma Unreferenced (Timing);

   begin

      --  Ada.Text_IO.put_Line("Playing a sound");

      --  Allocate the prepared buffer
      H :=
        to_LPWAVEHDR
          (GlobalAlloc
             (uFlags  => LMEM_FIXED,
              dwBytes => WAVEHDR'Size / System.Storage_Unit));

      --  Ada.Text_IO.put_Line("Header allocated");

      if H = null then
         raise Program_Error with "Failed to allocate the wave header buffer";
      end if;

      H.dwBufferLength := Buffer'Length * PCM_Frame'Size / System.Storage_Unit;
      --  Copy the buffer

      --  Ada.Text_IO.Put_Line("buffer size :" & DWORD'Image(H.dwBufferLength));

      H.dwFlags := 0; -- mandatory

      --  allocate the associated datas
      H.lpData := To_LPBYTE (GlobalAlloc
                            (uFlags  => LMEM_FIXED,
                             dwBytes => H.dwBufferLength));

      --  Ada.Text_IO.put_Line("Data allocated");

      if H.lpData = null then
         raise Program_Error with "Failed to allocate the data buffer";
      end if;

      --  copy the bytes to prepare the buffer
      declare
         Dest : Buffer_Range;
         for Dest'Address use To_Address (H.lpData);
      begin
         --  copy the buffer to global win32 mem
         Dest := Buffer_Range (Buffer.all);
      end;

      --  start timing
      Timing := Clock;

      --  Prepare the buffer
      result :=
        waveOutPrepareHeader
          (hwo  => Driver.hWo,
           pwh  => H,
           cbwh => WAVEHDR'Size / System.Storage_Unit);

      --  put_line("prepare in " & Duration'Image(To_Duration( Clock - Timing)));

      if result > 0 then
         raise Program_Error
           with "Error preparing the output waveOutPrepareHeader : " &
           MMRESULT'Image (result);
      end if;

      --  M.Seize;

      SBuffer.Passen;

      --  add a buffer to played one, normally take by the callback, if filled
      SBufferCursor.Passen;
      Driver.Buffer_First := Play_Buffer_Cursor'Succ (Driver.Buffer_First);
      Driver.Buffer (Driver.Buffer_First) := H;
      SBufferCursor.Verlassen;

      declare
         BufferToPlay : PWAVEHDR;
      begin

         --  Ada.Text_IO.Put_Line("Elements in buffer :"
         -- & Natural'Image(SBuffer.Allocated));

         if SBuffer.Allocated <= 2 then
            --  Only one buffer, first play
            --  the others will be played by call back
            --  Ada.Text_IO.Put_Line("** First Play");

            SBufferCursor.Passen; -- consume the ring buffer
            Driver.Buffer_Last := Play_Buffer_Cursor'Succ (Driver.Buffer_Last);
            BufferToPlay := Driver.Buffer (Driver.Buffer_Last);
            SBufferCursor.Verlassen;

            --  ada.Text_IO.Put_Line("Play the next ..");

            --  launch the play
            result :=
              waveOutWrite
                (hwo  => Driver.hWo,
                 pwh  => BufferToPlay,
                 cbwh => WAVEHDR'Size);

            if result > 0 then
               raise Program_Error
                 with "Error calling the play procedure waveOutWrite :" &
                 MMRESULT'Image (result);
            end if;
         end if;

      exception
         when e: others =>
            SBufferCursor.Verlassen;
            DumpException (e);

            raise;
      end;

   end Play;

   -----------
   -- Close --
   -----------

   procedure Close (Driver : in out WIN32_Driver) is
      result : MMRESULT;
   begin

      result := waveOutClose (Driver.hWo);

      if result > 0 then
         raise Program_Error
           with "Error closing the device :" & MMRESULT'Image (result);
      end if;

   end Close;

   protected body Semaphore is

      entry Passen when Current > 0 is
      begin
         Current := Current - 1;
      end Passen;

      entry Verlassen when Current < N is
      begin
         Current := Current + 1;
      end Verlassen;

      function Allocated return Natural is
      begin
         return N - Current;
      end Allocated;

   end Semaphore;

end Synth.Driver.Win32;
