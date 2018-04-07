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

with Win32; use Win32;
with Win32.Mmsystem; use Win32.Mmsystem;
with Ada.Unchecked_Conversion;
with System;


------------------------
-- Synth.Driver.Win32 --
------------------------

package Synth.Driver.Win32 is



   type WIN32_Driver is new Sound_Driver with private;

   type WIN32_Driver_Access is access all WIN32_Driver;


   ----------
   -- Open --
   ----------
   -- factory
   procedure Open (Driver : out Sound_Driver_Access);

   -----------
   -- Close --
   -----------

   overriding procedure Close (Driver : in out WIN32_Driver);

   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out WIN32_Driver;
      Buffer : in     PCM_Frame_Array_Access);


private

   type Play_Buffer_Cursor is mod 5;
   type Play_Buffer_Type is array (Play_Buffer_Cursor) of LPWAVEHDR;


   function To_PWAVEHDR is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => PWAVEHDR);




   type WIN32_Driver is new Synth.Driver.Sound_Driver with record
      hWo : aliased HWAVEOUT;
      wfx : LPCWAVEFORMATEX;
      Buffer : Play_Buffer_Type := Play_Buffer_Type'(others => null);
      Buffer_First : Play_Buffer_Cursor := Play_Buffer_Cursor'First;
      Buffer_Last : Play_Buffer_Cursor := Play_Buffer_Cursor'First;
      IsPlaying : boolean := false;
   end record;


   -- call back definition

   type Sound_Call_Back_Type is access procedure(hwo : HWAVEOUT ;
                                    uMsg : UINT;
                                    dwInstance , dwParam1 , dwParam2: LPDWORD);
   pragma Convention(StdCall, Sound_Call_Back_Type);


   procedure Sound_Driver_Call_Back(hwo : HWAVEOUT ;
                                    uMsg : UINT;
                                    dwInstance , dwParam1 , dwParam2: LPDWORD);
   pragma Export(StdCall,Sound_Driver_Call_Back);


   -- simple semaphone for synchronous play and buffers creation
   protected type Semaphore(N:Positive) is
      entry Passen;
      entry Verlassen;
      function Allocated return Natural;
   private
      Current : Natural := N;
   end Semaphore;

   SBuffer : Semaphore(3); -- for handling the buffers
   SBufferCursor : Semaphore(1); -- for handling the cursors


end Synth.Driver.Win32;
