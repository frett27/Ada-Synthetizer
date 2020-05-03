------------------------------------------------------------------------------
--                            Ada Synthetizer                               --
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

with Soundio; use Soundio;
with System;
with Ada.Unchecked_Conversion;

package body Synth.Driver.CxSoundio is

   function To_Soundio_Driver_Access is
     new Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Soundio_Driver_Access);
   function From_Soundio_Driver_Access is
     new Ada.Unchecked_Conversion (Source => Soundio_Driver_Access,
                                  Target => System.Address);

   procedure Write_Float_Sample is new Write_Sample (Float);

   ---------------------------
   -- Write_Device_Callback --
   ---------------------------

   procedure Write_Device_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int)
   is
      pragma Unreferenced (Frame_Count_Min, Frame_Count_Max);
      Driver : constant Soundio_Driver_Access :=
        To_Soundio_Driver_Access (Out_Stream.User_Data);

      Areas : SoundIo_Channel_Area_Ptr;
      Frame_Count : int;
      Frame : int;
      Sample : PCM_Frame;
      Float_Sample : Float;

      use Soundio_Channel_Area_Ptrs;

   begin
      if Driver.CurrentPlayedBuffer = null then
             return;
      end if;

      Frame_Count := int (Driver.CurrentPlayedBuffer'Length
                          - Driver.CurrentIndex);

      if Frame_Count = 0 then
               Driver.CurrentPlayedBuffer := null;
         SBuffer.Verlassen;
         return;
      end if;

      Check_Error (Outstream_Begin_Write (Out_Stream, Areas, Frame_Count));

      Frame := 0;
      while Frame_Count > 0 loop
         Sample := Driver.CurrentPlayedBuffer (Driver.CurrentIndex);
         Float_Sample := Float (Sample) / Float (2**16);
         --  both voices on left and right
         Write_Float_Sample
           (Get_Area (Areas, 0), Frame, Float_Sample);
         Write_Float_Sample
           (Get_Area (Areas, 1), Frame, Float_Sample);

         Driver.CurrentIndex := Natural'Succ (Driver.CurrentIndex);
         Frame_Count := int'Pred (Frame_Count);
         Frame := int'Succ (Frame);
      end loop;

      --  Frame_Count = 0;
      Driver.CurrentPlayedBuffer := null;
      Check_Error (Outstream_End_Write (Out_Stream));
     --  Put_Line (Err'Img);

      SBuffer.Verlassen;
   exception
      when others =>
         --  don't propagate exception in the C callback play ..
         SBuffer.Verlassen;

   end Write_Device_Callback;

   ----------
   -- Open --
   ----------

   procedure Open
     (Driver : out Sound_Driver_Access;
      Frequency : Frequency_Type := 44100.0)
   is
      Default_Device_Index : int;
      SIODriver : Soundio_Driver_Access;
   begin

      SIODriver := new Soundio_Driver;
      SIODriver.Frequency := Frequency;

      SIODriver.IO := Create;
      Check_Error (Connect (SIODriver.IO));

      declare
         Out_Stream : access SoundIo_Out_Stream renames SIODriver.Out_Stream;
      begin

         Flush_Events (SIODriver.IO);

         Default_Device_Index := Default_Output_Device_Index (SIODriver.IO);

         SIODriver.Device := Get_Output_Device (SIODriver.IO,
                                                Default_Device_Index);

         Out_Stream := Outstream_Create (SIODriver.Device);

         Out_Stream.Format := Format_Float32NE;
         Out_Stream.Sample_Rate := Interfaces.C.int (Frequency);
         Out_Stream.Write_Callback :=
              Write_Device_Callback'Access;

         Out_Stream.User_Data :=
           From_Soundio_Driver_Access (SIODriver.all'Access);

         Check_Error (Outstream_Open (Out_Stream));

         Check_Error (Outstream_Start (Out_Stream));

         Driver := SIODriver.all'Access;

      end;
   end Open;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Driver : in out Soundio_Driver) is
   begin
      Outstream_Destroy (Driver.Out_Stream);
      Device_Unref (Driver.Device);
      Destroy (Driver.IO);
   end Close;

   ----------
   -- Play --
   ----------

   overriding procedure Play
     (Driver : in out Soundio_Driver;
      Buffer : PCM_Frame_Array_Access;
      Play_Reference_Buffer_Start_Time : Synthetizer_Time)
   is
   begin

      SBuffer.Passen;
      Driver.CurrentPlayedBuffer := Buffer;
      Driver.CurrentIndex := Buffer'First;

      Driver.Current_Buffer_Start_Time := Play_Reference_Buffer_Start_Time;
      Driver.Buffer_Playing_Ref_Time := Clock;

      SBuffer.Passen; -- block the exit, to avoid freeing the buffer by caller
                      --  will be released on callback
      SBuffer.Verlassen;

   end Play;

   overriding function Get_Current_Play_Time (Driver: Soundio_Driver)
                                             return Synthetizer_Time is
      --  T : Time := Clock;
     --  Modifier_Time  :Time_Span := T - Driver.Buffer_Playing_Ref_Time;
   begin
      return Driver.Current_Buffer_Start_Time; -- + Synthetizer_Time(Modifier_Time);
   end Get_Current_Play_Time;

   -------------------
   -- Get_Frequency --
   -------------------

   overriding function Get_Frequency
     (Driver : Soundio_Driver)
      return Frequency_Type
   is
   begin
      return Driver.Frequency;
   end Get_Frequency;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      entry Passen when Current > 0 and then Verlassen'Count = 0 is
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

end Synth.Driver.CxSoundio;
