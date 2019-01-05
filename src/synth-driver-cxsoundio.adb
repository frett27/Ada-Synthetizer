
with Soundio; use Soundio;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;


package body Synth.Driver.CxSoundio is


   function To_Soundio_Driver_Access is
     new Ada.Unchecked_Conversion(Source => System.Address,
                                  Target => Soundio_Driver_Access);
   function From_Soundio_Driver_Access is
     new Ada.Unchecked_Conversion(Source => Soundio_Driver_Access,
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
      Driver : constant Soundio_Driver_Access :=
        To_Soundio_Driver_Access(Out_Stream.User_Data);

      Areas : SoundIo_Channel_Area_Ptr;
      Frame_Count : int;
      Frame : int;
      Sample : PCM_Frame;
      Float_Sample : Float;
      Err : SoundIo_Error;

      use Soundio_Channel_Area_Ptrs;

   begin
      if Driver.CurrentPlayedBuffer = null then
             return;
      end if;

      Frame_Count := int( Driver.CurrentPlayedBuffer'Length - Driver.CurrentIndex );

      if Frame_Count = 0 then
               Driver.CurrentPlayedBuffer := null;

      SBuffer.Verlassen;
         return;
      end if;

      Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);
      -- Put_Line (Err'Img);


      Frame := 0;
      while Frame_Count > 0 loop
         Sample := Driver.CurrentPlayedBuffer(Driver.CurrentIndex);
         Float_Sample := Float(Sample) / Float(2**16);
         Write_Float_Sample
           (Get_Area (Areas, 0), Frame, Float_Sample);

         Driver.CurrentIndex := Natural'Succ(Driver.CurrentIndex);
         Frame_Count := int'Pred(Frame_Count);
         Frame := int'Succ(Frame);
      end loop;

      --  Frame_Count = 0;
      Driver.CurrentPlayedBuffer := null;
      Err := Outstream_End_Write(Out_Stream);
     -- Put_Line (Err'Img);


      SBuffer.Verlassen;


   end Write_Device_Callback;

   ----------
   -- Open --
   ----------

   procedure Open
     (Driver : out Sound_Driver_Access;
      Frequency : Frequency_Type := 44100.0)
   is
      Err : SoundIo_Error;
      Default_Device_Index : int;
      SIODriver : Soundio_Driver_Access;
   begin

      SIODriver := new Soundio_Driver;
      SIODriver.Frequency := Frequency;

      SIODriver.IO := Create;
      Err := Connect (SIODriver.IO);
      Put_Line (Err'Img);

      declare
         Out_Stream : access SoundIo_Out_Stream renames SIODriver.Out_Stream;
      begin

         Flush_Events (SIODriver.IO);

         Default_Device_Index := Default_Output_Device_Index (SIODriver.IO);

         SIODriver.Device := Get_Output_Device (SIODriver.IO,
                                                Default_Device_Index);

         Out_Stream := Outstream_Create (SIODriver.Device);
         Out_Stream.Format := Format_Float32NE;
         Out_Stream.Write_Callback :=
              Write_Device_Callback'Access;

         Out_Stream.User_Data :=
           From_Soundio_Driver_Access(SIODriver.all'Access);

          Err := Outstream_Open (Out_Stream);
   Put_Line (Err'Img);

   Err := Outstream_Start (Out_Stream);
   Put_Line (Err'Img);



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
      Buffer : PCM_Frame_Array_Access)
   is
   begin

      -- is current play buffer out ?
      SBuffer.Passen;
      Driver.CurrentPlayedBuffer := Buffer;
      Driver.CurrentIndex := Buffer'First;

      -- Wake_Up(Driver.IO);

      SBuffer.Passen;
      SBuffer.Verlassen;

   end Play;

   -------------------
   -- Get_Frequency --
   -------------------

   overriding function Get_Frequency
     (Driver : in out Soundio_Driver)
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
