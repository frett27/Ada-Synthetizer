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

   overriding function Get_Frequency (Driver : in out Soundio_Driver)
           return Frequency_Type;

private

   type Play_Buffer_Cursor is mod 4;
   type Play_Buffer_Type is 
     array (Play_Buffer_Cursor) of PCM_Frame_Array_Access;

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
   
end Synth.Driver.cxSoundio;
