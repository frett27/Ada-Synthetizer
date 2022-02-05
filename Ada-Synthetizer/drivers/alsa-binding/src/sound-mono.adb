--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
Interfaces.C.Strings;

package body Sound.Mono is
   procedure Close (Line : in out Line_Type) is
      use type Interfaces.C.int;
      Error : Interfaces.C.int;
   begin
      Error := snd_pcm_close (Line);

      if Error /= 0 then
         raise Program_Error with "snd_pcm_close failed: " & Error'Img;
      end if;
   end Close;

   function Is_Open (Line : in     Line_Type) return Boolean is
      use Sound.ALSA;
   begin
      case snd_pcm_state (Line) is
         when Prepared | Running =>
            return True;
         when Open | Setup | XRun | Draining | Paused | Suspended
            | Disconnected =>
            return False;
      end case;
   end Is_Open;

   procedure Open (Line        : in out Line_Type;
                   Mode        : in     Line_Mode;
                   Resolution  : in out Sample_Frequency;
                   Buffer_Size : in out Duration;
                   Period      : in out Duration) is
      use Interfaces.C, Interfaces.C.Strings, Sound.ALSA;
      Name       : aliased char_array :=  To_C ("default");
      --  To_C ("plughw:0,0");
      Error      : Interfaces.C.int;
      Local_Line : aliased Line_Type := Line;
      Settings   : aliased Params_Access;
   begin
      Error := snd_pcm_open (pcmp   => Local_Line'Access,
                             name   => To_Chars_Ptr (Name'Unchecked_Access),
                             stream => Sound.ALSA.Value (Mode),
                             mode   => 0);

      if Error < 0 then
         declare
            errMsg : constant Interfaces.C.Strings.chars_ptr :=
              snd_strerror (Error);
         begin

            raise Program_Error with "Error code (snd_pcm_open): " & Error'Img
              & " " & Interfaces.C.Strings.Value (errMsg);
         end;

      end if;

      Error := snd_pcm_hw_params_malloc (Settings'Access);
      if Error < 0 then
         declare
            errMsg : constant Interfaces.C.Strings.chars_ptr :=
              snd_strerror (Error);
         begin

            raise Program_Error with "Error code (snd_pcm_hw_params_malloc): "
              & Error'Img
              & " " & Interfaces.C.Strings.Value (errMsg);
         end;
      end if;

      Clear_Settings :
      begin
         Error := snd_pcm_hw_params_any (pcm    => Local_Line,
                                         params => Settings);

         if Error < 0 then
            declare
               errMsg : constant Interfaces.C.Strings.chars_ptr :=
                 snd_strerror (Error);
            begin
               raise Program_Error with
                 "Error code (snd_pcm_hw_params_any): " & Error'Img
                 & " " & Interfaces.C.Strings.Value (errMsg);
            end;
         end if;
      end Clear_Settings;

      Set_Resampling_Rate :
      begin
         Error := snd_pcm_hw_params_set_rate_resample
           (pcm    => Local_Line,
            params => Settings,
            val    => False);

         if Error < 0 then
            declare
               errMsg : constant Interfaces.C.Strings.chars_ptr :=
                 snd_strerror (Error);
            begin
               raise Program_Error with
                 "Error code (snd_pcm_hw_params_set_rate_resample): "
                 & Error'Img  & " " & Interfaces.C.Strings.Value (errMsg);
            end;

         end if;
      end Set_Resampling_Rate;

      Set_Sampling_Layout :
      begin
         Error := snd_pcm_hw_params_set_access
           (pcm    => Local_Line,
            params => Settings,
            val    => Read_Write_Interleaved);

         if Error < 0 then
            declare
               errMsg : constant Interfaces.C.Strings.chars_ptr :=
                 snd_strerror (Error);
            begin
               raise Program_Error with
                 "Error code (snd_pcm_hw_params_set_access): "
                 & Error'Img & " " & Interfaces.C.Strings.Value (errMsg);
            end;

         end if;
      end Set_Sampling_Layout;

      Set_Recording_Format :
      begin
         Error := snd_pcm_hw_params_set_format
           (pcm    => Local_Line,
            params => Settings,
            format => Sound.ALSA.Signed_16_Bit);

         if Error < 0 then
            raise Program_Error with
              "Error code (snd_pcm_hw_params_set_format): " & Error'Img;
         end if;
      end Set_Recording_Format;

      Set_Channel_Count :
      begin
         Error := snd_pcm_hw_params_set_channels
           (pcm    => Local_Line,
            params => Settings,
            val    => 1);

         if Error < 0 then
            raise Program_Error with
              "Error code (snd_pcm_hw_params_set_channels): " & Error'Img;
         end if;
      end Set_Channel_Count;

      Set_Sample_Frequency :
      declare
         Sample_Rate   : aliased Interfaces.C.unsigned :=
           Interfaces.C.unsigned (Resolution);
         Approximation : aliased Sound.ALSA.Approximation_Direction := 1;
      begin
         Error := snd_pcm_hw_params_set_rate_near
           (pcm    => Local_Line,
            params => Settings,
            val    => Sample_Rate'Access,
            dir    => Approximation'Access);

         if Error < 0 then
            raise Program_Error with
              "Error code (snd_pcm_hw_params_set_rate_near): " & Error'Img;
         end if;

         Resolution := Sample_Frequency (Sample_Rate);
      end Set_Sample_Frequency;

      Set_Buffer_Time :
      declare
         Buffer_Time   : aliased Interfaces.C.unsigned :=
           Interfaces.C.unsigned (1_000_000 * Buffer_Size);
         Approximation : aliased Sound.ALSA.Approximation_Direction := 0;
      begin
         Error := snd_pcm_hw_params_set_buffer_time_near
           (pcm    => Local_Line,
            params => Settings,
            val    => Buffer_Time'Access,
            dir    => Approximation'Access);

         if Error < 0 then
            raise Program_Error with
              "Error code (snd_pcm_hw_params_set_buffer_time_near): " &
              Error'Img;
         end if;

         Buffer_Size := Duration (Buffer_Time) / 1_000_000.0;
      end Set_Buffer_Time;

--        Set_Period :
--        declare
--           Period_Time   : aliased Interfaces.C.unsigned :=
--             Interfaces.C.unsigned (1_000_000 * Period);
--           Approximation : aliased Sound.ALSA.Approximation_Direction := 0;
--        begin
--           Error := snd_pcm_hw_params_set_period_time_near
--             (pcm    => Local_Line,
--              params => Settings,
--              val    => Period_Time'Access,
--              dir    => Approximation'Access);
--
--           if Error < 0 then
--              raise Program_Error with
--                "Error code (snd_pcm_hw_params_set_period_time_near): " &
--                Error'Img;
--           end if;
--
--           Period := Duration (Period_Time) / 1_000_000.0;
--        end Set_Period;

      Register_Settings :
      begin
         Error := snd_pcm_hw_params (pcm    => Local_Line,
                                     params => Settings);

         if Error < 0 then
            raise Program_Error with
              "Error code (snd_pcm_hw_params): " & Error'Img;
         end if;
      end Register_Settings;

      Line := Local_Line;
   end Open;

   procedure Read (Line : in     Line_Type;
                   Item :    out Frame_Array;
                   Last :    out Natural) is
      pragma Unmodified (Item); -- As we cheat with "function snd_pcm_readi".

      function snd_pcm_readi (pcm    : in     Line_Type;
                              buffer : in     Frame_Array; -- actually "out"
                              size   : in     ALSA.snd_pcm_uframes_t)
                              return ALSA.snd_pcm_sframes_t;
      pragma Import (C, snd_pcm_readi);

      use type Sound.ALSA.snd_pcm_sframes_t;
      Received_Frame_Count : Sound.ALSA.snd_pcm_sframes_t;
   begin
      Received_Frame_Count := snd_pcm_readi (pcm    => Line,
                                             buffer => Item,
                                             size   => Item'Length);

      if Received_Frame_Count < 0 then
         raise Program_Error with
           "snd_pcm_readi failed: " & Received_Frame_Count'Img;
      else
         Last := Item'First - 1 + Natural (Received_Frame_Count);
      end if;
   end Read;

   procedure Write (Line : in     Line_Type;
                    Item : in     Frame_Array;
                    Last :    out Natural) is
      function snd_pcm_writei (pcm    : in     Line_Type;
                               buffer : in     Frame_Array;
                               size   : in     ALSA.snd_pcm_uframes_t)
                               return ALSA.snd_pcm_sframes_t;
      pragma Import (C, snd_pcm_writei);

      function snd_pcm_recover (pcm   : in    Line_Type;
                                frames : in ALSA.snd_pcm_sframes_t;
                                i : in Interfaces.C.int)
                                return ALSA.snd_pcm_sframes_t;
      pragma Import (C, snd_pcm_recover);

      use type Sound.ALSA.snd_pcm_sframes_t;
      --  use type Interfaces.C.int;
      Written_Frame_Count : Sound.ALSA.snd_pcm_sframes_t;
      --  RetCode : Interfaces.C.int;
   begin
      --  RetCode := snd_pcm_drain (Line);
      --  if RetCode < 0 then
      --     raise Program_Error with
      --       "snd_pcm_drain failed: " & Integer'Image (Integer (RetCode));
      --  end if;

      Written_Frame_Count := snd_pcm_writei (pcm    => Line,
                                             buffer => Item,
                                             size   => Item'Length);

      if Written_Frame_Count < 0 then
         Written_Frame_Count := snd_pcm_recover (Line,
                                                 Written_Frame_Count, 0);
      end if;

      if Written_Frame_Count < 0 then
         raise Program_Error with
           "snd_pcm_writei failed: " & Written_Frame_Count'Img;
      else

         Last := Item'First - 1 + Natural (Written_Frame_Count);
      end if;
   end Write;
end Sound.Mono;
