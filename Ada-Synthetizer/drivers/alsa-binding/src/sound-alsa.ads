--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Interfaces.C,
  Interfaces.C.Strings;
with
  Sound.Constants;

private
package Sound.ALSA is
   use type Interfaces.C.int;

   type void_ptr is private;
   type snd_pcm_t_ptr is private;

   type snd_pcm_stream_t is (Playback, Capture);
   for snd_pcm_stream_t use (Playback => Sound.Constants.Playback_Stream,
                             Capture  => Sound.Constants.Capture_Stream);
   Value : constant array (Line_Mode) of snd_pcm_stream_t
             := (Input  => Capture,
                 Output => Playback);

   type snd_pcm_state_t is (Open, Setup, Prepared, Running, XRun, Draining,
                            Paused, Suspended, Disconnected);
   for snd_pcm_state_t
     use (Open         => Sound.Constants.State_Open,
          Setup        => Sound.Constants.State_Setup,
          Prepared     => Sound.Constants.State_Prepared,
          Running      => Sound.Constants.State_Running,
          XRun         => Sound.Constants.State_XRun,
          Draining     => Sound.Constants.State_Draining,
          Paused       => Sound.Constants.State_Paused,
          Suspended    => Sound.Constants.State_Suspended,
          Disconnected => Sound.Constants.State_Disconnected);

   type snd_pcm_format_t is (Unknown,
                             Signed_8_Bit,
                             Unsigned_8_Bit,
                             Signed_16_Bit_Little_Endian,
                             Signed_16_Bit_Big_Endian,
                             Unsigned_16_Bit_Little_Endian,
                             Unsigned_16_Bit_Big_Endian,
                             Signed_24_Bit_Little_Endian,
                             Signed_24_Bit_Big_Endian,
                             Unsigned_24_Bit_Little_Endian,
                             Unsigned_24_Bit_Big_Endian,
                             Signed_32_Bit_Little_Endian,
                             Signed_32_Bit_Big_Endian,
                             Unsigned_32_Bit_Little_Endian,
                             Unsigned_32_Bit_Big_Endian,
                             FLOAT_Little_Endian,
                             FLOAT_Big_Endian,
                             FLOAT64_Little_Endian,
                             FLOAT64_Big_Endian,
                             IEC958_SUBFRAME_Little_Endian,
                             IEC958_SUBFRAME_Big_Endian,
                             MU_LAW,
                             A_LAW,
                             IMA_ADPCM,
                             MPEG,
                             GSM,
                             SPECIAL,
                             S24_3LE,
                             S24_3BE,
                             U24_3LE,
                             U24_3BE,
                             S20_3LE,
                             S20_3BE,
                             U20_3LE,
                             U20_3BE,
                             S18_3LE,
                             S18_3BE,
                             U18_3LE,
                             U18_3BE);
   for snd_pcm_format_t use
     (Unknown                       => Sound.Constants.Format_Unknown,
      Signed_8_Bit                  => Sound.Constants.Format_Signed_8_Bit,
      Unsigned_8_Bit                => Sound.Constants.Format_Unsigned_8_Bit,
      Signed_16_Bit_Little_Endian   =>
        Sound.Constants.Format_Signed_16_Bit_Little_Endian,
      Signed_16_Bit_Big_Endian      =>
        Sound.Constants.Format_Signed_16_Bit_Big_Endian,
      Unsigned_16_Bit_Little_Endian =>
        Sound.Constants.Format_Unsigned_16_Bit_Little_Endian,
      Unsigned_16_Bit_Big_Endian    =>
        Sound.Constants.Format_Unsigned_16_Bit_Big_Endian,
      Signed_24_Bit_Little_Endian   =>
        Sound.Constants.Format_Signed_24_Bit_Little_Endian,
      Signed_24_Bit_Big_Endian      =>
        Sound.Constants.Format_Signed_24_Bit_Big_Endian,
      Unsigned_24_Bit_Little_Endian =>
        Sound.Constants.Format_Unsigned_24_Bit_Little_Endian,
      Unsigned_24_Bit_Big_Endian    =>
        Sound.Constants.Format_Unsigned_24_Bit_Big_Endian,
      Signed_32_Bit_Little_Endian   =>
        Sound.Constants.Format_Signed_32_Bit_Little_Endian,
      Signed_32_Bit_Big_Endian      =>
        Sound.Constants.Format_Signed_32_Bit_Big_Endian,
      Unsigned_32_Bit_Little_Endian =>
        Sound.Constants.Format_Unsigned_32_Bit_Little_Endian,
      Unsigned_32_Bit_Big_Endian    =>
        Sound.Constants.Format_Unsigned_32_Bit_Big_Endian,
      FLOAT_Little_Endian           => Sound.Constants.Format_FLOAT_LE,
      FLOAT_Big_Endian              => Sound.Constants.Format_FLOAT_BE,
      FLOAT64_Little_Endian         => Sound.Constants.Format_FLOAT64_LE,
      FLOAT64_Big_Endian            => Sound.Constants.Format_FLOAT64_BE,
      IEC958_SUBFRAME_Little_Endian =>
        Sound.Constants.Format_IEC958_SUBFRAME_LE,
      IEC958_SUBFRAME_Big_Endian    =>
        Sound.Constants.Format_IEC958_SUBFRAME_BE,
      MU_LAW                        => Sound.Constants.Format_MU_LAW,
      A_LAW                         => Sound.Constants.Format_A_LAW,
      IMA_ADPCM                     => Sound.Constants.Format_IMA_ADPCM,
      MPEG                          => Sound.Constants.Format_MPEG,
      GSM                           => Sound.Constants.Format_GSM,
      SPECIAL                       => Sound.Constants.Format_SPECIAL,
      S24_3LE                       => Sound.Constants.Format_S24_3LE,
      S24_3BE                       => Sound.Constants.Format_S24_3BE,
      U24_3LE                       => Sound.Constants.Format_U24_3LE,
      U24_3BE                       => Sound.Constants.Format_U24_3BE,
      S20_3LE                       => Sound.Constants.Format_S20_3LE,
      S20_3BE                       => Sound.Constants.Format_S20_3BE,
      U20_3LE                       => Sound.Constants.Format_U20_3LE,
      U20_3BE                       => Sound.Constants.Format_U20_3BE,
      S18_3LE                       => Sound.Constants.Format_S18_3LE,
      S18_3BE                       => Sound.Constants.Format_S18_3BE,
      U18_3LE                       => Sound.Constants.Format_U18_3LE,
      U18_3BE                       => Sound.Constants.Format_U18_3BE);
   for snd_pcm_format_t'Size use Interfaces.C.int'Size;
   function Signed_16_Bit return snd_pcm_format_t;
   function Unsigned_16_Bit return snd_pcm_format_t;

   type snd_pcm_access_t is (Memory_Mapped_Interleaved,
                             Memory_Mapped_Noninterleaved,
                             Memory_Mapped_Complex,
                             Read_Write_Interleaved,
                             Read_Write_Noninterleaved);
   for snd_pcm_access_t use
     (Memory_Mapped_Interleaved    =>
        Constants.Access_Memory_Mapped_Interleaved,
      Memory_Mapped_Noninterleaved =>
        Constants.Access_Memory_Mapped_Noninterleaved,
      Memory_Mapped_Complex        => Constants.Access_Memory_Mapped_Complex,
      Read_Write_Interleaved       => Constants.Access_Read_Write_Interleaved,
      Read_Write_Noninterleaved    =>
        Constants.Access_Read_Write_Noninterleaved);
   for snd_pcm_access_t'Size use Interfaces.C.int'Size;

   type snd_pcm_hw_params_t is private;

   type snd_pcm_sframes_t is new Interfaces.C.long;
   type snd_pcm_uframes_t is new Interfaces.C.unsigned_long;

   subtype Approximation_Direction is Interfaces.C.int range -1 .. 1;

   type Boolean is new Standard.Boolean;
   for Boolean'Size use Interfaces.C.unsigned'Size;

   function snd_pcm_open
     (pcmp   : access snd_pcm_t_ptr;
      name   : in     Interfaces.C.Strings.chars_ptr;
      stream : in     snd_pcm_stream_t;
      mode   : in     Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, snd_pcm_open);

   function snd_pcm_close (pcm : in     snd_pcm_t_ptr) return Interfaces.C.int;
   pragma Import (C, snd_pcm_close);

   function snd_pcm_state (pcm : in     snd_pcm_t_ptr) return snd_pcm_state_t;
   pragma Import (C, snd_pcm_state);

   function snd_pcm_hw_params_any
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_any);

   function snd_pcm_hw_params_set_rate_resample
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : in     Boolean) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_rate_resample);

   function snd_pcm_hw_params_set_access
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : in     snd_pcm_access_t) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_access);

   function snd_pcm_hw_params_set_format
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      format : in     snd_pcm_format_t) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_format);

   function snd_pcm_hw_params_set_channels
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : in     Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_channels);

   function snd_pcm_hw_params_set_rate_near
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : access Interfaces.C.unsigned;
      dir    : access Approximation_Direction) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_rate_near);

   function snd_pcm_hw_params_set_buffer_time_near
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : access Interfaces.C.unsigned;
      dir    : access Approximation_Direction) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_buffer_time_near);

   function snd_pcm_hw_params_set_period_time_near
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t;
      val    : access Interfaces.C.unsigned;
      dir    : access Approximation_Direction) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params_set_period_time_near);

   function snd_pcm_hw_params
     (pcm    : in     snd_pcm_t_ptr;
      params : access snd_pcm_hw_params_t) return Interfaces.C.int;
   pragma Import (C, snd_pcm_hw_params);
private
   type void_ptr is new Interfaces.C.Strings.chars_ptr;
   type snd_pcm_t_ptr is new Interfaces.C.Strings.chars_ptr;

   type hw_params_Bits is
     array (1 .. Sound.Constants.hw_params_Size) of Standard.Boolean;
   pragma Pack (hw_params_Bits);
   for hw_params_Bits'Size use Sound.Constants.hw_params_Size;

   type snd_pcm_hw_params_t is
      record
         Content : hw_params_Bits := (others => False);
      end record;
   pragma Pack (snd_pcm_hw_params_t);
   for snd_pcm_hw_params_t'Size use Sound.Constants.hw_params_Size;
end Sound.ALSA;
