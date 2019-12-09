--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package body Sound.ALSA is
   function Signed_16_Bit return snd_pcm_format_t is
   begin
      case Sound.Constants.Format_Signed_16_Bit is
         when Sound.Constants.Format_Signed_16_Bit_Little_Endian =>
            return Signed_16_Bit_Little_Endian;
         when Sound.Constants.Format_Signed_16_Bit_Big_Endian =>
            return Signed_16_Bit_Big_Endian;
         when others =>
            raise Program_Error;
      end case;
   end Signed_16_Bit;

   function Unsigned_16_Bit return snd_pcm_format_t is
   begin
      case Sound.Constants.Format_Unsigned_16_Bit is
         when Sound.Constants.Format_Unsigned_16_Bit_Little_Endian =>
            return Unsigned_16_Bit_Little_Endian;
         when Sound.Constants.Format_Unsigned_16_Bit_Big_Endian =>
            return Unsigned_16_Bit_Big_Endian;
         when others =>
            raise Program_Error;
      end case;
   end Unsigned_16_Bit;
end Sound.ALSA;
