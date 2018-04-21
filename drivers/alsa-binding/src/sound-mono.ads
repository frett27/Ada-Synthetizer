--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

private with Sound.ALSA;

package Sound.Mono is
   type Line_Type is private;

   type Frame is range -(2 ** 15) .. (2 ** 15) - 1;
   for Frame'Size use 16;
   type Frame_Array is array (Integer range <>) of aliased Frame;
   pragma Convention (C, Frame_Array);

   procedure Open (Line        : in out Line_Type;
                   Mode        : in     Line_Mode;
                   Resolution  : in out Sample_Frequency;
                   Buffer_Size : in out Duration;
                   Period      : in out Duration);
   function Is_Open (Line : in     Line_Type) return Boolean;
   procedure Close (Line : in out Line_Type);
   procedure Read (Line : in     Line_Type;
                   Item :    out Frame_Array;
                   Last :    out Natural);
   procedure Write (Line : in     Line_Type;
                    Item : in     Frame_Array;
                    Last :    out Natural);
private
   type Line_Type is new Sound.ALSA.snd_pcm_t_ptr;
end Sound.Mono;
