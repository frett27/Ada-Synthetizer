--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package Sound is
   type Sample_Frequency is range 4_000 .. 196_000;
   Default_Sample_Frequency : constant Sample_Frequency := 48_000;

   type Line_Mode is (Input, Output);
end Sound;
