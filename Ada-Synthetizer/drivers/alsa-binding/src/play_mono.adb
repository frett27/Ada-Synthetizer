--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with Ada.Text_IO;
with Sound.Mono;

procedure Play_Mono is
   Speakers    : Sound.Mono.Line_Type;
   Resolution  : Sound.Sample_Frequency := 44_100;
   Buffer_Size : Duration := 0.5;
   Period      : Duration := 0.1;
begin
   Sound.Mono.Open (Line        => Speakers,
                    Mode        => Sound.Output,
                    Resolution  => Resolution,
                    Buffer_Size => Buffer_Size,
                    Period      => Period);

   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Resolution [samples/s]: " &
              Sound.Sample_Frequency'Image (Resolution));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Buffer size [s]: " & Duration'Image (Buffer_Size));
   Ada.Text_IO.Put_Line
     (File => Ada.Text_IO.Standard_Error,
      Item => "Period [s]: " & Duration'Image (Period));

   declare
      Note_Frequency   : constant := 2_000;
      Note_Level       : constant := 10_000;
      Note_Half_Length : constant Positive :=
                           (Positive (Resolution) / Note_Frequency) / 2;
      Note      : Sound.Mono.Frame_Array (-Note_Half_Length ..
                                          +Note_Half_Length);
      Played_To : Integer;
   begin
      for Index in Note'Range loop
         Note (Index) :=
           Sound.Mono.Frame (Index * Note_Level / Note_Half_Length);
      end loop;

      loop
         Played_To := Note'First - 1;

         Play_Full_Note :
         while Played_To < Note'Last loop
            Sound.Mono.Write (Line => Speakers,
                              Item => Note (Played_To + 1 .. Note'Last),
                              Last => Played_To);
         end loop Play_Full_Note;
      end loop;
   end;
end Play_Mono;
