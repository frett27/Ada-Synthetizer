
with Ada.Text_IO;

package body Log is




   procedure Print (S : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Ada.Text_IO.Standard_Error, S);
   end Print;



end Log;
