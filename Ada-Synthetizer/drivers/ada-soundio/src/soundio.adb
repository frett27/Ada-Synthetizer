with Ada.Unchecked_Conversion;

package body Soundio is

   ------------------
   -- Write_Sample --
   ------------------

   procedure Write_Sample
     (Area    : SoundIo_Channel_Area;
      Index   : int;
      Sample  : Sample_Type)
   is
      type Sample_Type_Access is access all Sample_Type;
      function Address_To_Access is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Sample_Type_Access);

      Sample_Access_As_Char : Chars_Ptr;
      Sample_Access : Sample_Type_Access;
      use Chars_Ptrs;
   begin
      Sample_Access_As_Char :=
        Area.Ptr + ptrdiff_t (Index * Area.Step);
      Sample_Access := Address_To_Access
        (Sample_Access_As_Char.all'Address);
      Sample_Access.all := Sample;
   end Write_Sample;

   --------------
   -- Get_Area --
   --------------

   function Get_Area
     (Areas : SoundIo_Channel_Area_Ptr;
      Index : int) return SoundIo_Channel_Area
   is
      use Soundio_Channel_Area_Ptrs;
      Area_Ptr : SoundIo_Channel_Area_Ptr := Areas + ptrdiff_t (Index);
   begin
      return Area_Ptr.all;
   end Get_Area;

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error (Error : SoundIo_Error) is
   begin
      if Error /= Error_None then
         raise SoundIo_Exception with "Error";
      end if;
   end Check_Error;

end Soundio;
