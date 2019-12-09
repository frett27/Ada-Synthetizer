------------------------------------------------------------------------------
--  File:            Comp_Zip_Prc.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Demonstrates the new Zip.Traverse procedure
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Interfaces;                        use Interfaces;

with Zip;
with UnZip.Streams;                     use UnZip.Streams;

procedure Comp_Zip_Prc(z1, z2: Zip.Zip_info; quiet: Natural) is
  z: array(1..2) of Zip.Zip_info;
  total_1,
  total_2,
  common,
  size_failures,
  compare_failures,
  missing_1_in_2,
  just_a_directory,
  missing_2_in_1,
  total_errors: Natural:= 0;
  total_bytes: Integer_64:= 0;

  procedure Compare_1_file( name: String ) is
    f: array(1..2) of Zipped_File_Type;
    s: array(1..2) of Stream_Access;
    c: array(1..2) of Character;
    p: Integer_64:= 1;

    function CutName(n:String; l:Natural) return String is
      dots: constant String:= "...";
    begin
      if n'Length > l then
        return dots & n( n'Last - (l-1) + dots'Length .. n'Last );
      else
        return n;
      end if;
    end CutName;

    l: constant:= 20;
    mininame: constant String:= To_Lower(CutName(name,l));
    stuffing: constant String(1..l-mininame'Length+1):= (others => ' ');

  begin
    if quiet = 0 then
      Put("   [" & stuffing & mininame & "] ");
    end if;
    for i in 1..2 loop
      begin
        Open( f(i), z(i), name );
        if i = 1 then
          total_1:= total_1 + 1;
        end if;
      exception
        when Zip.File_name_not_found =>
          if quiet = 0 then
            Put( "   # Not found in archive [" & Zip.Zip_name(z(i)) & ']' );
          end if;
          if i = 1 then
            Put_Line("-- internal error!");
          else
            Close(f(1));
          end if;
          if name(name'Last)='/' or name(name'Last)='\' then
            just_a_directory:= just_a_directory + 1;
            if quiet = 0 then
              Put_Line(" (just a dir.)");
            end if;
          else
            if quiet = 0 then
              New_Line;
            end if;
          end if;
          missing_1_in_2:= missing_1_in_2 + 1;
          return;
      end;
      s(i):= Stream(f(i));
    end loop;
    -- File found, now the comparison:
    while not End_Of_File(f(1)) loop
      if End_Of_File(f(2)) then
        if quiet = 0 then
          Put_Line("   # Shorter in [" & Zip.Zip_name(z(2)) & "] at position" &
                   Integer_64'Image(p) );
        end if;
        Close(f(1));
        Close(f(2));
        size_failures:= size_failures + 1;
        return;
      end if;
      --  Read one character in each stream
      for i in 1..2 loop
        Character'Read(s(i),c(i));
      end loop;
      if c(1)/=c(2) then
        if quiet = 0 then
          Put_Line("   # Difference at position" & Integer_64'Image(p) );
        end if;
        Close(f(1));
        Close(f(2));
        compare_failures:= compare_failures + 1;
        return;
      end if;
      p:= p+1;
    end loop;
    if not End_Of_File(f(2)) then
      if quiet = 0 then
        Put_Line("   # Longer in [" & Zip.Zip_name(z(2)) & "]" );
      end if;
      Close(f(1));
      Close(f(2));
      size_failures:= size_failures + 1;
      return;
    end if;
    Close(f(1));
    Close(f(2));
    if quiet = 0 then
      Put_Line("OK -" & Integer_64'Image(p-1) & " bytes compared");
    end if;
    total_bytes:= total_bytes + (p-1);
  end Compare_1_file;

  procedure Compare_all_files is new Zip.Traverse( Compare_1_file );

  err_str: String(1..5);

begin
  z(1):= z1;
  z(2):= z2;
  Put_Line("* Comparing [" & Zip.Zip_name(z(1)) & "] and [" & Zip.Zip_name(z(2)) & "]");
  Compare_all_files(z(1));
  total_2:= Zip.Entries(z(2));
  common:= total_1 - missing_1_in_2;
  if quiet < 2 then
    Put_Line("* === Results ===");
    Put_Line("  1st archive: [" & Zip.Zip_name(z(1)) & "], total files:" & Natural'Image(total_1));
    Put_Line("  2nd archive: [" & Zip.Zip_name(z(2)) & "], total files:" & Natural'Image(total_2));
    Put_Line("  Total files compared: " & Natural'Image(common));
    Put_Line("  Total of correct bytes: " & Integer_64'Image(total_bytes));
  end if;
  Put_Line("* === Comparison summary ===");
  Put(err_str,size_failures);
  Put_Line("    Size failures . . . . . . . . . . . :" & err_str);
  Put(err_str,compare_failures);
  Put_Line("    Content comparison failures . . . . :" & err_str);
  Put(err_str,missing_1_in_2);
  Put("    Files of 1st archive missing in 2nd :" & err_str);

  if just_a_directory > 0 then
    Put_Line(" (" & Integer'Image(just_a_directory) & " useless dir. names)");
  else
    New_Line;
  end if;

  missing_2_in_1:= total_2 - common;
  -- t2 - m21 = t1 - m12 = # common files
  Put(err_str,missing_2_in_1);
  for i in err_str'Range loop
    if err_str(i)=' ' then err_str(i):='_'; end if;
  end loop;
  Put_Line("  __Files of 2nd archive missing in 1st :" & err_str & "__");

  total_errors:=
     size_failures + compare_failures +
     missing_1_in_2 + missing_2_in_1;

  Put(err_str,total_errors);
  Put_Line("  Total of errors . . . . . . . . . . . :" & err_str);
end Comp_Zip_Prc;
