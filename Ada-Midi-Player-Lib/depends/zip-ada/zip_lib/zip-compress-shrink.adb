-- Legal licensing note:

--  Copyright (c) 2006 .. 2018 Gautier de Montmollin
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found on the site
-- http://www.opensource.org/licenses/mit-license.php

with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;
with Zip.CRC_Crypto;

procedure Zip.Compress.Shrink
 (input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type;
  feedback        : Feedback_proc;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  crypto          : in out Crypto_pack;
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed < uncompressed
)
is
  --------------------------------------------------------------------------

  ------------------
  -- Buffered I/O --
  ------------------

  --  Define data types needed to implement input and output file buffers

  procedure Dispose is
    new Ada.Unchecked_Deallocation(Byte_Buffer, p_Byte_Buffer);

  InBuf: p_Byte_Buffer;  --  I/O buffers
  OutBuf: p_Byte_Buffer;

  InBufIdx: Positive;  --  Points to next char in buffer to be read
  OutBufIdx: Positive; --  Points to next free space in output buffer

  MaxInBufIdx: Natural;  --  Count of valid chars in input buffer
  InputEoF: Boolean;     --  End of file indicator

  procedure Read_Block is
  begin
    Zip.BlockRead(
      stream        => input,
      buffer        => InBuf.all,
      actually_read => MaxInBufIdx
    );
    InputEoF:= MaxInBufIdx = 0;
    InBufIdx := 1;
  end Read_Block;

  -- Exception for the case where compression works but produces
  -- a bigger file than the file to be compressed (data is too "random").
  Compression_unefficient: exception;

  procedure Write_Block is
    amount: constant Integer:= OutBufIdx-1;
  begin
    output_size:= output_size + File_size_type(Integer'Max(0,amount));
    if input_size_known and then output_size >= input_size then
      -- The compression so far is obviously unefficient for that file.
      -- Useless to go further.
      -- Stop immediately before growing the file more than the
      -- uncompressed size.
      raise Compression_unefficient;
    end if;
    Encode(crypto, OutBuf(1 .. amount));
    Zip.BlockWrite(output, OutBuf(1 .. amount));
    OutBufIdx := 1;
  end Write_Block;

  procedure Put_byte(B : Unsigned_8) is
  begin
    OutBuf(OutBufIdx) := B;
    OutBufIdx:= OutBufIdx + 1;
    if OutBufIdx > OutBuf.all'Last then
      Write_Block;
    end if;
  end Put_byte;

  procedure Flush_output is
  begin
    if OutBufIdx > 1 then
      Write_Block;
    end if;
  end Flush_output;

  --------------------------------------------------------------------------

  ------------------------------------------------------
  --  Bit code buffer, for sending data at bit level  --
  ------------------------------------------------------

  --  Output buffer. Bits are inserted starting at the right (least
  --  significant bits). The width of bit_buffer must be at least 16 bits.
  subtype U32 is Unsigned_32;
  bit_buffer: U32:= 0;
  --  Number of valid bits in bit_buffer.  All bits above the last valid bit are always zero.
  valid_bits: Integer:= 0;

  procedure Flush_bit_buffer is
  begin
    while valid_bits > 0 loop
      Put_byte(Byte(bit_buffer and 16#FF#));
      bit_buffer:= Shift_Right(bit_buffer, 8);
      valid_bits := Integer'Max(0, valid_bits - 8);
    end loop;
    bit_buffer := 0;
  end Flush_bit_buffer;

  Min_bits: constant:= 9;    --  Starting code size of 9 bits
  Max_bits: constant:= 13;   --  Maximum code size of 13 bits

  subtype Code_size_type is Integer range 1..Max_bits;
  code_size: Code_size_type;     --  Size of codes (in bits) currently being written

  --  Send a value on a given number of bits.
  procedure Put_code(code: Natural) is
  pragma Inline(Put_code);
  begin
    --  Put bits from code at the left of existing ones. They might be shifted away
    --  partially on the left side (or even entirely if valid_bits is already = 32).
    bit_buffer:= bit_buffer or Shift_Left(U32(code), valid_bits);
    valid_bits:= valid_bits + code_size;
    if valid_bits > 32 then
      --  Flush 32 bits to output as 4 bytes
      Put_byte(Byte(bit_buffer and 16#FF#));
      Put_byte(Byte(Shift_Right(bit_buffer,  8) and 16#FF#));
      Put_byte(Byte(Shift_Right(bit_buffer, 16) and 16#FF#));
      Put_byte(Byte(Shift_Right(bit_buffer, 24) and 16#FF#));
      valid_bits:= valid_bits - 32;
      --  Empty buffer and put on it the rest of the code
      bit_buffer := Shift_Right(U32(code), code_size - valid_bits);
    end if;
  end Put_code;

  Table_full: Boolean; -- Flag indicating a full symbol table

  -- Define data types needed to implement a code table for LZW compression
  type CodeRec is record  --  Code Table record format...
    Child   : Integer;       --  Index of 1st suffix for this prefix
    Sibling : Integer;       --  Index of next suffix in chain
    Suffix  : Natural;       --  Suffix
  end record;

  TABLESIZE: constant:= 8191; --  We'll need 4K entries in table

  SPECIAL  : constant:= 256;  --  Special function code
  INCSIZE  : constant:= 1;    --  Code indicating a jump in code size
  CLEARCODE: constant:= 2;    --  Code indicating code table has been cleared

  FIRSTENTRY: constant:= 257;    --  First available table entry
  UNUSED: constant:= -1;     --  Prefix indicating an unused code table entry

  type Code_array is array (0 .. TABLESIZE ) of CodeRec;
  --  Define the code table

  type Table_access is access Code_array;
  procedure Dispose is new Ada.Unchecked_Deallocation(Code_array, Table_access);

  Code_table: Table_access;      --  Points to code table for LZW compression

  --  Define data types needed to implement a free node list
  type Free_list_array is array (FIRSTENTRY .. TABLESIZE) of Natural;
  type Free_list_access is access Free_list_array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation(Free_list_array, Free_list_access);

  Free_list: Free_list_access;   --  Table of free code table entries
  Next_free: Integer;       --  Index into free list table

  ---------------------------------------------------------------------------
  -- The following routines are used to allocate, initialize, and de-allocate
  -- various dynamic memory structures used by the LZW compression algorithm
  ---------------------------------------------------------------------------

  procedure Build_Data_Structures is
  begin
    Code_table:= new Code_array;
    Free_list:= new Free_list_array;
  end Build_Data_Structures;

  ---------------------------------------------------------------------------
  procedure Destroy_Data_Structures is
  begin
    Dispose(Code_table);
    Dispose(Free_list);
  end Destroy_Data_Structures;

  ---------------------------------------------------------------------------

  procedure Initialize_Data_Structures is
  begin
    for I in 0 .. TABLESIZE loop
      Code_table(I).Child   := UNUSED;
      Code_table(I).Sibling := UNUSED;
      if I <= 255 then
        Code_table(I).Suffix := I;
      end if;
      if I >= 257 then
        Free_list(I) := I;
      end if;
    end loop;
    Next_free:= FIRSTENTRY;
    Table_full:= False;
  end Initialize_Data_Structures;

  ---------------------------------------------------------------------------
  -- The following routines handle manipulation of the LZW Code Table
  ---------------------------------------------------------------------------

  ClearList: array (0 .. 1023 ) of Unsigned_8;
  --  Bit mapped structure used in during adaptive resets

  procedure Prune(Parent : Integer) is
    --  Prune leaves from a subtree - Note: this is a recursive procedure
    CurrChild: Integer;
    NextSibling: Integer;
  begin
    CurrChild := Code_table(Parent).Child;
    --  Find first Child that has descendants .. clear any that don't

    while CurrChild /= UNUSED and then
          Code_table(CurrChild).Child = UNUSED
    loop
      Code_table(Parent).Child := Code_table(CurrChild).Sibling;
      Code_table(CurrChild).Sibling := UNUSED;
      --  Turn on ClearList bit to indicate a cleared entry
      ClearList(CurrChild / 8) :=
          ClearList(CurrChild / 8)  or
          (Shift_Left(1, CurrChild  mod  8));
      CurrChild:= Code_table(Parent).Child;
    end loop;

    if CurrChild /= UNUSED then    --  If there are any children left ...
      Prune(CurrChild);
      NextSibling := Code_table(CurrChild).Sibling;
      while NextSibling /= UNUSED loop
        if  Code_table(NextSibling).Child = UNUSED then
          Code_table(CurrChild).Sibling :=
            Code_table(NextSibling).Sibling;
          Code_table(NextSibling).Sibling := UNUSED;
          --  Turn on ClearList bit to indicate a cleared entry

          ClearList(NextSibling / 8) :=
            ClearList(NextSibling / 8)  or
            (Shift_Left(1, NextSibling  mod  8));
          NextSibling := Code_table(CurrChild).Sibling;
        else
          CurrChild := NextSibling;
          Prune(CurrChild);
          NextSibling := Code_table(CurrChild).Sibling;
        end if;
      end loop;
    end if;
  end Prune;

  ---------------------------------------------------------------------------

  procedure Clear_Table is
  begin
    ClearList:= (others => 0);
    --  Remove all leaf nodes by recursively pruning subtrees
    for Node  in  0 .. 255 loop
      Prune(Node);
    end loop;
    --  Next, re-initialize our list of free table entries
    Next_free := TABLESIZE + 1;
    for Node in reverse FIRSTENTRY .. TABLESIZE loop
      if (ClearList(Node / 8)  and  (Shift_Left(1, Node  mod  8))) /= 0 then
        Next_free:= Next_free - 1;
        Free_list(Next_free) := Node;
      end if;
    end loop;
    --
    Table_full:= Next_free > TABLESIZE;
  end Clear_Table;

  ---------------------------------------------------------------------------

  procedure Table_Add(Prefix_0 : Natural; Suffix : Natural) is
    FreeNode: Natural;
    Prefix: Natural:= Prefix_0;
  begin
    if Next_free <= TABLESIZE then
      FreeNode := Free_list(Next_free);
      Next_free:= Next_free + 1;
      Code_table(FreeNode).Child := UNUSED;
      Code_table(FreeNode).Sibling := UNUSED;
      Code_table(FreeNode).Suffix := Suffix;
      if Code_table(Prefix).Child = UNUSED then
        Code_table(Prefix).Child := FreeNode;
      else
        Prefix := Code_table(Prefix).Child;
        while Code_table(Prefix).Sibling /= UNUSED loop
          Prefix := Code_table(Prefix).Sibling;
        end loop;
        Code_table(Prefix).Sibling := FreeNode;
      end if;
    end if;
    --
    Table_full:= Next_free > TABLESIZE;
  end Table_Add;

  ---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- Search for a Prefix:Suffix pair in our Symbol table. If found, return
  -- the index value where found.  If not found, return False and set
  -- Found_at to UNUSED.
  ---------------------------------------------------------------------------
  procedure Table_Lookup(
      TargetPrefix: Integer;
      TargetSuffix: Natural;
      Found_at    : out Integer;
      Found       : out Boolean
  )
  is
    -- Was in 16-bit ASM
    idx: Natural:= TargetPrefix;
  begin
    -- Lookup an entry in the Hash Table. If found, return TRUE and set
    -- parameter Found_at with the index of the entry at which the match
    -- was found. If not found, return False and plug an UNUSED into Found_at.
    if Code_table(idx).Child = UNUSED then
      Found_at:= UNUSED;
      Found:= False;
    else
      idx:= Code_table(idx).Child;
      loop
        if Code_table(idx).Suffix = TargetSuffix then
          Found_at:= idx;
          Found:= True;
          return;
        elsif Code_table(idx).Sibling = UNUSED then
          Found_at:= UNUSED;
          Found:= False;
          return;
        else
          idx:= Code_table(idx).Sibling;
        end if;
      end loop;
    end if;
  end Table_Lookup;

  ---------------------------------------------------------------------------
  --  The actual Crunching algorithm
  ---------------------------------------------------------------------------

  Last_code: Integer:= 0;
  First_atom: Boolean; --  Flag indicating the START of a shrink operation
  Max_code: Natural;   --  Largest code that can be written in Code_size bits

  procedure Shrink_Atom(Suffix: Integer) is
    WhereFound: Integer;
    lookup_ok: Boolean;
  begin
    if First_atom then            --  If just getting started ...
      bit_buffer:= 0;
      valid_bits:= 0;
      code_size := Min_bits;   --    Initialize code size to minimum
      Max_code  := 2 ** code_size - 1;
      Last_code := Suffix;      --    get first character from input,
      First_atom  := False;       --    and reset the first char flag.
    elsif Suffix = UNUSED then --  Nothing to crunch... must be EOF on input
      Put_code(Last_code);         --  Write last prefix code
      Flush_bit_buffer;
      Flush_output;               --  Flush our output buffer
    elsif Table_full then
      --  Ok, lets clear the code table (adaptive reset)
      Put_code(Last_code);
      Put_code(SPECIAL);
      Put_code(CLEARCODE);
      Clear_Table;
      Table_Add(Last_code, Suffix);
      Last_code:= Suffix;
    else
      Table_Lookup(Last_code, Suffix, WhereFound, lookup_ok);
      if lookup_ok then
        --  If Last_code:Suffix pair is found in the code table, then ...
        --  ... set Last_code to the entry where the pair is located
        Last_code:= WhereFound;
      else
        --  Not in table
        Put_code(Last_code);            --  Write current Last_code code
        Table_Add(Last_code, Suffix);  --  Attempt to add to code table
        Last_code:= Suffix;           --  Reset Last_code code for new char
        if (
             code_size < Max_bits and
             not Table_full
             -- 12-Dec-2007: the Pascal code had an out-of-range access
             --  with Free_list(Next_free) below when the table was full!
             --  NB: according to tests, and surely it can be proven,
             --  the case (Code_size < Max_bits and Table_Full) never happens,
             --  so that
             --    "Code_size < Max_bits and then Free_list(Next_free) > Max_code"
             --  could be sufficient. But until it is proven, I prefer to
             --  keep the "and not Table_Full"
           )
           and then
           Free_list(Next_free) > Max_code
        then
          --  Time to increase the code size and change the max. code
          Put_code(SPECIAL);
          Put_code(INCSIZE);
          code_size:= code_size + 1;
          Max_code:= 2 **  code_size - 1;
        end if;
      end if;
    end if;
  end Shrink_Atom;

  X_Percent: Natural;
  Bytes_in   : Natural;  --  Count of input file bytes processed

  procedure Process_Input(Source: Byte_Buffer) is
    PctDone: Natural;
    user_aborting: Boolean;
    Last_processed: Integer:= Source'First-1;
  begin
    if Source'Length < 1 then
      Shrink_Atom(UNUSED);
    else
      for I in Source'Range loop
        Bytes_in:= Bytes_in + 1;
        if feedback /= null then
          if Bytes_in = 1 then
            feedback(0, False, user_aborting);
          end if;
          if X_Percent > 0 and then --  Bugfix GdM 23-Dec-2002
             ((Bytes_in-1) mod X_Percent = 0
              or Bytes_in = Integer(input_size))
          then
            if input_size_known then
              PctDone := Integer( (100.0 * Float( Bytes_in)) / Float(input_size));
              feedback(PctDone, False, user_aborting);
            else
              feedback(0, False, user_aborting);
            end if;
            if user_aborting then
              raise User_abort;
            end if;
          end if;
        end if;
        Shrink_Atom(Integer(Source(I)));
        Last_processed:= I;
        if input_size_known and then Bytes_in >= Integer(input_size) then
          -- The job is done, even though there are more in the buffer
          InputEoF:= True;
          exit;
        end if;
      end loop;
      Zip.CRC_Crypto.Update(CRC, Source(Source'First .. Last_processed));
    end if;
  end Process_Input;

  Remaining: Natural;

begin
  --  Allocate input and output buffers ...
  if input_size_known then
    InBuf:= new Byte_Buffer
      (1..Integer'Min(Integer'Max(8,Integer(input_size)), buffer_size));
  else
    InBuf:= new Byte_Buffer(1..buffer_size);
  end if;
  OutBuf:= new Byte_Buffer(1..buffer_size);
  OutBufIdx := 1;
  Build_Data_Structures;   --  ... and other data structures required
  Initialize_Data_Structures;
  output_size:= 0;
  Bytes_in := 0;
  --
  begin
    Read_Block;                --  Prime the input buffer
    First_atom   := True;         --  1st character flag for Crunch procedure
    if input_size_known then
      X_Percent := Integer(input_size / 40);
    else
      X_Percent := 0;
    end if;
    while not InputEoF loop
      Remaining := MaxInBufIdx - InBufIdx + 1;
      if Remaining = 0 then
        Read_Block;
      else
        Process_Input(InBuf(InBufIdx..InBufIdx+Remaining-1));
        InBufIdx:= InBufIdx + Remaining;
      end if;
    end loop;
    Process_Input(InBuf(1..0));  --  This forces EOF processing
    compression_ok:= Bytes_in > 0;
  exception
    when Compression_unefficient =>
      compression_ok:= False;
  end;
  --
  Destroy_Data_Structures;
  Dispose(InBuf);
  Dispose(OutBuf);
end Zip.Compress.Shrink;
