--  ________  ___   ______       ______      ___
-- /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--    /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

-- Zip library
--------------
--
-- Library for manipulating archive files in the Zip format
--
-- Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.
--
-- Version / date / download info: see the version, reference, web strings
--   defined at the end of the public part of this package.

-- Legal licensing note:

--  Copyright (c) 1999 .. 2018 Gautier de Montmollin
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

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip_Streams;
with Ada.Calendar, Ada.Streams.Stream_IO, Ada.Text_IO;
with Interfaces;
with System;

package Zip is

  --------------
  -- Zip_info --
  --------------

  -- Zip_info contains the Zip file name or input stream,
  -- and the archive's sorted directory
  type Zip_info is private;

  -----------------------------------------------------------------------
  -- Load the whole .zip directory in archive (from) into a tree, for  --
  -- fast searching                                                    --
  -----------------------------------------------------------------------

   type Duplicate_name_policy is
     ( admit_duplicates,    --  two entries in the Zip archive may have the same full name
       error_on_duplicate   --  raise exception on attempt to add twice the same entry name
     );

  -- Load from a file

  procedure Load(
    info            : out Zip_info;
    from            : in  String; -- Zip file name
    case_sensitive  : in  Boolean:= False;
    duplicate_names : in  Duplicate_name_policy:= error_on_duplicate
  );

  -- Load from a stream

  procedure Load(
    info            :    out Zip_info;
    from            : in out Zip_Streams.Root_Zipstream_Type'Class;
    case_sensitive  : in     Boolean:= False;
    duplicate_names : in     Duplicate_name_policy:= error_on_duplicate
  );

  Archive_corrupted,
  Zip_file_open_error,
  Duplicate_name: exception;

  --  Old name for Archive_corrupted. Change: 22-Oct-2017
  --  Issues: archive stream is not necessarily a *file*; the naming
  --  ("Error") didn't clarify that it covered cases where the data
  --  is corrupted, which is different than an usual I/O error.

  --  Zip_file_Error: exception renames Archive_corrupted;   --   Now really obsolete.
  --  pragma Obsolescent(Zip_file_Error);                    --   Now really obsolete.

  function Is_loaded( info: in Zip_info ) return Boolean;

  function Zip_name( info: in Zip_info ) return String;

  function Zip_comment( info: in Zip_info ) return String;

  function Zip_stream( info: in Zip_info ) return Zip_Streams.Zipstream_Class_Access;

  function Entries( info: in Zip_info ) return Natural;

  procedure Delete( info : in out Zip_info );

  Forgot_to_load_zip_info: exception;

  -- Data sizes in archive
  subtype File_size_type is Interfaces.Unsigned_32;

  ---------

  --  Compression "methods" - actually, formats - in the "official" PKWARE Zip format.
  --  Details in appnote.txt, part V.J
  --   C: supported by Zip-Ada for compressing
  --   D: supported by Zip-Ada for decompressing

  type PKZip_method is
   ( store,     -- C,D
     shrink,    -- C,D
     reduce_1,  -- C,D
     reduce_2,  -- C,D
     reduce_3,  -- C,D
     reduce_4,  -- C,D
     implode,   --   D
     tokenize,
     deflate,   -- C,D
     deflate_e, --   D - "Enhanced deflate" or "Deflate64"
     bzip2,     --   D
     lzma_meth, -- C,D
     ppmd,
     unknown
   );

  subtype reduce is PKZip_method range reduce_1..reduce_4;

  -- Return a String image, nicer than the 'Image attribute.
  function Image(m: PKZip_method) return String;

  -- Technical: translates the method code as set in zip archives
  function Method_from_code(x: Interfaces.Unsigned_16) return PKZip_method;
  function Method_from_code(x: Natural) return PKZip_method;

  -- Internal time definition
  subtype Time is Zip_Streams.Time;
  function Convert(date : in Ada.Calendar.Time) return Time
    renames Zip_Streams.Calendar.Convert;
  function Convert(date : in Time) return Ada.Calendar.Time
    renames Zip_Streams.Calendar.Convert;

  -- Entry names within Zip archives are encoded either with
  --    * the IBM PC (the one with a monochrome screen, only text mode)'s
  --        character set: IBM 437
  -- or
  --    * Unicode UTF-8
  --
  -- Documentation: PKWARE's Appnote.txt, APPENDIX D - Language Encoding (EFS)

  type Zip_name_encoding is (IBM_437, UTF_8);

  -- Traverse a whole Zip_info directory in sorted order, giving the
  -- name for each entry to an user-defined "Action" procedure.
  -- Concretely, you can process a whole Zip file that way, by extracting data
  -- with Extract, or open a reader stream with UnZip.Streams.
  -- See the Comp_Zip or Find_Zip tools as application examples.
  generic
    with procedure Action( name: String ); -- 'name' is compressed entry's name
  procedure Traverse( z: Zip_info );

  -- Same as Traverse, but Action gives also full name information.
  -- The pair (name, name_encoding) allows for an unambiguous Unicode
  -- name decoding. See the AZip project for an implementation.
  generic
    with procedure Action(
      name          : String; -- 'name' is compressed entry's name
      name_encoding : Zip_name_encoding
    );
  procedure Traverse_Unicode( z: Zip_info );

  -- Same as Traverse, but Action gives also full technical informations
  -- about the compressed entry.
  generic
    with procedure Action(
      name             : String; -- 'name' is compressed entry's name
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : File_size_type;
      uncomp_size      : File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      name_encoding    : Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    );
  procedure Traverse_verbose( z: Zip_info );

  -- Academic: see how well the name tree is balanced
  procedure Tree_stat(
    z        : in     Zip_info;
    total    :    out Natural;
    max_depth:    out Natural;
    avg_depth:    out Float
  );

  --------------------------------------------------------------------------
  -- Offsets - various procedures giving 1-based indexes to local headers --
  --------------------------------------------------------------------------

  -- Find 1st offset in a Zip stream (i.e. the first's archived entry's offset)

  procedure Find_first_offset(
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    file_index     :    out Zip_Streams.ZS_Index_Type );

  --  If the archive is empty (the 22 byte .zip file), there is no first entry or offset.
  Archive_is_empty: exception;

  -- Find offset of a certain compressed file
  -- in a Zip file (file opened and kept open)

  procedure Find_offset(
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  );

  -- Find offset of a certain compressed file in a pre-loaded Zip_info data

  procedure Find_offset(
    info           : in     Zip_info;
    name           : in     String;
    name_encoding  :    out Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  );

  -- Find offset of a certain compressed file in a pre-loaded Zip_info data.
  -- This version scans the whole catalogue and returns the index of the first
  -- entry with a matching name, ignoring directory information.
  -- For instance, if the Zip archive contains "zip-ada/zip_lib/zip.ads",
  -- "zip.ads" will match - or even "ZIP.ads" if info has been loaded in case-insensitive mode.
  -- Caution: this may be much slower than the exact search with Find_offset.

  procedure Find_offset_without_directory(
    info           : in     Zip.Zip_info;
    name           : in     String;
    name_encoding  :    out Zip.Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  );

  File_name_not_found: exception;

  function Exists(info: Zip_info; name: String) return Boolean;

  -- User code: any information e.g. as a result of a string search,
  -- archive comparison, archive update, recompression,...

  procedure Set_user_code(
    info           : in Zip_info;
    name           : in String;
    code           : in Integer
  );

  function User_code(
    info           : in Zip_info;
    name           : in String
  )
  return Integer;

  procedure Get_sizes(
    info           : in     Zip_info;
    name           : in     String;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  -- User-defined procedure for feedback occuring during
  -- compression or decompression (entry_skipped meaningful
  -- only for the latter)

  type Feedback_proc is access
    procedure (
      percents_done:  in Natural;  -- %'s completed
      entry_skipped:  in Boolean;  -- indicates one can show "skipped", no %'s
      user_abort   : out Boolean   -- e.g. transmit a "click on Cancel" here
    );

  -------------------------------------------------------------------------
  -- Goodies - things used internally by Zip-Ada but are not bound to    --
  -- Zip archive purposes and that might be generally useful.            --
  -------------------------------------------------------------------------

  -- BlockRead: general-purpose procedure (nothing really specific to Zip /
  -- UnZip): reads either the whole buffer from a file, or if the end of
  -- the file lays inbetween, a part of the buffer.
  --
  -- The procedure's names and parameters match Borland Pascal / Delphi

  subtype Byte is Interfaces.Unsigned_8;
  type Byte_Buffer is array(Integer range <>) of aliased Byte;
  type p_Byte_Buffer is access Byte_Buffer;

  procedure BlockRead(
    file         : in     Ada.Streams.Stream_IO.File_Type;
    buffer       :    out Byte_Buffer;
    actually_read:    out Natural
    -- = buffer'Length if no end of file before last buffer element
  );

  -- Same for general streams
  --
  procedure BlockRead(
    stream       : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer       :    out Byte_Buffer;
    actually_read:    out Natural
    -- = buffer'Length if no end of stream before last buffer element
  );

  -- Same, but instead of giving actually_read, raises End_Error if
  -- the buffer cannot be fully read.
  -- This mimics the 'Read stream attribute; can be a lot faster, depending
  -- on the compiler's run-time library.
  procedure BlockRead(
    stream : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer :    out Byte_Buffer
  );

  -- This mimics the 'Write stream attribute; can be a lot faster, depending
  -- on the compiler's run-time library.
  -- NB: here we can use the root stream type: no question of size, index,...
  procedure BlockWrite(
    stream : in out Ada.Streams.Root_Stream_Type'Class;
    buffer : in     Byte_Buffer
  );

  -- Copy a chunk from a stream into another one, using a temporary buffer
  procedure Copy_chunk (
    from       : in out Zip_Streams.Root_Zipstream_Type'Class;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    bytes      : Natural;
    buffer_size: Positive:= 1024*1024;
    Feedback   : Feedback_proc:= null
  );

  -- Copy a whole file into a stream, using a temporary buffer
  procedure Copy_file(
    file_name  : String;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    buffer_size: Positive:= 1024*1024
  );

  -- This does the same as Ada 2005's Ada.Directories.Exists
  -- Just there as helper for Ada 95 only systems
  --
  function Exists(name:String) return Boolean;

  -- Write a string containing line endings (possibly from another system)
  --   into a text file, with the "correct", native line endings.
  --   Works for displaying/saving correctly
  --   CR&LF (DOS/Win), LF (UNIX), CR (Mac OS < 9)
  --
  procedure Put_Multi_Line(
    out_file :        Ada.Text_IO.File_Type;
    text     :        String
  );

  procedure Write_as_text(
    out_file :        Ada.Text_IO.File_Type;
    buffer   :        Byte_Buffer;
    last_char: in out Character -- track line-ending characters between writes
  );

  function Hexadecimal(x: Interfaces.Unsigned_32) return String;

  -----------------------------------------------------------------
  --  Information about this package - e.g., for an "about" box  --
  -----------------------------------------------------------------

  version   : constant String:= "55";
  reference : constant String:= "22-Nov-2018";
  web       : constant String:= "http://unzip-ada.sf.net/";
  --  Hopefully the latest version is at that URL...  --^

  ---------------------
  --  Private items  --
  ---------------------

private

  --  Zip_info, 23.VI.1999.
  --
  --  The PKZIP central directory is coded here as a binary tree
  --  to allow a fast retrieval of the searched offset in zip file.
  --  E.g. for a 1000-file archive, the offset will be found in less
  --  than 11 moves: 2**10=1024 (balanced case), without any read
  --  in the archive.
  --
  --  *Note* 19-Oct-2018: rev. 670 to 683 used a Hashed Map and a
  --  Vector (Ada.Containers). The loading of the dictionary was
  --  much faster (2x), but there were performance bottlenecks elsewhere,
  --  not solved by profiling. On an archive with 18000 small entries of
  --  around 1 KB each, comp_zip ran 100x slower!
  --  Neither the restricted use of Unbounded_String, nor the replacement
  --  of the Vector by an array helped solving the performance issue.

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right      : p_Dir_node;
    dico_name        : String(1..name_len); -- UPPER if case-insensitive search
    file_name        : String(1..name_len);
    file_index       : Zip_Streams.ZS_Index_Type;
    comp_size        : File_size_type;
    uncomp_size      : File_size_type;
    crc_32           : Interfaces.Unsigned_32;
    date_time        : Time;
    method           : PKZip_method;
    name_encoding    : Zip_name_encoding;
    read_only        : Boolean; -- TBD: attributes of most supported systems
    encrypted_2_x    : Boolean;
    user_code        : Integer;
  end record;

  type Zip_archive_format_type is (Zip_32, Zip_64);  --  Supported so far: Zip_32.

  type p_String is access String;

  type Zip_info is record
    loaded             : Boolean:= False;
    case_sensitive     : Boolean;
    zip_file_name      : p_String;                           -- a file name...
    zip_input_stream   : Zip_Streams.Zipstream_Class_Access; -- ...or an input stream
    -- ^ when not null, we use this, and not zip_file_name
    dir_binary_tree    : p_Dir_node;
    total_entries      : Natural;
    zip_file_comment   : p_String;
    zip_archive_format : Zip_archive_format_type := Zip_32;
  end record;

  --  System.Word_Size: 13.3(8): A word is the largest amount of storage
  --  that can be conveniently and efficiently manipulated by the hardware,
  --  given the implementation's run-time model.
  --
  min_bits_32: constant:= Integer'Max(32, System.Word_Size);
  min_bits_16: constant:= Integer'Max(16, System.Word_Size);

  --  We define an Integer type which is at least 32 bits, but n bits
  --  on a native n (> 32) bits architecture (no performance hit on 64+
  --  bits architectures).
  --  Integer_M16 not needed: Integer already guarantees 16 bits
  --
  type Integer_M32 is range -2**(min_bits_32-1) .. 2**(min_bits_32-1) - 1;
  subtype Natural_M32  is Integer_M32 range 0..Integer_M32'Last;
  subtype Positive_M32 is Integer_M32 range 1..Integer_M32'Last;

  type Unsigned_M16 is mod 2**min_bits_16;
  type Unsigned_M32 is mod 2**min_bits_32;

  --  Codes for compression formats in Zip archives
  --  See PKWARE's Appnote, "4.4.5 compression method"
  --
  package compression_format_code is
    store     : constant :=  0;
    shrink    : constant :=  1;
    reduce    : constant :=  2;
    implode   : constant :=  6;
    tokenize  : constant :=  7;
    deflate   : constant :=  8;
    deflate_e : constant :=  9;
    bzip2     : constant := 12;
    lzma      : constant := 14;
    ppmd      : constant := 98;
  end compression_format_code;

end Zip;
