-- LZMA.Decoding - Ada 95 translation of LzmaSpec.cpp, LZMA Reference Decoder 9.31
-- LzmaSpec.cpp : 2013-07-28 : Igor Pavlov : Public domain
----------------
--
--  Rework in 2016 by G. de Montmollin.
--    - some confusing identifiers were changed:
--        mostly, "range" was renamed "width", various names for probability data
--        have been renamed "probs", different things called "pos" have been renamed
--    - the whole probability model has been encapsulated
--    - parts common to encoding were moved to the root LZMA package.

-- Legal licensing note:

--  Copyright (c) 2014 .. 2018 Gautier de Montmollin (Maintainer of the Ada version)
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
with Ada.Exceptions;                    use Ada.Exceptions;

package body LZMA.Decoding is

  type Byte_buffer is array(UInt32 range <>) of Byte;
  type p_Byte_buffer is access Byte_buffer;

  type Out_Window is record
    buf       : p_Byte_buffer := null;
    pos       : UInt32        := 0;
    size      : UInt32;
    is_full   : Boolean       := False;
    total_pos : Unsigned      := 0;
  end record;

  procedure Create (o: in out Out_Window; new_dictionary_size: UInt32) is
  begin
    o.buf  := new Byte_buffer (0 .. new_dictionary_size - 1);
    o.size := new_dictionary_size;
  end Create;

  type Range_Decoder is record
    width     : UInt32  := 16#FFFF_FFFF#;  --  (*)
    code      : UInt32  := 0;
    corrupted : Boolean := False;
  end record;
  --  (*) called "range" in LZMA spec and "remaining width" in G.N.N. Martin's
  --      article about range encoding.

  procedure Init(o: in out Range_Decoder) is
  begin
    if Read_Byte /= 0 then
      o.corrupted := True;
    end if;
    for i in 0..3 loop
      o.code := Shift_Left(o.code, 8) or UInt32(Read_Byte);
    end loop;
    if o.code = o.width then
      o.corrupted := True;
    end if;
  end Init;

  procedure Decode_Properties(o: in out LZMA_Decoder_Info; b: Byte_buffer) is
    d: Unsigned := Unsigned(b(b'First));
  begin
    if d >= 9 * 5 * 5 then
      Raise_Exception(LZMA_Error'Identity, "Incorrect LZMA properties");
      -- raise LZMA_Error with "Incorrect LZMA properties"; -- Ada 2005+
    end if;
    o.lc := Literal_context_bits_range(d mod 9);
    d := d / 9;
    o.lp := Literal_position_bits_range(d mod 5);
    o.pb := Position_bits_range(d / 5);
    o.dictSizeInProperties := 0;
    for i in 0..3 loop
      o.dictSizeInProperties := o.dictSizeInProperties +
        UInt32(b(UInt32(i) + 1 + b'First)) * 2 ** (8 * i);
    end loop;
    o.dictionary_size := o.dictSizeInProperties;
    if o.dictionary_size < Min_dictionary_size then
      o.dictionary_size := Min_dictionary_size;
    end if;
  end Decode_Properties;

  procedure Decode_Contents(o: in out LZMA_Decoder_Info; res: out LZMA_Result) is
    state : State_range := 0;
    --  Small stack of recent distances used for LZ. Required: initialized with zero values.
    rep0, rep1, rep2, rep3 : UInt32 := 0;
    pos_state: Pos_state_range;
    --  Local copies of invariant properties.
    unpack_size_def: constant Boolean:= o.unpackSizeDefined;
    literal_pos_mask: constant UInt32:= 2 ** o.lp - 1;
    lc: constant Literal_context_bits_range:= o.lc;
    --
    use type Data_Bytes_Count;
    out_win : Out_Window;
    --  Local range decoder
    range_dec: Range_Decoder;
    --  Entire probability model. Max lit prob index: 3,145,727.
    probs: All_probabilities(last_lit_prob_index => 16#300# * 2 ** (o.lc + o.lp) - 1);

    --  Normalize corresponds to G.N.N. Martin's revised algorithm's adding of
    --  trailing digits - for encoding. Here we decode and know the encoded
    --  data, brought by Read_Byte.
    procedure Normalize is
    pragma Inline(Normalize);
    begin
      --  Assertion: the width is large enough for the normalization to be needed
      --  once per bit decoding. Worst case: width = 2**24 before; bound = (2**13) * (2**5-1)
      --  new width's (leading binary digit) = 2**17; after normalization: 2**(17+8) = 2**25.
      if range_dec.width < width_threshold then
        range_dec.width := Shift_Left(range_dec.width, 8);
        range_dec.code  := Shift_Left(range_dec.code, 8) or UInt32(Read_Byte);
      end if;
    end Normalize;

    procedure Decode_Bit(prob: in out CProb; symbol: out Unsigned) is
    pragma Inline(Decode_Bit);
      cur_prob: constant CProb:= prob; -- Local copy
      bound: constant UInt32:= Shift_Right(range_dec.width, Probability_model_bits) * UInt32(cur_prob);
      --  See encoder for explanations about the maths.
    begin
      if range_dec.code < bound then
        prob:= cur_prob + Shift_Right(Probability_model_count - cur_prob, Probability_change_bits);
        range_dec.width := bound;
        Normalize;
        symbol := 0;
      else
        prob:= cur_prob - Shift_Right(cur_prob, Probability_change_bits);
        range_dec.code  := range_dec.code - bound;
        range_dec.width := range_dec.width - bound;
        Normalize;
        symbol := 1;
      end if;
    end Decode_Bit;

    function Is_Empty return Boolean is
    pragma Inline(Is_Empty);
    begin
      return out_win.pos = 0 and then not out_win.is_full;
    end Is_Empty;

    procedure Put_Byte(b: Byte) is
    pragma Inline(Put_Byte);
    begin
      out_win.total_pos := out_win.total_pos + 1;
      out_win.buf(out_win.pos):= b;
      out_win.pos := out_win.pos + 1;
      if out_win.pos = out_win.size then
        out_win.pos := 0;
        out_win.is_full := True;
      end if;
      Write_Byte(b);
    end Put_Byte;

    function Get_Byte(dist: UInt32) return Byte is
    pragma Inline(Get_Byte);
    begin
      if dist <= out_win.pos then
        return out_win.buf(out_win.pos - dist);
      else
        return out_win.buf(out_win.pos - dist + out_win.size);
      end if;
    end Get_Byte;

    procedure Process_Literal is
    pragma Inline(Process_Literal);
      prev_byte    : Byte:= 0;
      symbol       : Unsigned:= 1;
      lit_state    : Integer;
      probs_idx    : Integer;
      bit_nomatch  : Unsigned;
    begin
      if o.unpackSize = 0 and then unpack_size_def then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (Process_Literal)"
        );
      end if;
      --
      if not Is_Empty then
        prev_byte := Get_Byte(dist => 1);
      end if;
      lit_state :=
        Integer(
          Shift_Left(UInt32(out_win.total_pos) and literal_pos_mask, lc) +
          Shift_Right(UInt32(prev_byte), 8 - lc)
        );
      probs_idx:= 16#300# * lit_state;
      if state < 7 then
        loop
          Decode_Bit(probs.lit(probs_idx + Integer(symbol)), bit_nomatch);
          symbol := (symbol + symbol) or bit_nomatch;
          exit when symbol >= 16#100#;
        end loop;
      else
        declare
          --
          --  The probabilities used for decoding this literal assume
          --  that the current literal sequence resembles to the last
          --  distance-length copied sequence.
          --
          match_byte     : UInt32 := UInt32(Get_Byte(dist => rep0 + 1));
          match_bit      : UInt32;    -- either 0 or 16#100#
          prob_idx_match : Integer;   -- either 0 (normal case without match), 16#100# or 16#200#
          bit_a, bit_b   : Unsigned;
        begin
          loop
            match_byte := match_byte + match_byte;
            match_bit  := match_byte and 16#100#;
            prob_idx_match:= Integer(16#100# + match_bit);
            Decode_Bit(probs.lit(probs_idx + prob_idx_match + Integer(symbol)), bit_a);
            symbol := (symbol + symbol) or bit_a;
            exit when symbol >= 16#100#;
            if match_bit /= Shift_Left(UInt32(bit_a), 8) then
              --  No bit match, then give up byte match
              loop
                Decode_Bit(probs.lit(probs_idx + Integer(symbol)), bit_b);
                symbol := (symbol + symbol) or bit_b;
                exit when symbol >= 16#100#;
              end loop;
              exit;
            end if;
          end loop;
        end;
      end if;
      Put_Byte(Byte(symbol - 16#100#)); -- The output of a simple literal happens here.
      --
      state := Update_State_Literal(state);
      o.unpackSize:= o.unpackSize - 1;
    end Process_Literal;

    dict_size : constant UInt32:= o.dictionary_size;

    function Is_Finished_OK return Boolean is
    pragma Inline(Is_Finished_OK);
    begin
      return range_dec.code = 0;
    end Is_Finished_OK;

    type DL_Return_Code is (Normal, End_Of_Stream);

    function Process_Distance_and_Length return DL_Return_Code is
    pragma Inline(Process_Distance_and_Length);
      --
      procedure Bit_Tree_Decode(
        prob     : in out CProb_array;
        num_bits :        Positive;
        m        :    out Unsigned)
      is
      pragma Inline(Bit_Tree_Decode);
        a_bit: Unsigned;
      begin
        m:= 1;
        for count in reverse 1 .. num_bits loop
          Decode_Bit(prob(Integer(m) + prob'First), a_bit);
          m:= m + m + a_bit;
        end loop;
        m:= m - 2**num_bits;
      end Bit_Tree_Decode;
      --
      len: Unsigned:= 0;
      --
      procedure Copy_Match(dist: UInt32) is
      pragma Inline(Copy_Match);
        len32: constant UInt32:= UInt32(len);
        --  Conversion to UInt64 needed for dictionary size > 2**32 - 273:
        will_fill: constant Boolean:=
          UInt64 (out_win.pos) + UInt64 (len32) >= UInt64 (out_win.size);
        --
        procedure Easy_case is
        pragma Inline(Easy_case);
          src_from, src_to: UInt32;
          b1: Byte;
        begin
          --  The src and dest slices are within circular buffer bounds.
          --  May overlap (len32 > dist), even several times.
          src_from := out_win.pos - dist;
          src_to   := out_win.pos - dist + len32 - 1;
          --  We copy in forward order, with eventual overlapping(s)..
          for i in src_from .. src_to loop
            b1:= out_win.buf(i);
            out_win.buf(i + dist):= b1;
            Write_Byte(b1);
          end loop;
          out_win.pos := out_win.pos + len32;
        end Easy_case;
        --
        procedure Modulo_case is
        pragma Inline(Modulo_case);
          b2, b3: Byte;
        begin
          -- src starts below 0 or dest goes beyond size-1
          for count in reverse 1..len loop
            if dist <= out_win.pos then
              b2:= out_win.buf(out_win.pos - dist);
              out_win.buf(out_win.pos):= b2;
              out_win.pos := out_win.pos + 1;
              if out_win.pos = out_win.size then
                out_win.pos := 0;
              end if;
              Write_Byte(b2);
            else
              b3:= out_win.buf(out_win.size - dist + out_win.pos);
              out_win.buf(out_win.pos):= b3;
              out_win.pos := out_win.pos + 1;
              if out_win.pos = out_win.size then
                out_win.pos := 0;
              end if;
              Write_Byte(b3);
            end if;
          end loop;
        end Modulo_case;
      begin
        out_win.is_full := will_fill or else out_win.is_full;
        out_win.total_pos := out_win.total_pos + len;
        if dist <= out_win.pos and not will_fill then
          Easy_case;
        else
          Modulo_case;
        end if;
      end Copy_Match;
      --
      procedure Decode_Distance(dist: out UInt32) is
      pragma Inline(Decode_Distance);
        --
        decode_direct: UInt32;
        --
        procedure Decode_Direct_Bits(num_bits : Natural) is
        pragma Inline(Decode_Direct_Bits);
          t: UInt32;
        begin
          decode_direct := 0;
          for count in reverse 1..num_bits loop
            range_dec.width := Shift_Right(range_dec.width, 1);
            range_dec.code := range_dec.code - range_dec.width;
            t := - Shift_Right(range_dec.code, 31);
            range_dec.code := range_dec.code + (range_dec.width and t);
            if range_dec.code = range_dec.width then
              range_dec.corrupted := True;
            end if;
            Normalize;
            decode_direct := decode_direct + decode_direct + t + 1;
          end loop;
        end Decode_Direct_Bits;
        --
        procedure Bit_Tree_Reverse_Decode(prob: in out CProb_array; num_bits: in Natural) is
        pragma Inline(Bit_Tree_Reverse_Decode);
          m: Unsigned := 1;
          a_bit: Unsigned;
        begin
          for i in 0 .. num_bits-1 loop
            Decode_Bit(prob(Integer(m) + prob'First), a_bit);
            m := m + m + a_bit;
            dist := dist or Shift_Left(UInt32(a_bit), i);
          end loop;
        end Bit_Tree_Reverse_Decode;
        --
        --  len has been set up previously by Decode_Length.
        len_state     : constant Unsigned := Unsigned'Min(len, Len_to_pos_states - 1);
        dist_slot     : Unsigned;
        numDirectBits : Natural;
        --
      begin  --  Decode_Distance
        Bit_Tree_Decode(probs.dist.slot_coder(len_state), Dist_slot_bits, dist_slot);
        if dist_slot < Start_dist_model_index then
          dist:= UInt32(dist_slot);
          return;
        end if;
        numDirectBits := Natural(Shift_Right(UInt32(dist_slot), 1) - 1);
        dist := Shift_Left(2 or (UInt32(dist_slot) and 1), numDirectBits);
        if dist_slot < End_dist_model_index then
          Bit_Tree_Reverse_Decode(
            probs.dist.pos_coder(Integer(dist) - Integer(dist_slot) .. Pos_coder_range'Last),
            numDirectBits
          );
        else
          Decode_Direct_Bits(numDirectBits - Align_bits);
          dist:= dist + Shift_Left(decode_direct, Align_bits);
          Bit_Tree_Reverse_Decode(probs.dist.align_coder, Align_bits);
        end if;
      end Decode_Distance;
      --
      procedure Decode_Length(probs_len: in out Probs_for_LZ_Lengths) is
      pragma Inline(Decode_Length);
        choice: Unsigned;
      begin
        Decode_Bit(probs_len.choice_1, choice);
        if choice = 0 then
          Bit_Tree_Decode(probs_len.low_coder(pos_state), Len_low_bits, len);
          --  final length is in 2 + [0..7]
          return;
        end if;
        Decode_Bit(probs_len.choice_2, choice);
        if choice = 0 then
          Bit_Tree_Decode(probs_len.mid_coder(pos_state), Len_mid_bits, len);
          len:= len + Len_low_symbols;
          --  final length is in 2 + [8..15]
          return;
        end if;
        Bit_Tree_Decode(probs_len.high_coder, Len_high_bits, len);
        len:= len + Len_low_symbols + Len_mid_symbols;
        --  final length is in 2 + [16..271]
      end Decode_Length;
      --
      function Check_Distance return Boolean is
      pragma Inline(Check_Distance);
      begin
        return rep0 <= out_win.pos or out_win.is_full;
      end Check_Distance;
      --
      isError: Boolean;
      dist: UInt32;
      bit_a, bit_b, bit_c, bit_d, bit_e: Unsigned;
      --
    begin -- Process_Distance_and_Length
      Decode_Bit(probs.switch.rep(state), bit_a);
      if bit_a = Simple_match_choice then
        --  "Simple Match"
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        Decode_Length(probs.len);
        state := Update_State_Match(state);
        Decode_Distance(dist => rep0);
        if rep0 = end_of_stream_magic_distance then
          if Is_Finished_OK then
            return End_Of_Stream;
          else
            Raise_Exception(
              LZMA_Error'Identity,
              "Range decoder not finished on EOS marker (in Process_Distance_and_Length)"
            );
          end if;
        end if;
        if (o.unpackSize = 0 and then unpack_size_def) or
            rep0 >= dict_size or not Check_Distance
        then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #2)." &
            "; Distance =" & UInt32'Image(rep0) &
            "; Dictionary size =" & UInt32'Image(dict_size) &
            "; Position =" & UInt32'Image(out_win.pos) &
            "; Is window full ? " & Boolean'Image(out_win.is_full)
          );
        end if;
      else
        --  "Rep Match"
        if o.unpackSize = 0 and then unpack_size_def then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #1)"
          );
        end if;
        if Is_Empty then
          Raise_Exception(
            LZMA_Error'Identity,
            "Output window buffer is empty (in Process_Distance_and_Length)"
          );
        end if;
        Decode_Bit(probs.switch.rep_g0(state), bit_b);
        if bit_b = The_distance_is_rep0_choice then
          Decode_Bit(probs.switch.rep0_long(state, pos_state), bit_c);
          if bit_c = The_length_is_1_choice then
            state := Update_State_ShortRep(state);
            Put_Byte(Get_Byte(dist => rep0 + 1));
            o.unpackSize:= o.unpackSize - 1;
            return Normal;  -- GdM: this way, we go to the next iteration (C++: continue)
          end if;
        else
          Decode_Bit(probs.switch.rep_g1(state), bit_d);
          if bit_d = The_distance_is_rep1_choice then
            dist := rep1;
          else
            Decode_Bit(probs.switch.rep_g2(state), bit_e);
            if bit_e = The_distance_is_rep2_choice then
              dist := rep2;
            else
              dist := rep3;
              rep3 := rep2;
            end if;
            rep2 := rep1;
          end if;
          rep1 := rep0;
          rep0 := dist;
        end if;
        Decode_Length(probs.rep_len);
        state := Update_State_Rep(state);
      end if;
      len := len + Min_match_length;
      isError := False;
      if o.unpackSize < Data_Bytes_Count(len) and then unpack_size_def then
        len := Unsigned(o.unpackSize);
        isError := True;
      end if;
      -- The LZ distance/length copy happens here.
      Copy_Match(dist => rep0 + 1);
      o.unpackSize:= o.unpackSize - Data_Bytes_Count(len);
      if isError then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (in Process_Distance_and_Length, #3)"
        );
      end if;
      return Normal;
    end Process_Distance_and_Length;

    bit_choice: Unsigned;
    pos_bits_mask : constant UInt32 := 2 ** o.pb - 1;
    size_defined_and_marker_not_mandatory: constant Boolean:=
      unpack_size_def and not o.markerIsMandatory;

    procedure Finalize is
      procedure Dispose is new Ada.Unchecked_Deallocation(Byte_buffer, p_Byte_buffer);
    begin
      Dispose(out_win.buf);
      o.range_dec_corrupted:= range_dec.corrupted;
    end Finalize;

  begin
    Create(out_win, o.dictionary_size);
    Init(range_dec);
    loop
      if o.unpackSize = 0
        and then Is_Finished_OK
        and then size_defined_and_marker_not_mandatory
      then
        res:= LZMA_finished_without_marker;
        Finalize;
        return;
      end if;
      pos_state := Pos_state_range(UInt32(out_win.total_pos) and pos_bits_mask);
      Decode_Bit(probs.switch.match(state, pos_state), bit_choice);
      --  LZ decoding happens here: either we have a new literal
      --  in 1 byte, or we copy a slice of past data.
      if bit_choice = Literal_choice then
        Process_Literal;
      else
        case Process_Distance_and_Length is
          when Normal =>
            null;
          when End_Of_Stream =>
            res:= LZMA_finished_with_marker;
            Finalize;
            return;
        end case;
      end if;
    end loop;
  end Decode_Contents;

  procedure Decode_Header(o: out LZMA_Decoder_Info; hints: LZMA_Hints) is
    header: Byte_buffer(0..12);
    b: Byte;
    use type Data_Bytes_Count;
    last_bit: Natural;
  begin
    o.unpackSize := 0;
    o.unpackSizeDefined := False;

    for i in header'Range loop
      header(i):= Read_Byte;
      exit when i = 4 and not hints.has_size;
    end loop;

    Decode_Properties(o, header);

    if hints.has_size then
      for i in UInt32'(0)..7 loop
        b:= header(5 + i);
        if b /= 16#FF# then
          o.unpackSizeDefined := True;
        end if;
      end loop;
      if o.unpackSizeDefined then
        for i in UInt32'(0)..7 loop
          b:= header(5 + i);
          if b /= 0 then
            for bit_pos in 0 .. 7 loop
              if (b and Shift_Left(Byte'(1), bit_pos)) /= 0 then
                last_bit:= bit_pos;
              end if;
            end loop;
            last_bit:= last_bit + Natural(8 * i);
            if last_bit > Data_Bytes_Count'Size - 1 then
              Raise_Exception(
                LZMA_Error'Identity,
                "Indicated size bits for decoded data," &
                Natural'Image(last_bit) &
                ", exceeds the maximum file size bits," &
                Natural'Image(Data_Bytes_Count'Size - 1)
              );
            else
              o.unpackSize := o.unpackSize + Data_Bytes_Count(b) * 2 ** Natural(8 * i);
            end if;
          end if;
        end loop;
        o.unpackSize_as_defined:= o.unpackSize;
      else
        o.unpackSize:= Data_Bytes_Count'Last;
      end if;
    else
      o.unpackSize:= hints.given_size;
      o.unpackSizeDefined:= True;
    end if;
    o.markerIsMandatory := hints.marker_expected or not o.unpackSizeDefined;
  end Decode_Header;

  procedure Decode(info: out LZMA_Decoder_Info; hints: LZMA_Hints; res: out LZMA_Result) is
  begin
    Decode_Header (info, hints);
    Decode_Contents (info, res);
    if hints.fail_on_bad_range_code and info.range_dec_corrupted then
      Raise_Exception (LZMA_Error'Identity, "Range decoder had a corrupted value (code = range)");
    end if;
  end Decode;

  procedure Decompress(hints: LZMA_Hints) is
    --  Technical informations are discarded in this version of Decompress.
    info : LZMA_Decoder_Info;
    res  : LZMA_Result;
  begin
    Decode (info, hints, res);
  end Decompress;

  function Literal_context_bits(info: LZMA_Decoder_Info) return Natural is
  begin
    return info.lc;
  end Literal_context_bits;

  function Literal_pos_bits(info: LZMA_Decoder_Info) return Natural is
  begin
    return info.lp;
  end Literal_pos_bits;

  function Pos_bits(info: LZMA_Decoder_Info) return Natural is
  begin
    return info.pb;
  end Pos_bits;

  function Unpack_size_defined(info: LZMA_Decoder_Info) return Boolean is
  begin
    return info.unpackSizeDefined;
  end Unpack_size_defined;

  function Unpack_size_as_defined(info: LZMA_Decoder_Info) return Data_Bytes_Count is
  begin
    return info.unpackSize_as_defined;
  end Unpack_size_as_defined;

  function Probability_model_size(info: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
    probs: All_probabilities(last_lit_prob_index => 16#300# * 2 ** (info.lc + info.lp) - 1);
  begin
    return probs'Size / 8;
  end Probability_model_size;

  function Dictionary_size(info: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return info.dictionary_size;
  end Dictionary_size;

  function Dictionary_size_in_properties(info: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return info.dictSizeInProperties;
  end Dictionary_size_in_properties;

  function Range_decoder_corrupted(info: LZMA_Decoder_Info) return Boolean is
  begin
    return info.range_dec_corrupted;
  end Range_decoder_corrupted;

end LZMA.Decoding;
