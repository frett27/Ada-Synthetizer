------------------------------------------------------------------------------
--                             Ada Midi Player                              --
--                                                                          --
--                         Copyright (C) 2018-2019                          --
--                                                                          --
--  Authors: Patrice Freydiere                                              --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------
with System;
with Zip;use Zip;
With Unzip.Streams;use Unzip.Streams;
with Zip_Streams; use Zip_Streams;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;use Ada.Text_IO;
with Gnat.Regpat;

with Synth.Wav; use Synth.Wav;

with Interfaces;

package body Synth.SoundBank is
   
   -- Bridge between wav and zip stream
   
   type Wav_Stream_From_Stream is new WAV_Read_Stream with record
      C: Natural;
      S: access Ada.Streams.Root_Stream_Type'Class;
   end record;
   
   procedure Read
     (Stream : in out Wav_Stream_From_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) ;
   
   procedure Write
     (Stream : in out Wav_Stream_From_Stream;
      Item   : Stream_Element_Array);
   
   function End_Of_Stream (Stream : Wav_Stream_From_Stream) return Boolean is
   begin
      return Stream.C <= 0;
   end End_Of_Stream;

   --  Read data from the stream.
   procedure Read
     (Stream : in out Wav_Stream_From_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Read(Stream.S.all, Item, Last);
      Stream.C := Stream.C - Natural(Item'Size / System.Storage_Unit);
      -- Put_Line("element left :" & Natural'image(Stream.c));
      if Stream.C < 0 then 
         raise Constraint_Error;
      end if;
   end;
   

   procedure Write
     (Stream : in out Wav_Stream_From_Stream;
      Item   : Stream_Element_Array) is
   begin
      raise Unsupported_Call;
   end;
   
   procedure FromZipStream(ZipStream: Stream_Access; 
                           Size_To_Read: File_Size_Type;
                           WavS: out Wav_Stream_From_Stream) is
   begin
      Wavs.C := Natural(Size_To_Read);
      Wavs.S := ZipStream;
   end;
   
    
   
   ----------
   -- Read --
   ----------
   
   function Read(FileName: String) return SoundBank_Access is
      
      -- use type Zip_Streams.File_Mode;
      
      package Pat renames Gnat.Regpat;
      -- example : DEFAULT_48_63_3137373934385F5F7562696B70686F6E696B5F5F63312E61696666_-1_-1_60.wav
      Pattern : constant Pat.Pattern_Matcher := 
        Pat.Compile(Expression => "^([A-Z0-9\-]+)_([0-9]{1,3})_([0-9]{1,3})_(.*)_([0-9]{1,3}).wav");
      
      type Decoded_Sound_Name is record
         RegisterSet: Unbounded_String;
         RootKey : Natural;
         From: Natural;
         To: Natural;
      end record;
      
      
      ZI : Zip_info;
      
      Input_Stream : aliased Zip_Streams.File_Zipstream;
      
      SoundSampleNumber: Natural := 0;
      
      
      SSA : SoundSample_Array (1..500);
      
      WavS: aliased Wav_Stream_From_Stream;
      
      SoundBank_Result : SoundBank_Access := null;
      
      Bank_Head : Bank_Access := null;
      
      procedure Decode_File_Name(FileName : String; 
                                 isCorrect: out boolean;
                                 Decoded: out Decoded_Sound_Name) is
         Result : Pat.Match_Array(0..Pat.Paren_Count(Pattern));
         use type Pat.Match_Location;
      begin
         Pat.Match(Self => Pattern,
                   Data => FileName,
                   Matches => Result);
         if Result(0) = Pat.No_Match then
            Put_Line("No Match");
            isCorrect := False;
            return;
         end if;
         
         -- Ada.Text_IO.Put_Line(FileName);
         
         pragma Assert(Result'Length >= 6);
         Decoded.RootKey := Natural'Value(Filename(Result(5).First..Result(5).Last));           
         --  Ada.Text_IO.Put_Line(Natural'Image(Decoded.RootKey));
         Decoded.From := Natural'Value(Filename(Result(2).First..Result(2).Last));
         Decoded.To := Natural'Value(Filename(Result(3).First..Result(3).Last));
         
         Decoded.RegisterSet := To_Unbounded_String(FileName(Result(1).First..
                                                      Result(1).Last));
                                                    
         --  Ada.Text_IO.Put_Line("Decoded :" & To_String(Decoded.RegisterSet));
         isCorrect := True;
      end Decode_File_Name;
      
      -- Find or create the bank pointer
      function Find_Bank(Bank_Name: Unbounded_String) return Bank_Access is
         Current : Bank_Access := Bank_Head;
      begin
        
         while Current /= null and then Current.Name /= Bank_Name loop
            Current := Current.Next;
         end loop;
         
        
         if Current = null then
            Ada.Text_IO.Put_Line("Not Found " & To_String(Bank_Name) & " Create It");
            Current := new Bank_Type'(Name         => Bank_Name,
                                      Note_Mapping => (others => No_Mapping),
                                      Next         => Bank_Head);
            Bank_Head := Current;
         end if;
         
         pragma Assert(Current /= null);
         
         return Current;
         
      end Find_Bank;
      
         
      
      
      procedure Process(name : String) is 
         file_index : Zip_Streams.ZS_Index_Type;
         comp_size : File_size_type;
         uncomp_size : File_size_type;
         crc_32 : Interfaces.Unsigned_32;
         Z : Zipped_File_Type;
      begin
         -- Put_Line(name);
         if name(name'Last - 3 .. name'Last) = ".wav" then
            -- Put_Line("read sound");
            -- search for offset in compressed stream
            Find_offset(
                        file => Input_Stream,
                        name => name,
                        case_sensitive => True,
                        file_index => file_index,
                        comp_size => comp_size,
                        uncomp_size => uncomp_size,
                        crc_32 => crc_32
                       );
            -- Put_Line(File_size_type'Image(comp_size));
            Set_Index(S  => Input_Stream,
                      To => file_index);
            
            unzip.Streams.Open(File             => Z,
                               Archive_Info     => ZI,
                               Name             => name);
                 
            declare
               S : constant Stream_Access := Stream(Z);
               R : Decoded_Sound_Name;
               isCorrect : Boolean;
            begin
               Decode_File_Name(FileName => name, 
                                isCorrect => isCorrect,
                                Decoded => R);
               if isCorrect then 
                  
                  -- get the stream
                  FromZipStream(ZipStream    => S,
                                Size_To_Read => uncomp_size,
                                WavS         => WavS);
            
                  -- pre increment
                  SoundSampleNumber := Natural'Succ(SoundSampleNumber);
                  
                  -- load Wav from Stream
                  Synth.Wav.Load(Wav_Stream_Access => WavS'Unchecked_Access,
                                 Sample            => SSA(SoundSampleNumber));
               
               
                  SSA(SoundSampleNumber).Note_Frequency := 
                    MIDICode_To_Frequency(R.RootKey);
                  
                  
                  SSA(SoundSampleNumber).Cant_Stop := false;
                  
                  -- find the bank
                  declare 
                     B : constant Bank_Access := Find_Bank(R.RegisterSet);
                  begin
                     -- others
                     for I in R.From..R.To loop
                        B.Note_Mapping(I) := SoundSampleNumber;
                     end loop;                        
                  end;
                  
               else
                  raise Program_Error with "bad name " & name;
               end if;
            end;
        
         end if; 
      end Process;
      
      
      procedure ExploreZip is new Zip.Traverse(Action => Process);
      
   begin
      Zip_Streams.Set_Name(Input_Stream, FileName);
      Zip_Streams.Open(Str => Input_Stream,
                       Mode => In_File);
      Zip.Load(info => ZI,
               from => Input_Stream);
      ExploreZip(ZI);
      
      SoundBank_Result := new SoundBank_Type(SoundSampleNumber);
      SoundBank_Result.Samples(1..SoundSampleNumber) := SSA(1..SoundSampleNumber); 
      SoundBank_Result.Banks := Bank_Head;
      
      declare 
         C : Bank_Access := SoundBank_Result.Banks;
      begin
         Ada.text_IO.put_line("List Sound Bank ====");
         while c /= null loop
            Ada.text_IO.put_line("Sound Bank :" & To_String(c.Name));
            C := C.Next;
         end loop;
      end;
      
        
      return SoundBank_Result;
   end Read;
   
   function GetSoundSample(S: SoundBank_Type; Midi_Note: Natural) 
                           return SoundSample is

   begin
      return GetSoundSample(s         => s,
                            Bank_Name => To_Unbounded_String("DEFAULT"),
                            Midi_Note => Midi_Note);
   end GetSoundSample;
     
   
   function GetSoundSample(s: SoundBank_Type; Bank_Name: Unbounded_String; Midi_Note:Natural)
                           return SoundSample is
   begin
      declare
         B: Bank_Access := S.Banks;
      begin
         while B/=null and then B.Name /= Bank_name loop
            B := B.Next;
         end loop;
         
         if B = null then
            Ada.Text_IO.Put_Line("Sound bank " & To_String(Bank_Name) & " Not Found");
            return Null_Sound_Sample;
         else
            declare 
               SoundMapping : constant Natural := B.Note_Mapping(Midi_Note);
            begin
               if SoundMapping = No_Mapping then
                  return Null_Sound_Sample;
               end if;
               

               return S.Samples(SoundMapping);
            end;         
         end if;
         end;
      end GetSoundSample;
   end Synth.SoundBank;
