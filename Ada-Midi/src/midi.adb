------------------------------------------------------------------------------
--                                 Ada Midi                                 --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
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

--  $Id: ais.ads,v 1.4 2003/09/30 05:48:30 frett Exp $

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Midi is

   ------------------------
   -- Dispose_Byte_Array --
   ------------------------

   procedure Dispose_Byte_Array is new Ada.Unchecked_Deallocation
     (Object => Byte_Array,
      Name   => Byte_Array_Access);

   --------------------------
   -- ReadVarLengthNatural --
   --------------------------

   procedure ReadVarLengthNatural
     (Ab    :        Byte_Array_Access;
      Pos   : in out Natural;
      Value :    out Natural)
   is
   begin
      Value := 0;
      loop
         Value := Value * 128 + (Natural (Ab (Pos)) mod 128);
         Pos   := Pos + 1;
         exit when Ab (Pos - 1) < 128;
      end loop;
   end ReadVarLengthNatural;

   --------------------------
   -- ReadFixedNatural --
   --------------------------

   procedure ReadFixedNatural
     (Ab    :         Byte_Array_Access;
      Pos   :  in out Natural;
      Size  :         Natural;
      Value :     out Natural)
   is
      LeftToRead : Natural := Size;
   begin
      Value := 0;
      while LeftToRead >= 1 loop
         Value := Value * 256 + Natural (Ab (Pos));
         Pos   := Pos + 1;
         LeftToRead := Natural'Pred (LeftToRead);
      end loop;
   end ReadFixedNatural;

   ------------------
   -- Word2Natural --
   ------------------

   function Word2Natural (B1, B2 : Byte) return Natural is
   begin
      return Natural (B1) * 256 + Natural (B2);
   end Word2Natural;

    -----------
   -- Parse --
   -----------

   procedure Parse (C : Chunk;
                    EH : Event_Handler) is
   begin
      Parse (C  => C,
             EH => EH,
             Error_Handler => null);
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse (C : Chunk;
                    EH : Event_Handler;
                    Error_Handler : Error_Handler_Procedure_Access) is
      I             : Natural := 1;
      Ticks         : Natural;
      V             : Natural;
      Runningstatus : Byte;

   begin

      --  Check Mtrk
      if C.ChunkType /= "MTrk" then
         raise Invalid_HeaderChunk;
      end if;

      while I < C.Length loop

         --  Read Ticks, for tempo
         ReadVarLengthNatural (C.Data, I, V);
         Ticks := V;

         --  Check if a status is here
         if C.Data (I) in 16#0# .. 16#7F# then
            --  use last status
            null;
         else
            Runningstatus := C.Data (I);
            I             := I + 1;
         end if;

         case Runningstatus is
            when 16#80# .. 16#8F# => -- note off

               --   Text_Io.Put("Note OFF - "
               --   & byte'Image(runningstatus mod 16));

               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := NoteOFF;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  --  Data are : note number, velocity
                  E.Data :=
                    new Byte_Array'(1 => C.Data (I), 2 => C.Data (I + 1));
                  if EH /= null then --   send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
               end;
               I := I + 2;

            when 16#90# .. 16#9F# => --   note on

               --    Text_Io.Put("Note ON - "
               --      & byte'Image(runningstatus mod 16));
               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := NoteON;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  --  datas are: note number and velocity
                  E.Data :=
                    new Byte_Array'(1 => C.Data (I), 2 => C.Data (I + 1));
                  --  if velocity is null, then it is a note off event
                  if C.Data (I + 1) = 0 then
                     E.Cmd := NoteOFF;
                  end if;

                  if EH /= null then --   send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
               end;
               I := I + 2;

            when 16#A0# .. 16#AF# => --  after touch
               --  Text_IO.Put ("After touch ");

               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := AfterTouch;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  E.Data    :=
                    new Byte_Array'(1 => C.Data (I), 2 => C.Data (I + 1));
                  if EH /= null then -- send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;

               end;
               I := I + 2;

            when 16#B0# .. 16#BF# =>
               --  Control Changed
               --  followed by two bytes : the controller no,
               --  the value
               --  Text_Io.Put("Control Changed " & Byte'Image(C.Data(I)));

               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := ControlChange;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);

                  case C.Data (I) is
                     when 16#0# .. 16#7F# =>
                        --  Text_Io.Put("Velocity " & Byte'Image(C.Data(I+1)));
                        E.Data :=
                          new Byte_Array'
                            (1 => C.Data (I), 2 => C.Data (I + 1));
                     when others =>
                        raise Invalid_Function;
                  end case;
                  I := I + 2; --  2 bytes data

                  if EH /= null then --   send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;

               end;

            when 16#C0# .. 16#CF# =>
               --       Text_Io.Put("Prg change "
               --         & Byte'Image( Runningstatus mod 16 ));

               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := ProgramChange;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  E.Data    := new Byte_Array'(1 => C.Data (I));
                  if EH /= null then -- send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
               end;
               I := I + 1;

            when 16#D0# .. 16#DF# =>
               --  Text_Io.Put("Channel pressure ");
               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := ChannelAfterTouch;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  E.Data    := new Byte_Array'(1 => C.Data (I));
                  if EH /= null then --   send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
               end;
               I := I + 1;

            when 16#E0# .. 16#EF# =>
               --  Text_Io.Put("Pitch Range ");
               declare
                  E : Event (MIDIEvent);
               begin
                  E.Cmd     := PitchRange;
                  E.Ticks   := Ticks;
                  E.Channel := ChannelType (Runningstatus mod 16);
                  E.Data    :=
                    new Byte_Array'(1 => C.Data (I), 2 => C.Data (I + 1));
                  if EH /= null then -- send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
               end;
               I := I + 2;

            when 16#F0# =>
               --  SysEx
               declare
                  Next : Byte := C.Data (I);
               begin
                  --  real time sysex
                  while C.Data (I) /= 16#F7# loop
                     I := I + 1;
                  end loop;
                  I := I + 1;
               end;

            when 16#F1# .. 16#FE# =>
               --               Text_IO.Put ("Function Not supported :" &
               --                           Byte'Image (Runningstatus));

               raise Event_Not_Supported with "Meta event "
                 &  Byte'Image (Runningstatus)
                 & " Unsupported";

            when 16#FF# => -- FF <Type> <Length>
               Runningstatus := 0;
               --  Text_Io.Put("Meta (Type:" & Byte'Image(C.Data(I))
               --     & ") length : " & Byte'Image(C.Data(I+1)) );
               declare
                  E : Event (MetaEvent);
                  A : Byte_Array_Access;
                  V : Natural; -- length of the transmit elements
               begin

                  E.Service := C.Data (I);

                  case E.Service is
                     when 16#58# =>
                        E.MetaService := TimeSignature;

                     when 16#51# =>
                        E.MetaService := Tempo;

                     when others =>
                        --  Ada.Text_IO.Put_Line ("unknown Service :"  &
                        --                         Byte'Image (E.Service));
                        E.MetaService := Unknown;
                  end case;

                  I := I + 1;

                  ReadVarLengthNatural (C.Data, I, V);

                  A := new Byte_Array (1 .. Integer (V));

                  A (A'Range) := C.Data (I .. I + Integer (V) - 1);

                  E.Data  := A;
                  E.Ticks := Ticks;
                  if EH /= null then --   send event to the handler
                     begin
                        EH (E);
                     exception
                        when Error : others =>
                           if Error_Handler /= null then
                              Error_Handler (Error);
                           end if;
                     end;
                  end if;
                  I := I + Integer (V);

               end;

            when others =>
               raise Event_Not_Supported with "Unexpected Value "
                 & Byte'Image (C.Data (I))
                 & " at offset " & Natural'Image (I);
         end case;

      end loop;
   end Parse;

   --------------
   -- AddEvent --
   --------------
   --  add an event to the chunk
   procedure AddEvent (C : in out Chunk; E : Event'Class) is
      B : Byte_Array := ToByteArray (E);
   begin
      --  Add Event in the chunk
      --  not implemented yet
      if C.Data = null then
         --  Text_IO.Put_Line ("Create a new buffer ");
         --  allocate a buffer
         C.Data   := new Byte_Array (1 .. 1024);
         C.Length := 0;
      end if;

      if C.Length + B'Length > C.Data'Length then
         --  must increase the buffer size
         declare
            --  double size
            NewArray : Byte_Array_Access :=
              new Byte_Array (1 .. C.Data'Length * 2);
         begin
            --  Text_IO.Put_Line ("Increase Chunk Length ");
            NewArray (C.Data'Range) := C.Data.all;
            Dispose_Byte_Array (C.Data);
            C.Data := NewArray;
         end;
      end if;

      --  add event at the end
      C.Data (C.Length + 1 .. C.Length + B'Length) := B (1 .. B'Length);
      C.Length                                     := C.Length + B'Length;

   end AddEvent;

   ------------
   -- Adjust --
   ------------
   --  Adjusting event Extra Datas
   procedure Adjust (O : in out Event) is
      A : Byte_Array_Access;
   begin
      --  copy data arrays
      A           := new Byte_Array (O.Data'Range);
      A (A'Range) := O.Data (A'Range);
      O.Data      := A;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Chunk) is
      A : Byte_Array_Access;
   begin
      --  copying data arrays
      A           := new Byte_Array (O.Data'Range);
      A (A'Range) := O.Data (A'Range);
      O.Data      := A;
   end Adjust;

   ----------------------------
   -- Create_EOF_Track_Event --
   ----------------------------

   function Create_EOF_Track_Event return Event is
      Result : Event (MetaEvent);
   begin
      Result.Service := 16#2F#;
      Result.Data    := new Midi.Byte_Array'(1 => 0);
      return Result;
   end Create_EOF_Track_Event;

   -----------------------
   -- Create_Note_Event --
   -----------------------

   function Create_Note_Event
     (Ticks    : Natural;
      Channel  : ChannelType;
      Note     : Midi.Note;
      Status   : Boolean; -- on/off
      Velocity : Midi.Vel) return Event
   is
      Result : Event (MIDIEvent);
   begin
      if Status then
         Result.Cmd := NoteON;
      else
         Result.Cmd := NoteOFF;
      end if;

      Result.Channel := Channel;
      Result.Ticks   := Ticks;
      Result.Data    := new Midi.Byte_Array'(1 => Note, 2 => Velocity);

      return Result;
   end Create_Note_Event;

   ---------------------------------
   -- Create_Program_Change_Event --
   ---------------------------------

   function Create_Program_Change_Event
     (Ticks      : Natural;
      Channel    : ChannelType;
      NewProgram : Byte) return Event
   is
      Result : Event (MIDIEvent);
   begin
      Result.Cmd     := ProgramChange;
      Result.Channel := Channel;
      Result.Ticks   := Ticks;
      Result.Data    := new Midi.Byte_Array'(1 => NewProgram);
      return Result;
   end Create_Program_Change_Event;

   ------------------------
   -- Create_Tempo_Event --
   ------------------------

   function Create_Tempo_Event
     (Ticks           : Natural;
      MicroPerQuarter : Integer_24) return Event
   is
      Result : Event (MetaEvent);
      B1     : Byte := Byte (MicroPerQuarter mod 256);
      B2     : Byte := Byte ((MicroPerQuarter / 256) mod 256);
      B3     : Byte := Byte (((MicroPerQuarter / 256) / 256) mod 256);
   begin
      Result.Ticks   := Ticks;
      Result.Service := 16#51#;
      Result.Data    :=
        new Midi.Byte_Array'(1 => 16#03#, 2 => B3, 3 => B2, 4 => B1);

      return Result;
   end Create_Tempo_Event;

   --------------
   -- Finalize --
   --------------
   --  Release event extra datas

   procedure Finalize (O : in out Chunk) is
   begin
      if O.Data /= null then
         Dispose_Byte_Array (O.Data);
         O.Data := null;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Event) is
   begin
      if O.Data /= null then
         --  free event datas
         Dispose_Byte_Array (O.Data);
         O.Data := null;
      end if;

   end Finalize;

   --  Initialize events
   procedure Initialize (O : in out Chunk) is
   begin
      O.Data := null;
   end Initialize;

   procedure Initialize (O : in out Event) is
   begin
      O.Data := null;
   end Initialize;

   --  Convert Natural tick, in Midi Coded Byte suite
   function ToMidiTick (N : Natural) return Byte_Array is
      ByteCount : Natural range 1 .. 100 := 1;
      Result    : Byte_Array (1 .. 10); -- MAX Bytes for 1 Number
      K         : Natural                := N;
      Temp      : Byte_Array (1 .. 10);
   begin

      Temp (ByteCount) := Byte (K mod 128);
      while (K / 128) > 0 loop
         K                := K / 128;
         ByteCount        := ByteCount + 1;
         Temp (ByteCount) := Byte ((K mod 128) + 128);
      end loop;
      --  Reverse order
      for I in 1 .. ByteCount loop
         Result (ByteCount + 1 - I) := Temp (I);
      end loop;

      return Result (1 .. ByteCount);

   end ToMidiTick;

   --  helper function for creating events
   --  Convert Event in Byte suite ..
   function ToByteArray (O : Event) return Byte_Array is
      ByteCount : Natural range 1 .. 100 := 1;
      Ticks     : Byte_Array := ToMidiTick (O.Ticks); -- calculate midi ticks
      Result    : Byte_Array (1 .. 100); -- MAX Bytes for an event
   begin

      Result (Ticks'Range) := Ticks;
      ByteCount            := Ticks'Length;

      --  Ticks Added

      case O.ET is
         when MIDIEvent =>
            ByteCount := ByteCount + 1;
            case O.Cmd is
               when NoteOFF =>
                  Result (ByteCount) := Byte (16#80#) + Byte (O.Channel);

               when NoteON =>
                  Result (ByteCount) := Byte (16#90#) + Byte (O.Channel);

               when AfterTouch =>
                  Result (ByteCount) := Byte (16#A0#) + Byte (O.Channel);

               when ControlChange =>
                  Result (ByteCount) := Byte (16#B0#) + Byte (O.Channel);

               when ProgramChange =>
                  Result (ByteCount) := Byte (16#C0#) + Byte (O.Channel);

               when PitchRange =>
                  Result (ByteCount) := Byte (16#E0#) + Byte (O.Channel);

               when ChannelAfterTouch =>
                  Result (ByteCount) := Byte (16#D0#) + Byte (O.Channel);

            end case;

         when MetaEvent =>

            ByteCount := Natural'Succ (ByteCount);
            --  raise Event_Not_Supported;

            Result (ByteCount) := 16#FF#;

            ByteCount := Natural'Succ (ByteCount);

            Result (ByteCount) := O.Service;

         when SysEvent =>
            raise Event_Not_Supported;
      end case;

      --  dump all datas with the cmds
      return Result (1 .. ByteCount) & O.Data.all;

   end ToByteArray;

end Midi;
