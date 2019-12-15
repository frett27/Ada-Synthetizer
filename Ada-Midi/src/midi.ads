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

with Ada.Finalization;
with Ada.Exceptions;

package Midi is

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Byte_Array is array (Positive range <>) of Byte;
   type Byte_Array_Access is access Byte_Array;

   type Chunk is private;

   type Chunk_Access is access Chunk;

   type EventType is (MIDIEvent, MetaEvent, SysEvent);

   type MidiCmd is
     (NoteON,
      NoteOFF,
      AfterTouch,
      ControlChange,
      ProgramChange,
      ChannelAfterTouch,
      PitchRange);

   type MetaEventService is
     (TimeSignature,
      Tempo,
      Unknown);

   type ChannelType is range 0 .. 15;

   type Integer_24 is range 0 .. 2**24 - 1;
   for Integer_24'Size use 24;

   type Event (ET : EventType) is new Ada.Finalization.Controlled with record
      Ticks : Natural;
      Data  : Byte_Array_Access; -- Command Datas
      case ET is
         when MIDIEvent =>
            Channel : ChannelType;
            Cmd     : MidiCmd;
         when MetaEvent =>  -- FF Messages
            MetaService : MetaEventService;
            --  in case the metaevent service is unknown, the service
            --  is populated with the First data byte
            Service : Byte;
         when SysEvent =>  -- F0 F7 Messages
            null;
      end case;
   end record;

   type Event_Handler is access procedure (E : Event);

   Invalid_HeaderChunk : exception;
   Invalid_Function : exception;
   Invalid_Track : exception;
   Event_Not_Supported : exception;

   --  Parse a Chunk, and send events to the Event_handler
   --  if Event_handler is null, parse is done, but handler is
   --  not called.
   procedure Parse (C : Chunk;
                    EH : Event_Handler);

   type Error_Handler_Procedure_Access is access
        procedure (E : Ada.Exceptions.Exception_Occurrence);

   --  Parse a Chunk, and behave as above,
   --  this function takes an error handler to be called
   --  in case the event handler raise an exception
   procedure Parse (C : Chunk;
                    EH : Event_Handler;
                    Error_Handler : Error_Handler_Procedure_Access);

   --  Managing chunck structure
   procedure Initialize (O : in out Chunk);
   procedure Adjust (O : in out Chunk);
   procedure Finalize (O : in out Chunk);

   procedure AddEvent (C : in out Chunk; E : Event'Class);

   --  event handling
   procedure Initialize (O : in out Event);
   procedure Adjust (O : in out Event);
   procedure Finalize (O : in out Event);

   function ToByteArray (O : Event) return Byte_Array;

   subtype Note is Byte range 0 .. 127;
   subtype Vel is Byte range 0 .. 127;

   --  event creation helper methods
   function Create_Note_Event
     (Ticks    : Natural;
      Channel  : ChannelType;
      Note     : Midi.Note;
      Status   : Boolean; -- on/off
      Velocity : Midi.Vel) return Event;

   function Create_EOF_Track_Event return Event;

   function Create_Tempo_Event
     (Ticks           : Natural;
      MicroPerQuarter : Integer_24) return Event;

   function Create_Program_Change_Event
     (Ticks      : Natural;
      Channel    : ChannelType;
      NewProgram : Byte) return Event;

   --  Utility functions
   procedure ReadFixedNatural
     (Ab    :         Byte_Array_Access;
      Pos   :  in out Natural;
      Size  :         Natural;
      Value :     out Natural);

private

   function Word2Natural (B1, B2 : Byte) return Natural;

   type Chunk is new Ada.Finalization.Controlled with record
      --  by default, it's a music track ..
      ChunkType : String (1 .. 4) := "MTrk";
      Length    : Natural; -- Data Length
      Data      : Byte_Array_Access;
   end record;

end Midi;
