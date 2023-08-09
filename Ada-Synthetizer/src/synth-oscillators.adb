------------------------------------------------------------------------------
--                              Synthetizer                                 --
--                                                                          --
--                         Copyright (C) 2015-2018                          --
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

with Ada.Text_IO;
use Ada.Text_IO;

package body Synth.Oscillators is 
   
   function Can_Be_Stopped(O: in Wav_Oscillator_Type) return Boolean is
      begin
           if O.Play_Sample.Cant_Stop then
            return False;
         end if;

         if O.Stop_Play_Sample /= Not_Defined_Clock then
            return False;
         end if;

         return True;

   end Can_Be_Stopped;


   procedure Stop(X: in out Wav_Oscillator_Type) is
   begin
       X.Stop_Play_Sample := Synthetizer_Time_First;

   end;

   procedure Stop(X: in out Wav_Oscillator_Type; Stop: in Synthetizer_Time) is
   begin
       X.Stop_Play_Sample := Stop;

   end;

   procedure Change_Current_Play_Position(X: in out Wav_Oscillator_Type;
                                          Position: in Play_Second) is
   begin
      X.Current_Sample_Position := position;
   end;

   function Is_EndLess(X : in Wav_Oscillator_Type) return Boolean is
   begin
      return X.Play_Sample.HasLoop;
   end Is_EndLess;



   procedure Apply_Oscillator_To_Buffer (O : in out Wav_Oscillator_Type;
                                         Buffer : Frame_Array_Access;
                                         Volume_Factor : Float := 1.0;
                                         Driver_Play_Frequency : Frequency_Type;
                                         Start_Buffer_Time : Synthetizer_Time;
                                         ReachEndOscillator : out Boolean;
                                         Returned_Current_Oscillator_Position : out Play_Second) is

      Current_Sample_Position : Play_Second :=
        O.Current_Sample_Position;

      Driver_Play_Period : constant Play_Second :=
        Play_Second (1) / Play_Second (Driver_Play_Frequency);

      One_Sample_Frame_Period : constant Play_Second :=
        Play_Second (1.0) /
        Play_Second( Driver_Play_Frequency *
             O.Play_Sample.Frequency /
               Driver_Play_Frequency);

      One_Played_Sample_Frame_Period : constant Play_Second :=
        One_Sample_Frame_Period *
          Play_Second(O.Play_Sample.Note_Frequency /
            O.Note_Play_Frequency);

      function To_Second (Frame_Pos : Natural) return Play_Second is
      begin
         return Play_Second (Frame_Pos - O.Play_Sample.Mono_Data'First) *
           One_Played_Sample_Frame_Period;
      end To_Second;

      SS : constant SoundSample := O.Play_Sample;

      Sample_Data_Seconds : constant Play_Second :=
        To_Second (SS.Mono_Data'Length);

      function To_Pos_InSample (Pos : Play_Second) return Natural is
         Pos_In_Array : constant Natural :=
           Natural (Pos / One_Played_Sample_Frame_Period);
      begin
         return SS.Mono_Data'First + Pos_In_Array;
      end To_Pos_InSample;

      procedure Move_Next
        (Pos_In_Sample       : in out Play_Second;
         Reach_End :    out Boolean)
      is
      begin
         --  increment

         Pos_In_Sample := Pos_In_Sample + Driver_Play_Period;

         Reach_End := False;

         case O.Play_Sample.HasLoop is
            when True =>
               if Pos_In_Sample > To_Second (SS.Loop_End) then
                  Pos_In_Sample := Pos_In_Sample - To_Second (SS.Loop_Start);
               end if;
            when False =>
               begin
                  if Pos_In_Sample > Sample_Data_Seconds then
                     Reach_End := True;
                  end if;
               end;

         end case;
      end Move_Next;

   begin

      --  if VSA.Stopped and then VSA.Stop_Play_Sample = Not_Defined_Clock then
      --   ReachEndSample := True;
      --   return;
      --  end if;

      if O.Play_Sample = Null_Sound_Sample then
         Put_Line ("Null sample");
         ReachEndOscillator := True;
         return;
      end if;

      for i in Buffer'Range loop

         declare
            From_Start : constant Synthetizer_Time :=
              Synthetizer_Time (
                                Microseconds (Natural (Driver_Play_Period * Play_Second (i - Buffer'First)
                                  * Play_Second (1_000_000.0))));
            IsStarted : Boolean := True;
            CurrentTime : constant Synthetizer_Time := Start_Buffer_Time + From_Start;
         begin

            if Current_Sample_Position = 0.0 then -- sample has not started yet

               --  check if sample has started
               if CurrentTime < O.Start_Play_Sample then
                  --  continue loop
                  IsStarted := False;
               end if;
            end if;

            if  O.Stop_Play_Sample /= Not_Defined_Clock
              and then O.Stop_Play_Sample < CurrentTime then
               IsStarted := False;
               ReachEndOscillator := True;
               return;
            end if;

            if IsStarted then

               --  interpolate ?
               declare
                  Pos_In_Sample  : constant Natural :=
                    To_Pos_InSample (Current_Sample_Position);
               begin

                  if Pos_In_Sample > O.Play_Sample.Mono_Data'Last then
                     --  debug info,
                     Ada.Text_IO.Put_Line (" pos : " & Natural'Image (Pos_In_Sample));
                     Ada.Text_IO.Put_Line (" index pos : "
                                           & Natural'Image (SS.Mono_Data'Last));

                  end if;

                  pragma Assert (Pos_In_Sample >= SS.Mono_Data'First);
                  pragma Assert (Pos_In_Sample <= SS.Mono_Data'Last);
                  pragma Assert (Pos_In_Sample in SS.Mono_Data'Range);

                  declare
                     V1 : constant Float := SS.Mono_Data (Pos_In_Sample);
                     R  : Boolean;
                     F : Float :=  Buffer (i) + V1 * Volume_Factor; -- * VSA.Volume;
                  begin

                     if F > 1.0 then
                        F := 1.0;  -- saturation
                     end if;

                     if F < -1.0 then
                        F := -1.0;  -- saturation
                     end if;

                     Buffer (i) := F;

                     Move_Next (Pos_In_Sample => Current_Sample_Position,
                                Reach_End => R);
                     if R then
                        ReachEndOscillator := True;
                        Returned_Current_Oscillator_Position := Current_Sample_Position;
                        return;
                     end if;
                  end;
               end;

            end if;

         end;

      end loop;
      ReachEndOscillator := False;
      Returned_Current_Oscillator_Position := Current_Sample_Position;

   end Apply_Oscillator_To_Buffer;


end Synth.Oscillators;

