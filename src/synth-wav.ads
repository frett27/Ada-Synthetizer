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

package Synth.Wav
  -- with SPARK_Mode => On
is

   ----------
   -- Load --
   ----------

   --  Load a Wav File from the file
   procedure Load (FileName : String; Sample : out SoundSample);
   --  with Post =>
   --    (Sample.Mono_Data /= null) and (Sample.Frequency >= 1.0) and
   --    (if Sample.HasLoop then Sample.Loop_Start >= Sample.Loop_End),
   --       Depends => (Sample => (FileName));

end Synth.Wav;
