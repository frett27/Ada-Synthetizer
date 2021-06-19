with Ada.Text_IO;
with Synth.SoundBank; use Synth.SoundBank;


procedure TestReadSoundBank is
   S : SoundBank_Access := Read ("30notes.instrumentbundle");
begin
   Ada.Text_IO.Put_Line ("Test Read 30 notes");
end TestReadSoundBank;
