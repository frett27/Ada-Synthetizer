with Ada.Text_IO;

with ALSA_Binding.Mercurial;

procedure ALSA_Binding.Demo is
begin
   Ada.Text_IO.Put_Line (Mercurial.Revision);
end ALSA_Binding.Demo;
