# ChangeLog

*Patrice Freydiere - June 2021*

## **Version 0.25:**

**Ada-Midi-Player-Lib** , a midi player library for playing midi files, and activating sound banks on the fly. This capability has been added to create embedded plateform for playing midi music. 

A simple API provided on midi player to play MIDI files :

```
   --  Init the bam_engine
   procedure Init (SoundDriver : Synth.Driver.Sound_Driver_Access);

   --  define the sound bank playing
   procedure Define_SoundBank (S : Synth.SoundBank.SoundBank_Access);

   --  play the given midi filename
   procedure Play (FileName : String);

   --  change the tempo factor
   procedure Change_Tempo_Factor (Tempo_Factor : Float);

   --  activate a given bank name by its name
   procedure Activate_Bank (Bank_Name : String);

   --  deactivate a given bank name by its name
   procedure Deactivate_Bank (Bank_Name : String);

   --  is it playing ?
   function IsPlaying return Boolean;

   --  stop the play, release
   procedure Stop;
   
   --  close the midi player task
   procedure Close;
```



## **Version 0.2 :**

<u>Using the synthetizer in External Lib and Java Wrapper</u>

The library has been integrated in external software using C api, using the **Ada-Synthetizer-Lib** project. For this purpose, a java wrapper has been setted up to ease the reuse on Java. See documentation

**Ada Midi Player** has been added to demonstrate the use of the library, in a command line. Several options are available :

```
Ada-Midi-Player>main --help
Usage: main.exe [switches] [arguments]

 -f ARG         Specify the midi file to read
 -t ARG         Tempo factor
 -b, --bank=ARG instrument filename
 -w ARG         Output to Wav File
 --tempo=ARG    Enable long option. Arg is an integer
 --help         Display help
```

The Midi Player is very limited, and is able to play a midi (merging all tracks, using a single recorded instrument). You can pass the -b option to use the **instrumentbundle** instrument file.



**APrint Studio Soundbank reading from Ada** 

This sub project is more related to [Barrel Organ Discovery](https://www.barrel-organ-discovery.org), but a reading of its sound bank has been setted up to use recorded instruments in playing.