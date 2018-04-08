# AdaSynthetizer

This simple library provides a synthetizer for making music from samples (.wav files).

A Synthetizer take on one side the samples, and on the other the Notes to play and Notes to Mix.

Disclamer : This is the early stage of 

### Features

#### Sound input

- Wav File (16 bits / Signed PCM / Mono) reading

you can also create your own file format reading, populating the SoundSample type

#### Synthetizer

- 0.05 ms lattency for the moment, suitable for a lot of applications

#### Drivers

- Win32 SoundDriver (Needs Profiling / Memory Leak check)



## Using the Synthetizer

Samples are provided, but the code is shown below for using it in 5 mins:



```pascal
 procedure Test_Open_And_Play_A_Sample(T : in out Test_Case'Class) is

    use Synth.Synthetizer;
      S : Synthetizer_Type;
      D : Synth.Driver.Sound_Driver_Access;
      Sample : Synth.SoundSample;
      Sample2 : Synth.SoundSample;
      V,V1 : Voice;
   begin

      Put_Line("Load the Sound Driver, from WIN32 driver");
      Synth.Driver.Win32.Open (Driver => D);
      
      Put_Line("Open the synth");
      Open (D => D, S => S);
      
      Put_Line("Load the sample test.wav");

      Synth.Wav.Load ("test-files/test.wav", Sample);
      Synth.Wav.Load ("test-files/test.wav", Sample2);

      Sample.Note_Frequency := 440.0; -- the sample is a A, 440 Hz

      Put_Line("Play Sound");
      Play(Synt         => S,
           S            => Sample,
           Frequency    => 440.0,
           Channel      => 1,
           Opened_Voice => V1);

      -- wait a bit to ear the 1st sound
      delay 0.5;

      Sample2.Note_Frequency := 440.0;
      Put_Line("Play Sound");
      Play(Synt         => S,
           S            => Sample2,
           Frequency    => 880.0,  -- play the second sample at a +1 Octave
           Channel      => 1,
           Opened_Voice => V);
      Put_Line("Stop the Voice :" & Voice'Image(V1));
      
      -- stop the 1st sample, given the Opened Voice
      Stop(Synt => S, Opened_Voice => V1);

      -- wait to ear the 2nd sample
      delay 2.0;


      S.Close;

   exception
            when E : others =>
               Synth.DumpException(E);

   end;
```



