# AdaSynthetizer

*Patrice Freydiere - 2018*



This simple **library** provides a **synthetizer** for making music from samples (.wav files). A similar larger project is for example : timidity.

A Synthetizer take on one side the samples, and on the other side the Notes to play.

**Disclamer** : This library is still in the early stage of a sound engine for using on organ software, this is not yet ready for production.

### Features

#### Sound input

- Wav File (16 bits / Signed PCM / Mono) reading

you can also create your own file format reading, populating the SoundSample type

#### Synthetizer

- 0.05 ms lattency for the moment, suitable for a lot of applications
- The max number of voice depends on the hardware provided, there is no hardcoded limitations, one can change the MAX_VOICES constant, and see whether it match the requierments.
- Specify volume for each playing sound
- Nearest neighborhood algorithm for resampling (can be improved)

#### Drivers

- Win32 SoundDriver (Needs Profiling / Memory Leak check)



## Example using the Synthetizer by code

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



## Next actions

Version 0.1:

- Fix Memory Leak for Win32 driver
- Eval portaudio for output rendering (X Plateform sound toolkit)
- Add Alsa Driver for linux plateforms
- Add Wav Driver for debugging purpose or to disk exports
- Interfaçing with C, Java for consuming the library

Version 0.2: 

- Improving Sound quality, add filters

Version 0.3:

- Porting to Spark, remove the task part for embedded usage
