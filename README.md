# AdaSynthetizer

*Patrice Freydiere - Avril 2018*



This **library** provides a **synthetizer** for making music from samples (.wav files). A similar larger project is for example : timidity. This library has been setted up to be able to handle Highly Polyphonic rendering. 

[Example of rendering Here](http://www.barrel-organ-discovery.org/work/Record_Synth_Test_LowBandWidth_Applied.wav)  (Nota : The record made is not as good as the real time rendering)



The Synthetizer take on one side the samples, and on the other side the Notes to play. Each Wav can be reused for multiple play. 



![](doc/Synthetizer.png)

As this library handle a simple level of synthetizer, there are no notions of instruments. These concepts can be introduced easily in a layer based on this library.

An example of a result, reading a Midi File can be listened here : 2018-04-22_example_rendering

**Disclamer** : This library is still in the early stage of a sound engine for using on organ software, this is not yet ready for production. But opened to any contributions, or improvements



### Current Features

The API now works on Windows and Linux (Rpi). A conditional compilation, depending on the OS, compile the associated default driver. There may not have any Linux or Windows dependent code in using the library.  

#### Sound input

- Wav File (16 bits / Signed PCM / Mono) reading

Implementing your own file format reading is possible in populating the **SoundSample** type. Additional contribution can permit to handle more input file format.

#### Synthetizer capabilities

- 0.05 ms lattency for the moment, suitable for a lot of applications
- The max number of voice depends on the hardware provided, there is no hardcoded limitations, one can change the **MAX_VOICES** constant, and see whether it match the requierments. (As the number of polyphonie is increased, the processing may be heavier and can lead to increase the jitter and buffer sizes).
- Parametrized volume for each playing sound.
- Resampling : currently Nearest neighborhood algorithm (can be improved)

#### Current Drivers

- Win32 SoundDriver
- Alsa SoundDriver


Can be extended outside the library, depending on needs. Theses drivers show how to implement one.



## Using the Synthetizer by example : the code

Below, an example of the use of the synthetizer in 5 mins:



```pascal
 procedure Test_Open_And_Play_A_Sample(T : in out Test_Case'Class) is

    use Synth.Synthetizer;
      S : Synthetizer_Type;
      D : Synth.Driver.Sound_Driver_Access;
      Sample : Synth.SoundSample;
      Sample2 : Synth.SoundSample;
      V,V1 : Voice;
   begin

      Put_Line("Load the Sound Driver, from WIN32/Linux driver");
      Synth.Driver.Open (Driver => D);
      
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



## Feedbacks

The synthetizer behave nicely, there are no large amount or synchro between components. Playing Midi file is really amazing, and the quality for a first shot is quite interessing, be can be improved.

CPU consumption is not large, but on typical computer, process priority has to be upgraded to populate the sound buffer.



## Next actions

Version 0.1:

- ~~Fix Memory Leak for Win32 driver~~
- ~~Eval portaudio for output rendering (X Plateform sound toolkit)~~
- ~~Add Alsa Driver for linux plateforms~~
- Add Wav Driver for debugging purpose or to disk exports
- Interfaçing with C, Java for consuming the library

Version 0.2: 

- Improving Sound quality, add filters

Version 0.3:

- Porting to Spark, remove the task part for embedded usage

  ​

### Areas That can be covered next (if time permit)

Digital Signal processing : Permit to have Low / High Bandwidth filters, Compressors, Volume regulation, FadeIn / FadeOut

FM generators : The current buffers are filled with Wav samples, but a short abstraction can be done to introduce signal generators and modulators to extends the use to "analog like" synthetizers.