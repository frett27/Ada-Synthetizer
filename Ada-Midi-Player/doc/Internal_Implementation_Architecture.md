# Synthetizer internal implementation

*2019-10-06  Patrice Freydiere*

The synthetizer takes input notes and output frequency to prepare buffers that will be sent to external sound system. 



## SoundSample & SoundBank

The synthetizer use soundsample, this concept integrate Wave form, and also sound loops definitions, and wavform base frequency.

The sound sample wav form can be loaded from a Wav File, and the other properties can be setted on the structure.

In the 2019 implementation, Sound sample are fully read into main memory unit, and referenced using a pointer in internal structures.

Sound sample can also be read using a sound bank object. The sound bank is a zip file, including .wav files, using a specific schema for the names.



## Voice management

Each played note with the "Play" primitive in the synthetizer initiate a voice structure, which is synthetizer internal and keep track of the current played status.

In the implementation, keep internally :

- the soundsample reference
- the target resampling frequency
- the status of the internal wavform pointer for output
- Start and stop timing



Voices are preallocated and maintained in a protected object, `Voices_Type` . The "free" status of a voice is managed in a opened_voice boolean array associated to the initial structure.



### Time Management

Once the Synthetizer is start, a time reference if taken from the current system clock. All internal timings will be based on this reference.



### Buffer preparation

A Ring Buffer is used to manage the buffer preparation, permitting to be prepared ahead of time. Once the preparing task take a buffer to provide, it takes a start_time reference and associated open voices snapshot. 

For each active voices, the buffer will aggregate all wav form, using the target frequency, the internal wavform cursor and other properties (specific voice volume).

Once the buffer has been prepared, a new status of the voices are send to the synthetizer to update the internal state globally.

#### Buffer internal representation for voices and assembly

The internal buffer representation is a float between range -1..1, all merge or computation. Whereas the output will be in PCM format, the sound system will convert it to the physical one.

#### Resampling

The resampling use a "nearest neighbourhood" reampling algorithm. 



## Sound device subsystem

The sound subsystem is system dependent. A SoundIO device is provided for WIN32/WIN64, Linux, and other system.

A Default WAV output is provided.

