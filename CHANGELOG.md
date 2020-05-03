# ChangeLog

*Patrice Freydiere - Avril 2020*



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

This sub project is more related to Barrel Organ Discovery, but a reading of its sound bank has been setted up to use recorded instruments in playing.