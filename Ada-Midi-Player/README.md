# Ada Midi Player - Command line interface



This project use the [Ada-Midi-Player-Lib](../Ada-Midi-Player-Lib) subproject to play a midi file from command line.




	./midiplayer --help
	Usage: midiplayer [switches] [arguments]
	
	 -f ARG          Specify the midi file to read
	 -t ARG          Tempo factor
	 -b, --bank=ARG  instrument filename
	 -m, --music-box music box behaviour
	 -w ARG          Output to Wav File
	 -r ARG          Frequency for playing
	 --tempo=ARG     Enable long option. Arg is an integer
	 --help          Display help

To run the player from command line:

	./midiplayer -f chpn_op7_1.mid -b boitemusique_-1621402058.instrumentbundle -m



## Options documentation 

### -w [wav filename] option

This option permit to export the midi play into an output wavfile (and not the current sound system).



### -t  [tempo factor] option

tempo factor permit to change the speed of the play using a decimal number. Schedules times will be multiplied by this argument. 



### -m option

This option will ignore all the noteOff events, remaining the played sound to it's natural end.

