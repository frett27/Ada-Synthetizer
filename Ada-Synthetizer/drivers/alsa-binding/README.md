[Ada sound recording API][1]
============================

Implemented features:

+ 16-bit mono recording (Linux)
+ 16-bit mono playback (Linux)
+ 16-bit stereo recording (Linux)

The library is released under the Beer-Ware License (revision 42).


Build dependencies
------------------

+ Bash
+ GNU Make
+ GNU Parallel (or `xargs`)
+ GNAT
+ Mercurial (`hg`)
+ Perl
+ libasound2


Build procedure
---------------

    hg clone http://repositories.jacob-sparre.dk/alsa-binding
    cd alsa-binding
    make


Test procedure
--------------

Sound as ASCII numbers for visual inspection:

    ./bin/test_alsa_binding > sound.text

Recording a WAV file (hard-coded to fail on big-endian machines):

    ./bin/microphone_to_wav > sound-mono.wav
    ./bin/record_stereo_wav > sound-stereo.wav

Playing a 2 kHz saw-tooth note:

    ./bin/playmono


Limitations
-----------

+ Currently only with implementations for Linux.
+ No installation procedure.


Copyright
---------

The copyright to the software belongs to JSA Research & Innovation.  The
software is distributed under the Beer Ware License (revision 42):

>  <jacob@jacob-sparre.dk> wrote this. As long as you retain this notice
>  you can do whatever you want with this stuff. If we meet some day, and
>  you think this stuff is worth it, you can buy me a beer in return.


Links
-----

If you want to find free Ada tools or libraries, [AdaIC][2] is an excellent
starting point.  You can also take a look at
[my other source text repositories][3] or [my web site][4].

[1]: http://repositories.jacob-sparre.dk/alsa-binding "Source text repository"
[2]: http://www.adaic.org/ada-resources/tools-libraries/ "Free Ada Tools and Libraries"
[3]: http://repositories.jacob-sparre.dk/ "My repositories on Bitbucket"
[4]: http://www.jacob-sparre.dk/ "My web site"

