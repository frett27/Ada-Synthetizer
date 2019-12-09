Libsoundio Ada bindings
=======================

This is a set of bindings to Libsoundio, a cross-platform/cross-toolkit sound
input and output library. See https://github.com/andrewrk/libsoundio for more
information.

For the moment the bindings are pretty basic. To use them, just include
`soundio.gpr` to your own project, and make sure to add `-lsoundio` to your
linker switches. You can check [the example](https://github.com/raph-amiard/ada-soundio/blob/master/example) to have more details.

In time I will try to provide idiomatic Ada bindings. Stay tuned !

Prerequisites
=============

You need a GNAT toolchain at least. You can also build libsoundio yourself or
use the build script to build it automatically.

Building
========

You can build it by hand or use the build script this way, if you are on a
fairly standard linux distribution. There's even a chance it'll work on cygwin,
although I did not test it.

~~~bash
$ ./build.script
~~~

After that, you still need to put libsoundio in your library path, which you
can do by sourcing the `env.sh` file.

All the build script does is build libsoundio for you. If you want you can
alternatively build libsoundio yourself, install it manually, and just compile
ada-soundio by running:

~~~bash
$ gprbuild -p -P soundio.gpr
~~~
