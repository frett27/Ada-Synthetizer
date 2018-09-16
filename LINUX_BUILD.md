
# Building

dependencies (packages):

- libasound2-dev
- gprbuild
- gnat

cd drivers/alsa-binding/
make

gprbuild synthetizer_linux.gpr

or

gnatmake -d -p -P synthetizer_linux.gpr

