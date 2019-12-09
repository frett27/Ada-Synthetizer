.PHONY: clean
.PHONY: all
.PHONY: tests

ubuntu: all

all: clean
	gnatmake -d -p -P adamidi.gpr

clean:
	gnat clean -P adamidi.gpr

clean-all:
	gnat clean -r -P adamidi.gpr

clean-tests:
	gnat clean -r -P adamidi.gpr
