MIDIPLAYER_MOD_DIR := /home/use/projets/2021-04_bam/Ada-Synthetizer/Ada-Midi-Player-Lib/micropython

# Add all C files to SRC_USERMOD.
SRC_MOD += $(MIDIPLAYER_MOD_DIR)/midiplayer.c

# We can add our module folder to include paths if needed
# This is not actually needed in this example.
CFLAGS_USERMOD += -I$(EXAMPLE_MOD_DIR)
