

all: alsa-binding-prep


Ada-Synthetizer/drivers/alsa-binding/src/sound-constants.ads:
	make -C Ada-Synthetizer/drivers/alsa-binding

alsa-binding-prep: Ada-Synthetizer/drivers/alsa-binding/src/sound-constants.ads


.PHONY: all

