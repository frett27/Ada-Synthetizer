package net.frett27.synthetizer;

import com.sun.jna.Callback;
import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.PointerByReference;

public class Synthetizer {

	static {
		// load library at startup
		System.loadLibrary("libsynthlib");
	}

	interface callback extends Callback {
		void prepare(Pointer p, long currentTime, long nextTime);
	}

	interface CLibrary extends Library {

		void synthlibinit();

		void synthlibfinal();

		void validatecall();

		// synthetizer initialization
		long synthetizer_init(PointerByReference p, callback prepareCallBack);

		long synthetizer_close(PointerByReference p);

		// sound sample
		long synthetizer_load_wav_sample(Pointer filename, float sample_frequency, LongByReference voiceOut);

		// play a sound
		long synthetizer_play(Pointer p, long sample_no, float frequency, LongByReference voice_result);

		long synthetizer_stop(Pointer p, long voice);

		long synthetizer_timed_play(Pointer p, long sample_no, float frequency, long time,
				LongByReference voice_result);

		long synthetizer_timed_stop(Pointer p, long voice, long time);

	}

	static CLibrary clibrary = null;

	public Synthetizer() {
		if (clibrary == null) {
			// call via JNA
			clibrary = (CLibrary) Native.loadLibrary("libsynthlib", CLibrary.class);
			clibrary.synthlibinit();
		}
	}

	private Pointer cSynth;

	public void open() throws Exception {

		long ret = clibrary.synthetizer_init(new PointerByReference(cSynth), new callback() {
			@Override
			public void prepare(Pointer p, long currentTime, long nextTime) {
				// System.out.println(currentTime);
				// System.out.println(nextTime);

			}
		});
		if (ret > 0) {
			throw new Exception("Exception in openning the synthetizer, it returns :" + ret);
		}
	}

	public void close() throws Exception {
		long ret = clibrary.synthetizer_close(new PointerByReference(cSynth));
		if (ret > 0) {
			throw new Exception("Exception in closing the synthizer, it returns :" + ret);
		}
	}

	/**
	 * Load a Wav file into Sound memory
	 * 
	 * @param wavFilename the filename
	 * @return
	 */
	public long loadSound(String wavFilename) throws Exception {
		LongByReference soundSampleOut = new LongByReference();

		byte[] data = Native.toByteArray(wavFilename);
		Memory strpointer = new Memory(data.length + 1);
		strpointer.write(0, data, 0, data.length);
		strpointer.setByte(data.length, (byte) 0);

		long ret = clibrary.synthetizer_load_wav_sample(strpointer, 440.0f, soundSampleOut);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return soundSampleOut.getValue();
	}

	public long play(long sound, float frequency) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_play(cSynth, sound, frequency, voiceReturn);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	public long play(long sound, float frequency, long time) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_timed_play(cSynth, sound, frequency, time, voiceReturn);
		if (ret > 0) {
			throw new Exception("Error in timed play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	public long stop(long voice) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_stop(cSynth, voice);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	public long stop(long voice, long time) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_timed_stop(cSynth, voice, time);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

}
