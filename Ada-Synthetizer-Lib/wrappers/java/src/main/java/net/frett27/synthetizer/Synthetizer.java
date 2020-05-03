package net.frett27.synthetizer;

import java.io.Closeable;
import java.util.ArrayList;

import com.sun.jna.Callback;
import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Simple Java class to consume the Synthetizer Ada Library
 * 
 * @author pfreydiere
 *
 */
public class Synthetizer implements Closeable {

	static {
		// load library at startup
		System.loadLibrary("libsynthlib");
	}

	/**
	 * call back interface for preparing the buffers, using synth calls.
	 * 
	 * @author pfreydiere
	 *
	 */
	interface callback extends Callback {
		void prepare(Pointer p, long currentTime, long nextTime);
	}

	/**
	 * provide functions to call associated to the callback execution context, for
	 * interact with synth.
	 *
	 */
	public interface BufferPrepareFacilities {
		/**
		 * trigger a sound play, this function return a voice id
		 * 
		 * @param time
		 * @param soundid
		 * @param frequency
		 * @return
		 */
		long play(long time, long soundid, float frequency);

		/**
		 * stop a voice
		 * 
		 * @param time
		 * @param voiceid
		 */
		void stop(long time, long voiceid);
	}

	/**
	 * function to implement in synth call back
	 * 
	 * @author pfreydiere
	 */
	public interface BufferPrepare {
		void prepareBuffer(BufferPrepareFacilities synth, long startBufferTime, long stopBufferTime);
	}

	/**
	 * internal C interface of synthetizer api
	 */
	interface CLibrary extends Library {

		void synthlibinit();

		void synthlibfinal();

		void validatecall();

		// synthetizer initialization
		long synthetizer_init(PointerByReference p, int bufferSize, callback prepareCallBack);

		long synthetizer_close(PointerByReference p);

		long synthetizer_get_time(PointerByReference p, LongByReference time);

		long synthetizer_estimate_play_time(PointerByReference p, LongByReference time);

		// sound sample
		long synthetizer_load_wav_sample(Pointer filename, float sample_frequency, LongByReference voiceOut);

		long synthetizer_load_sample(Pointer floatFeatures, int floatFeaturesSize, float sampleFrequency,
				float noteFrequency, int cantstop, int hasloop, int loopStart, int loopend, LongByReference voiceOut);

		long synthetizer_free_sample(long voice);

		// play a sound
		long synthetizer_play(Pointer p, long sample_no, float frequency, LongByReference voice_result);

		long synthetizer_stop(Pointer p, long voice);

		long synthetizer_timed_play(Pointer p, long sample_no, float frequency, long time,
				LongByReference voice_result);

		long synthetizer_timed_stop(Pointer p, LongByReference voice, long time);

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
	private BufferPrepare javaCallBack;

	public void setPrepareBufferCallBack(BufferPrepare callback) {
		this.javaCallBack = callback;
	}

	private callback referenceToKeepCodeNotBeenGarbaged;

	public int defaultBufferSize = 50_000;

	public void open() throws Exception {

		BufferPrepareFacilities bufferApi = new BufferPrepareFacilities() {

			LongByReference voiceReturn = new LongByReference();

			@Override
			public long play(long time, long soundid, float frequency) {
				long ret = clibrary.synthetizer_timed_play(cSynth, soundid, frequency, time, voiceReturn);
				if (ret > 0) {
					// error handling,
					// we don't want exception to be raised in the synth
				}
				return voiceReturn.getValue();
			}

			@Override
			public void stop(long time, long voiceid) {
				voiceReturn.setValue(voiceid);
				long ret = clibrary.synthetizer_timed_stop(cSynth, voiceReturn, time);
				if (ret > 0) {
					// error handling,
					// we don't want exception to be raised in the synth
				}
			}
		};

		PointerByReference pByRef = new PointerByReference();
		referenceToKeepCodeNotBeenGarbaged = new callback() {
			@Override
			public void prepare(Pointer p, long currentTime, long nextTime) {
				if (javaCallBack != null) {
					javaCallBack.prepareBuffer(bufferApi, currentTime, nextTime);
				} else {
					System.out.println("javacallbak is null");
				}
			}
		};

		long ret = clibrary.synthetizer_init(pByRef, defaultBufferSize, referenceToKeepCodeNotBeenGarbaged);
		if (ret > 0) {
			throw new Exception("Exception in openning the synthetizer, it returns :" + ret);
		}

		cSynth = pByRef.getValue();
	}

	public void freeSample(long soundHandle) throws Exception {
		long ret = clibrary.synthetizer_free_sample(soundHandle);
		if (ret > 0) {
			throw new Exception("exception is releasing sound " + soundHandle + " it returns :" + ret);
		}
		loadedSounds.remove(soundHandle);
	}

	/**
	 * Close Synthetizer
	 * 
	 * @throws Exception
	 */
	@Override
	public void close() {
		// release loaded sounds
		while (loadedSounds.size() > 0) {
			Long handle = loadedSounds.get(0);

			try {
				if (handle != null) {
					freeSample(handle);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}

		long ret = clibrary.synthetizer_close(new PointerByReference(cSynth));
		if (ret > 0) {
			throw new RuntimeException("Exception in closing the synthizer, it returns :" + ret);
		}

	}

	/**
	 * get internal synthetizer time
	 * 
	 * @return
	 * @throws Exception
	 */
	public long getTime() throws Exception {
		LongByReference returnedTime = new LongByReference();
		long ret = clibrary.synthetizer_get_time(new PointerByReference(cSynth), returnedTime);
		if (ret > 0) {
			throw new Exception("Exception in getting time: " + ret);
		}
		return returnedTime.getValue();
	}

	/**
	 * get estimated synthetizer play time (taking into account the buffer play
	 * size)
	 * 
	 * @return
	 * @throws Exception
	 */
	public long getEstimateTime() throws Exception {
		LongByReference returnedTime = new LongByReference();
		long ret = clibrary.synthetizer_estimate_play_time(new PointerByReference(cSynth), returnedTime);
		if (ret > 0) {
			throw new Exception("Exception in getting estimated time: " + ret);
		}
		return returnedTime.getValue();
	}

	private ArrayList<Long> loadedSounds = new ArrayList<Long>();

	/**
	 * Load a Wav file into Sound memory
	 * 
	 * @param wavFilename the filename
	 * @return the sound handle
	 */
	public long loadSound(String wavFilename, float sampleMainFrequency) throws Exception {
		LongByReference soundSampleOut = new LongByReference();

		byte[] data = Native.toByteArray(wavFilename);
		Memory strpointer = new Memory(data.length + 1);
		strpointer.write(0, data, 0, data.length);
		strpointer.setByte(data.length, (byte) 0);

		long ret = clibrary.synthetizer_load_wav_sample(strpointer, sampleMainFrequency, soundSampleOut);
		if (ret > 0) {
			throw new Exception("Error in load sound, " + ret + " returned");
		}
		long soundHandle = soundSampleOut.getValue();
		loadedSounds.add(soundHandle);
		return soundHandle;
	}

	/**
	 * load a sample into synth memory
	 * 
	 * @param buffer
	 * @param sampleFrequency
	 * @param noteFrequency
	 * @param cantStop
	 * @return
	 * @throws Exception
	 */
	public long loadSample(float[] buffer, float sampleFrequency, float noteFrequency, boolean cantStop)
			throws Exception {
		return loadSample(buffer, sampleFrequency, noteFrequency, false, 0, 0, cantStop);

	}

	/**
	 * load a sample into synth memory
	 * 
	 * @param buffer          the samples (float -1 .. 1)
	 * @param sampleFrequency
	 * @param noteFrequency
	 * @param cantStop
	 * @return sound handle
	 * @throws Exception
	 */
	public long loadSample(float[] buffer, float sampleFrequency, float noteFrequency,
			boolean hasLoop, int loop_start, int loop_end,  boolean cantStop) throws Exception {
		LongByReference sampleOut = new LongByReference();
		assert buffer != null;

		final Pointer pFloatFeatures = new Memory(buffer.length * Native.getNativeSize(Float.TYPE));
		for (int i = 0; i < buffer.length; i++) {
			pFloatFeatures.setFloat(i * Native.getNativeSize(Float.TYPE), buffer[i]);
		}

		long ret = clibrary.synthetizer_load_sample(pFloatFeatures, buffer.length, sampleFrequency, noteFrequency,
				cantStop ? 1 : 0, hasLoop ? 1 : 0, loop_start, loop_end, sampleOut);
		if (ret > 0) {
			throw new Exception("Error in creating sound sample sound, " + ret + " returned");
		}
		long soundHandle = sampleOut.getValue();
		loadedSounds.add(soundHandle);
		return soundHandle;
	}

	/**
	 * play a sound at a given frequency
	 * 
	 * @param sound     the sound handle
	 * @param frequency
	 * @return
	 * @throws Exception
	 */
	public long play(long sound, float frequency) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_play(cSynth, sound, frequency, voiceReturn);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	/**
	 * play a sound at given time
	 * 
	 * @param sound     the soundid
	 * @param frequency play frequency
	 * @param time      time at which the sound is played
	 * @return the opened voice number
	 * @throws Exception
	 */
	public long play(long sound, float frequency, long time) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_timed_play(cSynth, sound, frequency, time, voiceReturn);
		if (ret > 0) {
			throw new Exception("Error in timed play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	/**
	 * stop given voice immediately
	 * 
	 * @param voice
	 * @return
	 * @throws Exception
	 */
	public long stop(long voice) throws Exception {
		LongByReference voiceReturn = new LongByReference();
		long ret = clibrary.synthetizer_stop(cSynth, voice);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

	/**
	 * 
	 * @param voice the voice previously created with play
	 * @param time  the time at which the voice is stopped
	 * @return the voice id
	 * @throws Exception
	 */
	public long stop(long voice, long time) throws Exception {
		LongByReference voiceReturn = new LongByReference(voice);
		long ret = clibrary.synthetizer_timed_stop(cSynth, voiceReturn, time);
		if (ret > 0) {
			throw new Exception("Error in play sound, " + ret + " returned");
		}
		return voiceReturn.getValue();
	}

}
