import com.sun.jna.Callback;
import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Test for calling the DLL from java
 * 
 * @author pfreydiere
 *
 */
public class TestSynth {

	static {
		// load library at startup
		System.loadLibrary("libsynthlib");
	}

	public interface callback extends Callback {
		void prepare(Pointer p, long currentTime, long nextTime);
	}

	public interface CLibrary extends Library {
		void synthlibinit();

		void synthlibfinal();

		void validatecall();

		// synthetizer initialization
		void synthetizer_init(PointerByReference p, callback prepareCallBack);

		void synthetizer_close(PointerByReference p);

		// sound sample
		void synthetizer_load_wav_sample(Pointer filename, float sample_frequency, LongByReference voiceOut);

		// play a sound
		void synthetizer_play(Pointer p, long sample_no, float frequency, LongByReference voice_result);

		void synthetizer_stop(Pointer p, long voice);

		void synthetizer_timed_play(Pointer p, long sample_no, float frequency, long time,
				LongByReference voice_result);

		void synthetizer_timed_stop(Pointer p, long voice);

	}

	public static void main(String[] args) throws Exception {

		System.out.println("Test Synth");
		System.out.println(System.getenv("PATH"));
		System.out.println(System.getProperty("java.library.path"));

		// call via JNA
		CLibrary clibrary = (CLibrary) Native.loadLibrary("libsynthlib", CLibrary.class);
		clibrary.synthlibinit();

		LongByReference soundSampleOut = new LongByReference();

		String filename = "DR2-0003N_BaM.wav";
		byte[] data = Native.toByteArray(filename);
		Pointer strpointer = new Memory(data.length + 1);
		strpointer.write(0, data, 0, data.length);
		strpointer.setByte(data.length, (byte) 0);

		clibrary.synthetizer_load_wav_sample(strpointer, 440.0f, soundSampleOut);

		System.out.println("voice :" + soundSampleOut.getValue());

		PointerByReference p = new PointerByReference();
		clibrary.synthetizer_init(p, new callback() {

			@Override
			public void prepare(Pointer p, long currentTime, long nextTime) {
				System.out.println("prepare " + currentTime + " - " + nextTime);

			}
		});
		
		Thread.sleep(1000);

		LongByReference voiceOut = new LongByReference();

		clibrary.synthetizer_play(p.getValue(), soundSampleOut.getValue(), 440.0f, voiceOut);
		Thread.sleep(1000);
		
		clibrary.synthetizer_play(p.getValue(), soundSampleOut.getValue(), 440.0f * 2, voiceOut);
		Thread.sleep(100);
		
		clibrary.synthetizer_stop(p.getValue(), voiceOut.getValue());
		Thread.sleep(20000);

		clibrary.synthetizer_close(p);

	}

}
