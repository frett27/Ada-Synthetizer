package net.frett27.synthetizer;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import net.frett27.synthetizer.Synthetizer.BufferPrepare;
import net.frett27.synthetizer.Synthetizer.BufferPrepareFacilities;

public class TestSynthetizer {

	public void testLoadwavSample() throws Exception {

		Synthetizer s = new Synthetizer();

		s.open();
		try {

			long nn = s.loadSound("DR2-0003N_BaM.wav", 440f);
			Thread.sleep(1000);
			s.play(nn, 440);

			Thread.sleep(10_000);

		} finally {
			s.close();
		}

	}

	@Test
	public void testLoadSampleFromMemory() throws Exception {

		Synthetizer s = new Synthetizer();

		s.open();
		try {

			long no = createSinSoundSample(s);
			Thread.sleep(2000);
			System.out.println("play");
			s.play(no, 440.0f);
			Thread.sleep(10000);

		} finally {
			s.close();
		}

	}

	private long createSinSoundSample(Synthetizer s) throws Exception {

		float[] sin = new float[100_000];
		float freq = 44_100; // Hz
		float pFreq = 1 / freq;
		float noteFreq = 440;
		float pnfreq = 1 / noteFreq;

		float volumeFactor = 0.2f;

		for (int i = 0; i < sin.length; i++) {
			sin[i] = (float) Math.sin(i * (2 * Math.PI / (pnfreq)) * pFreq) * volumeFactor
					* (float) (1.0f - Math.pow(i * 1.0f / sin.length, 2));
		}

		System.out.println("load sample");
		long no = s.loadSample(sin, freq, noteFreq, false);
		return no;
	}

	@Test
	public void testCallback() throws Exception {

		// hi stress load
		ExecutorService executor = Executors.newFixedThreadPool(8);

		AtomicBoolean a = new AtomicBoolean(false);
		for (int i = 0; i < 8; i++) {
			executor.submit(() -> {
				while (!a.get()) {
				};

			});

		}

		Synthetizer s = new Synthetizer();
		AtomicInteger i = new AtomicInteger(0);
		s.defaultBufferSize = 10_000;
		s.open();
		try {
			long soundid = createSinSoundSample(s);

			s.setPrepareBufferCallBack(new BufferPrepare() {
				@Override
				public void prepareBuffer(BufferPrepareFacilities synth, long startBufferTime, long stopBufferTime) {
					float f = (i.getAndIncrement() % 12) / 12.0f;
					float freq = (float) (440f * Math.pow(2, f));
					System.out.println(startBufferTime + ":" + stopBufferTime + "->" + f + " " + freq);
					if (i.get() % 2 == 0) {
						long s = synth.play((startBufferTime + stopBufferTime) / 2, soundid, freq);
						System.out.println("voice :" + s);
						synth.stop((startBufferTime + stopBufferTime) / 2 + 3 * (stopBufferTime - startBufferTime), s);
					}
				}
			});
			s.getTime();
			Thread.sleep(10_000);
		} finally {
			s.close();
		}
		a.set(true);
		
	}

}
