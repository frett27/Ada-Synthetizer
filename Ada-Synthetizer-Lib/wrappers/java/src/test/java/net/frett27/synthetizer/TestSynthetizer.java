package net.frett27.synthetizer;

import org.junit.Test;

public class TestSynthetizer {

	public void testLoadwavSample() throws Exception {

		Synthetizer s = new Synthetizer();

		s.open();
		try {

			long nn = s.loadSound("DR2-0003N_BaM.wav");
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

			float[] sin = new float[100000];
			float freq = 44_100; // Hz
			float pFreq = 1 / freq;
			float noteFreq = 440;
			float pnfreq = 1 / noteFreq;

			for (int i = 0; i < sin.length; i++) {
				sin[i] = (float) Math.sin(i * (2 * Math.PI / (pnfreq)) * pFreq);
			}

			System.out.println("load sample");
			long no = s.loadSample(sin, freq, noteFreq, false);
			Thread.sleep(2000);
			System.out.println("play");
			s.play(no, 440.0f);
			Thread.sleep(10000);

		} finally {
			s.close();
		}

	}

}
