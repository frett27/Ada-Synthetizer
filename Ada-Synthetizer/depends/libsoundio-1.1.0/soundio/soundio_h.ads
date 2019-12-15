pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;

package soundio_h is

   --  unsupported macro: SOUNDIO_EXTERN_C extern "C"
   --  unsupported macro: SOUNDIO_EXPORT SOUNDIO_EXTERN_C __declspec(dllimport)
   --  unsupported macro: SoundIoFormatS16NE SoundIoFormatS16LE
   --  unsupported macro: SoundIoFormatU16NE SoundIoFormatU16LE
   --  unsupported macro: SoundIoFormatS24NE SoundIoFormatS24LE
   --  unsupported macro: SoundIoFormatU24NE SoundIoFormatU24LE
   --  unsupported macro: SoundIoFormatS32NE SoundIoFormatS32LE
   --  unsupported macro: SoundIoFormatU32NE SoundIoFormatU32LE
   --  unsupported macro: SoundIoFormatFloat32NE SoundIoFormatFloat32LE
   --  unsupported macro: SoundIoFormatFloat64NE SoundIoFormatFloat64LE
   --  unsupported macro: SoundIoFormatS16FE SoundIoFormatS16BE
   --  unsupported macro: SoundIoFormatU16FE SoundIoFormatU16BE
   --  unsupported macro: SoundIoFormatS24FE SoundIoFormatS24BE
   --  unsupported macro: SoundIoFormatU24FE SoundIoFormatU24BE
   --  unsupported macro: SoundIoFormatS32FE SoundIoFormatS32BE
   --  unsupported macro: SoundIoFormatU32FE SoundIoFormatU32BE
   --  unsupported macro: SoundIoFormatFloat32FE SoundIoFormatFloat32BE
   --  unsupported macro: SoundIoFormatFloat64FE SoundIoFormatFloat64BE
   SOUNDIO_MAX_CHANNELS : constant := 24;  --  soundio.h:300

  -- * Copyright (c) 2015 Andrew Kelley
  -- *
  -- * This file is part of libsoundio, which is MIT licensed.
  -- * See http://opensource.org/licenses/MIT
  --  

  --/ \cond
  --/ \endcond
  --* \mainpage
  -- *
  -- * \section intro_sec Overview
  -- *
  -- * libsoundio is a C library for cross-platform audio input and output. It is
  -- * suitable for real-time and consumer software.
  -- *
  -- * Documentation: soundio.h
  --  

  --* \example sio_list_devices.c
  -- * List the available input and output devices on the system and their
  -- * properties. Supports watching for changes and specifying backend to use.
  --  

  --* \example sio_sine.c
  -- * Play a sine wave over the default output device.
  -- * Supports specifying device and backend to use.
  --  

  --* \example sio_record.c
  -- * Record audio to an output file.
  -- * Supports specifying device and backend to use.
  --  

  --* \example sio_microphone.c
  -- * Stream the default input device over the default output device.
  -- * Supports specifying device and backend to use.
  --  

  --* \example backend_disconnect_recover.c
  -- * Demonstrates recovering from a backend disconnecting.
  --  

  --/ See also ::soundio_strerror
   type SoundIoError is 
     (SoundIoErrorNone,
      SoundIoErrorNoMem,
      SoundIoErrorInitAudioBackend,
      SoundIoErrorSystemResources,
      SoundIoErrorOpeningDevice,
      SoundIoErrorNoSuchDevice,
      SoundIoErrorInvalid,
      SoundIoErrorBackendUnavailable,
      SoundIoErrorStreaming,
      SoundIoErrorIncompatibleDevice,
      SoundIoErrorNoSuchClient,
      SoundIoErrorIncompatibleBackend,
      SoundIoErrorBackendDisconnected,
      SoundIoErrorInterrupted,
      SoundIoErrorUnderflow,
      SoundIoErrorEncodingString);
   pragma Convention (C, SoundIoError);  -- soundio.h:72

  --/ Out of memory.
  --/ The backend does not appear to be active or running.
  --/ A system resource other than memory was not available.
  --/ Attempted to open a device and failed.
  --/ The programmer did not comply with the API.
  --/ libsoundio was compiled without support for that backend.
  --/ An open stream had an error that can only be recovered from by
  --/ destroying the stream and creating it again.
  --/ Attempted to use a device with parameters it cannot support.
  --/ When JACK returns `JackNoSuchClient`
  --/ Attempted to use parameters that the backend cannot support.
  --/ Backend server shutdown or became inactive.
  --/ Buffer underrun occurred.
  --/ Unable to convert to or from UTF-8 to the native string format.
  --/ Specifies where a channel is physically located.
   type SoundIoChannelId is 
     (SoundIoChannelIdInvalid,
      SoundIoChannelIdFrontLeft,
      SoundIoChannelIdFrontRight,
      SoundIoChannelIdFrontCenter,
      SoundIoChannelIdLfe,
      SoundIoChannelIdBackLeft,
      SoundIoChannelIdBackRight,
      SoundIoChannelIdFrontLeftCenter,
      SoundIoChannelIdFrontRightCenter,
      SoundIoChannelIdBackCenter,
      SoundIoChannelIdSideLeft,
      SoundIoChannelIdSideRight,
      SoundIoChannelIdTopCenter,
      SoundIoChannelIdTopFrontLeft,
      SoundIoChannelIdTopFrontCenter,
      SoundIoChannelIdTopFrontRight,
      SoundIoChannelIdTopBackLeft,
      SoundIoChannelIdTopBackCenter,
      SoundIoChannelIdTopBackRight,
      SoundIoChannelIdBackLeftCenter,
      SoundIoChannelIdBackRightCenter,
      SoundIoChannelIdFrontLeftWide,
      SoundIoChannelIdFrontRightWide,
      SoundIoChannelIdFrontLeftHigh,
      SoundIoChannelIdFrontCenterHigh,
      SoundIoChannelIdFrontRightHigh,
      SoundIoChannelIdTopFrontLeftCenter,
      SoundIoChannelIdTopFrontRightCenter,
      SoundIoChannelIdTopSideLeft,
      SoundIoChannelIdTopSideRight,
      SoundIoChannelIdLeftLfe,
      SoundIoChannelIdRightLfe,
      SoundIoChannelIdLfe2,
      SoundIoChannelIdBottomCenter,
      SoundIoChannelIdBottomLeftCenter,
      SoundIoChannelIdBottomRightCenter,
      SoundIoChannelIdMsMid,
      SoundIoChannelIdMsSide,
      SoundIoChannelIdAmbisonicW,
      SoundIoChannelIdAmbisonicX,
      SoundIoChannelIdAmbisonicY,
      SoundIoChannelIdAmbisonicZ,
      SoundIoChannelIdXyX,
      SoundIoChannelIdXyY,
      SoundIoChannelIdHeadphonesLeft,
      SoundIoChannelIdHeadphonesRight,
      SoundIoChannelIdClickTrack,
      SoundIoChannelIdForeignLanguage,
      SoundIoChannelIdHearingImpaired,
      SoundIoChannelIdNarration,
      SoundIoChannelIdHaptic,
      SoundIoChannelIdDialogCentricMix,
      SoundIoChannelIdAux,
      SoundIoChannelIdAux0,
      SoundIoChannelIdAux1,
      SoundIoChannelIdAux2,
      SoundIoChannelIdAux3,
      SoundIoChannelIdAux4,
      SoundIoChannelIdAux5,
      SoundIoChannelIdAux6,
      SoundIoChannelIdAux7,
      SoundIoChannelIdAux8,
      SoundIoChannelIdAux9,
      SoundIoChannelIdAux10,
      SoundIoChannelIdAux11,
      SoundIoChannelIdAux12,
      SoundIoChannelIdAux13,
      SoundIoChannelIdAux14,
      SoundIoChannelIdAux15);
   pragma Convention (C, SoundIoChannelId);  -- soundio.h:106

  --/< First of the more commonly supported ids.
  --/< Last of the more commonly supported ids.
  --/< First of the less commonly supported ids.
  --/ Mid/side recording
  --/ first order ambisonic channels
  --/ X-Y Recording
  --/< First of the "other" channel ids
  --/< Last of the "other" channel ids
  --/ Built-in channel layouts for convenience.
   type SoundIoChannelLayoutId is 
     (SoundIoChannelLayoutIdMono,
      SoundIoChannelLayoutIdStereo,
      SoundIoChannelLayoutId2Point1,
      SoundIoChannelLayoutId3Point0,
      SoundIoChannelLayoutId3Point0Back,
      SoundIoChannelLayoutId3Point1,
      SoundIoChannelLayoutId4Point0,
      SoundIoChannelLayoutIdQuad,
      SoundIoChannelLayoutIdQuadSide,
      SoundIoChannelLayoutId4Point1,
      SoundIoChannelLayoutId5Point0Back,
      SoundIoChannelLayoutId5Point0Side,
      SoundIoChannelLayoutId5Point1,
      SoundIoChannelLayoutId5Point1Back,
      SoundIoChannelLayoutId6Point0Side,
      SoundIoChannelLayoutId6Point0Front,
      SoundIoChannelLayoutIdHexagonal,
      SoundIoChannelLayoutId6Point1,
      SoundIoChannelLayoutId6Point1Back,
      SoundIoChannelLayoutId6Point1Front,
      SoundIoChannelLayoutId7Point0,
      SoundIoChannelLayoutId7Point0Front,
      SoundIoChannelLayoutId7Point1,
      SoundIoChannelLayoutId7Point1Wide,
      SoundIoChannelLayoutId7Point1WideBack,
      SoundIoChannelLayoutIdOctagonal);
   pragma Convention (C, SoundIoChannelLayoutId);  -- soundio.h:189

   type SoundIoBackend is 
     (SoundIoBackendNone,
      SoundIoBackendJack,
      SoundIoBackendPulseAudio,
      SoundIoBackendAlsa,
      SoundIoBackendCoreAudio,
      SoundIoBackendWasapi,
      SoundIoBackendDummy);
   pragma Convention (C, SoundIoBackend);  -- soundio.h:218

   type SoundIoDeviceAim is 
     (SoundIoDeviceAimInput,
      SoundIoDeviceAimOutput);
   pragma Convention (C, SoundIoDeviceAim);  -- soundio.h:228

  --/< capture / recording
  --/< playback
  --/ For your convenience, Native Endian and Foreign Endian constants are defined
  --/ which point to the respective SoundIoFormat values.
   type SoundIoFormat is 
     (SoundIoFormatInvalid,
      SoundIoFormatS8,
      SoundIoFormatU8,
      SoundIoFormatS16LE,
      SoundIoFormatS16BE,
      SoundIoFormatU16LE,
      SoundIoFormatU16BE,
      SoundIoFormatS24LE,
      SoundIoFormatS24BE,
      SoundIoFormatU24LE,
      SoundIoFormatU24BE,
      SoundIoFormatS32LE,
      SoundIoFormatS32BE,
      SoundIoFormatU32LE,
      SoundIoFormatU32BE,
      SoundIoFormatFloat32LE,
      SoundIoFormatFloat32BE,
      SoundIoFormatFloat64LE,
      SoundIoFormatFloat64BE);
   pragma Convention (C, SoundIoFormat);  -- soundio.h:235

  --/< Signed 8 bit
  --/< Unsigned 8 bit
  --/< Signed 16 bit Little Endian
  --/< Signed 16 bit Big Endian
  --/< Unsigned 16 bit Little Endian
  --/< Unsigned 16 bit Little Endian
  --/< Signed 24 bit Little Endian using low three bytes in 32-bit word
  --/< Signed 24 bit Big Endian using low three bytes in 32-bit word
  --/< Unsigned 24 bit Little Endian using low three bytes in 32-bit word
  --/< Unsigned 24 bit Big Endian using low three bytes in 32-bit word
  --/< Signed 32 bit Little Endian
  --/< Signed 32 bit Big Endian
  --/< Unsigned 32 bit Little Endian
  --/< Unsigned 32 bit Big Endian
  --/< Float 32 bit Little Endian, Range -1.0 to 1.0
  --/< Float 32 bit Big Endian, Range -1.0 to 1.0
  --/< Float 64 bit Little Endian, Range -1.0 to 1.0
  --/< Float 64 bit Big Endian, Range -1.0 to 1.0
  --/ The size of this struct is OK to use.
   type SoundIoChannelLayout_channels_array is array (0 .. 23) of aliased SoundIoChannelId;
   type SoundIoChannelLayout is record
      name : Interfaces.C.Strings.chars_ptr;  -- soundio.h:303
      channel_count : aliased int;  -- soundio.h:304
      channels : aliased SoundIoChannelLayout_channels_array;  -- soundio.h:305
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoChannelLayout);  -- soundio.h:302

  --/ The size of this struct is OK to use.
   type SoundIoSampleRateRange is record
      min : aliased int;  -- soundio.h:310
      max : aliased int;  -- soundio.h:311
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoSampleRateRange);  -- soundio.h:309

  --/ The size of this struct is OK to use.
  --/ Base address of buffer.
   type SoundIoChannelArea is record
      ptr : Interfaces.C.Strings.chars_ptr;  -- soundio.h:317
      step : aliased int;  -- soundio.h:320
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoChannelArea);  -- soundio.h:315

  --/ How many bytes it takes to get from the beginning of one sample to
  --/ the beginning of the next sample.
  --/ The size of this struct is not part of the API or ABI.
  --/ Optional. Put whatever you want here. Defaults to NULL.
   type SoundIo;
   type SoundIo is record
      userdata : System.Address;  -- soundio.h:326
      on_devices_change : access procedure (arg1 : access SoundIo);  -- soundio.h:329
      on_backend_disconnect : access procedure (arg1 : access SoundIo; arg2 : int);  -- soundio.h:346
      on_events_signal : access procedure (arg1 : access SoundIo);  -- soundio.h:350
      current_backend : aliased SoundIoBackend;  -- soundio.h:354
      app_name : Interfaces.C.Strings.chars_ptr;  -- soundio.h:360
      emit_rtprio_warning : access procedure;  -- soundio.h:368
      jack_info_callback : access procedure (arg1 : Interfaces.C.Strings.chars_ptr);  -- soundio.h:376
      jack_error_callback : access procedure (arg1 : Interfaces.C.Strings.chars_ptr);  -- soundio.h:379
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIo);  -- soundio.h:324

  --/ Optional callback. Called when the list of devices change. Only called
  --/ during a call to ::soundio_flush_events or ::soundio_wait_events.
  --/ Optional callback. Called when the backend disconnects. For example,
  --/ when the JACK server shuts down. When this happens, listing devices
  --/ and opening streams will always fail with
  --/ SoundIoErrorBackendDisconnected. This callback is only called during a
  --/ call to ::soundio_flush_events or ::soundio_wait_events.
  --/ If you do not supply a callback, the default will crash your program
  --/ with an error message. This callback is also called when the thread
  --/ that retrieves device information runs into an unrecoverable condition
  --/ such as running out of memory.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorOpeningDevice - unexpected problem accessing device
  --/   information
  --/ Optional callback. Called from an unknown thread that you should not use
  --/ to call any soundio functions. You may use this to signal a condition
  --/ variable to wake up. Called when ::soundio_wait_events would be woken up.
  --/ Read-only. After calling ::soundio_connect or ::soundio_connect_backend,
  --/ this field tells which backend is currently connected.
  --/ Optional: Application name.
  --/ PulseAudio uses this for "application name".
  --/ JACK uses this for `client_name`.
  --/ Must not contain a colon (":").
  --/ Optional: Real time priority warning.
  --/ This callback is fired when making thread real-time priority failed. By
  --/ default, it will print to stderr only the first time it is called
  --/ a message instructing the user how to configure their system to allow
  --/ real-time priority threads. This must be set to a function not NULL.
  --/ To silence the warning, assign this to a function that does nothing.
  --/ Optional: JACK info callback.
  --/ By default, libsoundio sets this to an empty function in order to
  --/ silence stdio messages from JACK. You may override the behavior by
  --/ setting this to `NULL` or providing your own function. This is
  --/ registered with JACK regardless of whether ::soundio_connect_backend
  --/ succeeds.
  --/ Optional: JACK error callback.
  --/ See SoundIo::jack_info_callback
  --/ The size of this struct is not part of the API or ABI.
  --/ Read-only. Set automatically.
   type SoundIoDevice is record
      the_soundio : access SoundIo;  -- soundio.h:385
      id : Interfaces.C.Strings.chars_ptr;  -- soundio.h:395
      name : Interfaces.C.Strings.chars_ptr;  -- soundio.h:397
      aim : aliased SoundIoDeviceAim;  -- soundio.h:400
      layouts : access SoundIoChannelLayout;  -- soundio.h:407
      layout_count : aliased int;  -- soundio.h:408
      current_layout : aliased SoundIoChannelLayout;  -- soundio.h:410
      formats : access SoundIoFormat;  -- soundio.h:414
      format_count : aliased int;  -- soundio.h:416
      current_format : aliased SoundIoFormat;  -- soundio.h:435
      sample_rates : access SoundIoSampleRateRange;  -- soundio.h:443
      sample_rate_count : aliased int;  -- soundio.h:447
      sample_rate_current : aliased int;  -- soundio.h:450
      software_latency_min : aliased double;  -- soundio.h:456
      software_latency_max : aliased double;  -- soundio.h:461
      software_latency_current : aliased double;  -- soundio.h:467
      is_raw : aliased Extensions.bool;  -- soundio.h:474
      ref_count : aliased int;  -- soundio.h:478
      probe_error : aliased int;  -- soundio.h:489
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoDevice);  -- soundio.h:383

  --/ A string of bytes that uniquely identifies this device.
  --/ If the same physical device supports both input and output, that makes
  --/ one SoundIoDevice for the input and one SoundIoDevice for the output.
  --/ In this case, the id of each SoundIoDevice will be the same, and
  --/ SoundIoDevice::aim will be different. Additionally, if the device
  --/ supports raw mode, there may be up to four devices with the same id:
  --/ one for each value of SoundIoDevice::is_raw and one for each value of
  --/ SoundIoDevice::aim.
  --/ User-friendly UTF-8 encoded text to describe the device.
  --/ Tells whether this device is an input device or an output device.
  --/ Channel layouts are handled similarly to SoundIoDevice::formats.
  --/ If this information is missing due to a SoundIoDevice::probe_error,
  --/ layouts will be NULL. It's OK to modify this data, for example calling
  --/ ::soundio_sort_channel_layouts on it.
  --/ Devices are guaranteed to have at least 1 channel layout.
  --/ See SoundIoDevice::current_format
  --/ List of formats this device supports. See also
  --/ SoundIoDevice::current_format.
  --/ How many formats are available in SoundIoDevice::formats.
  --/ A device is either a raw device or it is a virtual device that is
  --/ provided by a software mixing service such as dmix or PulseAudio (see
  --/ SoundIoDevice::is_raw). If it is a raw device,
  --/ current_format is meaningless;
  --/ the device has no current format until you open it. On the other hand,
  --/ if it is a virtual device, current_format describes the
  --/ destination sample format that your audio will be converted to. Or,
  --/ if you're the lucky first application to open the device, you might
  --/ cause the current_format to change to your format.
  --/ Generally, you want to ignore current_format and use
  --/ whatever format is most convenient
  --/ for you which is supported by the device, because when you are the only
  --/ application left, the mixer might decide to switch
  --/ current_format to yours. You can learn the supported formats via
  --/ formats and SoundIoDevice::format_count. If this information is missing
  --/ due to a probe error, formats will be `NULL`. If current_format is
  --/ unavailable, it will be set to #SoundIoFormatInvalid.
  --/ Devices are guaranteed to have at least 1 format available.
  --/ Sample rate is the number of frames per second.
  --/ Sample rate is handled very similar to SoundIoDevice::formats.
  --/ If sample rate information is missing due to a probe error, the field
  --/ will be set to NULL.
  --/ Devices which have SoundIoDevice::probe_error set to #SoundIoErrorNone are
  --/ guaranteed to have at least 1 sample rate available.
  --/ How many sample rate ranges are available in
  --/ SoundIoDevice::sample_rates. 0 if sample rate information is missing
  --/ due to a probe error.
  --/ See SoundIoDevice::current_format
  --/ 0 if sample rate information is missing due to a probe error.
  --/ Software latency minimum in seconds. If this value is unknown or
  --/ irrelevant, it is set to 0.0.
  --/ For PulseAudio and WASAPI this value is unknown until you open a
  --/ stream.
  --/ Software latency maximum in seconds. If this value is unknown or
  --/ irrelevant, it is set to 0.0.
  --/ For PulseAudio and WASAPI this value is unknown until you open a
  --/ stream.
  --/ Software latency in seconds. If this value is unknown or
  --/ irrelevant, it is set to 0.0.
  --/ For PulseAudio and WASAPI this value is unknown until you open a
  --/ stream.
  --/ See SoundIoDevice::current_format
  --/ Raw means that you are directly opening the hardware device and not
  --/ going through a proxy such as dmix, PulseAudio, or JACK. When you open a
  --/ raw device, other applications on the computer are not able to
  --/ simultaneously access the device. Raw devices do not perform automatic
  --/ resampling and thus tend to have fewer formats available.
  --/ Devices are reference counted. See ::soundio_device_ref and
  --/ ::soundio_device_unref.
  --/ This is set to a SoundIoError representing the result of the device
  --/ probe. Ideally this will be SoundIoErrorNone in which case all the
  --/ fields of the device will be populated. If there is an error code here
  --/ then information about formats, sample rates, and channel layouts might
  --/ be missing.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorOpeningDevice
  --/ * #SoundIoErrorNoMem
  --/ The size of this struct is not part of the API or ABI.
  --/ Populated automatically when you call ::soundio_outstream_create.
   type SoundIoOutStream;
   type SoundIoOutStream is record
      device : access SoundIoDevice;  -- soundio.h:495
      format : aliased SoundIoFormat;  -- soundio.h:499
      sample_rate : aliased int;  -- soundio.h:503
      layout : aliased SoundIoChannelLayout;  -- soundio.h:507
      software_latency : aliased double;  -- soundio.h:534
      userdata : System.Address;  -- soundio.h:537
      write_callback : access procedure
           (arg1 : access SoundIoOutStream;
            arg2 : int;
            arg3 : int);  -- soundio.h:555
      underflow_callback : access procedure (arg1 : access SoundIoOutStream);  -- soundio.h:560
      error_callback : access procedure (arg1 : access SoundIoOutStream; arg2 : int);  -- soundio.h:567
      name : Interfaces.C.Strings.chars_ptr;  -- soundio.h:575
      non_terminal_hint : aliased Extensions.bool;  -- soundio.h:580
      bytes_per_frame : aliased int;  -- soundio.h:584
      bytes_per_sample : aliased int;  -- soundio.h:586
      layout_error : aliased int;  -- soundio.h:591
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoOutStream);  -- soundio.h:493

  --/ Defaults to #SoundIoFormatFloat32NE, followed by the first one
  --/ supported.
  --/ Sample rate is the number of frames per second.
  --/ Defaults to 48000 (and then clamped into range).
  --/ Defaults to Stereo, if available, followed by the first layout
  --/ supported.
  --/ Ignoring hardware latency, this is the number of seconds it takes for
  --/ the last sample in a full buffer to be played.
  --/ After you call ::soundio_outstream_open, this value is replaced with the
  --/ actual software latency, as near to this value as possible.
  --/ On systems that support clearing the buffer, this defaults to a large
  --/ latency, potentially upwards of 2 seconds, with the understanding that
  --/ you will call ::soundio_outstream_clear_buffer when you want to reduce
  --/ the latency to 0. On systems that do not support clearing the buffer,
  --/ this defaults to a reasonable lower latency value.
  --/
  --/ On backends with high latencies (such as 2 seconds), `frame_count_min`
  --/ will be 0, meaning you don't have to fill the entire buffer. In this
  --/ case, the large buffer is there if you want it; you only have to fill
  --/ as much as you want. On backends like JACK, `frame_count_min` will be
  --/ equal to `frame_count_max` and if you don't fill that many frames, you
  --/ will get glitches.
  --/
  --/ If the device has unknown software latency min and max values, you may
  --/ still set this, but you might not get the value you requested.
  --/ For PulseAudio, if you set this value to non-default, it sets
  --/ `PA_STREAM_ADJUST_LATENCY` and is the value used for `maxlength` and
  --/ `tlength`.
  --/
  --/ For JACK, this value is always equal to
  --/ SoundIoDevice::software_latency_current of the device.
  --/ Defaults to NULL. Put whatever you want here.
  --/ In this callback, you call ::soundio_outstream_begin_write and
  --/ ::soundio_outstream_end_write as many times as necessary to write
  --/ at minimum `frame_count_min` frames and at maximum `frame_count_max`
  --/ frames. `frame_count_max` will always be greater than 0. Note that you
  --/ should write as many frames as you can; `frame_count_min` might be 0 and
  --/ you can still get a buffer underflow if you always write
  --/ `frame_count_min` frames.
  --/
  --/ For Dummy, ALSA, and PulseAudio, `frame_count_min` will be 0. For JACK
  --/ and CoreAudio `frame_count_min` will be equal to `frame_count_max`.
  --/
  --/ The code in the supplied function must be suitable for real-time
  --/ execution. That means that it cannot call functions that might block
  --/ for a long time. This includes all I/O functions (disk, TTY, network),
  --/ malloc, free, printf, pthread_mutex_lock, sleep, wait, poll, select,
  --/ pthread_join, pthread_cond_wait, etc.
  --/ This optional callback happens when the sound device runs out of
  --/ buffered audio data to play. After this occurs, the outstream waits
  --/ until the buffer is full to resume playback.
  --/ This is called from the SoundIoOutStream::write_callback thread context.
  --/ Optional callback. `err` is always SoundIoErrorStreaming.
  --/ SoundIoErrorStreaming is an unrecoverable error. The stream is in an
  --/ invalid state and must be destroyed.
  --/ If you do not supply error_callback, the default callback will print
  --/ a message to stderr and then call `abort`.
  --/ This is called from the SoundIoOutStream::write_callback thread context.
  --/ Optional: Name of the stream. Defaults to "SoundIoOutStream"
  --/ PulseAudio uses this for the stream name.
  --/ JACK uses this for the client name of the client that connects when you
  --/ open the stream.
  --/ WASAPI uses this for the session display name.
  --/ Must not contain a colon (":").
  --/ Optional: Hint that this output stream is nonterminal. This is used by
  --/ JACK and it means that the output stream data originates from an input
  --/ stream. Defaults to `false`.
  --/ computed automatically when you call ::soundio_outstream_open
  --/ computed automatically when you call ::soundio_outstream_open
  --/ If setting the channel layout fails for some reason, this field is set
  --/ to an error code. Possible error codes are:
  --/ * #SoundIoErrorIncompatibleDevice
  --/ The size of this struct is not part of the API or ABI.
  --/ Populated automatically when you call ::soundio_outstream_create.
   type SoundIoInStream;
   type SoundIoInStream is record
      device : access SoundIoDevice;  -- soundio.h:597
      format : aliased SoundIoFormat;  -- soundio.h:601
      sample_rate : aliased int;  -- soundio.h:605
      layout : aliased SoundIoChannelLayout;  -- soundio.h:609
      software_latency : aliased double;  -- soundio.h:623
      userdata : System.Address;  -- soundio.h:626
      read_callback : access procedure
           (arg1 : access SoundIoInStream;
            arg2 : int;
            arg3 : int);  -- soundio.h:639
      overflow_callback : access procedure (arg1 : access SoundIoInStream);  -- soundio.h:644
      error_callback : access procedure (arg1 : access SoundIoInStream; arg2 : int);  -- soundio.h:651
      name : Interfaces.C.Strings.chars_ptr;  -- soundio.h:659
      non_terminal_hint : aliased Extensions.bool;  -- soundio.h:664
      bytes_per_frame : aliased int;  -- soundio.h:667
      bytes_per_sample : aliased int;  -- soundio.h:669
      layout_error : aliased int;  -- soundio.h:673
   end record;
   pragma Convention (C_Pass_By_Copy, SoundIoInStream);  -- soundio.h:595

  --/ Defaults to #SoundIoFormatFloat32NE, followed by the first one
  --/ supported.
  --/ Sample rate is the number of frames per second.
  --/ Defaults to max(sample_rate_min, min(sample_rate_max, 48000))
  --/ Defaults to Stereo, if available, followed by the first layout
  --/ supported.
  --/ Ignoring hardware latency, this is the number of seconds it takes for a
  --/ captured sample to become available for reading.
  --/ After you call ::soundio_instream_open, this value is replaced with the
  --/ actual software latency, as near to this value as possible.
  --/ A higher value means less CPU usage. Defaults to a large value,
  --/ potentially upwards of 2 seconds.
  --/ If the device has unknown software latency min and max values, you may
  --/ still set this, but you might not get the value you requested.
  --/ For PulseAudio, if you set this value to non-default, it sets
  --/ `PA_STREAM_ADJUST_LATENCY` and is the value used for `fragsize`.
  --/ For JACK, this value is always equal to
  --/ SoundIoDevice::software_latency_current
  --/ Defaults to NULL. Put whatever you want here.
  --/ In this function call ::soundio_instream_begin_read and
  --/ ::soundio_instream_end_read as many times as necessary to read at
  --/ minimum `frame_count_min` frames and at maximum `frame_count_max`
  --/ frames. If you return from read_callback without having read
  --/ `frame_count_min`, the frames will be dropped. `frame_count_max` is how
  --/ many frames are available to read.
  --/
  --/ The code in the supplied function must be suitable for real-time
  --/ execution. That means that it cannot call functions that might block
  --/ for a long time. This includes all I/O functions (disk, TTY, network),
  --/ malloc, free, printf, pthread_mutex_lock, sleep, wait, poll, select,
  --/ pthread_join, pthread_cond_wait, etc.
  --/ This optional callback happens when the sound device buffer is full,
  --/ yet there is more captured audio to put in it.
  --/ This is never fired for PulseAudio.
  --/ This is called from the SoundIoInStream::read_callback thread context.
  --/ Optional callback. `err` is always SoundIoErrorStreaming.
  --/ SoundIoErrorStreaming is an unrecoverable error. The stream is in an
  --/ invalid state and must be destroyed.
  --/ If you do not supply `error_callback`, the default callback will print
  --/ a message to stderr and then abort().
  --/ This is called from the SoundIoInStream::read_callback thread context.
  --/ Optional: Name of the stream. Defaults to "SoundIoInStream";
  --/ PulseAudio uses this for the stream name.
  --/ JACK uses this for the client name of the client that connects when you
  --/ open the stream.
  --/ WASAPI uses this for the session display name.
  --/ Must not contain a colon (":").
  --/ Optional: Hint that this input stream is nonterminal. This is used by
  --/ JACK and it means that the data received by the stream will be
  --/ passed on or made available to another stream. Defaults to `false`.
  --/ computed automatically when you call ::soundio_instream_open
  --/ computed automatically when you call ::soundio_instream_open
  --/ If setting the channel layout fails for some reason, this field is set
  --/ to an error code. Possible error codes are: #SoundIoErrorIncompatibleDevice
  --/ See also ::soundio_version_major, ::soundio_version_minor, ::soundio_version_patch
   function soundio_version_string return Interfaces.C.Strings.chars_ptr;  -- soundio.h:677
   pragma Import (C, soundio_version_string, "soundio_version_string");

  --/ See also ::soundio_version_string, ::soundio_version_minor, ::soundio_version_patch
   function soundio_version_major return int;  -- soundio.h:679
   pragma Import (C, soundio_version_major, "soundio_version_major");

  --/ See also ::soundio_version_major, ::soundio_version_string, ::soundio_version_patch
   function soundio_version_minor return int;  -- soundio.h:681
   pragma Import (C, soundio_version_minor, "soundio_version_minor");

  --/ See also ::soundio_version_major, ::soundio_version_minor, ::soundio_version_string
   function soundio_version_patch return int;  -- soundio.h:683
   pragma Import (C, soundio_version_patch, "soundio_version_patch");

  --/ Create a SoundIo context. You may create multiple instances of this to
  --/ connect to multiple backends. Sets all fields to defaults.
  --/ Returns `NULL` if and only if memory could not be allocated.
  --/ See also ::soundio_destroy
   function soundio_create return access SoundIo;  -- soundio.h:689
   pragma Import (C, soundio_create, "soundio_create");

   procedure soundio_destroy (the_soundio : access SoundIo);  -- soundio.h:690
   pragma Import (C, soundio_destroy, "soundio_destroy");

  --/ Tries ::soundio_connect_backend on all available backends in order.
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid - already connected
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorNoSuchClient - when JACK returns `JackNoSuchClient`
  --/ See also ::soundio_disconnect
   function soundio_connect (the_soundio : access SoundIo) return int;  -- soundio.h:700
   pragma Import (C, soundio_connect, "soundio_connect");

  --/ Instead of calling ::soundio_connect you may call this function to try a
  --/ specific backend.
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid - already connected or invalid backend parameter
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorBackendUnavailable - backend was not compiled in
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorNoSuchClient - when JACK returns `JackNoSuchClient`
  --/ * #SoundIoErrorInitAudioBackend - requested `backend` is not active
  --/ * #SoundIoErrorBackendDisconnected - backend disconnected while connecting
  --/ See also ::soundio_disconnect
   function soundio_connect_backend (the_soundio : access SoundIo; backend : SoundIoBackend) return int;  -- soundio.h:712
   pragma Import (C, soundio_connect_backend, "soundio_connect_backend");

   procedure soundio_disconnect (the_soundio : access SoundIo);  -- soundio.h:713
   pragma Import (C, soundio_disconnect, "soundio_disconnect");

  --/ Get a string representation of a #SoundIoError
   function soundio_strerror (error : int) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:716
   pragma Import (C, soundio_strerror, "soundio_strerror");

  --/ Get a string representation of a #SoundIoBackend
   function soundio_backend_name (backend : SoundIoBackend) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:718
   pragma Import (C, soundio_backend_name, "soundio_backend_name");

  --/ Returns the number of available backends.
   function soundio_backend_count (the_soundio : access SoundIo) return int;  -- soundio.h:721
   pragma Import (C, soundio_backend_count, "soundio_backend_count");

  --/ get the available backend at the specified index
  --/ (0 <= index < ::soundio_backend_count)
   function soundio_get_backend (the_soundio : access SoundIo; index : int) return SoundIoBackend;  -- soundio.h:724
   pragma Import (C, soundio_get_backend, "soundio_get_backend");

  --/ Returns whether libsoundio was compiled with backend.
   function soundio_have_backend (backend : SoundIoBackend) return Extensions.bool;  -- soundio.h:727
   pragma Import (C, soundio_have_backend, "soundio_have_backend");

  --/ Atomically update information for all connected devices. Note that calling
  --/ this function merely flips a pointer; the actual work of collecting device
  --/ information is done elsewhere. It is performant to call this function many
  --/ times per second.
  --/
  --/ When you call this, the following callbacks might be called:
  --/ * SoundIo::on_devices_change
  --/ * SoundIo::on_backend_disconnect
  --/ This is the only time those callbacks can be called.
  --/
  --/ This must be called from the same thread as the thread in which you call
  --/ these functions:
  --/ * ::soundio_input_device_count
  --/ * ::soundio_output_device_count
  --/ * ::soundio_get_input_device
  --/ * ::soundio_get_output_device
  --/ * ::soundio_default_input_device_index
  --/ * ::soundio_default_output_device_index
  --/
  --/ Note that if you do not care about learning about updated devices, you
  --/ might call this function only once ever and never call
  --/ ::soundio_wait_events.
   procedure soundio_flush_events (the_soundio : access SoundIo);  -- soundio.h:751
   pragma Import (C, soundio_flush_events, "soundio_flush_events");

  --/ This function calls ::soundio_flush_events then blocks until another event
  --/ is ready or you call ::soundio_wakeup. Be ready for spurious wakeups.
   procedure soundio_wait_events (the_soundio : access SoundIo);  -- soundio.h:755
   pragma Import (C, soundio_wait_events, "soundio_wait_events");

  --/ Makes ::soundio_wait_events stop blocking.
   procedure soundio_wakeup (the_soundio : access SoundIo);  -- soundio.h:758
   pragma Import (C, soundio_wakeup, "soundio_wakeup");

  --/ If necessary you can manually trigger a device rescan. Normally you will
  --/ not ever have to call this function, as libsoundio listens to system events
  --/ for device changes and responds to them by rescanning devices and preparing
  --/ the new device information for you to be atomically replaced when you call
  --/ ::soundio_flush_events. However you might run into cases where you want to
  --/ force trigger a device rescan, for example if an ALSA device has a
  --/ SoundIoDevice::probe_error.
  --/
  --/ After you call this you still have to use ::soundio_flush_events or
  --/ ::soundio_wait_events and then wait for the
  --/ SoundIo::on_devices_change callback.
  --/
  --/ This can be called from any thread context except for
  --/ SoundIoOutStream::write_callback and SoundIoInStream::read_callback
   procedure soundio_force_device_scan (the_soundio : access SoundIo);  -- soundio.h:775
   pragma Import (C, soundio_force_device_scan, "soundio_force_device_scan");

  -- Channel Layouts
  --/ Returns whether the channel count field and each channel id matches in
  --/ the supplied channel layouts.
   function soundio_channel_layout_equal (a : access constant SoundIoChannelLayout; b : access constant SoundIoChannelLayout) return Extensions.bool;  -- soundio.h:782
   pragma Import (C, soundio_channel_layout_equal, "soundio_channel_layout_equal");

   function soundio_get_channel_name (id : SoundIoChannelId) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:786
   pragma Import (C, soundio_get_channel_name, "soundio_get_channel_name");

  --/ Given UTF-8 encoded text which is the name of a channel such as
  --/ "Front Left", "FL", or "front-left", return the corresponding
  --/ SoundIoChannelId. Returns SoundIoChannelIdInvalid for no match.
   function soundio_parse_channel_id (str : Interfaces.C.Strings.chars_ptr; str_len : int) return SoundIoChannelId;  -- soundio.h:790
   pragma Import (C, soundio_parse_channel_id, "soundio_parse_channel_id");

  --/ Returns the number of builtin channel layouts.
   function soundio_channel_layout_builtin_count return int;  -- soundio.h:793
   pragma Import (C, soundio_channel_layout_builtin_count, "soundio_channel_layout_builtin_count");

  --/ Returns a builtin channel layout. 0 <= `index` < ::soundio_channel_layout_builtin_count
  --/
  --/ Although `index` is of type `int`, it should be a valid
  --/ #SoundIoChannelLayoutId enum value.
   function soundio_channel_layout_get_builtin (index : int) return access constant SoundIoChannelLayout;  -- soundio.h:798
   pragma Import (C, soundio_channel_layout_get_builtin, "soundio_channel_layout_get_builtin");

  --/ Get the default builtin channel layout for the given number of channels.
   function soundio_channel_layout_get_default (channel_count : int) return access constant SoundIoChannelLayout;  -- soundio.h:801
   pragma Import (C, soundio_channel_layout_get_default, "soundio_channel_layout_get_default");

  --/ Return the index of `channel` in `layout`, or `-1` if not found.
   function soundio_channel_layout_find_channel (layout : access constant SoundIoChannelLayout; channel : SoundIoChannelId) return int;  -- soundio.h:804
   pragma Import (C, soundio_channel_layout_find_channel, "soundio_channel_layout_find_channel");

  --/ Populates the name field of layout if it matches a builtin one.
  --/ returns whether it found a match
   function soundio_channel_layout_detect_builtin (layout : access SoundIoChannelLayout) return Extensions.bool;  -- soundio.h:809
   pragma Import (C, soundio_channel_layout_detect_builtin, "soundio_channel_layout_detect_builtin");

  --/ Iterates over preferred_layouts. Returns the first channel layout in
  --/ preferred_layouts which matches one of the channel layouts in
  --/ available_layouts. Returns NULL if none matches.
   function soundio_best_matching_channel_layout
     (preferred_layouts : access constant SoundIoChannelLayout;
      preferred_layout_count : int;
      available_layouts : access constant SoundIoChannelLayout;
      available_layout_count : int) return access constant SoundIoChannelLayout;  -- soundio.h:814
   pragma Import (C, soundio_best_matching_channel_layout, "soundio_best_matching_channel_layout");

  --/ Sorts by channel count, descending.
   procedure soundio_sort_channel_layouts (layouts : access SoundIoChannelLayout; layout_count : int);  -- soundio.h:819
   pragma Import (C, soundio_sort_channel_layouts, "soundio_sort_channel_layouts");

  -- Sample Formats
  --/ Returns -1 on invalid format.
   function soundio_get_bytes_per_sample (format : SoundIoFormat) return int;  -- soundio.h:825
   pragma Import (C, soundio_get_bytes_per_sample, "soundio_get_bytes_per_sample");

  --/ A frame is one sample per channel.
   function soundio_get_bytes_per_frame (format : SoundIoFormat; channel_count : int) return int;  -- soundio.h:828
   pragma Import (CPP, soundio_get_bytes_per_frame, "_ZL27soundio_get_bytes_per_frame13SoundIoFormati");

  --/ Sample rate is the number of frames per second.
   function soundio_get_bytes_per_second
     (format : SoundIoFormat;
      channel_count : int;
      sample_rate : int) return int;  -- soundio.h:833
   pragma Import (CPP, soundio_get_bytes_per_second, "_ZL28soundio_get_bytes_per_second13SoundIoFormatii");

  --/ Returns string representation of `format`.
   function soundio_format_string (format : SoundIoFormat) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:840
   pragma Import (C, soundio_format_string, "soundio_format_string");

  -- Devices
  --/ When you call ::soundio_flush_events, a snapshot of all device state is
  --/ saved and these functions merely access the snapshot data. When you want
  --/ to check for new devices, call ::soundio_flush_events. Or you can call
  --/ ::soundio_wait_events to block until devices change. If an error occurs
  --/ scanning devices in a background thread, SoundIo::on_backend_disconnect is called
  --/ with the error code.
  --/ Get the number of input devices.
  --/ Returns -1 if you never called ::soundio_flush_events.
   function soundio_input_device_count (the_soundio : access SoundIo) return int;  -- soundio.h:856
   pragma Import (C, soundio_input_device_count, "soundio_input_device_count");

  --/ Get the number of output devices.
  --/ Returns -1 if you never called ::soundio_flush_events.
   function soundio_output_device_count (the_soundio : access SoundIo) return int;  -- soundio.h:859
   pragma Import (C, soundio_output_device_count, "soundio_output_device_count");

  --/ Always returns a device. Call ::soundio_device_unref when done.
  --/ `index` must be 0 <= index < ::soundio_input_device_count
  --/ Returns NULL if you never called ::soundio_flush_events or if you provide
  --/ invalid parameter values.
   function soundio_get_input_device (the_soundio : access SoundIo; index : int) return access SoundIoDevice;  -- soundio.h:865
   pragma Import (C, soundio_get_input_device, "soundio_get_input_device");

  --/ Always returns a device. Call ::soundio_device_unref when done.
  --/ `index` must be 0 <= index < ::soundio_output_device_count
  --/ Returns NULL if you never called ::soundio_flush_events or if you provide
  --/ invalid parameter values.
   function soundio_get_output_device (the_soundio : access SoundIo; index : int) return access SoundIoDevice;  -- soundio.h:870
   pragma Import (C, soundio_get_output_device, "soundio_get_output_device");

  --/ returns the index of the default input device
  --/ returns -1 if there are no devices or if you never called
  --/ ::soundio_flush_events.
   function soundio_default_input_device_index (the_soundio : access SoundIo) return int;  -- soundio.h:875
   pragma Import (C, soundio_default_input_device_index, "soundio_default_input_device_index");

  --/ returns the index of the default output device
  --/ returns -1 if there are no devices or if you never called
  --/ ::soundio_flush_events.
   function soundio_default_output_device_index (the_soundio : access SoundIo) return int;  -- soundio.h:880
   pragma Import (C, soundio_default_output_device_index, "soundio_default_output_device_index");

  --/ Add 1 to the reference count of `device`.
   procedure soundio_device_ref (device : access SoundIoDevice);  -- soundio.h:883
   pragma Import (C, soundio_device_ref, "soundio_device_ref");

  --/ Remove 1 to the reference count of `device`. Clean up if it was the last
  --/ reference.
   procedure soundio_device_unref (device : access SoundIoDevice);  -- soundio.h:886
   pragma Import (C, soundio_device_unref, "soundio_device_unref");

  --/ Return `true` if and only if the devices have the same SoundIoDevice::id,
  --/ SoundIoDevice::is_raw, and SoundIoDevice::aim are the same.
   function soundio_device_equal (a : access constant SoundIoDevice; b : access constant SoundIoDevice) return Extensions.bool;  -- soundio.h:890
   pragma Import (C, soundio_device_equal, "soundio_device_equal");

  --/ Sorts channel layouts by channel count, descending.
   procedure soundio_device_sort_channel_layouts (device : access SoundIoDevice);  -- soundio.h:895
   pragma Import (C, soundio_device_sort_channel_layouts, "soundio_device_sort_channel_layouts");

  --/ Convenience function. Returns whether `format` is included in the device's
  --/ supported formats.
   function soundio_device_supports_format (device : access SoundIoDevice; format : SoundIoFormat) return Extensions.bool;  -- soundio.h:899
   pragma Import (C, soundio_device_supports_format, "soundio_device_supports_format");

  --/ Convenience function. Returns whether `layout` is included in the device's
  --/ supported channel layouts.
   function soundio_device_supports_layout (device : access SoundIoDevice; layout : access constant SoundIoChannelLayout) return Extensions.bool;  -- soundio.h:904
   pragma Import (C, soundio_device_supports_layout, "soundio_device_supports_layout");

  --/ Convenience function. Returns whether `sample_rate` is included in the
  --/ device's supported sample rates.
   function soundio_device_supports_sample_rate (device : access SoundIoDevice; sample_rate : int) return Extensions.bool;  -- soundio.h:909
   pragma Import (C, soundio_device_supports_sample_rate, "soundio_device_supports_sample_rate");

  --/ Convenience function. Returns the available sample rate nearest to
  --/ `sample_rate`, rounding up.
   function soundio_device_nearest_sample_rate (device : access SoundIoDevice; sample_rate : int) return int;  -- soundio.h:914
   pragma Import (C, soundio_device_nearest_sample_rate, "soundio_device_nearest_sample_rate");

  -- Output Streams
  --/ Allocates memory and sets defaults. Next you should fill out the struct fields
  --/ and then call ::soundio_outstream_open. Sets all fields to defaults.
  --/ Returns `NULL` if and only if memory could not be allocated.
  --/ See also ::soundio_outstream_destroy
   function soundio_outstream_create (device : access SoundIoDevice) return access SoundIoOutStream;  -- soundio.h:924
   pragma Import (C, soundio_outstream_create, "soundio_outstream_create");

  --/ You may not call this function from the SoundIoOutStream::write_callback thread context.
   procedure soundio_outstream_destroy (outstream : access SoundIoOutStream);  -- soundio.h:926
   pragma Import (C, soundio_outstream_destroy, "soundio_outstream_destroy");

  --/ After you call this function, SoundIoOutStream::software_latency is set to
  --/ the correct value.
  --/
  --/ The next thing to do is call ::soundio_instream_start.
  --/ If this function returns an error, the outstream is in an invalid state and
  --/ you must call ::soundio_outstream_destroy on it.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid
  --/   * SoundIoDevice::aim is not #SoundIoDeviceAimOutput
  --/   * SoundIoOutStream::format is not valid
  --/   * SoundIoOutStream::channel_count is greater than #SOUNDIO_MAX_CHANNELS
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorOpeningDevice
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorNoSuchClient - when JACK returns `JackNoSuchClient`
  --/ * #SoundIoErrorOpeningDevice
  --/ * #SoundIoErrorIncompatibleBackend - SoundIoOutStream::channel_count is
  --/   greater than the number of channels the backend can handle.
  --/ * #SoundIoErrorIncompatibleDevice - stream parameters requested are not
  --/   compatible with the chosen device.
   function soundio_outstream_open (outstream : access SoundIoOutStream) return int;  -- soundio.h:950
   pragma Import (C, soundio_outstream_open, "soundio_outstream_open");

  --/ After you call this function, SoundIoOutStream::write_callback will be called.
  --/
  --/ This function might directly call SoundIoOutStream::write_callback.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorBackendDisconnected
   function soundio_outstream_start (outstream : access SoundIoOutStream) return int;  -- soundio.h:961
   pragma Import (C, soundio_outstream_start, "soundio_outstream_start");

  --/ Call this function when you are ready to begin writing to the device buffer.
  --/  * `outstream` - (in) The output stream you want to write to.
  --/  * `areas` - (out) The memory addresses you can write data to, one per
  --/    channel. It is OK to modify the pointers if that helps you iterate.
  --/  * `frame_count` - (in/out) Provide the number of frames you want to write.
  --/    Returned will be the number of frames you can actually write, which is
  --/    also the number of frames that will be written when you call
  --/    ::soundio_outstream_end_write. The value returned will always be less
  --/    than or equal to the value provided.
  --/ It is your responsibility to call this function exactly as many times as
  --/ necessary to meet the `frame_count_min` and `frame_count_max` criteria from
  --/ SoundIoOutStream::write_callback.
  --/ You must call this function only from the SoundIoOutStream::write_callback thread context.
  --/ After calling this function, write data to `areas` and then call
  --/ ::soundio_outstream_end_write.
  --/ If this function returns an error, do not call ::soundio_outstream_end_write.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid
  --/   * `*frame_count` <= 0
  --/   * `*frame_count` < `frame_count_min` or `*frame_count` > `frame_count_max`
  --/   * function called too many times without respecting `frame_count_max`
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorUnderflow - an underflow caused this call to fail. You might
  --/   also get a SoundIoOutStream::underflow_callback, and you might not get
  --/   this error code when an underflow occurs. Unlike #SoundIoErrorStreaming,
  --/   the outstream is still in a valid state and streaming can continue.
  --/ * #SoundIoErrorIncompatibleDevice - in rare cases it might just now
  --/   be discovered that the device uses non-byte-aligned access, in which
  --/   case this error code is returned.
   function soundio_outstream_begin_write
     (outstream : access SoundIoOutStream;
      areas : System.Address;
      frame_count : access int) return int;  -- soundio.h:993
   pragma Import (C, soundio_outstream_begin_write, "soundio_outstream_begin_write");

  --/ Commits the write that you began with ::soundio_outstream_begin_write.
  --/ You must call this function only from the SoundIoOutStream::write_callback thread context.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorUnderflow - an underflow caused this call to fail. You might
  --/   also get a SoundIoOutStream::underflow_callback, and you might not get
  --/   this error code when an underflow occurs. Unlike #SoundIoErrorStreaming,
  --/   the outstream is still in a valid state and streaming can continue.
   function soundio_outstream_end_write (outstream : access SoundIoOutStream) return int;  -- soundio.h:1005
   pragma Import (C, soundio_outstream_end_write, "soundio_outstream_end_write");

  --/ Clears the output stream buffer.
  --/ This function can be called from any thread.
  --/ This function can be called regardless of whether the outstream is paused
  --/ or not.
  --/ Some backends do not support clearing the buffer. On these backends this
  --/ function will return SoundIoErrorIncompatibleBackend.
  --/ Some devices do not support clearing the buffer. On these devices this
  --/ function might return SoundIoErrorIncompatibleDevice.
  --/ Possible errors:
  --/
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorIncompatibleBackend
  --/ * #SoundIoErrorIncompatibleDevice
   function soundio_outstream_clear_buffer (outstream : access SoundIoOutStream) return int;  -- soundio.h:1020
   pragma Import (C, soundio_outstream_clear_buffer, "soundio_outstream_clear_buffer");

  --/ If the underlying backend and device support pausing, this pauses the
  --/ stream. SoundIoOutStream::write_callback may be called a few more times if
  --/ the buffer is not full.
  --/ Pausing might put the hardware into a low power state which is ideal if your
  --/ software is silent for some time.
  --/ This function may be called from any thread context, including
  --/ SoundIoOutStream::write_callback.
  --/ Pausing when already paused or unpausing when already unpaused has no
  --/ effect and returns #SoundIoErrorNone.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorIncompatibleDevice - device does not support
  --/   pausing/unpausing. This error code might not be returned even if the
  --/   device does not support pausing/unpausing.
  --/ * #SoundIoErrorIncompatibleBackend - backend does not support
  --/   pausing/unpausing.
  --/ * #SoundIoErrorInvalid - outstream not opened and started
   function soundio_outstream_pause (outstream : access SoundIoOutStream; pause : Extensions.bool) return int;  -- soundio.h:1041
   pragma Import (C, soundio_outstream_pause, "soundio_outstream_pause");

  --/ Obtain the total number of seconds that the next frame written after the
  --/ last frame written with ::soundio_outstream_end_write will take to become
  --/ audible. This includes both software and hardware latency. In other words,
  --/ if you call this function directly after calling ::soundio_outstream_end_write,
  --/ this gives you the number of seconds that the next frame written will take
  --/ to become audible.
  --/
  --/ This function must be called only from within SoundIoOutStream::write_callback.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorStreaming
   function soundio_outstream_get_latency (outstream : access SoundIoOutStream; out_latency : access double) return int;  -- soundio.h:1054
   pragma Import (C, soundio_outstream_get_latency, "soundio_outstream_get_latency");

  -- Input Streams
  --/ Allocates memory and sets defaults. Next you should fill out the struct fields
  --/ and then call ::soundio_instream_open. Sets all fields to defaults.
  --/ Returns `NULL` if and only if memory could not be allocated.
  --/ See also ::soundio_instream_destroy
   function soundio_instream_create (device : access SoundIoDevice) return access SoundIoInStream;  -- soundio.h:1064
   pragma Import (C, soundio_instream_create, "soundio_instream_create");

  --/ You may not call this function from SoundIoInStream::read_callback.
   procedure soundio_instream_destroy (instream : access SoundIoInStream);  -- soundio.h:1066
   pragma Import (C, soundio_instream_destroy, "soundio_instream_destroy");

  --/ After you call this function, SoundIoInStream::software_latency is set to the correct
  --/ value.
  --/ The next thing to do is call ::soundio_instream_start.
  --/ If this function returns an error, the instream is in an invalid state and
  --/ you must call ::soundio_instream_destroy on it.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid
  --/   * device aim is not #SoundIoDeviceAimInput
  --/   * format is not valid
  --/   * requested layout channel count > #SOUNDIO_MAX_CHANNELS
  --/ * #SoundIoErrorOpeningDevice
  --/ * #SoundIoErrorNoMem
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorSystemResources
  --/ * #SoundIoErrorNoSuchClient
  --/ * #SoundIoErrorIncompatibleBackend
  --/ * #SoundIoErrorIncompatibleDevice
   function soundio_instream_open (instream : access SoundIoInStream) return int;  -- soundio.h:1086
   pragma Import (C, soundio_instream_open, "soundio_instream_open");

  --/ After you call this function, SoundIoInStream::read_callback will be called.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorOpeningDevice
  --/ * #SoundIoErrorSystemResources
   function soundio_instream_start (instream : access SoundIoInStream) return int;  -- soundio.h:1095
   pragma Import (C, soundio_instream_start, "soundio_instream_start");

  --/ Call this function when you are ready to begin reading from the device
  --/ buffer.
  --/ * `instream` - (in) The input stream you want to read from.
  --/ * `areas` - (out) The memory addresses you can read data from. It is OK
  --/   to modify the pointers if that helps you iterate. There might be a "hole"
  --/   in the buffer. To indicate this, `areas` will be `NULL` and `frame_count`
  --/   tells how big the hole is in frames.
  --/ * `frame_count` - (in/out) - Provide the number of frames you want to read;
  --/   returns the number of frames you can actually read. The returned value
  --/   will always be less than or equal to the provided value. If the provided
  --/   value is less than `frame_count_min` from SoundIoInStream::read_callback this function
  --/   returns with #SoundIoErrorInvalid.
  --/ It is your responsibility to call this function no more and no fewer than the
  --/ correct number of times according to the `frame_count_min` and
  --/ `frame_count_max` criteria from SoundIoInStream::read_callback.
  --/ You must call this function only from the SoundIoInStream::read_callback thread context.
  --/ After calling this function, read data from `areas` and then use
  --/ ::soundio_instream_end_read` to actually remove the data from the buffer
  --/ and move the read index forward. ::soundio_instream_end_read should not be
  --/ called if the buffer is empty (`frame_count` == 0), but it should be called
  --/ if there is a hole.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorInvalid
  --/   * `*frame_count` < `frame_count_min` or `*frame_count` > `frame_count_max`
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorIncompatibleDevice - in rare cases it might just now
  --/   be discovered that the device uses non-byte-aligned access, in which
  --/   case this error code is returned.
   function soundio_instream_begin_read
     (instream : access SoundIoInStream;
      areas : System.Address;
      frame_count : access int) return int;  -- soundio.h:1126
   pragma Import (C, soundio_instream_begin_read, "soundio_instream_begin_read");

  --/ This will drop all of the frames from when you called
  --/ ::soundio_instream_begin_read.
  --/ You must call this function only from the SoundIoInStream::read_callback thread context.
  --/ You must call this function only after a successful call to
  --/ ::soundio_instream_begin_read.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorStreaming
   function soundio_instream_end_read (instream : access SoundIoInStream) return int;  -- soundio.h:1136
   pragma Import (C, soundio_instream_end_read, "soundio_instream_end_read");

  --/ If the underyling device supports pausing, this pauses the stream and
  --/ prevents SoundIoInStream::read_callback from being called. Otherwise this returns
  --/ #SoundIoErrorIncompatibleDevice.
  --/ This function may be called from any thread.
  --/ Pausing when already paused or unpausing when already unpaused has no
  --/ effect and always returns #SoundIoErrorNone.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorBackendDisconnected
  --/ * #SoundIoErrorStreaming
  --/ * #SoundIoErrorIncompatibleDevice - device does not support pausing/unpausing
   function soundio_instream_pause (instream : access SoundIoInStream; pause : Extensions.bool) return int;  -- soundio.h:1149
   pragma Import (C, soundio_instream_pause, "soundio_instream_pause");

  --/ Obtain the number of seconds that the next frame of sound being
  --/ captured will take to arrive in the buffer, plus the amount of time that is
  --/ represented in the buffer. This includes both software and hardware latency.
  --/
  --/ This function must be called only from within SoundIoInStream::read_callback.
  --/
  --/ Possible errors:
  --/ * #SoundIoErrorStreaming
   function soundio_instream_get_latency (instream : access SoundIoInStream; out_latency : access double) return int;  -- soundio.h:1159
   pragma Import (C, soundio_instream_get_latency, "soundio_instream_get_latency");

  --/ A ring buffer is a single-reader single-writer lock-free fixed-size queue.
  --/ libsoundio ring buffers use memory mapping techniques to enable a
  --/ contiguous buffer when reading or writing across the boundary of the ring
  --/ buffer's capacity.
   type SoundIoRingBuffer is null record;   -- incomplete struct

  --/ `requested_capacity` in bytes.
  --/ Returns `NULL` if and only if memory could not be allocated.
  --/ Use ::soundio_ring_buffer_capacity to get the actual capacity, which might
  --/ be greater for alignment purposes.
  --/ See also ::soundio_ring_buffer_destroy
   function soundio_ring_buffer_create (the_soundio : access SoundIo; requested_capacity : int) return access SoundIoRingBuffer;  -- soundio.h:1173
   pragma Import (C, soundio_ring_buffer_create, "soundio_ring_buffer_create");

   procedure soundio_ring_buffer_destroy (ring_buffer : access SoundIoRingBuffer);  -- soundio.h:1174
   pragma Import (C, soundio_ring_buffer_destroy, "soundio_ring_buffer_destroy");

  --/ When you create a ring buffer, capacity might be more than the requested
  --/ capacity for alignment purposes. This function returns the actual capacity.
   function soundio_ring_buffer_capacity (ring_buffer : access SoundIoRingBuffer) return int;  -- soundio.h:1178
   pragma Import (C, soundio_ring_buffer_capacity, "soundio_ring_buffer_capacity");

  --/ Do not write more than capacity.
   function soundio_ring_buffer_write_ptr (ring_buffer : access SoundIoRingBuffer) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:1181
   pragma Import (C, soundio_ring_buffer_write_ptr, "soundio_ring_buffer_write_ptr");

  --/ `count` in bytes.
   procedure soundio_ring_buffer_advance_write_ptr (ring_buffer : access SoundIoRingBuffer; count : int);  -- soundio.h:1183
   pragma Import (C, soundio_ring_buffer_advance_write_ptr, "soundio_ring_buffer_advance_write_ptr");

  --/ Do not read more than capacity.
   function soundio_ring_buffer_read_ptr (ring_buffer : access SoundIoRingBuffer) return Interfaces.C.Strings.chars_ptr;  -- soundio.h:1186
   pragma Import (C, soundio_ring_buffer_read_ptr, "soundio_ring_buffer_read_ptr");

  --/ `count` in bytes.
   procedure soundio_ring_buffer_advance_read_ptr (ring_buffer : access SoundIoRingBuffer; count : int);  -- soundio.h:1188
   pragma Import (C, soundio_ring_buffer_advance_read_ptr, "soundio_ring_buffer_advance_read_ptr");

  --/ Returns how many bytes of the buffer is used, ready for reading.
   function soundio_ring_buffer_fill_count (ring_buffer : access SoundIoRingBuffer) return int;  -- soundio.h:1191
   pragma Import (C, soundio_ring_buffer_fill_count, "soundio_ring_buffer_fill_count");

  --/ Returns how many bytes of the buffer is free, ready for writing.
   function soundio_ring_buffer_free_count (ring_buffer : access SoundIoRingBuffer) return int;  -- soundio.h:1194
   pragma Import (C, soundio_ring_buffer_free_count, "soundio_ring_buffer_free_count");

  --/ Must be called by the writer.
   procedure soundio_ring_buffer_clear (ring_buffer : access SoundIoRingBuffer);  -- soundio.h:1197
   pragma Import (C, soundio_ring_buffer_clear, "soundio_ring_buffer_clear");

end soundio_h;
