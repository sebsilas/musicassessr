console.log("loaded musicassessr.js");


// Initialise toneJS
toneJSInit();


preloadImage("https://adaptiveeartraining.com/assets/img/record.gif");
preloadImage("https://adaptiveeartraining.com/assets/img/SAA_intro.png");
preloadImage("https://adaptiveeartraining.com/assets/img/music.png");
preloadImage("https://adaptiveeartraining.com/assets/img/saxophone.png");
preloadImage("https://adaptiveeartraining.com/assets/drum.png");
preloadImage("https://adaptiveeartraining.com/assets/robot.png");



// Instantiate vars
let confidences = [];
let user_response_frequencies = [];
let timecodes = [];
let rmses = [];
let user_response_midi_note_on = [];
let user_response_midi_note_off = [];
let onsets_noteon = [];
let onsets_noteoff = [];
let stop_button_text;
let startTime;
let recordkey;
let file_url;
let onsets_noteon_timecode = [];
let stimulus_trigger_times = [];
let upload_to_s3 = false; // By default, updated at the beginning of the test where otherwise
let pattern; // the melodic pattern being played. We only want one to be played at once.
let get_async_feedback = false;
let intervalId;
let audioPlayerUserPaused = false;
let use_presigned_url = true;
let show_happy_with_response = false;
let apiUrl;
let page_label = '';
let stimuli = "";
let stimuli_durations = "";
let page_type = "";
let musicxml_file = "";

// MIDI
let interactive_midi = false;
let midi_device;
let play_midi_aloud = true;

// Grab the language from psychTestR via the URL parameter
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
const lang = urlParams.get('language');

// // Trial info

// Note stimuli and stimuli_durations are instantiated elsewhere (via R)
let db_trial_time_started;
let db_trial_time_completed;
let db_instrument;
let db_attempt;
let db_item_id;
let db_display_modality;
let db_phase;
let db_rhythmic;
let db_session_id;
let db_test_id;
let db_new_items_id;
let db_review_items_id;
let db_onset;
let db_user_id;
let db_feedback;
let db_feedback_type;
let db_trial_paradigm;
let db_melody_block_paradigm;
let db_additional;
let db_file_type;
let db_noise_filename;
let db_page_label;
let db_module;
let db_pyin_type;

// Functions


// // Setup functions

function initSynth() {

    window.synthParameters = {
      oscillator: {
        type: 'sine',
        partialCount: 4
      },
      envelope: { // http://shura.shu.ac.uk/8259/1/96913_Soranzo_psychoacoustics.pdf
        attack: 0.01,
        decay: 0.01,
        sustain: 0.50, // this is changed from the parameters above, which was 0.25
        release: 0.01,
        attackCurve: 'cosine'
      }
    };

  // Create a synth and connect it to the master output (your speakers)
  window.synth = new Tone.Synth(synthParameters).toMaster();

}

function initPiano() {

  // create a piano and connect to master output
  window.piano = SampleLibrary.load({
    instruments: "piano",
    minify: true
   });

  window.piano.toMaster();

}

function initVoiceDoo() {

      // create a piano and connect to master output
  window.voice_doo = SampleLibrary.load({
    instruments: "voice_doo",
    minify: true
   });

  window.voice_doo.toMaster();

}

function initVoiceDaa() {

      // create a piano and connect to master output
  window.voice_daa = SampleLibrary.load({
    instruments: "voice_daa",
    minify: true
   });

  window.voice_daa.toMaster();

}


function initRhythm() {

  // create a rhythm and connect to master output
  window.rhythm = SampleLibrary.load({
    instruments: "rhythm",
    minify: true
   });

  window.rhythm.toMaster();

}

function toneJSInit() {

  // sound: i.e "tone" or "piano"

  console.log("toneJS Inited!");

  initPiano();

  initSynth();

  initRhythm();

  //initVoiceDoo();

  //initVoiceDaa();

}

function connect_sound(sound) {
   // Create a synth and connect it to the master output (your speakers)
  if(sound === "tone") {
    //create a synth and connect it to the master output (your speakers)
    synth = new Tone.Synth(synthParameters).toMaster();

  } else {
    piano.toMaster();
  }
}


// Playback functions

function triggerNote(sound, freq_tone, seconds, time) {

  let triggerTime = new Date().getTime();
  stimulus_trigger_times.push(triggerTime);

  if (sound === "piano") {
  	piano.triggerAttackRelease(freq_tone, seconds, time);
  } else if (sound === "voice_doo") {
  	voice_doo.triggerAttackRelease(freq_tone, seconds, time);
  } else if (sound === "voice_daa") {
  	voice_daa.triggerAttackRelease(freq_tone, seconds, time);
  } else if (sound === "rhythm") {
  	rhythm.triggerAttackRelease(freq_tone, seconds, time);
  } else {
    synth.triggerAttackRelease(freq_tone, seconds, time);
  }

}

function playTone(tone, seconds) {

  // Play a tone (MIDI note) for x seconds

  connect_sound('tone');

  tone = Number(tone);

  let freq_tone = Tone.Frequency(tone, "midi").toNote();

  triggerNote('tone', freq_tone, seconds);

  Shiny.setInputValue("stimuli_pitch", tone);

}


function playTones (freq_list) {

    freq_list.forEach(element => console.log(element)); // testing
    freq_list = freq_list.map(x => Tone.Frequency(x));
    last_freq = freq_list[freq_list.length - 1];

    // Dispose of last pattern
    if(pattern) {
      pattern.dispose();
    }

    pattern = new Tone.Sequence(function(time, freq){

    synth.triggerAttackRelease(freq, 0.5);

    if (freq === last_freq) {
      pattern.stop();
      Tone.Transport.stop();
    }

    }, freq_list);


    pattern.start(0).loop = false;
    // begin at the beginning
    Tone.Transport.start();

}


function playSingleNote(note_list, dur_list, sound, trigger_end_of_stimuli_fun = null) {

  console.log('playSingleNote');
  console.log(note_list);
  console.log(dur_list);
  console.log(trigger_end_of_stimuli_fun);


  let freq_list = Tone.Frequency(note_list, "midi").toNote();


  triggerNote(sound, freq_list, dur_list);

  setTimeout(() => {
   // Execute on finish
   if(trigger_end_of_stimuli_fun !== null) {
      trigger_end_of_stimuli_fun();
   }
  }, dur_list * 1000);

}



function playSeq(id, note_list, dur_list = null, sound = 'piano',
                 trigger_start_of_stimuli_fun = null, trigger_end_of_stimuli_fun = null) {

  console.log('playSeq');
  console.log(note_list);
  console.log(dur_list);
  console.log(trigger_start_of_stimuli_fun);
  console.log(trigger_end_of_stimuli_fun);

  if(note_list.length === 1) {
    note_list = note_list[0];
    dur_list = dur_list[0];
  }

  console.log(note_list);
  console.log(dur_list);


  // Hide play buttons to avoid any double stimuli playing
  if(id !== 'firstMelodyPlay') {
    hidePlayButton();
    hidePlayButton('firstMelodyPlay');
  }
  // Empty previous stimulus trigger times buffer
  stimulus_trigger_times = [];
  Shiny.setInputValue("stimulus_trigger_times", JSON.stringify(stimulus_trigger_times));

  // Make sure not playing
  Tone.Transport.stop();

  // Connect sound
  connect_sound(sound);

  // This should go first before the piano editing:
  Shiny.setInputValue("stimuli_pitch", note_list);

  // Trigger an arbitrary function before the stimulus plays
  if(trigger_start_of_stimuli_fun !== null) {
    trigger_start_of_stimuli_fun();
  }

  if(typeof note_list === 'number') {
    playSingleNote(note_list, dur_list, sound, trigger_end_of_stimuli_fun);
  } else {

    // Dispose of last pattern; but this only needs to happen for sequences, not single notes
    if(pattern) {
      pattern.dispose();
    }


    // Convert to freqs
    let freq_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());

    let last_note = freq_list.length;
    let count = 0;

    let notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
    notesAndDurations = notesAndDurations.map(timeFromDurations);

    pattern = new Tone.Part((time, value) => {
                // the value is an object which contains both the note and the velocity

                triggerNote(sound, value.note, 0.50);
                count = count + 1;

                  if (count === last_note) {
                    // Stop the sequence
                    stopSeq(pattern);
                    // Trigger something else on stop
                    if(trigger_end_of_stimuli_fun !== null) {
                      trigger_end_of_stimuli_fun();
                    }

                  // Send trigger times to R
                  Shiny.setInputValue("stimulus_trigger_times", JSON.stringify(stimulus_trigger_times));
              }
                }, notesAndDurations);
    pattern.start(0).loop = false;
    Tone.Transport.start();
  }

}


function stopSeq(pattern) {
  pattern.stop();
  Tone.Transport.stop();
}



function timeFromDurations(value, i, arr) {
  const prevTime = arr[i - 1]?.time;
  value.time = prevTime + arr[i - 1]?.duration || 0;
  return value;
}

function bind_notes_and_durations(notes, durations) {
    let i;
    let currentNote;
    let currentDur;
    let result = [];

    for (i = 0; i < notes.length; i++) {
        currentNote = notes[i];
        currentDur = durations[i];
        result[i] = { duration: currentDur, note: currentNote };
    }
    return(result);
}

async function midiToToneJS (url, note_no, hidePlay, transpose, id, sound, bpm) {
  // load a midi file in the browser
  const midi = await Midi.fromUrl(url).then(midi => {
      toneJSPlay(midi, note_no, hidePlay, transpose, id, sound, bpm);

  })
}


// Define a function to handle status messages

  // Define a function to handle player events
function display_time(ev) {

  console.log(ev.time); // time in seconds, since start of playback

  MIDIjs.get_duration(url, function(seconds) {
    console.log("Duration: " + seconds);
    if (ev.time > seconds) {
        console.log("file finished!");
        MIDIjs.player_callback = null;
    } });
}

function display_message(mes) {
    console.log(mes);
}

/*
There is another function of the same name. Is this one deprecated?
function playMidiFile(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm) {

    console.log("bpm is " + bpm);

    console.log(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm);

    // hide after play
    hidePlayButton(id);
    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS

    if (toneJS) {
      midiToToneJS(url, note_no, hidePlay, transpose, id, sound, bpm);
    } else {

      MIDIjs.message_callback = display_message;
      MIDIjs.player_callback = display_time;
      console.log(MIDIjs.get_audio_status());
      MIDIjs.play(url);

    }

}
*/

// Define a function to handle player events
function display_time_record_after(ev) {

  console.log(ev.time); // time in seconds, since start of playback

  MIDIjs.get_duration(url,  function(seconds) { console.log("Duration: " + seconds);

  if (ev.time > seconds) {
      console.log("file finished!");
      MIDIjs.player_callback = null;
  }

  });

}

// Define a function to handle status messages

function playMidiFile(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm) {

    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS

    if (toneJS) {
      midiToToneJS(url, start_note, end_note, hidePlay, transpose, id, sound, bpm);
    }

    else {

      MIDIjs.message_callback = display_message;
      MIDIjs.player_callback = display_time_record_after;
      console.log(MIDIjs.get_audio_status());
      MIDIjs.play(url);
    }

}


// UI functions

function show_happy_with_response_message() {
  happy_with_response = document.getElementById("happy_with_response");
  happy_with_response.style.display = "block";
  if (!file_is_ready){
  	spinner = document.getElementsByClassName("hollow-dots-spinner");
  	if(typeof spinner[0] !== "undefined") {
  	  spinner[0].style.display = "block";
  	}
   }
  let lyrics = document.getElementById("lyrics");
  if(lyrics != null) {
    lyrics.remove();
  }
}

function hide_spinner() {
  spinner = document.getElementsByClassName("hollow-dots-spinner");
  if(typeof spinner[0] !== "undefined") {
    spinner[0].style.display = "none";
  }
}

function hide_happy_with_response_message() {
  happy_with_response = document.getElementById("happy_with_response");
  happy_with_response.style.display = "none";
}


function hideSingImg() {
  let sing_image = document.getElementById('singImage');
  if(sing_image !== 'undefined') {
    sing_image.style.display = 'none';
  }
}
function displayAsyncFeedback() {
  console.log('Display async feedback!');
  intervalId = setInterval(fetchData, pollingInterval);
  async_feedback_area = document.getElementById("async-feedback");
  async_feedback_area.style.display = "block";
  async_feedback_area.style.visibility = "visible";
  hideSingImg();
  hideTrialPageTitle();
  hideTrialPageText();
  hideStimuliArea();
}

function hideAsyncFeedback() {
  async_feedback_area = document.getElementById("async-feedback");
  async_feedback_area.style.display = "none";
}

function hidePlayButton(play_button_id = "playButton") {
  // Make sure play is hidden immediately after being clicked once! multiple clicks can cause problems.
  let playButton = document.getElementById(play_button_id);
  if (playButton !== null) {
    playButton.style.display = "none";
  }

}

function hideAudioFilePlayer() {
  let player = document.getElementById("player");
  player.style.display = "none";
}



function startRecording(type = "record_audio_page", stop_recording_automatically_after_ms = null) {

  console.log("start recording!");

  setTimeout(() => {

        // Initiate startTime
    startTime = new Date().getTime();
    Shiny.setInputValue('trial_start_time', startTime);

    page_type = type;

    if (type === "record_audio_page") {
      startAudioRecording();
    } else if(type === "record_midi_page") {
      let mute_midi_playback = !play_midi_aloud;
      instantiateMIDI(midi_device, interactive_midi, mute_midi_playback);
    } else {
      console.log('type not recognised');
    }

    // And do the same thing again to estimate potential latency
    startTime2 = new Date().getTime();
    Shiny.setInputValue('trial_start_time2', startTime2);

    if(typeof(stop_recording_automatically_after_ms) === 'number') {
      setTimeout(() => {
        stopRecording(type, trigger_next_page = true)
        }, stop_recording_automatically_after_ms);
    }

      }, 500);




}


function recordUpdateUI(page_type = null,
                        showStop = true,
                        hideRecord = true,
                        showRecording = true,
                        trigger_next_page = true,
                        show_sheet_music = false,
                        sheet_music_id = 'sheet_music') {


  removeElementIfExists("first_note");

  let volumeMeter = document.getElementById('volumeMeter');

  if(volumeMeter !== null) {
  volumeMeter.style.visibility = "visible";
  }

  if(showStop) {
    showStopButton(page_type, stop_button_text, show_sheet_music, trigger_next_page, sheet_music_id);
  }

  if(hideRecord) {
    hideRecordButton();
  }

  if(showRecording) {
    hideListenImage();
    showRecordingIcon();
  }
}



function showSheetMusic(sheet_music_id) {
  let sheet_music = document.getElementById(sheet_music_id);
  sheet_music.style.visibility = "visible";
}

function hideSheetMusic(sheet_music_id) {
  let sheet_music = document.getElementById(sheet_music_id);
  sheet_music.style.visibility = "hidden";
}


function hideLoading() {
  let loading = document.getElementById("loading");
  if (loading !== undefined) {
    loading.style.visibility = 'hidden';
  }
}


function stopRecording(page_type = "record_audio_page",
                       trigger_next_page = true) {

  // Check if audio player
  let player = document.getElementById("player");

  console.log('stop');

  console.log('audioPlayerUserPaused');
  console.log(audioPlayerUserPaused);

  // If the player is paused, the recording has been manually stopped, so only execute the below logic under that condition
  if (!audioPlayerUserPaused) {

    let volumeMeter = document.getElementById('volumeMeter');

    if(volumeMeter !== null) {
      volumeMeter.remove(); /* To remove empty space in UI */
    }

    setTimeout(() => {

      hideStopButton();
      hideRecordingIcon();
      hideLyrics();

      if(page_type === "record_audio_page") {
        stopAudioRecording();
      } else if(page_type === "record_midi_page") {
        stopMidiRecording();
      } else {
        console.log('Unknown page type: ' + page_type);
      }


      if(show_happy_with_response) {
        trigger_next_page = false;
      }

      if(trigger_next_page) {
        next_page();
      }

    }, 500); /* Record a little bit more */
  }

}

function showStopButton(page_type = null, stop_button_text = "Stop", show_sheet_music = false, trigger_next_page = true, sheet_music_id = 'sheet_music') {

  let stopButton = document.getElementById("stopButton");

  if(stopButton !== undefined) {
    createCorrectStopButton(page_type, show_sheet_music, sheet_music_id, trigger_next_page);
  }

}

function createCorrectStopButton(page_type, show_sheet_music, sheet_music_id = 'sheet_music', trigger_next_page = true) {

   setTimeout(() => {
        stopButton.style.visibility = 'visible';
      }, 500);



  stopButton.onclick = function () {
    if(show_happy_with_response && !get_async_feedback) {
      setTimeout(() => {
        show_happy_with_response_message();
      }, 500)
    }
    if(show_sheet_music) {
      // Because we are hiding *after* stopping i.e., after it has already been shown
      hideSheetMusic(sheet_music_id);
    }

    // If there is some audio playing, stop it
    stopRecording(page_type, trigger_next_page); // but make sure to stop the recording FIRST
    let player = document.getElementById("player");

    if(player !== null) {
      player.pause();
      audioPlayerUserPaused = true;
    }

  };
}


function showRecordingIcon() {

  let img = document.createElement("img");
  img.id = "recordingIcon";
  img.style.display = "block";
  img.src =  "https://adaptiveeartraining.com/assets/img/record.gif";
  img.width = "280";
  img.height = "280";
  let button_area = document.getElementById("button_area");

  setTimeout(() => {
        button_area.appendChild(img);
      }, 500);


}

function hideRecordingIcon() {
  let rec_icon = document.getElementById("recordingIcon");
  if (rec_icon.style.display === "none") {
    rec_icon.style.display = "block";
   } else {
    rec_icon.style.display = "none";
   }
}


function hideStopButton() {
  let stopButton = document.getElementById("stopButton");
  if (stopButton.style.display === "none") {
    stopButton.style.display = "block";
   } else {
    stopButton.style.display = "none";
   }
}

function hideRecordButton() {
  let recButton = document.getElementById("recordButton");
  if (recButton.style.display === "none") {
    recButton.style.display = "block";
  } else {
    recButton.style.display = "none";
  }
}


// Utils


function removeElementIfExists(element) {
  let el = document.getElementById(element);
  if(el !== null) {
    el.parentNode.removeChild(el);
  }
}

function removeElement(element) {
  let el = document.getElementById(element);
  el.parentNode.removeChild(el);
}

function getUTCDateTime() {
  let now = new Date();
  let year = now.getUTCFullYear();
  let month = now.getUTCMonth() + 1; // Months are zero-based in JS
  let day = now.getUTCDate();
  let hour = now.getUTCHours();
  let minute = now.getUTCMinutes();
  let second = now.getUTCSeconds();

  // Ensure two-digit formatting
  month = month.toString().padStart(2, '0');
  day = day.toString().padStart(2, '0');
  hour = hour.toString().padStart(2, '0');
  minute = minute.toString().padStart(2, '0');
  second = second.toString().padStart(2, '0');

  let dateTime = `${year}-${month}-${day} ${hour}:${minute}:${second} UTC`;
  return dateTime;
}

function vectorToString(vector) {

  if (!Array.isArray(vector)) {
    console.warn("Input is not a valid array");
    return String(vector)
  }

  return vector.join(', ');
}

function diff(ary) {
    let newA = [];
    for (let i = 1; i < ary.length; i++)  newA.push(ary[i] - ary[i - 1]);
    newA.unshift(0); // pop a 0 on the front
    return newA;
}

function preloadImage(url) {
    let img = new Image();
    img.src = url;
}


// Checks

function getUserInfo () {
    let _navigator = {};
    for (let i in navigator) _navigator[i] = navigator[i];
    delete _navigator.plugins;
    delete _navigator.mimeTypes;
    navigatorJSON = JSON.stringify(_navigator);
    Shiny.setInputValue("user_info", navigatorJSON);
}

function testFeatureCapability() {

    if (Modernizr.webaudio & testMediaRecorder()) {
        console.log("This browser has the necessary features");
        Shiny.setInputValue("browser_capable", "TRUE");
    }
    else {
        console.log("This browser does not have the necessary features");
        Shiny.setInputValue("browser_capable", "FALSE");
    }

}

function testMediaRecorder () {

  let isMediaRecorderSupported = false;

  try {
      MediaRecorder;
      isMediaRecorderSupported = true;
  } catch (err) {
      console.log("no MediaRecorder");
  }
  //console.log(isMediaRecorderSupported);
  return(isMediaRecorderSupported);
}

// Audio recorder stuff (previously in app.js)

//webkitURL is deprecated but nevertheless
URL = window.URL || window.webkitURL;
//

let file_is_ready = false;
let gumStream; 						//stream from getUserMedia()
let rec; 							//Recorder.js object
let input;
//MediaStreamAudioSourceNode we'll be recording
////////
// shim for AudioContext when it's not avb.
let AudioContext = window.AudioContext || window.webkitAudioContext;
let audioContext //audio context to help us record


function startAudioRecording() {

    let constraints = { audio: true, video:false }

	/*
    	We're using the standard promise based getUserMedia()
    	https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia
	*/

	navigator.mediaDevices.getUserMedia(constraints).then(function(stream) {
		console.log("getUserMedia() success, stream created, initializing Recorder.js ...");

		/*
			create an audio context after getUserMedia is called
			sampleRate might change after getUserMedia is called, like it does on macOS when recording through AirPods
			the sampleRate defaults to the one set in your OS for your playback device
		*/
		audioContext = new AudioContext();

		//update the format
		//document.getElementById("formats").innerHTML="Format: 1 channel pcm @ "+audioContext.sampleRate/1000+"kHz"

		/*  assign to gumStream for later use  */
		gumStream = stream;

		/* use the stream */
		input = audioContext.createMediaStreamSource(stream);

		/*
			Create the Recorder object and configure to record mono sound (1 channel)
			Recording 2 channels  will double the file size
		*/
		rec = new Recorder(input,{numChannels:1})

		//start the recording process
		rec.record()

		console.log("Recording started");

	}).catch(function(err) {

	    console.log('Error: ');
	    console.log(err);
	  	//enable the record button if getUserMedia() fails
    	recordButton.disabled = false;
    	stopButton.disabled = true;
    	//pauseButton.disabled = true
	});
}

function pauseRecording(){
	console.log("pauseButton clicked rec.recording=",rec.recording );
	if (rec.recording){
		// Pause
		rec.stop();
		pauseButton.innerHTML = "Resume";
	} else{
		// Resume
		rec.record();
		pauseButton.innerHTML = "Pause";

	}
}

function stopAudioRecording() {

	//tell the recorder to stop the recording
	rec.stop();

	//stop microphone access
	gumStream.getAudioTracks()[0].stop();

	//create the wav blob and pass it on to createDownloadLink

	recordkey = create_recordkey();
  file_url = recordkey + ".wav";

  Shiny.setInputValue("key", recordkey);
  Shiny.setInputValue("file_url", file_url);

  if(upload_to_s3) {
    rec.exportWAV(upload_file_to_s3);
  } else {
    rec.exportWAV(process_file_locally);
  }


}

function stopMidiRecording() {
  console.log('stopMidiRecording');
  WebMidi.disable();
}

function getPIdFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  return urlParams.get('p_id');
}

function create_recordkey() {

  let currentDate = new Date();

  let recordkey = currentDate.getDate().toString() + '-' + (currentDate.getMonth() + 1 ).toString() + '-' + currentDate.getFullYear().toString() + '--' + currentDate.getHours().toString() + '-' + currentDate.getMinutes()  + '--' + currentDate.getSeconds().toString();

  if (typeof page_label === 'string') {
    recordkey = page_label + '.' + recordkey;
  }

  const pId = getPIdFromUrl();
  console.log('pId:', pId);

  if (typeof pId === 'string') {
    recordkey = pId + '.' + recordkey;
  } else {
    recordkey = 'no_p_id.' + recordkey;
  }


  return(recordkey)
}



function process_file_locally(blob) {

	let xhr = new XMLHttpRequest();

	let fd = new FormData();

	fd.append("audio_data", blob, recordkey);

	if (typeof shiny_app_name !== 'undefined') {
	  fd.append("app_name", shiny_app_name);
	  Shiny.setInputValue("shiny_app_name", shiny_app_name);
	}

	if(this.musicassessr_state === "production") {
	  console.log("Upload local to musicassessr AWS server...")
	  xhr.open("POST","/api/store_audio/", true); // production
  } else {
    console.log('Upload local to user machine e.g...');
    xhr.open("POST","http://localhost:3000/upload-audio", true); // local
  }

	xhr.send(fd);


	xhr.onload = () => {
	  console.log(xhr.responseText);
		// call next page after credentials saved
		spinner = document.getElementsByClassName("hollow-dots-spinner");
		if(spinner[0] !== undefined) {
		  spinner[0].style.display = "none";
		}
		file_is_ready = true;
	};
}


async function upload_file_to_s3(blob) {

  let currentDate = new Date();
  let recordkey = create_recordkey();
  let file_url = recordkey;

  let md = {
    // Note, all metadata must be strings
    "stimuli": vectorToString(stimuli),
    "stimuli-durations": vectorToString(stimuli_durations),
    "trial-time-started": String(db_trial_time_started),
    "trial-time-completed": String(getUTCDateTime()),
    "instrument": String(db_instrument),
    "attempt": String(db_attempt),
    "item-id": String(db_item_id),
    "display-modality": String(db_display_modality),
    "phase": String(db_phase),
    "rhythmic": String(db_rhythmic),
    "session-id": String(db_session_id),
    "test-id": String(db_test_id),
    "onset": String(db_onset),
    "new-items-id": String(db_new_items_id),
    "review-items-id": String(db_review_items_id),
    "user-id": String(db_user_id),
    "feedback": String(db_feedback),
    "feedback-type": String(db_feedback_type),
    "trial-paradigm": String(db_trial_paradigm),
    "melody-block-paradigm": String(db_melody_block_paradigm),
    "additional": String(db_additional),
    "file-type": String(db_file_type),
    "noise-filename" : String(db_noise_filename),
    "page-label": String(db_page_label),
    "module": String(db_module),
    "pyin-type": String(db_pyin_type)
  };

  console.log(md);

  const requestBody = { filename: file_url, metadata: md };

  let endpoint;

  if (use_presigned_url) {
    endpoint = "v2/get-audio-presigned-url";
  } else {
    endpoint = "v2/get-audio-presigned-url-legacy";
  }

  try {
    const retrievedTokenString = localStorage.getItem('jwkToken');

    const response = await fetch(apiUrl + endpoint, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': retrievedTokenString
      },
      body: JSON.stringify(requestBody)
    });

    if (!response.ok) {
      throw new Error(`Failed to get response: ${response.status}`);
    }

    const responseData = await response.json();

    const url = responseData.url;

    const uploadResponse = await fetch(url, {
      method: 'PUT',
      body: blob // assuming 'blob' is the correct variable to use here
    });

    if (!uploadResponse.ok) {
      throw new Error(`Failed: ${uploadResponse.status}`);
    }
    console.log('Successful');
  } catch (error) {
    console.error(error);
  }

  // Feedback
  if (get_async_feedback) {
    fetchData();
    displayAsyncFeedback();
  }

  Shiny.setInputValue("key", file_url);
}




/* Async feedback funs */


const pollingInterval = 5000; // 5 seconds

async function fetchData() {

  console.log('Fetching feedback for ' + file_url);

  const payload = {
    filename: file_url
  };

  try {
    const response = await fetch(apiUrl + "v2/get-job-status", {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(payload)
    });
    if (!response.ok) {
      throw new Error('Network response was not ok ' + response.statusText);
    }
    const data = await response.json();

    // Check if the status is 'FINISHED'
    if (data.status === 'FINISHED') {
      console.log('Job is finished. Stopping polling.');
      const message = JSON.parse(data.message);
      const scores = message.feedback;
      console.log('scores:');
      console.log(scores);
      console.log("transcribed notes: " + scores.transcribed_notes[0]);
      console.log("opti3: " + scores.opti3[0]);
      console.log("ngrukkon: " + scores.ngrukkon[0]);
      console.log("rhythfuzz: " + scores.rhythfuzz[0]);
      console.log("harmcore: " + scores.harmcore[0]);
      console.log("benovelent_opti3: " + scores.benovelent_opti3[0]);
      console.log("rhythmic_weighted_edit_sim: " + scores.rhythmic_weighted_edit_sim[0]);
      console.log("notes_with_best_transposition: " + scores.notes_with_best_transposition[0]);
      console.log("stimulus: " + scores.stimulus[0]);
      displayScore(scores.benovelent_opti3[0]);
      stopPolling();
      appendNextButton(onClick = function() { /* Note, leave this here rather than allowing the participant to skip. Otherwise they might see feedback from an old trial */
        show_happy_with_response_message();
        hideAsyncFeedback();
      });
      hideLoader();
    }
  } catch (error) {
    console.error('Fetch error:', error);
  }
}

function displayScore(score) {

  const container = document.getElementById('data-container');

  if(isNaN(score)) {
    score = 0;
  }

  if(lang == "en") {

    container.innerHTML = `<p>Well done! </p> <p>Your score was ${score}!</p>`;


  } else if(lang == "de") {

    console.log(typeof score);

    container.innerHTML = `<p> ${ getFeedback(score) } </p> <p> Du hast ${score} von 10 Punkten erreicht.</p>`;

  } else {
    console.log("Language not supported!");
  }
}

function getFeedback(score) {

  let feedback;

  if (score >= 0 && score <= 2) {
    feedback = "Das kannst du besser!";
  } else if (score >= 3 && score <= 4) {
    feedback = "Schon ziemlich gut!";
  } else if (score >= 5 && score <= 7) {
    feedback = "Gut gemacht!";
  } else if (score >= 8 && score <= 9) {
    feedback = "Super, du bist klasse!";
  } else if (score === 10) {
    feedback = "Gratulation, das war hervorragend!";
  } else {
    feedback = "Ungültige Punktzahl";
  }

  return feedback;
}




function showLoader() {
  document.getElementById('loader').style.display = 'block';
}

function hideLoader() {
  document.getElementById('loader').style.display = 'none';
}


function showListenImage() {
  let listenImg = document.getElementById('listenImg');
  if(listenImg !== 'undefined') {
    listenImg.style.display = 'block';
  }
}

function hideListenImage() {
  let listenImg = document.getElementById('listenImg');
  if(listenImg !== 'undefined') {
    listenImg.style.display = 'none';
  }
}

function hideTrialPageTitle() {
  let trial_page_title = document.getElementById('trial_page_title');
  if(trial_page_title !== 'undefined') {
    trial_page_title.style.display = 'none';
  }
}

function hideTrialPageText() {
  let trial_page_text = document.getElementById('trial_page_text');
  if(trial_page_text !== 'undefined') {
    trial_page_text.style.display = 'none';
  }
}

function hideStimuliArea() {
  let stimuliArea = document.getElementById('stimuliArea');
  if(stimuliArea !== 'undefined') {
    stimuliArea.style.display = 'none';
  }
}

function hideLyrics() {
  let lyrics = document.getElementById('lyrics');
  if(lyrics != null) {
    lyrics.style.display = 'none';
  }
}




function stopPolling() {
  console.log("stop polling: ", intervalId);
  clearInterval(intervalId);
}


function appendNextButton(onClick = next_page, id = 'async-feedback') {
  // Create a new button element
  let nextButton = document.createElement('button');


  if(lang == "en"){
    // Set the button's text content
    nextButton.textContent = 'Next';
  } else if(lang == "de") {
    nextButton.textContent = 'Weiter';
  } else {
    console.log("Lang not supported!")

  }
  // Set an ID for the button
  nextButton.id = 'nextButton';

  nextButton.className = 'btn btn-default';

  // Set the onclick attribute
  nextButton.onclick = onClick;

  // Append the button to the element with ID
  let areaToAppendTo = document.getElementById(id);
  if (areaToAppendTo) {
    areaToAppendTo.appendChild(nextButton);
  } else {
    console.error('Element with ID ', + id, ' not found.');
  }
}


// Generalise job grabbing functionality

async function fetchDataApi(triggerWhenDataBack) {

  console.log('Fetching feedback for ' + file_url);

  const payload = {
    filename: file_url
  };

  try {
    const response = await fetch(apiUrl + "v2/get-job-status", {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(payload)
    });
    if (!response.ok) {
      throw new Error('Network response was not ok ' + response.statusText);
    }

    const data = await response.json();

    // Check if the status is 'FINISHED'
    if (data.status === 'FINISHED') {

      console.log('Job is finished. Stopping polling.');

      let message;

      try {
        message = JSON.parse(data.message);
         console.log('message:', message);
      Shiny.setInputValue("API_DATA_RESPONSE", message.feedback[0]);
      } catch (e) {
        message = data.message; // It's just a plain string, not JSON
        console.log('message:', message);
        Shiny.setInputValue("API_DATA_RESPONSE", message);
      }



      stopPolling();

      triggerWhenDataBack();

    }
  } catch (error) {
    console.error('Fetch error:', error);
  }
}

function pollDataApi(triggerWhenDataBack) {
  intervalId = setInterval(() => fetchDataApi(triggerWhenDataBack), pollingInterval);
}



function handleFeedbackRhythmBpm() {
  console.log('handleFeedbackRhythmBpm!');
  document.getElementById("nextButton").className = "btn btn-default action-button";
  hideLoader();
}

function handleFeedbackSNR() {
  console.log('handleFeedbackRhythmSNR!');
  hideLoader();
  next_page();
}

// Display score functionality

function displayScore(score) {

  const container = document.getElementById('data-container');

  if(isNaN(score)) {
    score = 0;
  }

  if(lang == "en") {

    container.innerHTML = `<p>Well done! </p> <p>Your score was ${score}!</p>`;


  } else if(lang == "de") {

    console.log(typeof score);

    container.innerHTML = `<p> ${ getFeedback(score) } </p> <p> Du hast ${score} von 10 Punkten erreicht.</p>`;

  } else {
    console.log("Language not supported!");
  }
}

