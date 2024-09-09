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
var confidences = [];
var user_response_frequencies = [];
var timecodes = [];
var rmses = [];
var user_response_midi_note_on = [];
var user_response_midi_note_off = [];
var onsets_noteon = [];
var onsets_noteoff = [];
var stop_button_text;
var startTime;
var midi_device;
var recordkey;
var file_url;
var onsets_noteon_timecode = [];
var stimulus_trigger_times = [];
var upload_to_s3 = false; // By default, updated at the beginning of the test where otherwise
var pattern; // the melodic pattern being played. We only want one to be played at once.
var get_async_feedback = false;
var intervalId;
var lang;
var audioPlayerUserPaused = false;

// // Trial info

// Note stimuli and stimuli_durations are instantiated elsewhere (via R)
var db_trial_time_started;
var db_trial_time_completed;
var db_instrument;
var db_attempt;
var db_item_id;
var db_display_modality;
var db_phase;
var db_rhythmic;
var db_session_id;
var db_test_id;
var db_new_items_id;
var db_review_items_id;
var db_onset;
var db_user_id;
var db_feedback;
var db_feedback_type;
var db_trial_paradigm;

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

  //create a synth and connect it to the master output (your speakers)
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
   //create a synth and connect it to the master output (your speakers)
  if(sound === "tone") {
    window.piano.disconnect();
    //window.voice_doo.disconnect();
    //window.voice_daa.disconnect();
    //create a synth and connect it to the master output (your speakers)
    window.synth = new Tone.Synth(synthParameters).toMaster();

  } else if(sound === "voice_doo") {

    window.piano.disconnect();
    window.synth.disconnect();
    //window.voice_daa.disconnect();
    //window.voice_doo.toMaster();

  } else if(sound === "voice_daa") {

    window.piano.disconnect();
    window.synth.disconnect();
    //window.voice_doo.disconnect();
    //window.voice_daa.toMaster();
  } else if(sound === "rhythm") {
    window.piano.disconnect();
    window.synth.disconnect();
  }
  else {
    window.synth.disconnect();
    //window.voice_daa.disconnect();
    //window.voice_doo.disconnect();
    window.piano.toMaster();
  }
}


// Playback functions

function triggerNote(sound, freq_tone, seconds, time) {

  var triggerTime = new Date().getTime();
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

  var freq_tone = Tone.Frequency(tone, "midi").toNote();

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


  var freq_list = Tone.Frequency(note_list, "midi").toNote();


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
    var freq_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());

    var last_note = freq_list.length;
    var count = 0;

    var notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
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
    var i;
    var currentNote;
    var currentDur;
    var result = [];

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
  var lyrics = document.getElementById("lyrics");
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

function displayAsyncFeedback() {
  console.log('Display async feedback!');
  intervalId = setInterval(fetchData, pollingInterval);
  async_feedback_area = document.getElementById("async-feedback");
  async_feedback_area.style.display = "block";
  async_feedback_area.style.visibility = "visible";
  hideSingImage();
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
  var playButton = document.getElementById(play_button_id);
  if (playButton !== null) {
    playButton.style.display = "none";
  }

}

function hideAudioFilePlayer() {
  var player = document.getElementById("player");
  player.style.display = "none";
}



function startRecording(type = "record_audio_page", stop_recording_automatically_after_ms = null) {

  console.log("start recording!");

  setTimeout(() => {

        // Initiate startTime
    startTime = new Date().getTime();
    Shiny.setInputValue('trial_start_time', startTime);

    if (type === "record_audio_page") {
      startAudioRecording();
    } else if(type === "record_midi_page") {
      instantiateMIDI(midi_device);
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

  var volumeMeter = document.getElementById('volumeMeter');
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
    showRecordingIcon();
  }
}



function showSheetMusic(sheet_music_id) {
  var sheet_music = document.getElementById(sheet_music_id);
  sheet_music.style.visibility = "visible";
}

function hideSheetMusic(sheet_music_id) {
  var sheet_music = document.getElementById(sheet_music_id);
  sheet_music.style.visibility = "hidden";
}


function hideLoading() {
  var loading = document.getElementById("loading");
  if (loading !== undefined) {
    loading.style.visibility = 'hidden';
  }
}


function stopRecording(page_type = "record_audio_page",
                       trigger_next_page = true) {

  // Check if audio player
  var player = document.getElementById("player");

  console.log('stop');

  console.log('audioPlayerUserPaused');
  console.log(audioPlayerUserPaused);

  // If the player is paused, the recording has been manually stopped, so only execute the below logic under that condition
  if (!audioPlayerUserPaused) {

    var volumeMeter = document.getElementById('volumeMeter');

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

  var stopButton = document.getElementById("stopButton");

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
    var player = document.getElementById("player");

    if(player !== null) {
      player.pause();
      audioPlayerUserPaused = true;
    }

  };
}


function showRecordingIcon() {

  var img = document.createElement("img");
  img.id = "recordingIcon";
  img.style.display = "block";
  img.src =  "https://adaptiveeartraining.com/assets/img/record.gif";
  img.width = "280";
  img.height = "280";
  var button_area = document.getElementById("button_area");

  setTimeout(() => {
        button_area.appendChild(img);
      }, 500);


}

function hideRecordingIcon() {
  var rec_icon = document.getElementById("recordingIcon");
  if (rec_icon.style.display === "none") {
    rec_icon.style.display = "block";
   } else {
    rec_icon.style.display = "none";
   }
}


function hideStopButton() {
  var stopButton = document.getElementById("stopButton");
  if (stopButton.style.display === "none") {
    stopButton.style.display = "block";
   } else {
    stopButton.style.display = "none";
   }
}

function hideRecordButton() {
  var recButton = document.getElementById("recordButton");
  if (recButton.style.display === "none") {
    recButton.style.display = "block";
  } else {
    recButton.style.display = "none";
  }
}


// Utils


function removeElementIfExists(element) {
  var el = document.getElementById(element);
  if(el !== null) {
    el.parentNode.removeChild(el);
  }
}

function removeElement(element) {
  var el = document.getElementById(element);
  el.parentNode.removeChild(el);
}

function getDateTime() {
  var now = new Date();
  var year = now.getFullYear();
  var month = now.getMonth()+1;
  var day = now.getDate();
  var hour = now.getHours();
  var minute = now.getMinutes();
  var second = now.getSeconds();
  if(month.toString().length == 1) {
    month = '0'+month;
  }
  if(day.toString().length == 1) {
    day = '0'+day;
  }
  if(hour.toString().length == 1) {
    hour = '0'+hour;
  }
  if(minute.toString().length == 1) {
       minute = '0'+minute;
  }
  if(second.toString().length == 1) {
    second = '0'+second;
  }
  var dateTime = year+'-'+month+'-'+day+' '+hour+':'+minute+':'+second;
  return dateTime;
}

function vectorToString(vector) {
  if (!Array.isArray(vector)) {
    return "Input is not a valid array";
  }

  return vector.join(', ');
}

function diff(ary) {
    var newA = [];
    for (var i = 1; i < ary.length; i++)  newA.push(ary[i] - ary[i - 1]);
    newA.unshift(0); // pop a 0 on the front
    return newA;
}

function preloadImage(url) {
    var img = new Image();
    img.src = url;
}


// Checks

function getUserInfo () {
    var _navigator = {};
    for (var i in navigator) _navigator[i] = navigator[i];
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

  var isMediaRecorderSupported = false;

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

var file_is_ready = false;
var gumStream; 						//stream from getUserMedia()
var rec; 							//Recorder.js object
var input;
//MediaStreamAudioSourceNode we'll be recording
////////
// shim for AudioContext when it's not avb.
var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext //audio context to help us record


function startAudioRecording() {

    var constraints = { audio: true, video:false }

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
    rec.exportWAV(process_file_on_server);
  }


}

function stopMidiRecording() {
  console.log('stopMidiRecording');
  WebMidi.disable();
}

function create_recordkey() {

  var currentDate = new Date();

  var recordkey = currentDate.getDate().toString() + '-' + (currentDate.getMonth() + 1 ).toString() + '-' + currentDate.getFullYear().toString() + '--' + currentDate.getHours().toString() + '-' + currentDate.getMinutes()  + '--' + currentDate.getSeconds().toString();

  if (typeof page_label === 'string') {
    recordkey = page_label + '.' + recordkey;
  }

  if (typeof p_id === 'string') {
    recordkey = p_id + '.' + recordkey;
  } else {
    recordkey = 'no_p_id.' + recordkey;
  }


  return(recordkey)
}

function store_file_locally(blob) {

	var xhr = new XMLHttpRequest();

	var fd = new FormData();

	fd.append("audio_data", blob, recordkey);

	if (typeof shiny_app_name !== 'undefined') {
	  fd.append("app_name", shiny_app_name);
	  Shiny.setInputValue("shiny_app_name", shiny_app_name);
	}

	if(this.musicassessr_state === "production") {
	  xhr.open("POST","/api/store_audio/",true); // production
  } else {
    console.log('Upload local...');
    xhr.open("POST","http://localhost:3000/upload-audio",true); // local
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



function process_file_on_server(blob) {

	var xhr = new XMLHttpRequest();

	var fd = new FormData();

	fd.append("audio_data", blob, recordkey);

	if (typeof shiny_app_name !== 'undefined') {
	  fd.append("app_name", shiny_app_name);
	  Shiny.setInputValue("shiny_app_name", shiny_app_name);
	}

	if(this.musicassessr_state === "production") {
	  xhr.open("POST","/api/store_audio/",true); // production
  } else {
    console.log('Upload local...');
    xhr.open("POST","http://localhost:3000/upload-audio",true); // local
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

  var currentDate = new Date();
  var recordkey = create_recordkey();
  var file_url = recordkey;

  var md = {
    // Note, all metadata must be strings
    "stimuli": vectorToString(stimuli),
    "stimuli-durations": vectorToString(stimuli_durations),
    "trial-time-started": String(db_trial_time_started),
    "trial-time-completed": String(getDateTime()),
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
    "trial-paradigm": String(db_trial_paradigm)
  };

  console.log(md);

  const requestBody = { filename: file_url, metadata: md };

  try {
    const retrievedTokenString = localStorage.getItem('jwkToken');
    const response = await fetch(apiUrl + "/v2/get-audio-presigned-url", {
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
      console.log("transcribed notes: " + scores.transcribed_notes);
      console.log("opti3: " + scores.opti3);
      console.log("ngrukkon: " + scores.ngrukkon);
      console.log("rhythfuzz: " + scores.rhythfuzz);
      console.log("harmcore: " + scores.harmcore);
      displayScore(scores.benovelent_opti3);
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

    container.innerHTML = `<p> ${ getFeedback(score) } </p> <p> Du hast ${score} von 10 Punkten erreicht.</p>`;

  } else {
    console.log("Language not supported!");
  }
}

function getFeedback(score) {
  let feedback;

  if (score >= 0 && score <= 2) {
    feedback = "Das kannst Du besser!";
  } else if (score >= 3 && score <= 4) {
    feedback = "Schon ziemlich gut!";
  } else if (score >= 5 && score <= 7) {
    feedback = "Gut gemacht!";
  } else if (score >= 8 && score <= 9) {
    feedback = "Super, Du bist klasse!";
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

function hideSingImage() {
  var sing_image = document.getElementById('singImage');
  if(sing_image !== 'undefined') {
    sing_image.style.display = 'none';
  }
}

function hideTrialPageTitle() {
  var trial_page_title = document.getElementById('trial_page_title');
  if(trial_page_title !== 'undefined') {
    trial_page_title.style.display = 'none';
  }
}

function hideTrialPageText() {
  var trial_page_text = document.getElementById('trial_page_text');
  if(trial_page_text !== 'undefined') {
    trial_page_text.style.display = 'none';
  }
}

function hideStimuliArea() {
  var stimuliArea = document.getElementById('stimuliArea');
  if(stimuliArea !== 'undefined') {
    stimuliArea.style.display = 'none';
  }
}

function hideLyrics() {
  var lyrics = document.getElementById('lyrics');
  if(lyrics != null) {
    lyrics.style.display = 'none';
  }
}




function stopPolling() {
  clearInterval(intervalId);
}


function appendNextButton(onClick = next_page, id = 'async-feedback') {
  // Create a new button element
  var nextButton = document.createElement('button');

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
  var areaToAppendTo = document.getElementById(id);
  if (areaToAppendTo) {
    areaToAppendTo.appendChild(nextButton);
  } else {
    console.error('Element with ID ', + id, ' not found.');
  }
}

