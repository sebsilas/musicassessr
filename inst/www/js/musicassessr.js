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


// // Trial info

var db_midi_vs_audio;
// Note stimuli and stimuli_durations are instantiated elsewhere (via R)
var db_trial_time_started;
var db_trial_time_completed;
var db_instrument;
var db_attempt;
var db_item_id;
var db_display_modality;
var db_phase;
var db_rhythmic;
var db_item_bank_id;
var db_session_id;
var db_test_id;

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

    var pattern = new Tone.Sequence(function(time, freq){

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



function playSeq(note_list, dur_list = null, sound = 'piano',
                 trigger_start_of_stimuli_fun = null, trigger_end_of_stimuli_fun = null) {

  // Empty previous stimulus trigger times buffer
  stimulus_trigger_times = [];
  Shiny.setInputValue("stimulus_trigger_times", JSON.stringify(stimulus_trigger_times));

  // Make sure not playing
  Tone.Transport.stop();
  pattern = null;

  // Connect sound
  connect_sound(sound);

  // This should go first before the piano editing:
  Shiny.setInputValue("stimuli_pitch", note_list);

  // Trigger an arbitrary function before the stimulus plays
  if(trigger_start_of_stimuli_fun !== null) {
    trigger_start_of_stimuli_fun();
  }

  if(typeof note_list === 'number') {
    if (sound === "piano" | sound === "voice_doo" | sound === "voice_daa") {
    note_list = note_list -12;  // There is a bug with the piano sound where it plays an octave higher
    }
    playSingleNote(note_list, dur_list, sound, trigger_end_of_stimuli_fun);
  } else {

    // Convert to freqs
    var freq_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());

    // There is a bug with the piano sound where it plays an octave higher

    if (sound === "piano" | sound === "voice_doo" | sound === "voice_daa") {
        note_list = note_list.map(x => x-12);
    }

    var last_note = freq_list.length;
    var count = 0;

    var notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
    notesAndDurations = notesAndDurations.map(timeFromDurations);

    var pattern = new Tone.Part((time, value) => {
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


function playSeqArrhythmic(freq_list, dur_list, sound, trigger_end_of_stimuli_fun = null) {

  console.log('playSeqArrhythmic');

  var last_note = freq_list.length;
  var count = 0;


  var pattern = new Tone.Sequence(function(time, note){

      triggerNote(sound, note, 0.50);

      count = count + 1;

      if (count === last_note) {
        // Stop playing
        stopSeq(pattern);
        // Trigger something else on stop
        if(trigger_end_of_stimuli_fun !== null) {
          trigger_end_of_stimuli_fun();
        }

     }

    }, freq_list);
}

function playSeqRhythmic(freq_list, dur_list, sound, trigger_end_of_stimuli_fun = null) {

  console.log('playSeqRhythmic');

  var last_note = freq_list.length;
  var count = 0;

  var notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
  notesAndDurations = notesAndDurations.map(timeFromDurations);

  var pattern = new Tone.Part((time, value) => {
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
            }
              }, notesAndDurations);
  pattern.start(0).loop = false;
  Tone.Transport.start();
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


/* Deprecated?

function toneJSPlay (midi, start_note, end_note, transpose, id, sound, bpm = 90) {

    // change tempo to whatever defined in R
    adjusted_tempo = midi;
    adjusted_tempo.header.tempos.bpm = bpm;
    adjusted_tempo.header.tempos[0].bpm = bpm;

    var now = Tone.now() + 0.5;
    var synths = [];
    adjusted_tempo.tracks.forEach(track => {

        if (end_note === "end") {
            notes_list = track.notes;

            // console.log(track.notes); // need to test full notes
            dur = track['duration'] * 1000;

        } else {
            // reduced note list
            var dur = 0;
            if(end_note === "end") {
              end_note = notes_list = track['notes'].length;
            }
            notes_list = track['notes'].slice(start_note, end_note);
            // get duration of contracted notes list
            notes_list.forEach(el => {
                   dur = dur + el['duration'];
                })
            dur = dur * 1000;

        }

        setTimeout(() => {
          recordAndStop(null, true, hidePlay, id); }, dur);

        //create a synth for each track
        const synth = new Tone.PolySynth(2, Tone.Synth, synthParameters).toMaster();
        synths.push(synth);

        // pop end note message to end

        //schedule all of the events
        notes_list.forEach(note => {

          transposed_note = Tone.Frequency(note.name).transpose(transpose);

          // correct bug where piano sound plays an octave too high

          if (sound === "piano") {
            transposed_note = transposed_note.transpose(-12);
          }


          triggerNote(sound, transposed_note, note.duration, note.time + now);

        });

        // containers to pass to shiny
        shiny_notes = [];
        shiny_ticks = [];
        shiny_durations = [];
        shiny_durationTicks = [];

        notes_list.forEach(note => {
          shiny_notes.push(note.midi);
          shiny_ticks.push(note.ticks);
          shiny_durations.push(note.duration);
          shiny_durationTicks.push(note.durationTicks);
        });

        // round the durations
        shiny_durations_round = [];
        shiny_durations.forEach(el => shiny_durations_round.push(el.toFixed(2)));

        Shiny.setInputValue("stimuli_pitch", JSON.stringify(shiny_notes));
        Shiny.setInputValue("stimuli_ticks", JSON.stringify(shiny_ticks));
        Shiny.setInputValue("stimuli_durations", JSON.stringify(shiny_durations_round));
        Shiny.setInputValue("stimuli_durationTicks", JSON.stringify(shiny_durationTicks));

    });

}
*/

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
}

function hide_happy_with_response_message() {
  happy_with_response = document.getElementById("happy_with_response");
  happy_with_response.style.display = "none";
}


function hidePlayButton(play_button_id = "playButton") {
  // make sure play is hidden immediately after being clicked once! multiple clicks can cause problems.
  var playButton = document.getElementById(play_button_id);
  if (playButton !== null) {
    playButton.style.display = "none";
  }

}

function hideAudioFilePlayer() {
  var player = document.getElementById("player");
  player.style.display = "none";
}



function startRecording(type) {


   // Initiate startTime
  startTime = new Date().getTime();
  Shiny.setInputValue('trial_start_time', startTime);

  console.log('Start time 1: ');
  console.log(startTime);

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

  console.log('Start time 2: ');
  console.log(startTime2);

}


function recordUpdateUI(page_type = null, showStop = true, hideRecord = true, showRecording = true) {

  console.log('recordUpdateUI');
  console.log(page_type);

  if(showStop) {
    showStopButton(page_type, stop_button_text);
  }

  if(hideRecord) {
    hideRecordButton();
  }

  if(showRecording) {
    showRecordingIcon();
  }
}


/*
var volumeMeter = document.getElementById('volumeMeter');
if(volumeMeter !== null) {
  volumeMeter.style.visibility = "visible";
}


*/

function showSheetMusic(sheet_music_id) {
  console.log('showSheetMusic');
  console.log(sheet_music_id);
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


function stopRecording(type) {

  setTimeout(() => {

    hideStopButton();
    hideRecordingIcon();

    if(type === "record_audio_page") {
      stopAudioRecording();
    } else if(type === "record_midi_page") {
      stopMidiRecording();
    } else {
      console.log('Unknown page type: ' + type);
    }
    next_page();

  }, 500); /* Record a little bit more */


}

function showStopButton(type = null, stop_button_text = "Stop", show_sheet_music = false) {

  console.log('showStopButton');
  console.log(type);

  var stopButton = document.getElementById("stopButton");

  if(stopButton !== undefined) {
    createCorrectStopButton(type, show_sheet_music);
  }

}

function createCorrectStopButton(type, show_sheet_music, sheet_music_id = 'sheet_music') {

  console.log('createCorrectStopButton');
  console.log(type);

  stopButton.style.visibility = 'visible';

  stopButton.onclick = function () {
    if(show_happy_with_response) {
      show_happy_with_response_message();
    }
    if(show_sheet_music) {
      // Because we are hiding *after* stopping i.e., after it has already been shown
      hideSheetMusic(sheet_music_id);
    }

    stopRecording(type);
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
  button_area.appendChild(img);

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
    //console.log(navigator);
    var _navigator = {};
    for (var i in navigator) _navigator[i] = navigator[i];
    delete _navigator.plugins;
    delete _navigator.mimeTypes;
    navigatorJSON = JSON.stringify(_navigator);
    console.log(navigatorJSON);
    console.log("Browser:" + navigator.userAgent);
    Shiny.setInputValue("user_info", navigatorJSON);
}

function testFeatureCapability() {

    // console.log(testMediaRecorder());
    // console.log(Modernizr.webaudio);

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

  console.log('startAudioRecording');

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

	    console.log('error...');
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

function hideButtonAreaShowUserRating() {
  button_area = document.getElementById("button_area");
	button_area.style.display = "none";
	stop_button = document.getElementById("stopButton");
	stop_button.style.display = "none";
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

function upload_file_to_s3(blob){


    var currentDate = new Date();

    var recordkey = create_recordkey();
    var file_url = recordkey + ".wav";

    AWS.config.update({
        region: bucketRegion,
        credentials: new AWS.CognitoIdentityCredentials({
            IdentityPoolId: IdentityPoolId
        })
    });

    var s3 = new AWS.S3({
        apiVersion: "2006-03-01",
        params: { Bucket: bucketName }
    });

    var md = {
              // Note, all metadata must be strings
              "midi-vs-audio": String(db_midi_vs_audio),
              "stimuli": vectorToString(stimuli),
              "stimuli-durations": vectorToString(stimuli_durations),
              "trial-time-started": String(db_trial_time_started),
              "trial-time-completed": String(db_trial_time_completed),
              "instrument": String(db_instrument),
              "attempt": String(db_attempt),
              "item-id": String(db_item_id),
              "display-modality": String(db_display_modality),
              "phase": String(db_phase),
              "rhythmic": String(db_rhythmic),
              "item-bank-id": String(db_item_bank_id),
              "session-id": String(db_session_id),
              "test-id": String(db_test_id)
            };

    console.log(md);

    var upload = new AWS.S3.ManagedUpload({

        params: {
            Bucket: bucketName,
            Key: file_url,
            ContentType: 'audio/wav',
            ACL: 'public-read',
            Body: blob,
            Metadata: md
        }
    });

    console.log(bucketName);
    console.log(file_url);
    console.log(destBucket);

    Shiny.setInputValue("sourceBucket", bucketName);
    Shiny.setInputValue("key", file_url);
    Shiny.setInputValue("destBucket", destBucket);

    var promise = upload.promise();

    promise.then(
        function (data) {
            console.log("Successfully uploaded new record to AWS bucket " + bucketName + "!");
        },
        function (err) {
            return alert("There was an error uploading your record: ", err.message);
        }
    );
}
