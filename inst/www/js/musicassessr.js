console.log("loaded musicassessr.js");


// initialise toneJS
toneJSInit();


preloadImage("https://adaptiveeartraining.com/magmaGold/img/record.gif");

// constants
// a little delay after playback finishes before hitting record
var record_delay = 400;
var playback_count = 0; // number of times user presses play in a trial
var confidences = [];
var user_response_frequencies = [];
var timecodes = [];
var rmses = [];

// functions


// // setup functions

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

function toneJSInit() {

  // sound: i.e "tone" or "piano"

  console.log("toneJS Inited!");

  initPiano();

  initSynth();

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
  }
  else {
    window.synth.disconnect();
    //window.voice_daa.disconnect();
    //window.voice_doo.disconnect();
    window.piano.toMaster();
  }
}


// playback functions


function updatePlaybackCount() {

    playback_count =  playback_count + 1;

    var playbackTimes = [];

    // record playback values in time
    playbackTimes.push(Date.now());

    var playbackTimesDiff = diff(playbackTimes);
    var playbackTimesCumSum = [];
    playbackTimesDiff.reduce(function(a,b,i) { return playbackTimesCumSum[i] = a+b; },0);
    Shiny.setInputValue("playback_count", playback_count);
    Shiny.setInputValue("playback_times", JSON.stringify(playbackTimesCumSum));

}

function triggerNote(sound, freq_tone, seconds, time) {

  if (sound === "piano") {
  	piano.triggerAttackRelease(freq_tone, seconds, time);
  } else if (sound === "voice_doo") {
  	voice_doo.triggerAttackRelease(freq_tone, seconds, time);
  } else if (sound === "voice_daa") {
  	voice_daa.triggerAttackRelease(freq_tone, seconds, time);
  } else {
    synth.triggerAttackRelease(freq_tone, seconds, time);
  }

}

function playTone(tone, seconds, id, sound, hidePlay = true, page_type = "aws_pyin", stop_button_text = "Stop", showStop = false, record_immediately = true) {
  // play a tone for x seconds
  console.log('playTone!');

  connect_sound('tone');

  tone = Number(tone);

  var freq_tone = Tone.Frequency(tone, "midi").toNote();

  triggerNote(sound, freq_tone, seconds);

  if(record_immediately) {
     setTimeout(() => {  recordAndStop(ms = seconds*1000,
                                    showStop = showStop, hidePlay = hidePlay, id = id, page_type = page_type, stop_button_text = stop_button_text); }, record_delay); // delay to avoid catching stimuli in recording

  } else {
    auto_next_page = true;
    var total_record_delay = record_delay + seconds*1000 ;
    setTimeout(() => {  recordAndStop(ms = null, showStop = true, hidePlay = hidePlay, id = id, page_type = page_type, stop_button_text = stop_button_text); }, total_record_delay); // delay to avoid catching stimuli in recording
  }

  updatePlaybackCount();

  Shiny.setInputValue("stimuli_pitch", tone);

}


function playTones (note_list) {

    note_list.forEach(element => console.log(element)); // testing
    note_list = note_list.map(x => Tone.Frequency(x));
    last_note = note_list[note_list.length - 1];

    var pattern = new Tone.Sequence(function(time, note){
    synth.triggerAttackRelease(note, 0.5);

    if (note === last_note) {
      console.log("finished!");
    }

    }, note_list);


    pattern.start(0).loop = false;
    // begin at the beginning
    Tone.Transport.start();

}


function playSingleNote(note_list, dur_list, hidePlay, id, page_type, stop_button_text, sound) {

  if (sound === "piano" | sound === "voice_doo" | sound === "voice_daa") {
    note_list = note_list-12;
  }


  var freq_list = Tone.Frequency(note_list, "midi").toNote();

  auto_next_page = true;
  triggerNote(sound, freq_list, dur_list);
  setTimeout(() => {  recordAndStop(null, true, hidePlay, id, page_type, stop_button_text); }, (dur_list*1000) + record_delay);

}


function playSeqArrhythmic(freq_list, dur_list, count, sound, last_note, page_type, hidePlay, id, stop_button_text) {
   var pattern = new Tone.Sequence(function(time, note){

        triggerNote(sound, note, 0.50);
        count = count + 1;
        if (count === last_note) {
          if(page_type == "aws_pyin" | page_type == "crepe" | page_type == "record_midi_page" | page_type == "record_audio_page") {
            setTimeout(() => {  recordAndStop(null, true, hidePlay, id, page_type, stop_button_text); }, 0.50 + record_delay); // delay to avoid catching stimuli in recording
          }
          pattern.stop();
          Tone.Transport.stop();
        }

      }, freq_list);
}

function playSeqRhythmic(freq_list, dur_list, count, sound, last_note, page_type, hidePlay, id, stop_button_text) {

  var notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
  notesAndDurations = notesAndDurations.map(timeFromDurations);
  var pattern = new Tone.Part((time, value) => {
              // the value is an object which contains both the note and the velocity
              if(sound === "voice_doo") {
                voice_doo.triggerAttackRelease(value.note, value.duration, time);
              } else if(sound === "voice_daa") {
                voice_daa.triggerAttackRelease(value.note, value.duration, time);
              } else {
                piano.triggerAttackRelease(value.note, value.duration, time);
              }
              count = count + 1;
                if (count === last_note) {
                  if(page_type !== 'null') {
                    setTimeout(() => {  recordAndStop(null, true, hidePlay, id, page_type, stop_button_text); }, value.duration*1000 + record_delay); // delay to avoid catching stimuli in recording
                  }

                  pattern.stop();
                  Tone.Transport.stop();
                }
              }, notesAndDurations);
    pattern.start(0).loop = false;
    Tone.Transport.start();
}


function playSeq(note_list, hidePlay, id, sound, page_type, stop_button_text = "Stop", dur_list = null, auto_next_page) {

  auto_next_page = auto_next_page;

  // make sure not playing
  Tone.Transport.stop();
  pattern = null;

  connect_sound(sound);

  // this should go first before the piano editing:
  Shiny.setInputValue("stimuli_pitch", note_list);

  // seems to be a bug with the piano sound where it plays an octave higher

  if(typeof note_list === 'number') {
      playSingleNote(note_list, dur_list, hidePlay, id, page_type, stop_button_text, sound)
  } else {

    if (sound === "piano" | sound === "voice_doo" | sound === "voice_daa") {
        note_list = note_list.map(x => x-12);
    }

    var freq_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());
    var last_note = freq_list.length;
    var count = 0;

    if(dur_list === null) {
      playSeqArrhythmic(freq_list, dur_list, count, sound, last_note, page_type, hidePlay, id, stop_button_text)
    } else {
      playSeqRhythmic(freq_list, dur_list, count, sound, last_note, page_type, hidePlay, id, stop_button_text)
    }
  }

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

function metronomeStart() {
		Tone.Transport.start();
}

function metronome () {
  // metronomeStart();

    // var player = new Tone.Player("./sounds/woodblock.wav").toMaster();
    // console.log(player)
		// Tone.Transport.bpm.value = bpm;
    //
		// Tone.Buffer.onload = function() {
		// 	//this will start the player on every quarter note
		// 	Tone.Transport.setInterval(function(time){
		// 	    player.start(time);
		// 	}, "4n");
		// 	//start the Transport for the events to start
		// 	Tone.Transport.start();
		// };
}

function toneJSPlay (midi, start_note, end_note, hidePlay, transpose, id, sound, bpm = 90) {

    // start timer

    window.startTime = new Date().getTime();

    //metronome();

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
          recordAndStop(null, true, hidePlay, id); }, dur + record_delay); // plus a little delay

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
    hidePlayButton();
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
      recordAndStop(null, true, true, id);
  }

  });

}

// Define a function to handle status messages

function playMidiFileAndRecordAfter(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm) {

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
  user_rating = document.getElementById("user_rating");
  user_rating.style.display = "none";
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

function myMain() {
  // NB: Give better name
  document.getElementById("response_ui").onclick = buton;
}
function buton(e) {
  if (e.target.tagName == 'BUTTON') {
        Shiny.onInputChange("user_rating", e.target.id);
  }
}

function hidePlayButton(play_button_id = "playButton") {

  var x = document.getElementById(play_button_id);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }

}

function hideAudioFilePlayer() {
  var player = document.getElementById("player");
  player.style.display = "none";
}


function recordAndStop (ms, showStop, hidePlay, id = null, type = "aws_pyin", stop_button_text = "Stop") {

    // start recording but then stop after x milliseconds
    window.startTime = new Date().getTime();

    if (type === "aws_pyin") {
      // aws record
      startRecording(updateUI = false);
    } else if(type === "crepe") {
      // crepe record
      initAudio();
      crepeResume();
    } else if(type === "record_midi_page") {
      instantiateMIDI(midi_device);
    }  else {
      console.log('type not recognised');
    }

     if (ms === null) {
        console.log('ms null');
        recordUpdateUI(showStop, hidePlay, type);
     } else {
        recordUpdateUI(showStop, hidePlay, type, stop_button_text);
        setTimeout(() => {  stopRecording();
        hideRecordImage(); }, ms);
     }

}

function recordUpdateUI(showStop, hidePlay, type = "aws_pyin", stop_button_text = "Stop") {

    if(['aws_pyin', 'crepe', 'record_audio_page', 'record_midi_page'].includes(type)) {
      // update the recording UI

      if(hidePlay) {
        hidePlayButton();
      }

      setTimeout(() => {  showRecordingIcon(); }, 500); // a little lag

      if (showStop) {
        setTimeout(function() {
        showStopButton(type, stop_button_text);
            }, 500); // a little more lag
      }
    }

}


function createCrepeStopButton (stop_button_text) {
  var stopButton = document.createElement("button");
  stopButton.style.display = "block";
  stopButton.style.textAlign = "center";
  stopButton.classList.add("btn", "btn-default", "action-button");
  stopButton.innerText = stop_button_text;
  stopButton.addEventListener("click", function () {
        crepeStop();
        next_page();
      });
  var button_area = document.getElementById("button_area");
  button_area.appendChild(stopButton);
}

function hideLoading() {
  var loading = document.getElementById("loading");
  if (loading !== undefined) {
    loading.style.visibility = 'hidden';
  }
}

function createCorrectStopButton(type) {

  stopButton.disabled = false;
  stopButton.style.visibility = 'visible';

  stopButton.onclick = function () {
    //next_page();
    if(show_happy_with_response) {
      show_happy_with_response_message();
    }
    if(type === "aws_pyin") {
      stopRecording();
    } else if(type === "record_midi_page") {
      WebMidi.disable();
      hideButtonAreaShowUserRating();
    } else {
      console.log('Unknown page type');
    }
  };
}

function showStopButton(type = 'aws_pyin', stop_button_text = "Stop") {

        if(type === "crepe") {
          createCrepeStopButton(stop_button_text);
        } else {
          if(type === "aws_pyin") {
            startRecording(updateUI = false);
            hideLoading();
          }
          var stopButton = document.getElementById("stopButton");

          if(stopButton !== undefined) {
            createCorrectStopButton(type);
          }
        }

}

function showRecordingIcon() {

  var img = document.createElement("img");
  img.style.display = "block";
  img.src =  "https://adaptiveeartraining.com/magmaGold/img/record.gif";
  img.width = "280";
  img.height = "280";

  var button_area = document.getElementById("button_area");
  button_area.appendChild(img);

}

function hideRecordButton() {
  var x = document.getElementById("recordButton");
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}

function hideRecordImage() {
  console.log('hideRecordImage called');
  var x = document.getElementById("button_area");
  if (x.style.display === "none") {
    x.style.display = "block";
   } else {
    x.style.display = "none";
   }
}

function toggleRecording(e) {
    if (e.classList.contains("recording")) {
      e.classList.remove("recording");
    } else {
      e.classList.add("recording");
    }
}

// utils

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


// checks

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

    console.log(testMediaRecorder());
    console.log(Modernizr.webaudio);

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
  console.log(isMediaRecorderSupported);
  return(isMediaRecorderSupported);
}

// audio recorder stuff (previously in app.js)

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

//Delete all nodes in a html element
function empty(id){
	parent = document.querySelector(id);
    while (parent.firstChild) {
        parent.removeChild(parent.firstChild);
    }
}

function startRecording(updateUI = true) {
	empty("#csv_file")
	empty("#loading")
	empty("#recordingsList")
    var constraints = { audio: true, video:false }

  if(updateUI) {

 	/*
    	Disable the record button until we get a success or fail from getUserMedia()
	*/
	stopButton.style.visibility = 'visible';
	recordButton.disabled = true;
	stopButton.disabled = false;
	pauseButton.disabled = false

  }

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
		document.getElementById("formats").innerHTML="Format: 1 channel pcm @ "+audioContext.sampleRate/1000+"kHz"

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
	  	//enable the record button if getUserMedia() fails
    	recordButton.disabled = false;
    	stopButton.disabled = true;
    	pauseButton.disabled = true
	});
}

function pauseRecording(){
	console.log("pauseButton clicked rec.recording=",rec.recording );
	if (rec.recording){
		//pause
		rec.stop();
		pauseButton.innerHTML="Resume";
	} else{
		//resume
		rec.record();
		pauseButton.innerHTML="Pause";

	}
}

function hideButtonAreaShowUserRating() {
  button_area = document.getElementById("button_area");
	button_area.style.display = "none";
	stop_button = document.getElementById("stopButton");
	stop_button.style.display = "none";
	user_rating = document.getElementById("user_rating");
	user_rating.style.display = "block";
}

function stopRecording() {

  hideButtonAreaShowUserRating();

	console.log("simpleStopButton clicked");

	//tell the recorder to stop the recording
	rec.stop();

	//stop microphone access
	gumStream.getAudioTracks()[0].stop();

  if(typeof musicassessr_state === 'undefined') { // if it's undefined, assume in production
    var musicassessr_state = "production";
  }

	//create the wav blob and pass it on to createDownloadLink
	if(musicassessr_state === "production") {
	  rec.exportWAV(upload_file_to_s3);
  } else {
    rec.exportWAV(upload_file_to_s3_local);
  }
}


function create_recordkey() {
  var currentDate = new Date();
  var recordkey = currentDate.getDate().toString() + '-' + (currentDate.getMonth() + 1 ).toString() + '-' + currentDate.getFullYear().toString() + '--' + currentDate.getHours().toString() + '-' + currentDate.getMinutes()  + '--' + currentDate.getSeconds().toString();
  return(recordkey)
}

function upload_file_to_s3(blob) {

  var recordkey = create_recordkey();

  var file_url = "/files/" + recordkey + ".wav"; // remote / production
  console.log(file_url);


	var xhr = new XMLHttpRequest();
	var filename = new Date().toISOString();
	var fd = new FormData();
	fd.append("audio_data",blob, recordkey);
	xhr.open("POST","/api/store_audio",true); // production
	xhr.send(fd);

    Shiny.setInputValue("key", recordkey);
    Shiny.setInputValue("file_url", file_url);

	xhr.onload = () => { console.log(xhr.responseText)
		// call next page after credentials saved
		spinner=document.getElementsByClassName("hollow-dots-spinner");
		spinner[0].style.display="none";
		file_is_ready = true;
		if(auto_next_page) {
			next_page();
			}
	};
}


function upload_file_to_s3_local(blob) {

  var recordkey = create_recordkey();

  var file_url = "/Users/sebsilas/aws-musicassessr-local-file-upload/files/" + recordkey + ".wav"; // remote / production
  console.log(file_url);

	var xhr=new XMLHttpRequest();
	var filename = new Date().toISOString();
	var fd=new FormData();
	fd.append("audio_data",blob, recordkey);
	xhr.open("POST","http://localhost:3000/upload-audio",true); // local
	xhr.send(fd);

  Shiny.setInputValue("key", recordkey);
  Shiny.setInputValue("file_url", file_url);

	xhr.onload = () => { console.log(xhr.responseText)
		// call next page after credentials saved
		spinner = document.getElementsByClassName("hollow-dots-spinner");
		spinner[0].style.display="none";
		file_is_ready = true;
		if(auto_next_page) {
			next_page();
			}
	};
}
