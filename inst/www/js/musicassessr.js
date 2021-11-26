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

// get amplitude info

var mediaStreamSource = null
var meter = null

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


function createCrepeStopButton () {
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
          createCrepeStopButton();
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

// crepe stuff


function crepe() {
  function error(message) {
    var status = document.getElementById("status");
    if(status) {
      document.getElementById('status').innerHTML = 'Error: ' + message;
    }
    return message;
  }

  window.audioContext;
  var running = false;

  try {
    const AudioContext = window.AudioContext || window.webkitAudioContext;
    audioContext = new AudioContext();
    var srate = document.getElementById("srate");
    if(srate) {
      document.getElementById('srate').innerHTML = audioContext.sampleRate;
    }
  } catch (e) {
    error('Could not instantiate AudioContext: ' + e.message);
    throw e;
  }

  // amplitude detection
  beginDetect();
}


// a function that accepts the activation vector for each frame
var updateActivation = (function() {
  const inferno = [ // the 'inferno' colormap from matplotlib
    [  0,  0,  3,255], [  0,  0,  4,255], [  0,  0,  6,255], [  1,  0,  7,255], [  1,  1,  9,255], [  1,  1, 11,255], [  2,  1, 14,255], [  2,  2, 16,255],
    [  3,  2, 18,255], [  4,  3, 20,255], [  4,  3, 22,255], [  5,  4, 24,255], [  6,  4, 27,255], [  7,  5, 29,255], [  8,  6, 31,255], [  9,  6, 33,255],
    [ 10,  7, 35,255], [ 11,  7, 38,255], [ 13,  8, 40,255], [ 14,  8, 42,255], [ 15,  9, 45,255], [ 16,  9, 47,255], [ 18, 10, 50,255], [ 19, 10, 52,255],
    [ 20, 11, 54,255], [ 22, 11, 57,255], [ 23, 11, 59,255], [ 25, 11, 62,255], [ 26, 11, 64,255], [ 28, 12, 67,255], [ 29, 12, 69,255], [ 31, 12, 71,255],
    [ 32, 12, 74,255], [ 34, 11, 76,255], [ 36, 11, 78,255], [ 38, 11, 80,255], [ 39, 11, 82,255], [ 41, 11, 84,255], [ 43, 10, 86,255], [ 45, 10, 88,255],
    [ 46, 10, 90,255], [ 48, 10, 92,255], [ 50,  9, 93,255], [ 52,  9, 95,255], [ 53,  9, 96,255], [ 55,  9, 97,255], [ 57,  9, 98,255], [ 59,  9,100,255],
    [ 60,  9,101,255], [ 62,  9,102,255], [ 64,  9,102,255], [ 65,  9,103,255], [ 67, 10,104,255], [ 69, 10,105,255], [ 70, 10,105,255], [ 72, 11,106,255],
    [ 74, 11,106,255], [ 75, 12,107,255], [ 77, 12,107,255], [ 79, 13,108,255], [ 80, 13,108,255], [ 82, 14,108,255], [ 83, 14,109,255], [ 85, 15,109,255],
    [ 87, 15,109,255], [ 88, 16,109,255], [ 90, 17,109,255], [ 91, 17,110,255], [ 93, 18,110,255], [ 95, 18,110,255], [ 96, 19,110,255], [ 98, 20,110,255],
    [ 99, 20,110,255], [101, 21,110,255], [102, 21,110,255], [104, 22,110,255], [106, 23,110,255], [107, 23,110,255], [109, 24,110,255], [110, 24,110,255],
    [112, 25,110,255], [114, 25,109,255], [115, 26,109,255], [117, 27,109,255], [118, 27,109,255], [120, 28,109,255], [122, 28,109,255], [123, 29,108,255],
    [125, 29,108,255], [126, 30,108,255], [128, 31,107,255], [129, 31,107,255], [131, 32,107,255], [133, 32,106,255], [134, 33,106,255], [136, 33,106,255],
    [137, 34,105,255], [139, 34,105,255], [141, 35,105,255], [142, 36,104,255], [144, 36,104,255], [145, 37,103,255], [147, 37,103,255], [149, 38,102,255],
    [150, 38,102,255], [152, 39,101,255], [153, 40,100,255], [155, 40,100,255], [156, 41, 99,255], [158, 41, 99,255], [160, 42, 98,255], [161, 43, 97,255],
    [163, 43, 97,255], [164, 44, 96,255], [166, 44, 95,255], [167, 45, 95,255], [169, 46, 94,255], [171, 46, 93,255], [172, 47, 92,255], [174, 48, 91,255],
    [175, 49, 91,255], [177, 49, 90,255], [178, 50, 89,255], [180, 51, 88,255], [181, 51, 87,255], [183, 52, 86,255], [184, 53, 86,255], [186, 54, 85,255],
    [187, 55, 84,255], [189, 55, 83,255], [190, 56, 82,255], [191, 57, 81,255], [193, 58, 80,255], [194, 59, 79,255], [196, 60, 78,255], [197, 61, 77,255],
    [199, 62, 76,255], [200, 62, 75,255], [201, 63, 74,255], [203, 64, 73,255], [204, 65, 72,255], [205, 66, 71,255], [207, 68, 70,255], [208, 69, 68,255],
    [209, 70, 67,255], [210, 71, 66,255], [212, 72, 65,255], [213, 73, 64,255], [214, 74, 63,255], [215, 75, 62,255], [217, 77, 61,255], [218, 78, 59,255],
    [219, 79, 58,255], [220, 80, 57,255], [221, 82, 56,255], [222, 83, 55,255], [223, 84, 54,255], [224, 86, 52,255], [226, 87, 51,255], [227, 88, 50,255],
    [228, 90, 49,255], [229, 91, 48,255], [230, 92, 46,255], [230, 94, 45,255], [231, 95, 44,255], [232, 97, 43,255], [233, 98, 42,255], [234,100, 40,255],
    [235,101, 39,255], [236,103, 38,255], [237,104, 37,255], [237,106, 35,255], [238,108, 34,255], [239,109, 33,255], [240,111, 31,255], [240,112, 30,255],
    [241,114, 29,255], [242,116, 28,255], [242,117, 26,255], [243,119, 25,255], [243,121, 24,255], [244,122, 22,255], [245,124, 21,255], [245,126, 20,255],
    [246,128, 18,255], [246,129, 17,255], [247,131, 16,255], [247,133, 14,255], [248,135, 13,255], [248,136, 12,255], [248,138, 11,255], [249,140,  9,255],
    [249,142,  8,255], [249,144,  8,255], [250,145,  7,255], [250,147,  6,255], [250,149,  6,255], [250,151,  6,255], [251,153,  6,255], [251,155,  6,255],
    [251,157,  6,255], [251,158,  7,255], [251,160,  7,255], [251,162,  8,255], [251,164, 10,255], [251,166, 11,255], [251,168, 13,255], [251,170, 14,255],
    [251,172, 16,255], [251,174, 18,255], [251,176, 20,255], [251,177, 22,255], [251,179, 24,255], [251,181, 26,255], [251,183, 28,255], [251,185, 30,255],
    [250,187, 33,255], [250,189, 35,255], [250,191, 37,255], [250,193, 40,255], [249,195, 42,255], [249,197, 44,255], [249,199, 47,255], [248,201, 49,255],
    [248,203, 52,255], [248,205, 55,255], [247,207, 58,255], [247,209, 60,255], [246,211, 63,255], [246,213, 66,255], [245,215, 69,255], [245,217, 72,255],
    [244,219, 75,255], [244,220, 79,255], [243,222, 82,255], [243,224, 86,255], [243,226, 89,255], [242,228, 93,255], [242,230, 96,255], [241,232,100,255],
    [241,233,104,255], [241,235,108,255], [241,237,112,255], [241,238,116,255], [241,240,121,255], [241,242,125,255], [242,243,129,255], [242,244,133,255],
    [243,246,137,255], [244,247,141,255], [245,248,145,255], [246,250,149,255], [247,251,153,255], [249,252,157,255], [250,253,160,255], [252,254,164,255],
    [252,254,164,255]
  ];

  // store the array for
  for (var i = 0; i < inferno.length; i++) {
    array = new Uint8ClampedArray(4);
    array.set(inferno[i]);
    inferno[i] = array;
  }

  const canvas = document.getElementById('activation');

  if(canvas !== null) {
    const ctx = canvas.getContext('2d');
      const buffer = ctx.createImageData(canvas.width,canvas.height);
      var column = 0;


  return function(activation) {
    for (var i = 0; i < 360; i++) {
      value = Math.floor(activation[i] * 256.0);
      if (isNaN(value) || value < 0) value = 0;
      if (value > 256) value = 1;
      buffer.data.set(inferno[value], ((canvas.height - 1 - i) * canvas.width + column) * 4);
    }

    column = (column + 1) % canvas.width;
    ctx.putImageData(buffer, canvas.width - column, 0);
    ctx.putImageData(buffer, -column, 0);
  };
  }
})();

// bin number -> cent value mapping
var cent_mapping = tf.add(tf.linspace(0, 7180, 360), tf.tensor(1997.3794084376191));


// perform resampling the audio to 16000 Hz, on which the model is trained.
// setting a sample rate in AudioContext is not supported by most browsers at the moment.
function resample(audioBuffer, onComplete) {
  const interpolate = (audioBuffer.sampleRate % 16000 != 0);
  const multiplier = audioBuffer.sampleRate / 16000;
  const original = audioBuffer.getChannelData(0);
  const subsamples = new Float32Array(1024);
  for (var i = 0; i < 1024; i++) {
    if (!interpolate) {
      subsamples[i] = original[i * multiplier];
    } else {
      // simplistic, linear resampling
      var left = Math.floor(i * multiplier);
      var right = left + 1;
      var p = i * multiplier - left;
      subsamples[i] = (1 - p) * original[left] + p * original[right];
    }
  }
  onComplete(subsamples);
}


function volumeAudioProcess(event) {
  const buf = event.inputBuffer.getChannelData(0);
  const bufLength = buf.length;
  let sum = 0;
  let x;

  // Do a root-mean-square on the samples: sum up the squares...
  for (var i = 0; i < bufLength; i++) {
    x = buf[i]
    if (Math.abs(x) >= this.clipLevel) {
        this.clipping = true
        this.lastClip = window.performance.now()
    }
    sum += x * x
  }

  // ... then take the square root of the sum.
  const rms = Math.sqrt(sum / bufLength);

  // Now smooth this out with the averaging factor applied
  // to the previous sample - take the max here because we
  // want "fast attack, slow release."

  var vol = Math.max(rms, this.volume * this.averaging);
  this.volume = vol;
  //document.getElementById('audio-value').innerHTML = this.volume;
}

function volumeAudioProcess_SJS(buf) {

  const bufLength = buf.length;

  let sum = 0;
  let x;

  // Do a root-mean-square on the samples: sum up the squares...
  for (var i = 0; i < bufLength; i++) {
    x = buf[i]
    if (Math.abs(x) >= this.clipLevel) {
        this.clipping = true
        this.lastClip = window.performance.now()
    }
    sum += x * x
  }

  // ... then take the square root of the sum.
  const rms = Math.sqrt(sum / bufLength);

  //console.log(rms);

  // Now smooth this out with the averaging factor applied
  // to the previous sample - take the max here because we
  // want "fast attack, slow release."

  //var vol = Math.max(rms, this.volume * this.averaging);


  return(rms)
}

function process_microphone_buffer(event) {

  resample(event.inputBuffer, function(resampled) {

    tf.tidy(() => {
      running = true;

      // run the prediction on the model
      const frame = tf.tensor(resampled.slice(0, 1024));
      const zeromean = tf.sub(frame, tf.mean(frame));
      const framestd = tf.tensor(tf.norm(zeromean).dataSync()/Math.sqrt(1024));
      const normalized = tf.div(zeromean, framestd);
      const input = normalized.reshape([1, 1024]);
      const activation = model.predict([input]).reshape([360]);

      // the confidence of voicing activity and the argmax bin
      const confidence = activation.max().dataSync()[0];
      const confidence_rounded = Number(confidence.toFixed(3));
      const center = activation.argMax().dataSync()[0];

      var voicing_confidence = document.getElementById('voicing-confidence');

      if (voicing_confidence) {
        document.getElementById('voicing-confidence').innerHTML = confidence.toFixed(3);
      }

      // slice the local neighborhood around the argmax bin
      const start = Math.max(0, center - 4);
      const end = Math.min(360, center + 5);
      const weights = activation.slice([start], [end - start]);
      const cents = cent_mapping.slice([start], [end - start]);

      // take the local weighted average to get the predicted pitch
      const products = tf.mul(weights, cents);
      const productSum = products.dataSync().reduce((a, b) => a + b, 0);
      const weightSum = weights.dataSync().reduce((a, b) => a + b, 0);
      const predicted_cent = productSum / weightSum;
      const predicted_hz = 10 * Math.pow(2, predicted_cent / 1200.0);
      const predicted_hz_rounded = Number(predicted_hz.toFixed(3));


      // update the UI and the activation plot
      var result = (confidence_rounded > minConfidence) ? predicted_hz_rounded + ' Hz' : '&nbsp;no voice&nbsp&nbsp;';
      var strlen = result.length;
      for (var i = 0; i < 11 - strlen; i++) result = "&nbsp;" + result;

      var estimated_pitch = document.getElementById('estimated-pitch');

      if(estimated_pitch) {
        document.getElementById('estimated-pitch').innerHTML = result;
      }

      if(updateActivation) {
        updateActivation(activation.dataSync());
      }

      // get time elapsed
      var responseTime = new Date().getTime();

      if (typeof window.startTime !== 'undefined') {
        // startTime is defined
        var timeElapsed = Math.abs(startTime - responseTime);
      }

      var curRMS = volumeAudioProcess_SJS(resampled);
      //console.log(curRMS);

      // record results
      if (confidence_rounded > minConfidence &&  predicted_hz_rounded < highestAllowedFreq && predicted_hz_rounded > lowestAllowedFreq && curRMS > 0.1)
      {
        //console.log('aafafs');
        //console.log(predicted_hz_rounded);
        //console.log(confidence_rounded);

        user_response_frequencies.push(predicted_hz_rounded);
        confidences.push(confidence.toFixed(3));
        timecodes.push(timeElapsed);
        rmses.push(curRMS);

      // send to Shiny
      Shiny.setInputValue("user_response_frequencies", JSON.stringify(user_response_frequencies));
      Shiny.setInputValue("confidences", JSON.stringify(confidences));
      Shiny.setInputValue("timecodes", JSON.stringify(timecodes));
      Shiny.setInputValue("rmses", JSON.stringify(rmses));
      }

    });
  });
}

function initAudio() {
  if (navigator.mediaDevices === undefined) {
    navigator.mediaDevices = {};
  }

  // Some browsers partially implement mediaDevices. We can't just assign an object
  // with getUserMedia as it would overwrite existing properties.
  // Here, we will just add the getUserMedia property if it's missing.
  if (navigator.mediaDevices.getUserMedia === undefined) {
    navigator.mediaDevices.getUserMedia = function(constraints) {

      // First get ahold of the legacy getUserMedia, if present
      var getUserMedia = navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

      // Some browsers just don't implement it - return a rejected promise with an error
      // to keep a consistent interface
      if (!getUserMedia) {
        return Promise.reject(new Error('getUserMedia is not implemented in this browser'));
      }

      // Otherwise, wrap the call to the old navigator.getUserMedia with a Promise
      return new Promise(function(resolve, reject) {
        getUserMedia.call(navigator, constraints, resolve, reject);
      });
    }
  }

  navigator.mediaDevices.getUserMedia({ audio: true})
  .then(function(stream) {
    status('Setting up AudioContext ...');
    console.log('Audio context sample rate = ' + audioContext.sampleRate);
    const mic = audioContext.createMediaStreamSource(stream);

    // We need the buffer size that is a power of two and is longer than 1024 samples when resampled to 16000 Hz.
    // In most platforms where the sample rate is 44.1 kHz or 48 kHz, this will be 4096, giving 10-12 updates/sec.
    const minBufferSize = audioContext.sampleRate / 16000 * 1024;
    for (var bufferSize = 4; bufferSize < minBufferSize; bufferSize *= 2);
    console.log('Buffer size = ' + bufferSize);
    const scriptNode = audioContext.createScriptProcessor(bufferSize, 1, 1);
    scriptNode.onaudioprocess = process_microphone_buffer;

    // It seems necessary to connect the stream to a sink for the pipeline to work, contrary to documentataions.
    // As a workaround, here we create a gain node with zero gain, and connect temp to the system audio output.
    const gain = audioContext.createGain();
    gain.gain.setValueAtTime(0, audioContext.currentTime);

    mic.connect(scriptNode);
    scriptNode.connect(gain);
    gain.connect(audioContext.destination);

    if (audioContext.state === 'running') {
      status('Running ...');
    } else {
      // user gesture (like click) is required to start AudioContext, in some browser versions
      status('<a href="javascript:crepe.resume();" style="color:red;">* Click here to start the demo *</a>')
    }
  })
  .catch(function(err) {
    console.log(err.name + ": " + err.message);
  });
}



async function initTF() {
  try {
    status('Loading Keras model...');
    window.model = await tf.loadLayersModel('https://adaptiveeartraining.com/files/model/model.json');
    console.log('model loading complete');
    status('Model loading complete');
  } catch (e) {
    throw error(e);
    console.log('error loading model');
  }
  initAudio();
}




function status(message) {
  var status = document.getElementById('status');
  if(status) {
    document.getElementById('status').innerHTML = message;
  }
}

function crepeResume() {
  audioContext.resume();
  status('Running ...');
}

function crepeStop(next_page) {
  audioContext.suspend();
  status('Not running.');

  if(next_page) {
    Shiny.onInputChange("next_page", performance.now());
  }
}




// for microphone calibration page
function toggleRecording( e ) {
    console.log(e.classList);
    if (e.classList.contains("recording")) {
        // stop recording
        e.classList.remove("recording");
    } else {
        // start recording
        e.classList.add("recording");
    }
}

function beginDetect() {
  audioContext = window.audioContext;
  if (navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
    navigator.mediaDevices.getUserMedia({audio: true}).then((stream) => {
      window.mediaStreamSource = audioContext.createMediaStreamSource(stream)
      window.meter = createAudioMeter(audioContext)
      window.mediaStreamSource.connect(meter);
    })
  }
}

function createAudioMeter(audioContext, clipLevel, averaging, clipLag) {
  const processor = audioContext.createScriptProcessor(512)
  processor.onaudioprocess = volumeAudioProcess
  processor.clipping = false
  processor.lastClip = 0
  processor.volume = 0
  processor.clipLevel = clipLevel || 0.98
  processor.averaging = averaging || 0.95
  processor.clipLag = clipLag || 750

  // this will have no effect, since we don't copy the input to the output,
  // but works around a current Chrome bug.
  processor.connect(audioContext.destination)

  processor.checkClipping = function () {
    if (!this.clipping) {
      return false
    }
    if ((this.lastClip + this.clipLag) < window.performance.now()) {
      this.clipping = false
    }
    return this.clipping
  }

  processor.shutdown = function () {
    this.disconnect();
    this.onaudioprocess = null;
  }

  return processor
}
