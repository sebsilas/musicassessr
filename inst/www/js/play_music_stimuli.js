console.log("loaded play_music_stimuli.js");

//console.log(stimuli);
// grab stimuli from R and send it back to psychTestR
//Shiny.setInputValue("stimuli", JSON.stringify(stimuli));


// initialise toneJS
toneJSInit();

// a little delay after playback finishes before hitting record
var record_delay = 400;

// general functions

function diff(ary) {
    var newA = [];
    for (var i = 1; i < ary.length; i++)  newA.push(ary[i] - ary[i - 1]);
    newA.unshift(0); // pop a 0 on the front
    return newA;
}


/// playback stuff ///


// //  constants
var playback_count = 0; // number of times user presses play in a trial


// //  functions

function updatePlaybackCount() {

    playback_count =  playback_count + 1;

    var playbackTimes = [];

    // record playback values in time
    playbackTimes.push(Date.now());

    var playbackTimesDiff = diff(playbackTimes);
    var playbackTimesCumSum = [];
    playbackTimesDiff.reduce(function(a,b,i) { return playbackTimesCumSum[i] = a+b; },0);
    console.log(playbackTimesCumSum);

    Shiny.setInputValue("playback_count", playback_count);
    Shiny.setInputValue("playback_times", JSON.stringify(playbackTimesCumSum));

}

function toneJSInit() {

  // sound: i.e "tone" or "piano"

  console.log("toneJS Inited!");

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


  // create a piano and connect to master output
  window.piano = SampleLibrary.load({
    instruments: "piano",
    minify: true
   });

  window.piano.toMaster();

    // create a piano and connect to master output
  window.voice_doo = SampleLibrary.load({
    instruments: "voice_doo",
    minify: true
   });

  window.voice_doo.toMaster();

}



function triggerNote(sound, freq_tone, seconds, time) {

  console.log('sound is...');
  console.log(sound);

  if (sound === "piano") {
  	piano.triggerAttackRelease(freq_tone, seconds, time);
  }

  else if (sound === "voice_doo") {
  	voice_doo.triggerAttackRelease(freq_tone, seconds, time);
  }

  else {
    synth.triggerAttackRelease(freq_tone, seconds, time);
  }

}

function playTone(tone, seconds, id, sound, hidePlay = true, page_type = "aws_pyin", stop_button_text = "Stop", showStop = false) {
  // play a tone for x seconds

  console.log('playTone called');
  rangeTest(tone);

  tone = Number(tone);
  console.log(tone);

  var freq_tone = Tone.Frequency(tone, "midi").toNote();
  console.log(freq_tone);

  triggerNote(sound, freq_tone, seconds);

  setTimeout(() => {  recordAndStop(ms = seconds*1000,
                                    showStop = showStop, hidePlay = hidePlay, id = id, page_type = page_type, stop_button_text = stop_button_text); }, record_delay); // delay to avoid catching stimuli in recording


  updatePlaybackCount();

  Shiny.setInputValue("stimuli_pitch", tone);

}


function playTones (note_list) {

    console.log(note_list); // testing
    note_list.forEach(element => console.log(element)); // testing

    note_list = note_list.map(x => Tone.Frequency(x));
    last_note = note_list[note_list.length - 1];

    var pattern = new Tone.Sequence(function(time, note){
    synth.triggerAttackRelease(note, 0.5);
    console.log(note);

    if (note === last_note) {
      console.log("finished!");
    }

    }, note_list);


    pattern.start(0).loop = false;
    // begin at the beginning
    Tone.Transport.start();


}



function playSeq(note_list, hidePlay, id, sound, page_type, stop_button_text = "Stop", dur_list = null) {

  console.log(playSeq);
  console.log(sound);
  console.log(page_type);

  // this should go first before the piano editing:
  Shiny.setInputValue("stimuli_pitch", note_list);


  // seems to be a bug with the piano sound where it plays an octave higher

  if (sound === "piano") {
    note_list = note_list.map(x => x-12);
  }

  var freq_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());
  var last_note = freq_list.length;
  var count = 0;

  if(dur_list === null) {
    var pattern = new Tone.Sequence(function(time, note){
      console.log(note);
      triggerNote(sound, note, 0.50);
      count = count + 1;
      if (count === last_note & page_type !== 'null') {
        setTimeout(() => {  recordAndStop(null, true, hidePlay, id, page_type, stop_button_text); }, 0.50 + record_delay); // delay to avoid catching stimuli in recording
        pattern.stop();
        Tone.Transport.stop();
      }

    }, freq_list);
  } else {
    var notesAndDurations = bind_notes_and_durations(freq_list, dur_list);
    console.log(notesAndDurations);
    notesAndDurations = notesAndDurations.map(timeFromDurations);
    console.log(notesAndDurations);
    var pattern = new Tone.Part((time, value) => {
                // the value is an object which contains both the note and the velocity
                if(sound === "voice_doo") {
                  console.log('voice_doo trigger!');
                  voice_doo.triggerAttackRelease(value.note, value.duration, time);
                } else {
                  console.log('piano trigger!');
                  piano.triggerAttackRelease(value.note, value.duration, time);
                }
                count = count + 1;
                  if (count === last_note & page_type !== 'null') {
                    setTimeout(() => {  recordAndStop(null, true, hidePlay, id, page_type, stop_button_text); }, value.duration*1000 + record_delay); // delay to avoid catching stimuli in recording
                    pattern.stop();
                    Tone.Transport.stop();
                  }
                }, notesAndDurations);
  }

  pattern.start(0).loop = false;
  Tone.Transport.start();

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
        console.log(i);
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
    console.log('hey!');
    console.log(midi);

    // start timer

    window.startTime = new Date().getTime();
    console.log(window.startTime);

    //metronome();

    // change tempo to whatever defined in R
    adjusted_tempo = midi;
    adjusted_tempo.header.tempos.bpm = bpm;
    adjusted_tempo.header.tempos[0].bpm = bpm;
    console.log('tempo adjusted');
    console.log(adjusted_tempo);

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

        console.log(dur);

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

        console.log('poo');
        console.log(shiny_durations_round);

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

function playMidiFile(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm) {

    console.log("bpm is " + bpm);

    console.log(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm);

    // hide after play
    hidePlayButton();

    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS

    if (toneJS) {
      midiToToneJS(url, note_no, hidePlay, transpose, id, sound, bpm);
    }

    else {
    function display_message(mes) {
        console.log(mes);
    }

    MIDIjs.message_callback = display_message;
    MIDIjs.player_callback = display_time;

    console.log(MIDIjs.get_audio_status());

    MIDIjs.play(url);

    // Define a function to handle player events
    function display_time(ev) {

    console.log(ev.time); // time in seconds, since start of playback

    MIDIjs.get_duration(url,  function(seconds) { console.log("Duration: " + seconds);

    if (ev.time > seconds) {
        console.log("file finished!");
        MIDIjs.player_callback = null;
    }

    });

    }
    }

}


// Define a function to handle status messages

function playMidiFileAndRecordAfter(url, toneJS, start_note, end_note, hidePlay, id, transpose, sound, bpm) {


    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS

    if (toneJS) {
      midiToToneJS(url, start_note, end_note, hidePlay, transpose, id, sound, bpm);
    }

    else {
    function display_message(mes) {
        console.log(mes);
    }

    MIDIjs.message_callback = display_message;
    MIDIjs.player_callback = display_time;

    console.log(MIDIjs.get_audio_status());

    MIDIjs.play(url);

    // Define a function to handle player events
    function display_time(ev) {

    console.log(ev.time); // time in seconds, since start of playback

    MIDIjs.get_duration(url,  function(seconds) { console.log("Duration: " + seconds);

    if (ev.time > seconds) {
        console.log("file finished!");
        MIDIjs.player_callback = null;
        recordAndStop(null, true, true, id);
    }

    });

    }
    }

}


// ranges

// //  constants

var soprano = range(60, 84, 1);
var alto = range(53, 77, 1);
var tenor = range(48, 72, 1);
var baritone = range(45, 69, 1);
var bass = range(40, 64, 1);


// // functions

function range(start, stop, step) {
  var a = [start], b = start;
  while (b < stop) {
      a.push(b += step || 1);
  }
  return a;
}

function rangeTest(notes_list) {

    if (typeof notes_list == 'number') {
      notes_list = [notes_list];
    }

    notes_list.forEach(function(note) {

      if (soprano.includes(note) === true) {
        console.log("this comes in the soprano range!");
      }

      if (alto.includes(note) === true) {
        console.log("this comes in the alto range!");
      }

      if (tenor.includes(note) === true) {
        console.log("this comes in the tenor range!");
      }

      if (baritone.includes(note) === true) {
        console.log("this comes in the baritone range!");
      }

      if (bass.includes(note) === true) {
        console.log("this comes in the bass range!");
      }

  });

}

// UI functions

function hidePlayButton() {
  console.log('now we real!');
  var x = document.getElementById("playButton");
  if (x.style.display === "none") {
  x.style.display = "block";
  } else {
  x.style.display = "none";
  }

}


//

function recordAndStop (ms, showStop, hidePlay, id, type = "aws_pyin", stop_button_text = "Stop") {
    // start recording but then stop after x milliseconds
    console.log('recordAndStop called');
    window.startTime = new Date().getTime();

    if (type === "aws_pyin") {
      console.log('11');
      // aws record
      startRecording(updateUI = false);
    }
    else if(type === "crepe") {
      console.log('22');
      // crepe record
      initAudio();crepeResume();
    }

    else if(type === "record_midi_page") {
      instantiateMIDI(midi_device);
    }

    else {
      console.log('33');
    }


     if (ms === null) {
        console.log('ms null');
        recordUpdateUI(showStop, hidePlay, type);
     }

     else {
        console.log('ms not null');
        console.log(ms);
        recordUpdateUI(showStop, hidePlay, type, stop_button_text);
        setTimeout(() => {  simpleStopRecording();hideRecordImage(); }, ms);
     }

}

function recordUpdateUI(showStop, hidePlay, type = "aws_pyin", stop_button_text = "Stop") {

    if(['aws_pyin', 'crepe', 'record_audio_page', 'record_midi_page'].includes(type)) {
      // update the recording UI
      // if showStop is true, then give the user the option to press the stop button
      // if hidePlay is true, then hide the play button

      if(hidePlay) {
        console.log('hide play bitch!');
        hidePlayButton();
      }

      setTimeout(() => {  showRecordingIcon(); }, 500); // a little lag


      if (showStop) {
          setTimeout(() => {  showStopButton(type, stop_button_text = stop_button_text); }, 500); // a little more lag
      }
    }

}


function showStopButton(type = "aws_pyin", stop_button_text = "Stop") {

        if(type === "crepe" | type === "record_midi_page") {

          var stopButton = document.createElement("button");
          stopButton.style.display = "block";
          stopButton.classList.add("btn", "btn-default", "action-button");
          stopButton.innerText = stop_button_text;
          stopButton.addEventListener("click", function () {
              if(type === "crepe") {
                 // crepe
                crepeStop();
              }

              else if (type === "record_midi_page") {
                 WebMidi.disable();
                 var button_area = document.getElementById("button_area");
                 button_area.appendChild(stopButton);
              }
              else {
                //
              }
              next_page();
              });
          var button_area = document.getElementById("button_area");
          button_area.appendChild(stopButton);
        }

        else {
          console.log('here we go1 1!');

          startRecording(updateUI = false);
          if (recordButton !== undefined) {
            recordButton.style.display = 'none';
          }

          var loading = document.getElementById("loading");
          if (loading !== undefined) {
            loading.style.visibility = 'hidden';
          }

          var stopButton = document.getElementById("stopButton");

          if(stopButton !== undefined) {
            stopButton.disabled = false;
            stopButton.style.visibility = 'visible';

            stopButton.onclick = function () {
              //next_page();
              simpleStopRecording();
            };
          }
        }

}

function preloadImage(url) {
    var img = new Image();
    img.src = url;
}

preloadImage("https://adaptiveeartraining.com/magmaGold/img/record.gif");

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

// checks

function getUserInfo () {
    console.log(navigator);
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
