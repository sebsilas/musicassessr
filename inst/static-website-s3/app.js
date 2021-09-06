//webkitURL is deprecated but nevertheless
URL = window.URL || window.webkitURL;
//
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

function stopRecording() {
	console.log("stopButton clicked");

	//disable the stop button, enable the record too allow for new recordings
	stopButton.disabled = true;
	recordButton.disabled = true;
	pauseButton.disabled = true;
	recordButton.innerHTML = "Next";

	//reset button just in case the recording is stopped while paused
	pauseButton.innerHTML="Pause";

	//tell the recorder to stop the recording
	rec.stop();

	//stop microphone access
	gumStream.getAudioTracks()[0].stop();


	rec.exportWAV(upload_file_to_s3);
}


function simpleStopRecording() {
	console.log("simpleStopButton clicked!");

	//tell the recorder to stop the recording
	rec.stop();

	//stop microphone access
	gumStream.getAudioTracks()[0].stop();



	rec.exportWAV(upload_file_to_s3);
}


function create_recordkey() {
  var currentDate = new Date();
  var recordkey = currentDate.getDate().toString() + '-' + currentDate.getMonth().toString() + '-' + currentDate.getFullYear().toString() + '--' + currentDate.getHours().toString() + '-' + currentDate.getMinutes()  + '--' + currentDate.getSeconds().toString();
  return(recordkey)
}

function upload_file_to_s3(blob){

  var recordkey = create_recordkey();

  var file_url = "https://"+bucketName+".s3.amazonaws.com/"+recordkey;
  console.log(file_url);

	var xhr=new XMLHttpRequest();
	var filename = new Date().toISOString();
	var fd=new FormData();
	fd.append("audio_data",blob, recordkey);
	xhr.open("POST","/api/store_audio",true);
	xhr.send(fd);

	Shiny.setInputValue("sourceBucket", bucketName);
    Shiny.setInputValue("key", recordkey);
    Shiny.setInputValue("file_url", file_url);
    Shiny.setInputValue("destBucket", destBucket);

	xhr.onload = () => { console.log(xhr.responseText)
		// call next page after credentials saved

		if(auto_next_page) {
		}
	};


}

// async function getFile(recordkey) {
// 	let response = await fetch(api_url,{ method: 'POST', body: JSON.stringify({"sourceBucket":bucketName,"key":recordkey,"destBucket":destBucket}) })
// 	let csv_file = await response.json()
// 	var link = document.createElement('a');

// 	link.href = "https://"+csv_file.Bucket+".s3.amazonaws.com/"+csv_file.key;
// 	link.download = true
// 	link.innerHTML = "csv file";

// 	var div = document.getElementById('csv_file');
//   var loading = document.getElementById("loading");

//   if (loading) {
//     loading.innerHTML="<p>Processing complete successfully</p>";
//   }

//   if (typeof recordButton !== 'undefined') {
//     recordButton.disabled = false;
//   }

//   if (div) {
//     div.appendChild(link);
//     parseCsvFile(link.href)
//   }

// }


// function createDownloadLink(key) {

// 	var au = document.createElement('audio');
// 	var li = document.createElement('li');
// 	var link = document.createElement('a');

// 	//name of .wav file to use during upload and download (without extendion)

// 	//add controls to the <audio> element
// 	au.controls = true;
// 	au.src =  "https://"+bucketName+".s3.amazonaws.com/"+key;

// 	//save to disk link
// 	link.href = "https://"+bucketName+".s3.amazonaws.com/"+key;
// 	link.download = true
// 	link.innerHTML = "Save to disk";

// 	//add the new audio element to li
// 	li.appendChild(au);

// 	//add the filename to the li
// 	// li.appendChild(document.createTextNode(filename+".wav "))

// 	//add the save to disk link to li
// 	li.appendChild(link);
// 	if(typeof recordingsList !== 'undefined') {
// 	  recordingsList.appendChild(li);
// 	}

// }

// //Export 3rd column of the csv file

// async function parseCsvFile(filename){
// 	let response = await fetch(filename,{ method: 'GET'})
// 	let csv_file = response
// 	//Convert each line to array and append each 3 element to a new array
// 	csv_file.text().then(text => { let result=text.split(/\n/).map(lineStr => lineStr.split(",")[2]).filter(item => item);
// 	  Shiny.setInputValue("user_response_notes", JSON.stringify(result)); // SJS
// 		var div = document.getElementById('csv_file');
// 		p=document.createElement('p');
// 		p.innerHTML=JSON.stringify(result);
// 		if(div) {
// 		  div.appendChild(p);
// 		}
// 	})

// }
