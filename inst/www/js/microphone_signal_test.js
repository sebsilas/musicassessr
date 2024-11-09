console.log('microphone_signal_test.js imported');
const volumeMeterEl = document.getElementById('volumeMeter');
const startButtonEl = document.getElementById('startButton');
const nextButtonEl = document.getElementById('nextButton');
const loading = document.getElementById('loading');
const hollowDotsSpinner = document.getElementById('hollowDotsSpinner');
loading.style.display = "none";
hollowDotsSpinner.style.display = "none";

let stream; // Declare stream variable to store the media stream

if (startButtonEl !== null) {

  startButtonEl.onclick = () => {

    loading.style.display = "block";
    hollowDotsSpinner.style.display = "block";
    startButtonEl.style.display = "none";

    console.log('start button clicked');

    function getMediaStream(callback) {
      navigator.mediaDevices.getUserMedia({ audio: true, video: false })
        .then(mediaStream => {
          stream = mediaStream; // Assign the obtained stream to the outer variable
          callback(null, stream); // Call the callback with the stream
        })
        .catch(error => {
          callback(error, null); // Call the callback with the error if there's an issue
        });
    }

    getMediaStream((error, mediaStream) => {

      if (error) {
        console.error("Error accessing media devices:", error);
        return;
      }
      console.log("Media stream is ready:", mediaStream);
      // `stream` is also assigned now, and you can use it outside this callback

      let audioContext = new AudioContext();
      let mediaStreamAudioSourceNode = audioContext.createMediaStreamSource(stream);
      let analyserNode = audioContext.createAnalyser();
      mediaStreamAudioSourceNode.connect(analyserNode);
      nextButtonEl.style.visibility = 'visible';
      startButtonEl.style.visibility = 'hidden';
      loading.style.display = "none";
      hollowDotsSpinner.style.display = "none";

      let pcmData = new Float32Array(analyserNode.fftSize);
      let onFrame = () => {
        analyserNode.getFloatTimeDomainData(pcmData);
        let sumSquares = 0.0;
        for (let amplitude of pcmData) { sumSquares += amplitude * amplitude; }
        let val = Math.sqrt(sumSquares / pcmData.length);

        /* Update the meter */
        if (volumeMeterEl.tagName === "IMG") {
          let val_scaled = 10 * val;
          volumeMeterEl.style.filter = "alpha(opacity=" + val_scaled + ")";
          volumeMeterEl.style.opacity = val_scaled;
        } else {
          volumeMeterEl.value = val;
        }

        window.requestAnimationFrame(onFrame);
      };
      window.requestAnimationFrame(onFrame);
    });

  };

}
