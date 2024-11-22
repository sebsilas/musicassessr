console.log('microphone_signal_test.js imported');
const volumeMeterEl = document.getElementById('volumeMeter');
const startButtonEl = document.getElementById('startButton');
const nextButtonEl = document.getElementById('nextButton');
const loading = document.getElementById('loading');
const hollowDotsSpinner = document.getElementById('hollowDotsSpinner');
loading.style.display = "none";
hollowDotsSpinner.style.display = "none";

let stream; // Declare stream variable to store the media stream

// Thresholds for color-coding and warning
const lowThreshold = 0.05;
const optimumThreshold = 0.15;
const moderateThreshold = 0.4;
const highThreshold = 0.50;
const maxThreshold = 0.6
let lastWarningTime = 0; // Tracks the last warning time for cooldown

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

        // Calculate RMS
        let sumSquares = 0.0;
        for (let amplitude of pcmData) {
          sumSquares += amplitude * amplitude;
        }
        let rms = Math.sqrt(sumSquares / pcmData.length);

        // Update the volume meter value directly
        if (volumeMeterEl.tagName === "METER") {
          volumeMeterEl.value = rms;

          // Color-coding by changing the meter attributes based on thresholds
          if (rms < lowThreshold) {
            volumeMeterEl.setAttribute("low", lowThreshold);
            volumeMeterEl.setAttribute("optimum", optimumThreshold);
          } else if (rms < moderateThreshold) {
            volumeMeterEl.setAttribute("low", lowThreshold);
            volumeMeterEl.setAttribute("high", moderateThreshold);
          } else if (rms < highThreshold) {
            volumeMeterEl.setAttribute("high", highThreshold);
            volumeMeterEl.setAttribute("max", maxThreshold);
          } else {
            // Set red level and trigger warning if above maxThreshold
            volumeMeterEl.setAttribute("max", maxThreshold);

            // Trigger warning if cooldown has elapsed
            const currentTime = Date.now();
            if (currentTime - lastWarningTime > 5000) { // 5-second cooldown
              alert("Warning: Microphone signal is too loud! Please reduce microphone gain or move away from the microphone.");
              lastWarningTime = currentTime; // Reset warning time
            }
          }
        } else if (volumeMeterEl.tagName === "IMG") {
          // For image type volume meter, only adjust opacity based on RMS
          let scaledOpacity = Math.min(1, rms); // Cap opacity at 1
          volumeMeterEl.style.opacity = scaledOpacity;
        }

        window.requestAnimationFrame(onFrame);
      };

      window.requestAnimationFrame(onFrame);
    });
  };
}
