console.log('microphone_signal_test.js imported');

// Functions to get elements dynamically (for elements that may change)
function getVolumeMeterEl() {
  return document.getElementById('volumeMeter');
}
function getStartButtonEl() {
  return document.getElementById('startButton');
}
function getNextButtonEl() {
  return document.getElementById('nextButton');
}
function getLoadingEl() {
  return document.getElementById('loading');
}
function getHollowDotsSpinnerEl() {
  return document.getElementById('hollowDotsSpinner');
}

// Store persistent variables in `window`
window.stream = window.stream || null;

// Store thresholds and tracking variables globally
window.lowThreshold = window.lowThreshold ?? 0.05;
window.optimumThreshold = window.optimumThreshold ?? 0.15;
window.moderateThreshold = window.moderateThreshold ?? 0.4;
window.highThreshold = window.highThreshold ?? 0.50;
window.maxThreshold = window.maxThreshold ?? 0.6;
window.lastWarningTime = window.lastWarningTime ?? 0; // Tracks the last warning time for cooldown

// Ensure elements exist before modifying them
if (getLoadingEl()) getLoadingEl().style.display = "none";
if (getHollowDotsSpinnerEl()) getHollowDotsSpinnerEl().style.display = "none";

if (getStartButtonEl()) {
  getStartButtonEl().onclick = () => {
    if (getLoadingEl()) getLoadingEl().style.display = "block";
    if (getHollowDotsSpinnerEl()) getHollowDotsSpinnerEl().style.display = "block";
    if (getStartButtonEl()) getStartButtonEl().style.display = "none";

    console.log('start button clicked');

    function getMediaStream(callback) {
      navigator.mediaDevices.getUserMedia({ audio: true, video: false })
        .then(mediaStream => {
          window.stream = mediaStream; // Assign globally
          callback(null, mediaStream);
        })
        .catch(error => {
          callback(error, null);
        });
    }

    getMediaStream((error, mediaStream) => {
      if (error) {
        console.error("Error accessing media devices:", error);
        return;
      }
      console.log("Media stream is ready:", mediaStream);

      let audioContext = new AudioContext();
      let mediaStreamAudioSourceNode = audioContext.createMediaStreamSource(window.stream);
      let analyserNode = audioContext.createAnalyser();
      mediaStreamAudioSourceNode.connect(analyserNode);

      if (getNextButtonEl()) getNextButtonEl().style.visibility = 'visible';
      if (getStartButtonEl()) getStartButtonEl().style.visibility = 'hidden';
      if (getLoadingEl()) getLoadingEl().style.display = "none";
      if (getHollowDotsSpinnerEl()) getHollowDotsSpinnerEl().style.display = "none";

      let pcmData = new Float32Array(analyserNode.fftSize);

      let onFrame = () => {
        analyserNode.getFloatTimeDomainData(pcmData);

        let sumSquares = 0.0;
        for (let amplitude of pcmData) {
          sumSquares += amplitude * amplitude;
        }
        let rms = Math.sqrt(sumSquares / pcmData.length);

        const volumeMeterEl = getVolumeMeterEl();
        if (volumeMeterEl && volumeMeterEl.tagName === "METER") {
          volumeMeterEl.value = rms;

          if (rms < window.lowThreshold) {
            volumeMeterEl.setAttribute("low", window.lowThreshold);
            volumeMeterEl.setAttribute("optimum", window.optimumThreshold);
          } else if (rms < window.moderateThreshold) {
            volumeMeterEl.setAttribute("low", window.lowThreshold);
            volumeMeterEl.setAttribute("high", window.moderateThreshold);
          } else if (rms < window.highThreshold) {
            volumeMeterEl.setAttribute("high", window.highThreshold);
            volumeMeterEl.setAttribute("max", window.maxThreshold);
          } else {
            volumeMeterEl.setAttribute("max", window.maxThreshold);

            const currentTime = Date.now();
            if (currentTime - window.lastWarningTime > 5000 && page_type === "record_audio_page") {
              alert("Warning: Microphone signal is too loud! Please reduce microphone gain or move away from the microphone.");
              window.lastWarningTime = currentTime;
            }
          }
        } else if (volumeMeterEl && volumeMeterEl.tagName === "IMG") {
          let scaledOpacity = Math.min(1, rms);
          volumeMeterEl.style.opacity = scaledOpacity;
        }

        window.requestAnimationFrame(onFrame);
      };

      window.requestAnimationFrame(onFrame);
    });
  };
}
