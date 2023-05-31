console.log('microphone_signal_test.js imported');
var volumeMeterEl = document.getElementById('volumeMeter');
var startButtonEl = document.getElementById('startButton');
var nextButtonEl = document.getElementById('nextButton');

if(startButtonEl !== null) {

  startButtonEl.onclick = async () => {
    console.log('start button clicked');
    nextButtonEl.style.visibility = 'visible';
    startButtonEl.style.visibility = 'hidden';
    var stream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false });
    var audioContext = new AudioContext();
    var mediaStreamAudioSourceNode = audioContext.createMediaStreamSource(stream);
    var analyserNode = audioContext.createAnalyser();
    mediaStreamAudioSourceNode.connect(analyserNode);

    var pcmData = new Float32Array(analyserNode.fftSize);
    var onFrame = () => {
        analyserNode.getFloatTimeDomainData(pcmData);
        let sumSquares = 0.0;
        for (var amplitude of pcmData) { sumSquares += amplitude*amplitude; }
        var val = Math.sqrt(sumSquares / pcmData.length);

        /* Update the meter */
        if(volumeMeterEl.tagName == "IMG") {
          val_scaled = 10 * val;
          volumeMeterEl.style.filter = "alpha(opacity=" + val_scaled + ")";
          volumeMeterEl.style.opacity = val_scaled;
        } else {
          volumeMeterEl.value = val;
        }

        window.requestAnimationFrame(onFrame);
    };
    window.requestAnimationFrame(onFrame);
  };

}
