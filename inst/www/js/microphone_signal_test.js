console.log('microphone_signal_test.js imported');
var volumeMeterEl = document.getElementById('volumeMeter');
var startButtonEl = document.getElementById('startButton');
var nextButtonEl = document.getElementById('nextButton');

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
      volumeMeterEl.value = Math.sqrt(sumSquares / pcmData.length);
      window.requestAnimationFrame(onFrame);
  };
  window.requestAnimationFrame(onFrame);
};
