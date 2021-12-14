console.log('microphone_signal_test.js imported');
const volumeMeterEl = document.getElementById('volumeMeter');
const startButtonEl = document.getElementById('startButton');
const nextButtonEl = document.getElementById('nextButton');

startButtonEl.onclick = async () => {
  console.log('start button clicked');
  nextButtonEl.style.visibility = 'visible';
  startButtonEl.style.visibility = 'hidden';
  const stream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false });
  const audioContext = new AudioContext();
  const mediaStreamAudioSourceNode = audioContext.createMediaStreamSource(stream);
  const analyserNode = audioContext.createAnalyser();
  mediaStreamAudioSourceNode.connect(analyserNode);

  const pcmData = new Float32Array(analyserNode.fftSize);
  const onFrame = () => {
      analyserNode.getFloatTimeDomainData(pcmData);
      let sumSquares = 0.0;
      for (const amplitude of pcmData) { sumSquares += amplitude*amplitude; }
      volumeMeterEl.value = Math.sqrt(sumSquares / pcmData.length);
      window.requestAnimationFrame(onFrame);
  };
  window.requestAnimationFrame(onFrame);
};
