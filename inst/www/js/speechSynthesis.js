var synth = window.speechSynthesis;

var inputForm = document.querySelector('form');
var voiceSelect = document.querySelector('select');

var playButton = document.getElementById('playButton');


var pitch = document.querySelector('#pitch');
var pitchValue = document.querySelector('.pitch-value');
var rate = document.querySelector('#rate');
var rateValue = document.querySelector('.rate-value');

var voices = [];


if (typeof window.voice === "undefined") {
  console.log("no voice!");
}

else {
  console.log("voice here!");
  console.log(window.voice);
}

// the first voice is Alex, so instantiate with this. It will be updated if user changes
Shiny.setInputValue("selected_voice", "Alex");

function populateVoiceList() {
  voices = synth.getVoices().sort(function (a, b) {
      const aname = a.name.toUpperCase(), bname = b.name.toUpperCase();
      if ( aname < bname ) return -1;
      else if ( aname == bname ) return 0;
      else return +1;
  });
  var selectedIndex = voiceSelect.selectedIndex < 0 ? 0 : voiceSelect.selectedIndex;
  voiceSelect.innerHTML = '';
  for(i = 0; i < voices.length ; i++) {
    var option = document.createElement('option');
    option.textContent = voices[i].name + ' (' + voices[i].lang + ')';

    if(voices[i].default) {
      option.textContent += ' -- DEFAULT';
    }

    option.setAttribute('data-lang', voices[i].lang);
    option.setAttribute('data-name', voices[i].name);
    voiceSelect.appendChild(option);
  }
  voiceSelect.selectedIndex = selectedIndex;
}

populateVoiceList();
if (speechSynthesis.onvoiceschanged !== undefined) {
  speechSynthesis.onvoiceschanged = populateVoiceList;
}


function speak(){
    if (synth.speaking) {
        console.error('speechSynthesis.speaking');
        return;
    }
    if (window.words !== '') {
    var utterThis = new SpeechSynthesisUtterance(window.words);
    utterThis.onend = function (event) {
        console.log('SpeechSynthesisUtterance.onend');
    }
    utterThis.onerror = function (event) {
        console.error('SpeechSynthesisUtterance.onerror');
    }

    if (typeof window.voice === "undefined") {
       var selectedOption = voiceSelect.selectedOptions[0].getAttribute('data-name');
        Shiny.setInputValue("selected_voice", selectedOption);
    }
    else {
        var selectedOption = voiceSelect.selectedOptions[0].getAttribute('data-name');
    }
    console.log(selectedOption);

    for(i = 0; i < voices.length ; i++) {
      if(voices[i].name === selectedOption) {
        utterThis.voice = voices[i];
        break;
      }
    }

    if (typeof window.voice === "undefined") {
      console.log("window.voice is undefined");
    }

    else {
      console.log("yup!")
      for(i = 0; i < voices.length ; i++) {
        if(voices[i].name === window.voice) {
          utterThis.voice = voices[i];
          break;
        }
      }
    }

    utterThis.pitch = 1;
    utterThis.rate = 1;
    synth.speak(utterThis);
  }
}

playButton.onclick = function(event) {
  console.log("play button clicked!");
  event.preventDefault();

  speak();

  //inputTxt.blur();
}


voiceSelect.onchange = function(){
  speak();
}
