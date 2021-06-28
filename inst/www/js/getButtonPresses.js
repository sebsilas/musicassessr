// stuff to record
var user_response_keypress = [];
var onsets = [];
var onsets_keyup = [];
var user_response_keypress_keyup = [];
var onsets_keydown = [];
var user_response_keypress_keydown = [];



//

//let textarea = document.getElementById('test-target'),
var consoleLog = document.getElementById('console-log');
// btnClearConsole = document.getElementById('btn-clear-console');

function logMessage(message) {
  if (consoleLog) {
    consoleLog.innerHTML += message + "<br>";
  }
  else {
    console.log(message);
  }
}

// textarea.addEventListener('keydown', (e) => {
//   if (!e.repeat)
//     logMessage(`Key "${e.key}" pressed  [event: keydown]`);
//   else
//     logMessage(`Key "${e.key}" repeating  [event: keydown]`);
// });

// textarea.addEventListener('beforeinput', (e) => {
//   logMessage(`Key "${e.data}" about to be input  [event: beforeinput]`);
// });

// textarea.addEventListener('input', (e) => {



  // grab time and response
//   var keyPressed = e.data;
//   logMessage(`Key "${keyPressed}" input  [event: input]`);
//   var responseTime = new Date().getTime();
//   var timeElapsed = Math.abs(startTime - responseTime);
//   onsets.push(timeElapsed);
//   user_response_keypress.push(keyPressed);

  // send to shiny
//   Shiny.setInputValue("user_response", JSON.stringify(user_response_keypress));
//   Shiny.setInputValue("onset", JSON.stringify(onsets));

  // console
//   console.log(user_response_keypress);
//   console.log(onsets);

// });

// textarea.addEventListener('keyup', (e) => {
//   logMessage(`Key "${e.key}" released  [event: keyup]`);
// });

// btnClearConsole.addEventListener('click', (e) => {
//   let child = consoleLog.firstChild;
//   while (child) {
//    consoleLog.removeChild(child);
//    child = consoleLog.firstChild;
//   }
// });



///

const audio = document.querySelector('audio');

audio.addEventListener('play', (event) => {
  window.startTime = new Date().getTime();
  console.log('The Boolean paused property is now false. Either the ' +
  'play() method was called or the autoplay attribute was toggled.');
});

var mainDiv = document.getElementById('mainDiv');

mainDiv.addEventListener('input', (e) => {

  console.log('mainDiv event listener');
  console.log(e);

  // grab time and response
  var keyPressed = e.data;
  logMessage(`Key "${keyPressed}" input  [event: input]`);
  var responseTime = new Date().getTime();
  var timeElapsed = Math.abs(startTime - responseTime);

  onsets.push(timeElapsed);
  user_response_keypress.push(keyPressed);

  // console
  console.log(user_response_keypress);
  console.log(onsets);

  // send to shiny
  Shiny.setInputValue("user_response_keypress", JSON.stringify(user_response_keypress));
  Shiny.setInputValue("onsets", JSON.stringify(onsets));


});

mainDiv.addEventListener('keydown', (e) => {

  var keyPressed = e.data;
  var responseTime = new Date().getTime();
  var timeElapsed = Math.abs(startTime - responseTime);

  onsets_keydown.push(timeElapsed);
  user_response_keypress_keydown.push(keyPressed);

    // console
  console.log(user_response_keypress_keydown);
  console.log(onsets_keydown);


  // send to shiny
  Shiny.setInputValue("user_response_keydown", JSON.stringify(user_response_keypress_keydown));
  Shiny.setInputValue("onsets_keydown", JSON.stringify(onsets_keydown));

  if (!e.repeat)
    logMessage(`Key "${e.key}" pressed  [event: keydown]`);
  else
    logMessage(`Key "${e.key}" repeating  [event: keydown]`);
});

mainDiv.addEventListener('keyup', (e) => {

  var keyPressed = e.data;
  var responseTime = new Date().getTime();
  var timeElapsed = Math.abs(startTime - responseTime);

  onsets_keyup.push(timeElapsed);
  user_response_keypress_keyup.push(keyPressed);

  // console
  console.log(user_response_keypress_keyup);
  console.log(onsets_keyup);

  // send to shiny
  Shiny.setInputValue("user_response_keypress_keyup", JSON.stringify(user_response_keypress_keyup));
  Shiny.setInputValue("onsets_keyup", JSON.stringify(onsets_keyup));


  logMessage(`Key "${e.key}" released  [event: keyup]`);
});
