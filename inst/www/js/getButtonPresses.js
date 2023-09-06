
console.log('getButtonPresses.js loaded');

// Stuff to record
var keypress_keyup = [];
var keypress_keydown = [];
var onsets_keyup = [];
var onsets_keydown = [];
var startTime = new Date().getTime();

var textarea = document.getElementById('test-target');
var consoleLog = document.getElementById('console-log');
var btnClearConsole = document.getElementById('btn-clear-console');

function logMessage(message) {
  if (consoleLog) {
    consoleLog.innerHTML += message + "<br>";
  }
  else {
    console.log(message);
  }
}



btnClearConsole.addEventListener('click', (e) => {
  let child = consoleLog.firstChild;
  while (child) {
  consoleLog.removeChild(child);
  child = consoleLog.firstChild;
  }
});



var mainDiv = document.getElementById('mainDiv');


mainDiv.addEventListener('keydown', (e) => {

  var keyPressed = e.key;
  var responseTime = new Date().getTime();
  var timeElapsed = Math.abs(startTime - responseTime);

  keypress_keydown.push(keyPressed);
  onsets_keydown.push(timeElapsed);


  // Send to shiny
  Shiny.setInputValue("keypress_keydown", JSON.stringify(keypress_keydown));
  Shiny.setInputValue("onsets_keydown", JSON.stringify(onsets_keydown));

  if (!e.repeat)
    logMessage(`Key "${e.key}" pressed  [event: keydown]`);
  else
    logMessage(`Key "${e.key}" repeating  [event: keydown]`);
});

mainDiv.addEventListener('keyup', (e) => {

  var keyPressed = e.key;
  var responseTime = new Date().getTime();
  var timeElapsed = Math.abs(startTime - responseTime);

  keypress_keyup.push(keyPressed);
  onsets_keyup.push(timeElapsed);

  // Send to shiny
  Shiny.setInputValue("keypress_keyup", JSON.stringify(keypress_keyup));
  Shiny.setInputValue("onsets_keyup", JSON.stringify(onsets_keyup));


  logMessage(`Key "${e.key}" released  [event: keyup]`);
});
