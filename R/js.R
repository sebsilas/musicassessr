#' musicassessr scripts
#'
#' @param state
#' @param visual_notation
#' @param midi_file_playback
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js <- function(state = "production",
                            visual_notation = FALSE,
                            midi_file_playback = FALSE) {

  # TODO add midi_input argument. This would make importing https://cdn.jsdelivr.net/npm/webmidi@2.5.1 and getMIDIin.js optional
  musicassessr_state <<- state

  c(
    get_musicassessr_state_js_script(state),
    "https://cdn.rawgit.com/mattdiamond/Recorderjs/08e7abd9/dist/recorder.js",
    "https://www.midijs.net/lib/midi.js",
    if(midi_file_playback) "https://unpkg.com/@tonejs/midi", # only required for midi file playback
    system.file("www/js/modernizr-custom.js", package = "musicassessr"),
    "https://unpkg.com/tone@13.8.25/build/Tone.js",
    system.file("www/js/Tonejs-Instruments.js", package = "musicassessr"),
    if(visual_notation) "https://cdn.jsdelivr.net/npm/opensheetmusicdisplay@0.7.6/build/opensheetmusicdisplay.min.js", # only required when with visual notation
    'https://unpkg.com/tone-rhythm@2.0.0/dist/tone-rhythm.min.js',
    system.file("www/spinner/spin.js", package = "musicassessr"),
    system.file("www/js/musicassessr.js", package = "musicassessr"),
    "https://cdn.jsdelivr.net/npm/webmidi@2.5.1",
    system.file("www/js/getMIDIin.js", package = "musicassessr")
  )
}


get_musicassessr_state_js_script <- function(state = "production") {

  musicassessr_state <<- state

  if(state == "production") {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {
    system2(command = "npx",
            args = "kill-port 3000")

    system2(command = "node",
            args = "/Users/sebsilas/aws-musicassessr-local-file-upload/app.js",
            wait = FALSE)
    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}



enable.cors <- '
// Create the XHR object.
function createCORSRequest(method, url) {
var xhr = new XMLHttpRequest();
if ("withCredentials" in xhr) {
// XHR for Chrome/Firefox/Opera/Safari.
xhr.open(method, url, true);
} else if (typeof XDomainRequest != "undefined") {
// XDomainRequest for IE.
xhr = new XDomainRequest();
xhr.open(method, url);
} else {
// CORS not supported.
xhr = null;
}
return xhr;
}
// Helper method to parse the title tag from the response.
function getTitle(text) {
return text.match(\'<title>(.*)?</title>\')[1];
}
// Make the actual CORS request.
function makeCorsRequest() {
// This is a sample server that supports CORS.
var url = \'https://eartrainer.app/melodic-production/js/midi.js\';
var xhr = createCORSRequest(\'GET\', url);
if (!xhr) {
alert(\'CORS not supported\');
return;
}
// Response handlers.
xhr.onload = function() {
var text = xhr.responseText;
var title = getTitle(text);
alert(\'Response from CORS request to \' + url + \': \' + title);
};
xhr.onerror = function() {
alert(\'Woops, there was an error making the request.\');
};
xhr.send();
}
'

