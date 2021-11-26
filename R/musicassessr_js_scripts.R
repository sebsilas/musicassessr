#' Import the required musicassessr dependencies into a webpage
#'
#' @param musicassessr_state
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js_scripts <- function(musicassessr_state = NULL) {

  shiny::tags$div(
    musicassessr::set_musicassessr_state(musicassessr_state),
    shiny::tags$script(htmltools::HTML(enable.cors)),
    shiny::tags$script(src="https://cdn.rawgit.com/mattdiamond/Recorderjs/08e7abd9/dist/recorder.js"),
    shiny::tags$script(src="https://sdk.amazonaws.com/js/aws-sdk-2.585.0.min.js"),
    shiny::tags$script(src="https://cdn.jsdelivr.net/npm/@tensorflow/tfjs/dist/tf.min.js"),
    shiny::includeCSS(path = system.file('www/css/crepe.css', package = "musicassessr")),
    shiny::includeCSS(path = system.file('www/css/style.css', package = "musicassessr")),
    shiny::tags$script(src="https://www.midijs.net/lib/midi.js"),
    shiny::tags$script(src="https://unpkg.com/@tonejs/midi"),
    shiny::includeScript(system.file("www/js/modernizr-custom.js", package = "musicassessr")),
    shiny::includeScript(system.file("www/js/Tone.js", package = "musicassessr")),
    shiny::includeScript(system.file("www/js/Tonejs-Instruments.js", package = "musicassessr")),
    shiny::includeScript(system.file("www/js/opensheetmusicdisplay.min.js", package = "musicassessr")),
    shiny::includeScript('https://unpkg.com/tone-rhythm@2.0.0/dist/tone-rhythm.min.js'),
    shiny::includeScript(system.file("www/spinner/spin.js", package = "musicassessr")),
    shiny::includeScript(system.file("www/js/musicassessr.js", package = "musicassessr"))
    )
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


