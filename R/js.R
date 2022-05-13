
create_app_from_template <- function(dir) {

  paste0("const express = require('express');
  const fileUpload = require('express-fileupload');
  const cors = require('cors');
  const bodyParser = require('body-parser');
  const morgan = require('morgan');
  const _ = require('lodash');

  const app = express();

  // enable files upload
  app.use(fileUpload({
    createParentPath: true
  }));

  //add other middleware
  app.use(cors());
  app.use(bodyParser.json());
  app.use(bodyParser.urlencoded({extended: true}));
  app.use(morgan('dev'));


  app.post('/upload-audio', async (req, res) => {
    try {
      if(!req.files) {
        res.send({
          status: false,
          message: 'No file uploaded'
        });
      } else {
        //Use the name of the input field (i.e. 'avatar') to retrieve the uploaded file
        let file = req.files.audio_data;

        //Use the mv() method to place the file in upload directory (i.e. 'uploads')
        file.mv(\'", dir, "/\'+ file.name+'.wav');

        //send response
        res.send({
          status: true,
          message: 'File is uploaded',
          data: {
            name: file.name,
            mimetype: file.mimetype,
            size: file.size
          }
        });
      }
    } catch (err) {
      res.status(500).send(err);
    }
  });

  //start app
  const port = process.env.PORT || 3000;

  app.listen(port, () =>
               console.log(`App is listening on port ${port}.`)
  );
  ")
}


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

  js_to_write <- paste0('const node_file_location = \"', system.file("node/files", package = "musicassessr"), '\";')

  write(js_to_write, file = paste0(system.file("www/js/", package = "musicassessr"), "/extra_js.js"))

  write(create_app_from_template(system.file("node/files/", package = "musicassessr")),
        file = paste0(system.file("node", package = "musicassessr"), "/app_gen.js"))

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
    system.file("www/js/getMIDIin.js", package = "musicassessr"),
    system.file("www/js/extra_js.js", package = "musicassessr")
  )
}


get_musicassessr_state_js_script <- function(state = "production") {

  musicassessr_state <<- state

  if(state == "production") {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {

    system2(command = "npx", args = "kill-port 3000")

    system2(command = "node",
            args = system.file('node/app_gen.js', package = 'musicassessr'),
            wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}


#' Include musicassessr scripts in a webpage
#'
#' @return
#' @export
#'
#' @examples
include_musicassessr_js <- function(visual_notation = FALSE) {
  htmltools::tagList(
    lapply(musicassessr::musicassessr_js(visual_notation = visual_notation), function(x) {
      if(base::startsWith(x, "http")) {
        htmltools::tags$script(src = x)
      } else {
        htmltools::includeScript(x)
      }
    })
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

