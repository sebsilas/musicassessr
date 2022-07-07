
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
#' @param copy_audio_to_location
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js <- function(musicassessr_aws = FALSE,
                            visual_notation = FALSE,
                            midi_file_playback = FALSE,
                            copy_audio_to_location = NULL) {

  if(is.null(copy_audio_to_location)) {
    copy_audio_to_location <- system.file("node/files/", package = "musicassessr")
  }


  js_to_write <- paste0('const node_file_location = \"', copy_audio_to_location, '\";')

  extra_js_id <- paste0("extra_js_", stringr::str_replace_all(copy_audio_to_location, "/", "_"), ".js")
  # so there can be multiple configs per package

  extra_dir_loc <- paste0(system.file("www/js/", package = "musicassessr"), extra_js_id)

  if(!file.exists(extra_dir_loc)) {
    write(js_to_write, file = extra_dir_loc)
  }

  write(create_app_from_template(copy_audio_to_location),
        file = paste0(system.file("node", package = "musicassessr"), "/app_gen.js"))

  # TODO add midi_input argument. This would make importing https://cdn.jsdelivr.net/npm/webmidi@2.5.1 and getMIDIin.js optional
  c(
    get_musicassessr_state_js_script(musicassessr_aws),
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
    system.file(paste0("www/js/", extra_js_id), package = "musicassessr")
  )
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


get_musicassessr_state_js_script <- function(musicassessr_aws = FALSE) {

  if(musicassessr_aws) {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {

    system2(command = "npx", args = "kill-port 3000")

    system2(command = "node",
            args = system.file('node/app_gen.js', package = 'musicassessr'),
            wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}


