

#' Include musicassessr scripts in a webpage
#'
#' @return
#' @export
#'
#' @examples
include_musicassessr_js <- function(visual_notation = FALSE, record_audio = TRUE) {
  htmltools::tagList(
    lapply(musicassessr::musicassessr_js(visual_notation = visual_notation,
                                         record_audio = record_audio), function(x) {
                                           if(base::startsWith(x, "http")) {
                                             htmltools::tags$script(src = x)
                                           } else {
                                             htmltools::includeScript(x)
                                           }
                                         })
  )
}


#' musicassessr scripts
#'
#' @param state
#' @param visual_notation
#' @param midi_file_playback
#' @param copy_audio_to_location
#' @param record_audio
#' @param app_name
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js <- function(musicassessr_aws = FALSE,
                            visual_notation = FALSE,
                            midi_file_playback = FALSE,
                            copy_audio_to_location = 'audio',
                            record_audio = TRUE,
                            app_name = character()) {

  if(record_audio) {
    record_audio_names <- record_audio_setup(copy_audio_to_location, app_name, musicassessr_aws)
  } else {
    record_audio_names <- NULL
  }

  # TODO add midi_input argument. This would make importing https://cdn.jsdelivr.net/npm/webmidi@2.5.1 and getMIDIin.js optional
  c(
    if(!is.null(record_audio_names$app_id)) get_musicassessr_state_js_script(record_audio_names$app_id, musicassessr_aws),
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
    if(!is.null(record_audio_names$extra_js_id)) paste0("tmp/", record_audio_names$extra_js_id)
  )
}




get_musicassessr_state_js_script <- function(app_script, musicassessr_aws = FALSE) {

  if(musicassessr_aws) {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {

    system2(command = "npx", args = "kill-port 3000")

    system2(command = "node", args = paste0('node/', app_script), wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}

record_audio_setup <- function(copy_audio_to_location, app_name, musicassessr_aws) {

  if(!dir.exists('tmp')) {
    R.utils::mkdirs('tmp')
  }

  if(copy_audio_to_location == "audio") {

    if(!dir.exists('www')) {
      R.utils::mkdirs('www')
    }

    if(!dir.exists('www/audio')) {
      R.utils::mkdirs('www/audio')
    }
  }

  if(musicassessr_aws) {
    js_to_write <- paste0('const node_file_location = \"', copy_audio_to_location, '\";
                        const shiny_app_name = \"', app_name, '\";
                        ')
    app_id <- app_name
  } else {
    js_to_write <- paste0('const shiny_app_name = \"', app_name, '\";')

    if(!dir.exists('node')) {
      R.utils::copyDirectory(system.file('node', package = 'musicassessr'), 'node')
    }

    app_id <- paste0("app_", stringr::str_replace_all(copy_audio_to_location, "/", "_"), ".js")

    if(!file.exists(app_id)) {
      write(create_app_from_template(copy_audio_to_location),
            file = paste0('node/', app_id))
    }
  }



  extra_js_id <- paste0("extra_js_", stringr::str_replace_all(copy_audio_to_location, "/", "_"), ".js")

  if(!file.exists(extra_js_id)) {
    write(js_to_write, file = paste0('tmp/', extra_js_id))
  }


  list(app_id = app_id,
       extra_js_id = extra_js_id)
}

create_app_from_template <- function(dir) {
  # NB: this ONLY controls a local app
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

        var move_file_to = \'", dir, "/\'+ file.name+'.wav'

        //Use the mv() method to place the file in upload directory (i.e. 'uploads')
        file.mv(move_file_to);

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
