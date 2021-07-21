# musicassessr

`musicassessr` is an R package for facilitating the deployment of stimuli in psychological tests as well as recording and scoring data. Essentially, it provides some convenience functions to get (particularly, musical) stimuli into [psychTestR](https://pmcharrison.github.io/psychTestR/).

musicassessr relies on functionality such as recording audio and processing it using fundamental frequency estimation and note detection algorithms, such as probabilistic YIN (pYIN) or Crepe. We have chosen to implement this functionality within the Amazon Web Services infrastructure so that audio files and their processed results may be stored in the cloud e.g., to avoid repeated downloading and manual processing.

To access this functionality for your own use, you need to make an AWS account and get your AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY. With this information, you can run our so-called Terraform scripts which setup the AWS architecture required to use musicassessr within your own account. This allows you to record audio files, process them etc. in your own AWS account, where only you would have access to the files.

## Notice
Please note, there are costs associated with AWS usage. We cannot accept responsibility for any charges incurred.

## References
