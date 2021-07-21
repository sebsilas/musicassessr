# musicassessr

`musicassessr` is an R package for facilitating the deployment of stimuli in psychological tests as well as recording and scoring data. Essentially, it provides some convenience functions to get (particularly, musical) stimuli into [psychTestR](https://pmcharrison.github.io/psychTestR/).

# Usage

The main function is `present_stimuli` which can be used in a number of ways:

``` r
# use cases
```

# Recording Audio

If you wish to record audio and use complementary functionality to process such data, you will need to take some further steps.

We have chosen to implement recording audio and some of the subsequent data processing within the Amazon Web Services infrastructure so that audio files and their processed results may be stored in the cloud e.g., to avoid repeated downloading and manual processing.

To access this functionality for your own use, you need to [create an AWS account](https://aws.amazon.com/resources/create-account/) and then [get your AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY](https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/getting-your-credentials.html). With this information, you can run our so-called Terraform scripts which setup the AWS architecture required to use musicassessr within your own account. This allows you to record audio files, process them etc. in your own AWS account, where only you would have access to the files.

These steps are required if you wish to use any of the following tests we have already developed:

- MST
- PBET
- SRT
- SST

## Notice

Please note, there are costs associated with AWS usage. We cannot accept responsibility for any charges incurred.

## References

Audio Processing:
- sonic annotator
- pYIN
- crepe

Data Scoring:
- opti3
