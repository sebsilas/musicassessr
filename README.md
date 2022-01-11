# musicassessr

`musicassessr` is an R package for facilitating the deployment of (particularly, musical) stimuli in psychological tests as well as recording and scoring data. Essentially, it provides some convenience functions to deploy stimuli via [`psychTestR`](https://pmcharrison.github.io/psychTestR/) and also supplies some functions collect new types of data, process and score this data, among other things. With `musicassessr` you can:


## Usage


## Presenting stimuli

The main function is `present_stimuli` which can be used in a number of ways:

``` r
# use cases
```

## Sampling from an item bank

present stimuli from a musical item bank (e.g already created by [`itembankr`](https://github.com/syntheso/itembankr/)) in [`psychTestR`](https://pmcharrison.github.io/psychTestR/)


## Recording non-standard data


### Recording audio

If you wish to record audio and use complementary functionality to process such data, you will need to take some further steps.

We have chosen to implement recording audio and some of the subsequent data processing within the Amazon Web Services infrastructure so that audio files and their processed results may be stored in the cloud e.g., to avoid repeated downloading and manual processing.

To access this functionality for your own use, you need to [create an AWS account](https://aws.amazon.com/resources/create-account/) and then [get your AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY](https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/getting-your-credentials.html). With this information, you can run our so-called Terraform scripts which setup the AWS architecture required to use musicassessr within your own account. This allows you to record audio files, process them etc. in your own AWS account, where only you would have access to the files.

These steps are required if you wish to use any of the following tests we have already developed:

- MST
- PBET
- SRT
- SST
- PDCT

### Recording MIDI

### Recording key presses

### Recording spoken words

## Notice

Please note, there are costs associated with AWS usage. You must take your own responsibility for charges incurred.

## References


Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021). Expert musical improvisations contain sequencing biases seen in language production. Journal of Experimental Psychology. https://doi.org/10.1037/xge0001107

Cannam, C., Jewell, M. O., Rhodes, C., Sandler, M., & d’Inverno, M. (2010). Linked Data And You: Bringing music research software into the Semantic Web. Journal of New Music Research, 39(4), 313–325.

Crayencour, H.-C., Velichkina, O., Frieler, K., Höger, F., Pfleiderer, M., Henry, L., Solis, G., Wolff, D., Weyde, T., Peeters, G., Basaran, D., Smith, J., & Proutskova, P. (2021). The DTL1000 Jazz Solo Dataset (in prep.). Journal on Computing and Cultural Heritage

Harrison, P. M. C. (2020). psychTestR: An R package for designing and conducting behavioural psychological experiments. Journal of Open Source Software, 5(49), 2088. https://doi.org/10.21105/joss.02088

Mauch, M., & Dixon, S. (2014). PYIN: a fundamental frequency estimator using probabilistic threshold distributions. Proceedings of the IEEE International Conference on Acoustics, Speech, and Signal Processing (ICASSP 2014).

Müllensiefen, D., & Frieler, K. (2007). Modelling experts’ notions of melodic similarity. Musicae Scientiae, 11(1_suppl), 183–210. https://doi.org/10.1177/102986490701100108


