# musicassessr

`musicassessr` is an R package for facilitating the deployment of (particularly, musical) stimuli in psychological tests as well as recording and scoring data. It provides convenience functions to deploy stimuli via [`psychTestR`](https://pmcharrison.github.io/psychTestR/), advanced [`psychTestR`](https://pmcharrison.github.io/psychTestR/) page types to collect new types of data, and utilities to process and score this data, among other things.

## The musicassessr ecosystem

`musicassessr` is part of a network of packages. See also:

- [`pyin`](https://github.com/sebsilas/pyin): transcribe monophonic audio in the *R* environment using the pYIN algorithm
- [`itembankr`](https://github.com/sebsilas/itembankr): produce musicassessr-compatible item banks with useful melodic features
- [`Berkowitz`](https://github.com/sebsilas/Berkowitz): a pre-made *itembankr* item bank of the Baker (2021) / Berkowitz (2017) sight-singing corpus.
- [`WJD`](https://github.com/sebsilas/WJD): a pre-made *itembankr* item bank of the [Weimar Jazz Database](https://jazzomat.hfm-weimar.de/dbformat/dboverview.html).

### Musical ability tests

*musicassessr* currently facilitates the following music ability tests:

- [`SAA`](https://saa.musicassessr.com) (*Singing Ability Assessment*; Silas, Müllensiefen, & Kopiez, 2023)
- [`PBET`](https://github.com/sebsilas/PBET)  (*Playing By Ear Test*)
- [`PDT`](https://github.com/sebsilas/PDT) (*Pitch Discrimination Test*; our re-implementation of Soranzo & Grassi, 2014)

### Cheat Sheet

<a href="https://musicassessr.com/assets/musicassessr_cheat_sheet.pdf"><img src="https://musicassessr.com/assets/musicassessr_cheatsheet.png" width="630" height="495"/></a>



### Analysis pipeline

![Analysis pipeline](https://musicassessr.com/assets/analysis_pipeline.jpg)

### Research and Documentation

You can find several articles and tutorials [here](https://sebsilas.github.io/musicassessr/articles/), which include summarised results of research utilising this software (see: white papers). For in depth reading, follow the results to the real publications.

## References

Baker, D. (2021). MeloSol Corpus. Empirical Musicology Review, 16, 106–113. https://doi.org/10.18061/emr.v16i1.7645

Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021). Expert musical improvisations contain sequencing biases seen in language production. Journal of Experimental Psychology. https://doi.org/10.1037/xge0001107

Berkowitz, S., Fontrier, G., Goldstein, P., & Smaldone, E. (2017). A new approach to sight singing (Sixth edition). W. W. Norton & Company.

Cannam, C., Jewell, M. O., Rhodes, C., Sandler, M., & d’Inverno, M. (2010). Linked Data And You: Bringing music research software into the Semantic Web. Journal of New Music Research, 39(4), 313–325.

Crayencour, H.-C., Velichkina, O., Frieler, K., Höger, F., Pfleiderer, M., Henry, L., Solis, G., Wolff, D., Weyde, T., Peeters, G., Basaran, D., Smith, J., & Proutskova, P. (2021). The DTL1000 Jazz Solo Dataset (in prep.). Journal on Computing and Cultural Heritage

Harrison, P. M. C. (2020). psychTestR: An R package for designing and conducting behavioural psychological experiments. Journal of Open Source Software, 5(49), 2088. https://doi.org/10.21105/joss.02088

Mauch, M., & Dixon, S. (2014). PYIN: a fundamental frequency estimator using probabilistic threshold distributions. Proceedings of the IEEE International Conference on Acoustics, Speech, and Signal Processing (ICASSP 2014).

Müllensiefen, D., & Frieler, K. (2007). Modelling experts’ notions of melodic similarity. Musicae Scientiae, 11(1_suppl), 183–210. https://doi.org/10.1177/102986490701100108

Silas, S., & Frieler, K. (2023). The musicassessr ecosystem: Record, measure, score and present feedback about musical production behaviour in real-time, supported by psychometric models. Deutsche Gesellschaft für Musikpsychologie, Hanover.

Silas, S., Müllensiefen, D., & Kopiez, R. (2023). Singing Ability Assessment: Development and validation of a singing test based on item response theory and a general open-source software environment for singing data. Behaviour Research Methods.


Silas, S., Müllensiefen, D., & Kopiez, R. (2023). Utilising a new generation of musical production tests to understand musical learning: Singing ability assessment, melodic recall and playing by ear. Deutsche Gesellschaft für Musikpsychologie, Hanover.

Silas, S., Kopiez, R., & Müllensiefen, D. (2021). What makes playing by ear difficult? SEMPRE conference.

Soranzo, A., & Grassi, M. (2014). Psychoacoustics: A comprehensive Matlab toolbox for auditory testing. Frontiers in Psychology, 5. https://doi.org/10.3389/fpsyg.2014.00712

