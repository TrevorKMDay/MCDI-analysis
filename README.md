# MCDI-analysis

**Author:** Trevor K.M. Day, [day00096@umn.edu](mailto:day00096@umn.edu)
*Now at:* [trevor.day@georgetown.edu](mailto:trevor.day@georgetown.ed), June
    2024

## Overview

This repository is for analysis of MacArthur-Bates Communicative Development
Inventory (American English) forms. The abbreviation *MCDI* is typically used
throughout, rather than *MB-CDI*, as a quirk of how the form was referred to in
internal [Elison Lab](https://innovation.umn.edu/elison-lab/) materials.

This code analyzes data from:

 - [Stanford Wordbank](wordbank.stanford.edu) (Frank et al., 2016)
 - University of Minnesota (UMN) Elison Lab data:
    - Baby Connectome Project ("BCP"; Howell et al., 2018)
    - UMN Phenoscreening ("PHE")
    - DevSocAtt (or "DSA")
 - EIRLI data shared with me by Donna Thal and Arielle Borovsky, which I don't
    have permission to share. See Phase IV of Thal et al. (2013).

All three projects use both Words & Gestures (WG) and Words & Sentences (WS).

## Code

The code is heavily inter-related; with multiple directions of analysis relying
on shared data wrangling scripts and principles, see `code/mcdi-setup.R`.

Below are some of the code directories:

 - `BCP/`: Analyses related to analysis of the BCP CDIs
 - `bootstrap/`: Old unusued project
 - `DSA/`: Analyses related to analysis of the DSA CDIs
 - `EIRLI/`: Analyses including the EIRLI data, current in submission, see
        OSF preprint Day et al. (2024).
 - `gcc/`: I forget what GCC stands for, but this is semantic networks using
        CDI data.
 - `growth_curving/`: Growth curve analyses (see Day et al., 2024), but most
        of this isn't that code; different attempt.
 - `LCA/`: Latent class analyses; which weren't too fruitful
 - `M1075`: Background analyses for a different project
 - `MCDI/`: Basic functions for background stats on the CDIs.
 - `MNLFA/`: Moderated non-linear factor analyses; which didn't result in much
        (Gottfredson et al., 2019).
 - `models/`: Directory saving modeling results from growth curves
 - `PHE/`: Phenoscreening analyses
 - `paper_v_digital/`: Do parents endorse fewer words on digital forms? Clicking
        being harder than using a pencil
 - `semnet/`: More smeantic networks (see also `gcc/`)
 - `WG2WS/`: Projecting WG scores collected on kids out-of-range (i.e. too old)
        onto WS scores, see Day et al. (2024).
 - `Wordbank/`: Analyses of the Stanford Wordbank data (Frank et al., 2017).

## Data

I separated input data from analyses. Here are some data directories that don't
correspond to a code directory.

 - `AoA/`: Age of acquisition
 - `ELab/`: Connecting data from various ELab Projects
 - `IBIS/`: Infant Brain Imaging Study data (no permission to share)
 - `Wordnet/`: Source data for semantic networks
 - `other/`: Background data, list of CDI words, etc.
 - `results/`: Various large archives with results of analyses

# Papers from this analyses:

 2. Day, T. K. M., & Elison, J. T. (2021). A broadened estimate of syntactic
    and lexical ability from the MB-CDI. *Journal of Child Language*, 1–18.
    https://doi.org/10.1017/S0305000921000283

 1. Day, T. K. M., Borovsky, A., Thal, D., & Elison, J. (2024).
    The CDI in two longitudinal datasets: Methods and differences across
    decades. *OSF*. https://doi.org/10.31234/osf.io/rwhcy

# References

 1. Frank, M. C., Braginsky, M., Yurovsky, D., & Marchman, V. A. (2016).
    Wordbank: An open repository for developmental vocabulary data.
    *Journal of Child Language*. doi: 10.1017/S0305000916000209.
 1. Howell, B. R., ... Elison, J. T. (2018).
    The UNC/UMN Baby Connectome Project (BCP): An overview of the study design
    and protocol development. *NeuroImage*.
    [10.1016/j.neuroimage.2018.03.049](https://doi.org/10.1016/j.neuroimage.2018.03.049)
 3. Thal, D. J., Marchman, V. A., & Tomblin, J. B. (2013).
    Late-talking toddlers: Characterization and prediction of continued delay.
    *Late Talkers*, 169–201.
 4. Day, T., Borovsky, A., Thal, D., & Elison, J. (2024).
    The CDI in two longitudinal datasets: Methods and differences across
    decades. *OSF*. https://doi.org/10.31234/osf.io/rwhcy
 5. Gottfredson, N. C., et al. (2019). Simplifying the implementation of modern
    scale scoring methods with an automated R package: Automated moderated
    nonlinear factor analysis (aMNLFA). *Addictive Behaviors*, 94, 65–73.
    https://doi.org/10.1016/j.addbeh.2018.10.031
 6. Frank, M. C., Braginsky, M., Yurovsky, D., & Marchman, V. A. (2017).
    Wordbank: An open repository for developmental vocabulary data.
    *Journal of Child Language*, 44(3), 677–694.
    https://doi.org/10.1017/S0305000916000209



