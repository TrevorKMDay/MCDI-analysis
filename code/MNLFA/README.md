# (automated) Moderated NonLinear Factor Analysis

## CFAs

The first step to performing an MNLFA is to perform CFAs on the factors individually based 
on the covariates of interest
to confirm their structure as a one-factor solution (i.e. in this case, this is different
from the goodness-of-fit of the two-factor solution). 

Here, the covariates of interest are (1) age, (2) sex, (3) diagnostic status, and 
(4) birth order. 

So here I test the one-factor solutions within age bins crossed with sex and diagnostic status
(separately) in the EIRLI data and within age, sex, and birth order in the Wordbank sample.
Age is not crossed in Wordbank because those data are cross-sectional.

`aMNLFA_CFAs_master.R` (which calls `aMNLFA_CFAs_run.R`) runs the naive CFA using no cross-correlations (`cfa1`).  Then 
`aMNLFA_CFAs_modindices.Rmd` describes the process of selecting cross-correlations, which 
are also run by `aMNLFA_CFAs_master.R`.

The stats are manually collated in `FIT_STATS.xlsx`.