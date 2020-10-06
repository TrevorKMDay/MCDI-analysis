# MCDI-analysis

**Author:** Trevor KM Day, [day00096@umn.edu
](mailto:day00096@umn.edu)

This repository is for analysis of MacArthur-Bates Communicative Development Inventory (American English) forms. The abbreviation *MCDI* is typically used throughout, rather than *MB-CDI*, as a quirk of how the form was referred to in internal [Elison Lab](https://innovation.umn.edu/elison-lab/) materials.

This code both analyzes data from:

 - [Stanford Wordbank](wordbank.stanford.edu) (Frank et al., 2016)
 - University of Minnesota (UMN) Baby Connectome Project (BCP; Howell et al., 2018)
 - UMN Phenoscreening (PHE)

All three projects use both Words & Gestures (WG) and Words & Sentences (WS).

## Analysis

The analysis is built up in multiple stages:

 1. Analysis of Wordbank WG and WS data individually.
 2. Joint Wordbank WG/WS analysis. Through steps 1-2, various functions for processing of Wordbank-format data are developed (`code/Wordbank/wordbank-functions.R`) that are shared across many scripts. 
 3. Conversion of BCP/PHE data to Wordbank format.
 4. Use of Wordbank analysis of BCP/PHE data (`code/BCP/`).
 5. Growth curve modeling of BCP data (`code/growth-curving/`).

### Wordbank

`wordbank-csv2rds.R` converts WG/WS data to RDS to speed read-in and also extract demographic information and saves that to another RDS for easy manipulation. 

# References

 1. Frank, M. C., Braginsky, M., Yurovsky, D., & Marchman, V. A. (2016). *Wordbank: An open repository for developmental vocabulary data.* Journal of Child Language. doi: 10.1017/S0305000916000209.
 1. Howell, B. R., ... Elison, J. T. (2018). The UNC/UMN Baby Connectome Project (BCP): *An overview of the study design and protocol development. NeuroImage.* [10.1016/j.neuroimage.2018.03.049](https://doi.org/10.1016/j.neuroimage.2018.03.049)
