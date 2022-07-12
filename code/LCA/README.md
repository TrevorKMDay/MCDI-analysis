# MB-CDI Latent Class Analysis

## 01-LCA_setup.R

This script combines the BCP and EIRLI datasets and saves them to
`data/LCA/BplusE.rds` (1 row per observation) and `data/LCA/Bplus_demographics.rds` (1 row per participant).

It also creates a number of plots (in `plots/setup`) 
that compare average trajectories.

## 02-adjust_BCP.R

This script adjusts the BCP data so the mean trajectory matches
that of the **Wordbank** data for better comparison with EIRLI
(not necessary).

Calls `../growth_curving/growth-curve-functions.R` and also creates 
a plot of category trajectories in WB, EDx-, and BCP.

Creates a table of singular values per month to add to original
data to adjust upward: `data/LCA/BCP_to_EIRLI.rds`.