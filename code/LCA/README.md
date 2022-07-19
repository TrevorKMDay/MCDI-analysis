# MB-CDI Latent Class Analysis

## 00-LCA_functions.R 

This script defines the following utility functions and is sourced in various other scripts.

- `get_fit_stats(g1_model, list_of_models)`: Given a one-group LCA and a list of *n*-group LCAs, returns
	a dataframe with fit statistics for the models together.
- `plot_fit_stats(fit_stats)`: Given the output of the previous functions, plots all these together for
	making decisions.
- `calc_ass(df, column, dx_class)`: Given a dataframe with columns `status` and `<column>`, the latter containing
	at least the value of `dx_class`, calculate the accuracy/specificity/sensitivity of Dx+ individuals to `dx_class`.
- `ass2str(v)`: Given a vector of three numbers, return a formatted string for easy addition to plots.

## `00-BCP_delay_criteria.R`

Using information from the BCP project, assign BCP participants a Delay 3+ criteria status.

- `x = 0`: No delay
- `0 < x < 1`: Probably no delay, but missing information
- `x >= 1`: Delay, see below.
	- `x = 1`: < 50 words at 24 months and parents expressed concern
	- `x = 2`: < 50 words at 24 months and 6+ ear infections
	- `x = 3`: not combining words and language concern
	- `x = 4`: not combining words and 6+ ear infections
	- Intermediate values reflect missing data, but **generally `x >= 1` indicates delay**

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

## Using unadjusted BCP data

### 03-LCA_raw.R

This script does LCAs on the Part IA: Inventory score (unadjusted for BCP).

### 04-LCA_cross.R

This script attempts some ways of separating out lexical/syntactic scores for the
model; with varying degrees of success. 

