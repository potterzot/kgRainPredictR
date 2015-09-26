##Code for the kaggle competition "[How Much Did It Rain II](https://www.kaggle.com/c/how-much-did-it-rain-ii)"

This package provides code for analysis of data, as well as various supporting 
functions. The package can be installed with:

    devtools::install_github("potterzot/kgRainPredictR")

In the `data-raw` folder, you'll find a script for converting the competition data
to RDS format. First you need to download the data, then you can edit that script to 
point to the location of the data and it will convert from zipped CSV files to 
RDS files, which are equivalent in size but which R can read in directly and quickly.

The various analysis functions are/will be in the `R` folder, or available via

    library(kgRainPredictR)

