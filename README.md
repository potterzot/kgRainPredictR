##Code for the kaggle competition "[How Much Did It Rain II](https://www.kaggle.com/c/how-much-did-it-rain-ii)"

This package provides code for analysis of the kaggle competition data, as well as various supporting 
functions. There are two ways to use this package. If you just want the functionality and the vignettes, you can install it as an R package with:

    install.packages("devtools") #if not already installed
    devtools::install_github("potterzot/kgRainPredictR")

If you want to see the code for the different models and analysis, then it is probably best to just fork this repository and clone it to your computer, which will add everything except the data itself.

###The Data
If you've installed the package, you can try things out quickly by using the sample data included with the package. To do so, just run

```
data(kgrpr_train)
data(kgrpr_test)
```

These are a small subset of the actual data, so they aren't useful for more than just testing things out.

If you are more interested in reproducing or extending the analysis, you should do the following to get the full data:

1. Fork and clone the repository.
2. Download the data from kaggle and put it in the `data-raw` folder.
3. In the `data-raw` folder, run `zip2csv.R` or `zip2ff2csv`, which will process the data and put it in `analysis/data`.
4. Profit!

The vignettes and function documentation use the sample data installed with the package (and cloned with this repository). The actual analysis in the `analysis` directory assume full data stored in `analysis/data`.


