#' Plot histogram and summary values.
#'
#' @param dat the data frame.
#' @param var the variable to plot.
#' @return a plot object
plot_histogram <- function(dat, var) {
  ggplot(dat, aes_string(x = var)) +
    geom_histogram(binwidth=.5, color="black", fill="white")
    #geom_vline(aes_string(xintercept=mean(var, na.rm=T)),   # Ignore NA values for mean
    #           color="red", linetype="dashed", size=1)
}

