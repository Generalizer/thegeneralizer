#'Return rows which values lie within a specified percentile interval.
#'
#'Use \code{set_percentile_limits} to find rows of a dataframe whose values lies within a specified percentile interval of selected variable.
#'
#'This function is compatible with dplyr. Use this to set limits on continuous variables within your data.
#'
#'@param df the data frame to transform
#'@param var the column corresponding to selected variable
#'@param min_quant lower boundary percentile
#'@param max_quant upper boundary percentile
#'@return A filtered dataframe, containing only rows meeting specified condition.
#' @examples
#' # Limiting the inference population to the middle 50% of schools in terms of their
#' # percentage of female students (using the post-secondary data).
#' set_limits(df = ipeds, var = PCT_Female, min_pct = 25, max_pct = 75)

set_percentile_limits <- function(df, var, min_pct, max_pct) {

  percentile_rank <- function(x = 0:99) {
    #set quartile
    percent_breaks <- c(-Inf,
                        quantile(x,
                                 probs = seq(0.05, 0.95, by = 0.05),
                                 na.rm = TRUE),
                        Inf)

    cut(x = x, breaks = percent_breaks, labels = FALSE)
  }

  var <- enquo(var)
  filter(df, percentile_rank(!!var) >= (1/5)*min_pct & percentile_rank(!!var) <= (1/5)*max_pct)

}
