#'Stratify a population data frame.
#'
#'This function is designed to receive a data frame containing information about
#'an inference population.
#'
#'...
#'
#'To complete the generalization process, the user should run this function on
#'their desired inference population data frame and save the output to an object.
#'The resulting object, referred to as \code{solution} in the package documentation,
#'should then be provided as input to the \code{mean_table()} and \code{schools_table()}
#'functions.
#'
#'@param data A data frame containing information about schools in your
#'  inference population. Must contain a column of ID numbers identifying each
#'  school, along with columns of stratifying variables.
#'@param metric A character string specifying the metric to be used in
#'  calculating distances for observations. The default is \code{gower}; other
#'  options are \code{euclidean} and \code{manhattan}.
#'@return A list. The first element contains the raw results of stratifying the
#'  data into the user-selected number of clusters. The second element contains
#'  several lists, each ranking the observations within clusters.
#'@seealso \url{http://thegeneralizer.org/}, also add other resources
#' @examples
#' \dontrun{
#' stratifier(x)
#' }

stratifier <- function(x, metric = "gower"){

# Define inference population  ---------------------------------------------

  pop <- menu(choices = (c("IPEDS (Post-secondary)", "Common Core (K-12)",
                         "I'm using a custom dataset from a different universe",
                         "This is not my inference population dataset.")),
              title = cat("Your chosen inference population is the '", deparse(substitute(x)),
                          "' dataset. Which universe did this come from?", sep=""))

  #Error message
  if(pop == 4){
    stop("Run stratifier() on your inference population dataset.")
  }

  #IPEDS/K12 dataset
  if(pop == 1 | pop == 2){

    cat("You have chosen the ")
    if(pop == 1){
      cat("IPEDS")
      idnum <- c("unitid", "instnm", "addr", "state", "zip", "gentele", "webaddr")
      } else {
        cat("K-12")}
    cat(" dataset.")

    cat("\n\nIf you would like to adjust or restrict your inference population (for example, if you are interested in only one location, etc.), make sure that you have altered the data frame appropriately. \n\n")

  }

  #Custom dataset
  if(pop == 3){

    read_id <- function()
    {
      cat("\n")
      n <- readline(prompt = "Enter the name of the ID Variable in your dataset: ")
      if(!n %in% names(x))
      {
        cat("We could not find that variable. Please make sure your dataset contains an variable containing IDs of your data.",
            "If necessary, press 'esc' to leave stratifier()")
        return(read_id())
      }
      return(n)
    }
    idnum <- read_id()
    cat("\n\nIf you want to adjust or restrict your inference population (e.g., if you are interested in only one location, etc.), make sure that you have altered the data frame appropriately. \n")
  }
0
  filterhelp <- menu(choices = c("Ready to proceed", "Need help filtering"), title = cat("If you need to alter the data frame, enter 0 to exit; you can use " %+% bold("dplyr::filter()") %+% " or " %+% bold("set_percentile_limits()") %+% " and return.\n"))
  if(filterhelp == 2){
    inference_help()
    return()
  }

  # Selecting the ID column(s)

  id <- x %>% select(idnum)

  # From now on the id column(s) is separated from the rest of the data frame, they're stored as "id".
  # "idnum" is a vector of the id column(s).

  x <- x %>% select(-idnum)

# Select stratifying variables --------------------------------------------

  cat("\nYou're now ready to select your stratification variables. The following are the variables available in your dataset. \n")

  if(pop == 3){
    cat(red$bold("\nDo note that the stratifier function will only accept continuous variables and binary (dummy) variables.\n"))
    }

  var_select <- function(){

    names <- switch(pop,
                    "1" = c("pct_grant_aid", "pct_pell", "pct_fed_loans", "pct_any_aid", "urbanicity",
                            "control_level", "total_undergrad_lg", "admit_rate",
                            "multisystem_status", "hs_remedial_services",
                            "remedial_services", "pct_female", "pct_nat_aa", "pct_asian", "pct_black", "pct_hispanic",
                            "pct_nat_hpi", "pct_white", "pct_low_inc"
                            ),
                    "2" = c("to be inserted. K12 not ready"),
                    "3" = names(x),
                    "4" = c(""))

    n <- select.list(choices = names,
                     title = cat("\nWhich key variables do you think may explain variation in your treatment effect?",
                                 "Typically, studies include up to 10 variables for stratification.\n"),
                     graphics = FALSE, multiple = TRUE)

    return(n)
  }

  variables <- var_select()

  if(length(variables) >= 1){

    #IPEDS customisation
    if(pop == 1 & "Urbanicity" %in% variables){

      variables <- c(variables, "City", "Suburb", "Town")
      variables <- variables[!variables %in% "Urbanicity"]

    }

    if(pop == 1 & "Control_Level (Public/Private/For Profit)" %in% variables){

      variables <- c(variables, "Public", "Private_Notforprofit")
      variables <- variables[!variables %in% "Control_Level (Public/Private/For Profit)"]

    }

    x <- x %>%
      select(variables)

  }else{
    stop("Make sure that you select some stratifying variables.")
  }

  # Verifying the variables

  if(menu(choices=c("Yes", "No"),
          title=cat("\nYou have selected the following stratifying variables: ",
                    paste(colnames(x), collapse=', '), paste(". \nIs this correct?\n"), sep="")) == 1){

  }else{
    stop("Make sure that you have selected the stratifying variables you want to use.")
  }

# Distribution and summary info about stratifying variables -----------

  cat("\nHere are summary statistics and histograms of each of your stratifying variables.\n\n")

  sumstats <- x %>%
    summarize_all(list(base::mean, stats::sd, base::min, base::max), na.rm=TRUE) # I don't know why I have to tell it this
  # but if I don't sometimes it randomly decides that it doesn't know
  # what one or more of the functions is
  means <- sumstats %>% select(contains("fn1"))
  sds <- sumstats %>% select(contains("fn2"))
  mins <- sumstats %>% select(contains("fn3"))
  maxs <- sumstats %>% select(contains("fn4"))
  colnames(means) <- colnames(x); colnames(sds) <- colnames(x)
  colnames(mins) <- colnames(x); colnames(maxs) <- colnames(x)
  sumstats_table <- data.frame(rbind(means, sds, mins, maxs))
  row.names(sumstats_table) <- c("Mean", "Standard Deviation", "Minimum Value", "Maximum Value")
  print(sumstats_table, digits = 4)

  suppressMessages(
    meltedx <- x %>%
      melt(variable.name = 'variable')
  )
  suppressWarnings(
    suppressMessages(
        for(i in 1:(length(levels(meltedx$variable)))){
          df <- meltedx %>%
            dplyr::filter(as.numeric(meltedx$variable) == i)
          hist <- ggplot(data = df, aes(value)) +
            geom_histogram(bins = 30) +
            theme_base() +
            labs(title = paste("Histogram of", levels(meltedx$variable)[i]))
          print(hist)
          par(ask = TRUE)
        }
      )
  )
  par(ask = FALSE)

  if(menu(choices = c("Yes", "No"), title = cat("\nDo you wish to proceed?")) == 1){

  }else{
    stop("You have stopped the stratification process.")
  }


# Clustering ---------------------------------------------------------

  clusterchoice <- (menu(choices = c(4, 5, 6, 7, 8),
                         title = cat("Choose a number of strata to divide your population into.",
                                     "\n\nTypically, the more strata, the better.",
                                     "However, increasing\nthe number of strata uses more resources,",
                                     "because you must recruit a given \nnumber of schools from each stratum.",
                                     "\n\nTherefore, choose a larger number if possible, and only if you have the",
                                     "resources to accommodate it. Otherwise, choose a smaller number.")) + 3)
  if(clusterchoice < 4 | clusterchoice > 8){
    stop("You should choose a number of clusters between 4 and 8.")
  }

  cat("This might take a little while. Please bear with us.")

  set.seed(111) # Can change this to whatever Programmer Katie uses
  # to see if we can match the web app results.

  # Soo I learned today that Kmeans breaks if there are ANY missing values in the distance matrix.
  # It seems that, although the gower metric is pretty good at handling missing data,
  # if there is a TON of missing data, it will still fail and leave
  # NAs in distance.
  # So this next line says to automatically use multiple imputation (mice) to fill in missing values.
  # Sometimes it may not be necessary at all, but some variables have a lot of missing data.
  suppressMessages(
    x <- mice::complete(mice(x, print = FALSE, m = 1), 1)
    )
  cat("\n1: Imputed missing data using mice package.")
  suppressWarnings(distance <- daisy(x, metric=metric))
  cat("\n2: Calculated distance matrix.")
  solution <- KMeans_rcpp(as.matrix(distance), clusters=clusterchoice, verbose = TRUE)


# Reattaching ID variable -------------------------------------------------

  x2 <- data.frame(id, x, clusterID = solution$clusters)
  sortedschools <- list(NULL)

  for(i in 1:clusterchoice){
    dat3 <- x2 %>%
      dplyr::filter(clusterID == i)
    idvar <- dat3 %>% select(idnum)
    dat4 <- dat3 %>% select(-c(idnum, clusterID))
    mu <- moment(dat4, order=1, central=FALSE) # population mean of stratifying vars
    v <- var(dat4)
    a <- diag(v)

    if(any(a == 0)){ a[which(a == 0)] <- 0.00000001 }
    cov.dat <- diag(a)
    ma.s <- mahalanobis(dat4,mu,cov.dat)
    dat4 <- data.frame(idvar, dat4, distance = ma.s, clusterID = dat3$clusterID)
    sortedschools[[i]] <- dat4 %>% # Produces a list of data frames, one per stratum, sorted by
      # distance (so the top N schools in each data frame are the "best," etc.)
      arrange(distance) %>%
      select(idnum)
  }

  cat(blue$bold("Congratulations, you have successfully grouped your data into", clusterchoice,
      "strata! \nNext, run mean_table(generalizer_output) and schools_table(generalizer_output)."))

  generalizer_output <<- list(solution, sortedschools, data=x, iddata=x2, idvar=idnum)

  return(invisible(generalizer_output))

}
