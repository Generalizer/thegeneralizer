stratify <- function(data, guided = TRUE){

  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));

  if(guided == TRUE){
    cat("Your chosen inference population is the '",
        deparse(substitute(data)), "' dataset.", sep="")

    cat("\n")
    cat("\n")

    idnum <- readline(prompt = "Enter the name of the ID Variable in your dataset: ")

    if(!idnum %in% names(data))
    stop(simpleError("We could not find that variable. Please make sure your \ndataset contains an ID variable."))

    cat("\n\nIf you want to adjust or restrict your inference population (e.g., if you are interested in only one location, etc.), make sure that you have altered the data frame appropriately. \nIf you need to alter your data frame, you can exit this function, use " %+% blue$bold("dplyr::filter()") %+% " or " %+% blue$bold("set_percentile_limits()") %+% ", and return.\n")

    if(menu(choices = c("Yes", "No"), title = cat("\nDo you wish to proceed?")) == 1){

    }else{
      stop(simpleError(blankMsg))
    }

    id <- data %>% select(all_of(idnum))
    data <- data %>% select(-all_of(idnum))

    cat("\nYou're now ready to select your stratification variables. The following are the variables available in your dataset.")

    names <- names(data)
    variables <- select.list(choices = names,
                     title = cat("\nWhich key variables do you think may explain variation in your treatment effect?",
                                 "Typically, studies include up to 10 variables for stratification.\n"),
                     graphics = FALSE, multiple = TRUE)

    if(length(variables) >= 1){
      data <- data %>%
        select(all_of(variables))
    }else{
      stop("You have to select some stratifying variables.")
    }

    var_overview <- skimr::skim(data) %>% tibble() %>%
      distinct(variable, type) %>%
      data.frame()
    colnames(var_overview) <- c("Variable", "Type")

    cat("\nYou have selected the following stratifying variables: \n")
    cat(paste(blue$bold(colnames(data)), collapse = ", "), ".\n\n", sep = "")
    print(var_overview, row.names = FALSE)

    if(menu(choices = c("Yes", "No"), title = cat("\nIs this correct?")) == 1){

    }else{
      stop(simpleError(blankMsg))
    }

    cat_data <- data %>%
      select_if(is.factor)
    cat_data_vars <- names(cat_data)
    if(dim(cat_data)[2] >= 1){
      cat_data_plot <- data.frame(cat_data)
      cat("Please review the descriptive statistics of your categorical variables (factors). Note that these will automatically be converted to dummy variables for analysis.\n")
      for(i in 1:(ncol(cat_data_plot))){
        var_name <- cat_data_vars[i]
        print(table(cat_data_plot[,i]))
        barfig <- ggplot(data = cat_data_plot, aes(x = cat_data_plot[,i])) +
          geom_bar() +
          theme_base() +
          xlab(var_name) +
          labs(title = paste("Bar Chart of", var_name))
        print(barfig)
        cat("\n")
        par(ask = TRUE)
      }
    }

    cont_data <- data %>%
      select_if(negate(is.factor))
    cont_data_vars <- names(cont_data)
    if(dim(cont_data)[2] >= 1){
      sumstats <- skimr::skim(cont_data) %>% tibble() %>%
        filter(stat != "hist" & stat != "missing" & stat != "n" & stat != "complete") %>%
        select(-type, -level, -formatted) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(Stat = stat) %>%
        select(Stat, everything(), -stat) %>%
        data.frame()
      cat("Please review the descriptive statistics of your continuous variables.\n\n")
      print(sumstats, row.names = FALSE)
      for(i in 1:ncol(cont_data)){
        cont_data_plot <- cont_data %>% data.frame()
        suppressWarnings(
          suppressMessages(
            hist <- ggplot(data = cont_data_plot, aes(x = cont_data_plot[,i])) +
              geom_histogram(bins = 30) +
              theme_base() +
              xlab(cont_data_vars[i]) +
              labs(title = paste("Histogram of", cont_data_vars[i]))
          )
        )
        print(hist)
      }
    }
    par(ask = FALSE)

    cat("Enter a number of strata to divide your population into. Typically, \nthe more strata, the better. However, increasing the number of strata \nuses more resources, because you must sample a given number of units \nfrom each stratum. Therefore, choose a larger number if possible, and \nonly if you have the resources to accommodate it. Otherwise, \nchoose a smaller number.")
    n_strata <- as.numeric(readline(prompt = "# of strata: "))

    cat("This might take a little while. Please bear with us.")

    cat_data <- fastDummies::dummy_cols(cat_data, remove_first_dummy = TRUE)
    data_full <- cbind(cat_data, cont_data)

    suppressWarnings(distance <- daisy(data_full, metric = metric))
    cat("\n1: Calculated distance matrix.")
    solution <- KMeans_rcpp(as.matrix(distance), clusters = n_strata, verbose = TRUE)

    x2 <- data.frame(id, data_full, clusterID = solution$clusters)
    sortedschools <- list(NULL)

    for(i in 1:n_strata){
      dat3 <- x2 %>%
        dplyr::filter(clusterID == i)
      idvar <- dat3 %>% select(all_of(idnum))
      dat4 <- dat3 %>% select(-c(all_of(idnum), clusterID))
      mu <- moment(dat4, order = 1, central = FALSE) # population mean of stratifying vars
      v <- var(dat4)
      a <- diag(v)

      if(any(a == 0)){ a[which(a == 0)] <- 0.00000001 }
      cov.dat <- diag(a)
      ma.s <- mahalanobis(dat4, mu, cov.dat)
      dat4 <- data.frame(idvar, dat4, distance = ma.s, clusterID = dat3$clusterID)
      sortedschools[[i]] <- dat4 %>% # Produces a list of data frames, one per stratum, sorted by
        # distance (so the top N schools in each data frame are the "best," etc.)
        arrange(distance) %>%
        select(idnum)
    }

  }else{

  }

  return(solution)
}
