#'Generate recruitment lists to achieve a target sample size.
#'
#'The input to this function must be the output of the \code{stratifer} function,
#'along with the sample size that the researcher aims to recruit (typically the
#'desired number of units).
#'
#'As output, this function prints a table with columns equal to the number of
#'strata specified in \code{stratifier()} and three rows. The first row presents
#'the number of units in the inference population that fall in each stratum. The
#'second row presents the proportion of the population in each stratum. Finally, the
#'third row contains the number of units per stratum that the researcher should
#'aim to recruit in order to ensure that the proportions in their sample mirror
#'those in the population.
#'
#'@param solution object storing the output of \code{stratifier}; usually called \code{generalizer_output}
#'@param number total desired sample size
#'@param sample Defaults to \code{NULL}.
#'@return thangs
#'@seealso \url{http://thegeneralizer.org/}, also add other resources
#' @examples
#' \dontrun{
#' # For an experiment where the researcher wants to recruit 100 units:
#' units_table(solution, number = 100)
#'
#' # For an experiment that has been conducted; the researcher wanted
#' # to recruit 100 units and has uploaded their unit IDs:
#' units_table(solution, number = 100, sample = idvars)
#' }

recruitment <- function(solution, number, sample = NULL){

  if(missing(number)){
    stop("You must specify the number of participants that you aim to recruit.")
  }

  data <- solution[[3]]
    if(is.null(sample)){
    cat("Your specified goal is to recruit", number, "participants out of your inference \npopulation of",
        dim(data)[1], "participants. Ideally, these", number, "participants would be divided \nproportionally across the", max(solution[[1]]$clusters), "strata. Doing so leads to the least bias \nand no increase in standard errors.\n\n")
      }


    data2 <- data.frame(data, clusters=as.character(solution[[1]]$clusters))
    num_units <- data2 %>%
      group_by(clusters) %>%
      dplyr::summarise(count = n()) %>%
      mutate(proportion = count/(length(solution[[1]]$clusters))) %>%
      mutate(to_recruit = round(number*proportion)) %>%
      select(-clusters)
    num_units <- t(num_units)
    Clusters <- NULL
    for(i in 1:(max(solution[[1]]$clusters))){
      Clusters[i] <- paste("Stratum", (i), sep=' ')
    }
    colnames(num_units) <- Clusters
    row.names(num_units) <- c("# of Participants", "Pop. Proportion","# to Recruit")
    num_units <- as.data.frame(num_units)
    num_units[2,] <- format(num_units[2,], digits = 3)
    num_units[1,] <- format(num_units[1,], digits = 0)

    # Note to self -- the following warning wouldn't trigger if there were, say,
    # 6, 7, 8 units (etc.) and there were no units proportionally in a stratum.
    # This could sort of inspire a conversation about other warnings/notes/traps to include?
    if(number < base::max(solution[[1]]$clusters)){
      stop("Warning: You are attempting to recruit fewer participants than there are strata. Consider recruiting additional participants or changing the number of strata.")
    }
    if(is.null(sample)){
      print.data.frame(num_units)
      if(menu(choices=c("Yes", "No"), title=cat("\nAre you ready to generate recruitment lists now?")) == 1){
        nclusters <- max(solution[[1]]$clusters)
        for(i in 1:nclusters){
          Rank <- seq(1:(num_units[1,i]))
          recruitlist <- data.frame(Rank, solution[[2]][[i]])
          recruitlist[,"CONTACTED? (Y/N)"] <- ''
          recruitlist[,"DATE OF CONTACT"] <- ''
          recruitlist[,"RESPONSE? (Y/N/NO-RESPONSE)"] <- ''
          recruitlist[,"IF NO, REASON? (OPEN RESPONSE)"] <- '' # These make sure columns are blank for data entry
          filename <- paste("recruitment_list_for_", i, ".csv", sep="")
          write.csv(recruitlist, file = filename, row.names = FALSE)
        }
        cat(nclusters, "recruitment lists have been generated, one per stratum. They \nhave been saved as .csv files to your current working directory. \nEach contains the ID information for the participants, ranked in \norder of desirability. \n\nAttempt to recruit the desired proportionate number of participants \nper stratum. If a participant declines or fails to respond, recruit \nthe next participant in the list. \n\nWhen you have finished recruiting participants, return here and rerun \nthis function, setting the 'sample' argument equal to the ID \ncolumn(s) of the participants you successfully recruited.")
      }else{
        cat("Return here when you are ready to generate recruitment lists.")
      }

      return(invisible(num_units))
  }else{

    ## Stuff in this "Else" bracket is to calculate the generalizability index
    # for samples that have already been collected.

    # Clusters has to be a factor below in order to not drop empty groups
    # if there are no units recruited in a given cluster.

    overall <- solution[[4]]
    overall$clusterID <- as.factor(overall$clusterID)

    sample <- sample %>%
      clean_names() %>%
      mutate(trial = rep(1, dim(sample)[1])) %>%
      mutate(unitid = factor(unitid)) %>%
      select(unitid, trial)
    suppressWarnings({
      test_sample <- sample %>%
        full_join((ipeds %>% clean_names()), by = c("unitid")) %>%
        select(unitid, trial, colnames(solution$data)) %>%
        replace_na(list(trial = 0)) %>%
        data.frame()
    })

    test_output <- assess(data = test_sample, trial = "trial",
           selection_covariates = colnames(solution$data),
           is_data_disjoint = TRUE)

    pop_size <- dim(solution[[4]])[1]
    num_strata <- max(solution[[4]]$clusterID)
    num_recruited <- dim(sample)[1]
    generalizability_index <- test_output$g_index

    cat("Your specified goal was to recruit", number,
        "participants out of your \ninference population of",
        pop_size, ". Ideally, these", number,
        "participants \nwould be divided proportionally across the", num_strata,
        "strata. \n\nYou successfully recruited", num_recruited,
        "participants. \n\nThis table displays the average value of each covariate in \nyour sample and in your inference population. The more \nsimilar these values are, the better your generalizability.\n\n")

    print(test_output$covariate_table)

    cat("\nThe sample of", num_recruited, "participants you recruited has a \ngeneralizability index of", format(generalizability_index, digits = 4), "relative to the \ninference population you selected.")

    below_50 <- paste("\n\nCAUTION: Your generalizability index is below 0.50. \nGeneralizations are COMPLETELY UNWARRANTED (based upon \nthe covariates you selected, ", paste(colnames(solution[[3]]), collapse=', '), ").", sep='')

    between_50_90 <- paste("\n\nCAUTION: Your generalizability index is between 0.50 and 0.90. \nThis means that your sample is not a miniature of your inference \npopulation. However, it may be similar enough for statistical \nadjustments to compensate. Adjustments are more likely to be \neffective if your index is closer to 0.90 than 0.50. For \nguidelines about making such adjustments, see the package references.")

    if(generalizability_index < 0.50){
      cat(red$bold(below_50))
    }else{
      if(generalizability_index >= 0.50 & generalizability_index < 0.90){
        cat(red$bold(between_50_90))
      }
      if(generalizability_index >= 0.90){
        cat(blue$bold("\n\nSuccess! \n\nYour sample should be as similar to your inference population \nas a random sample of the same size on the covariates you \nselected (", paste(colnames(solution[[3]]), collapse=', '), ").", sep=''))
        }
    }
  }
}
