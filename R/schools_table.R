#'Generate recruitment lists to achieve a target sample size.
#'
#'The input to this function must be the output of the \code{stratifer} function,
#'along with the sample size that the researcher aims to recruit (typically the
#'desired number of schools).
#'
#'As output, this function prints a table with columns equal to the number of
#'strata specified in \code{stratifier()} and three rows. The first row presents
#'the number of schools in the inference population that fall in each stratum. The
#'second row presents the proportion of the population in each stratum. Finally, the
#'third row contains the number of schools per stratum that the researcher should
#'aim to recruit in order to ensure that the proportions in their sample mirror
#'those in the population.
#'
#'@param solution sdfdf
#'@param number sdfsdfsd
#'@param sample Defaults to \code{NULL}.
#'@return thangs
#'@seealso \url{http://thegeneralizer.org/}, also add other resources
#' @examples
#' \dontrun{
#' # For an experiment where the researcher wants to recruit 100 schools:
#' schools_table(solution, number = 100)
#'
#' # For an experiment that has been conducted; the researcher wanted
#' # to recruit 100 schools and has uploaded their school IDs:
#' schools_table(solution, number = 100, sample = idvars)
#' }

schools_table <- function(solution, number, sample=NULL){

  if(missing(number)){
    stop("You must specify the number of schools that you aim to recruit.")
  }

  data <- solution[[3]]
    if(is.null(sample)){
    cat("Your specified goal is to recruit", number, "schools out of your inference \npopulation of",
        dim(data)[1], "schools. Ideally, these", number, "schools would be divided \nproportionally across the", max(solution[[1]]$clusters), "strata. Doing so leads to the least bias \nand no increase in standard errors.\n\n")
      }
  #### Note: should ask beth? This might be where the unit of randomization
    # plays the biggest role.
    data2 <- data.frame(data, clusters=as.character(solution[[1]]$clusters))
    num_schools <- data2 %>%
      group_by(clusters) %>%
      dplyr::summarise(count = n()) %>%
      mutate(proportion = count/(length(solution[[1]]$clusters))) %>%
      mutate(to_recruit = round(number*proportion)) %>%
      select(-clusters)
    num_schools <- t(num_schools)
    Clusters <- NULL
    for(i in 1:(max(solution[[1]]$clusters))){
      Clusters[i] <- paste("Stratum", (i), sep=' ')
    }
    colnames(num_schools) <- Clusters
    row.names(num_schools) <- c("# of Schools", "Pop. Proportion","# to Recruit")
    num_schools <- as.data.frame(num_schools)
    num_schools[2,] <- format(num_schools[2,], digits = 3)
    num_schools[1,] <- format(num_schools[1,], digits = 0)

    # Note to self -- the following warning wouldn't trigger if there were, say,
    # 6, 7, 8 schools (etc.) and there were no schools proportionally in a stratum.
    # This could sort of inspire a conversation about other warnings/notes/traps to include?
    if(number < base::max(solution[[1]]$clusters)){
      stop("Warning: You are attempting to recruit fewer schools than there are strata. Consider recruiting additional schools or changing the number of strata.")
    }
    if(is.null(sample)){
      print.data.frame(num_schools)
      if(menu(choices=c("Yes", "No"), title=cat("\nAre you ready to generate recruitment lists now?")) == 1){
        nclusters <- max(solution[[1]]$clusters)
        for(i in 1:nclusters){
          Rank <- seq(1:(num_schools[1,i]))
          recruitlist <- data.frame(Rank, solution[[2]][[i]])
          recruitlist[,"CONTACTED? (Y/N)"] <- ''
          recruitlist[,"DATE OF CONTACT"] <- ''
          recruitlist[,"RESPONSE? (Y/N/NO-RESPONSE)"] <- ''
          recruitlist[,"IF NO, REASON? (OPEN RESPONSE)"] <- '' # These make sure columns are blank for data entry
          filename <- paste("recruitment_list_for_", i, ".csv", sep="")
          write.csv(recruitlist, file = filename, row.names = FALSE)
        }
        cat(nclusters, "recruitment lists have been generated, one per stratum. They \nhave been saved as .csv files to your current working directory. \nEach contains the ID information for the schools, ranked in \norder of desirability. \n\nAttempt to recruit the desired proportionate number of schools \nper stratum. If a school declines or fails to respond, recruit \nthe next school in the list. \n\nWhen you have finished recruiting schools, return here and rerun \nthis function, setting the 'sample' argument equal to the ID \ncolumn(s) of the schools you successfully recruited.")
      }else{
        cat("Return here when you are ready to generate recruitment lists.")
      }

      return(invisible(num_schools))
  }else{

    ## Stuff in this "Else" bracket is to calculate the generalizability index
    # for samples that have already been collected.

    wpj <- as.numeric(num_schools[2,])

    # Clusters has to be a factor below in order to not drop empty groups
    # if there are no schools recruited in a given cluster.

    overall <- solution[[4]]
    overall$clusterID <- as.factor(overall$clusterID)

    sample_data <- suppressWarnings(suppressMessages(semi_join(overall, sample)))
    sample_data <- sample_data %>%
      group_by(clusterID, .drop = FALSE) %>%
      dplyr::summarise(count = n())
    sample_data <- sample_data %>%
      mutate(proportion = count/(sum(count)))

    wsj <- as.vector(sample_data$proportion)

    cat("Your specified goal was to recruit", number,
        "schools out of your \ninference population of",
        dim(solution[[4]])[1], "schools. Ideally, these", number,
        "schools \nwould be divided proportionally across the", max(solution[[4]]$clusterID),
        "strata. \n\nYou successfully recruited", sum(sample_data$count),
        "schools. \n\nThis table displays the proportion of schools per stratum \nin your sample and in your inference population. The more similar \nthese proportions are, the better your generalizability.\n\n")

    overall_outcomes <- data.frame(rbind(wpj, wsj))
    rownames(overall_outcomes) <- c("Proportion in Population",
                                    "Proportion in Sample")
    Clusters <- NULL
    for(i in 1:(max(solution[[1]]$clusters))){
      Clusters[i] <- paste("Stratum", (i), sep=' ')
    }
    colnames(overall_outcomes) <- Clusters

    print(overall_outcomes)

    generalizability_index <- sum(sqrt(wpj * wsj))
    cat("\nThe sample of", sum(sample_data$count), "schools you recruited has a \ngeneralizability index of", format(generalizability_index, digits = 4), "relative to the \ninference population you selected.")

    # The next bit is to make sure they pay attention to their index score, ESPECIALLY
    # if it's bad.

    you_done_goofed <- paste("\n\nCAUTION: Your generalizability index is below 0.50. \nGeneralizations are COMPLETELY UNWARRANTED (based upon \nthe covariates you selected, ", paste(colnames(solution[[3]]), collapse=', '), ").", sep='')

    you_done_kinda_goofed <- paste("\n\nCAUTION: Your generalizability index is between 0.50 and 0.90. \nThis means that your sample is not a miniature of your inference \npopulation. However, it may be similar enough for statistical \nadjustments to compensate. Adjustments are more likely to be \neffective if your index is closer to 0.90 than 0.50. For \nguidelines about making such adjustments, see the package references.")

    if(generalizability_index < 0.50){
      cat(red$bold(you_done_goofed))
    }else{
      if(generalizability_index >= 0.50 & generalizability_index < 0.90){
        cat(red$bold(you_done_kinda_goofed))
      }
      if(generalizability_index >= 0.90){
        cat(blue$bold("\n\nSuccess! \n\nYour sample should be as similar to your inference population \nas a random sample of the same size on the covariates you \nselected (", paste(colnames(solution[[3]]), collapse=', '), ").", sep=''))
        }
    }
  }
}
