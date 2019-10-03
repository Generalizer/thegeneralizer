#'Generate recruitment lists with added id variables based on universe dataset originated from
#'
#'@param solution object storing the output of \code{stratifier}, typically \code{generalizer_output}
#'@param custom_id a dataframe containing the ID Variable and additional id variables to be added to recruitment lists
#'@return Saves recruitment lists in project directory.
#' @examples


recruitment_list <- function(solution, custom_id){
  
  pop <- menu(choices = (c("IPEDS (Post-secondary)", "Common Core (K-12)",
                           "I'm using a custom dataset from a different universe")),
              title = cat("Which universe did your dataset come from?"))

  if(pop == 1){
    id_set <- ipeds_id
  }
  
  if(pop == 2){
    id_set <- cc_id
  }
  
  if(pop == 3){
    
    cat("You are choosing to add the following columns to your recruitment lists.\n\n")
    
    cat(paste(names(custom_id),collapse=", "))
    
    if(menu(choices = c("Yes", "No"), title = cat("\n\nDo you wish to proceed?")) == 1){
      
      id_set <- custom_id
      
    }else{
      stop("You have stopped generating recruitment lists.")
    }
  }
  
  ######## WOULD LIKE THIS TO BE  DEFAULT GENERATION BASED ON UNIVERSE
  ######## PROBABLY NEEDS ERROR TRAPPING
  
  # Generate recruitment lists
  
  nclusters <- max(solution[[1]]$clusters)
  for(i in 1:nclusters){
    Rank <- seq(1:nrow(generalizer_output[[2]][[i]]))
    recruitlist <- data.frame(Rank, generalizer_output[[2]][[i]] %>% left_join(id_set))
    recruitlist[,"CONTACTED? (Y/N)"] <- ''
    recruitlist[,"DATE OF CONTACT"] <- ''
    recruitlist[,"RESPONSE? (Y/N/NO-RESPONSE)"] <- ''
    recruitlist[,"IF NO, REASON? (OPEN RESPONSE)"] <- '' # These make sure columns are blank for data entry
    filename <- paste("recruitment_list_for_", i, ".csv", sep="")
    write.csv(recruitlist, file = filename, row.names = FALSE)
  }
  cat(nclusters, "recruitment lists have been generated, one per stratum.",
                 "They \nhave been saved as .csv files to your current working directory.",
                 "\nEach contains the ID information for the schools, ranked in \norder of desirability.",
                 "\n\nAttempt to recruit the desired proportionate number of schools \nper stratum.",
                 " If a school declines or fails to respond, recruit \nthe next school in the list.")
}
