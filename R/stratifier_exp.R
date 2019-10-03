stratifier_exp <- function(x, metric = "gower"){
  
    cat("Welcome to the expedited version of stratifier. If this is your first time using the generalizer,",
      " we recommend running the guided version, stratifier, instead. Using stratifier_exp requires that you have already",
      " filtered your dataset and selected your stratifying variables. Press [esc] to exit if necessary.\n")
  
  # Define inference population  ---------------------------------------------
  
    read_id <- function()
    {
      cat("\n")
      n <- readline(prompt = "Enter the name of the ID Variable in your dataset: ")
      if(!n %in% names(x))
      {
        cat("We could not find that variable. Please make sure your dataset has a variable containing IDs of your data.",
            "If necessary, press [esc] to leave stratifier()")
        return(read_id())
      }
      return(n)
    }
    idnum <- read_id()
  
  # Selecting the ID column(s)
  
  id <- x %>% select(idnum)
  
  # From now on the id column(s) is separated from the rest of the data frame, they're stored as "id".
  # "idnum" is a vector of the id column(s).
  
  x <- x %>% select(-idnum)
  
  # Select stratifying variables --------------------------------------------
  
  cat("\nYou're now ready to select your stratification variables.",
      "The following are the current stratifying variables selected. \n\n")
  
  cat(paste(names(x),collapse=", "))
  
  # Verifying the variables
  
  cat(red("\n\nPress [esc] to exit and remove the variables you do not wish to include in the stratification.",
      "Else, proceed."))
  
  if(menu(choices = c("Yes", "No"), title = cat("\nDo you wish to proceed?")) == 1){
    
  }else{
    stop("You have stopped the stratification process.")
  }
  
  # Clustering ---------------------------------------------------------
  
  clusterchoice <- (menu(choices = c(4, 5, 6, 7, 8),
                         title = cat("Choose a number of strata to divide your population into")) + 3)
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
  
  generalizer_output <<- list(solution, sortedschools, data=x, iddata=x2, idvar=idnum)
  
  readline(prompt = "Press [enter] to view the results")
  
 # Heatmap generation -------------------------------------------

    n_clusters <- max(solution$clusters)

    cat("\n\nYou have specified ")
    cat(bold (n_clusters)) 
    cat(" strata, which explains ")
    cat(bold(100*round(solution$between.SS_DIV_total.SS, 4), "%"))
    cat(" of the total variation in the population")
        
    cat("\n\nThe following table presents the average value (mean) for each covariate for each stratum.\nThe first row, 'All,' presents the average values for the entire inference population. \nThe last column, '# of Schools,' lists the total number of schools in the inference population \nthat fall within each stratum.\n\n")

    data <- data.frame(x, clusters=as.character(solution$clusters))

    simtab_pop <- data %>%
      dplyr::group_by(clusters) %>%
      dplyr::summarize_if(is.numeric, base::mean) %>%
      select(-clusters)

    num_schools <- data %>%
      group_by(clusters) %>%
      summarize(count = n()) %>%
      mutate("Number of Schools" = count) %>%
      select(-c(clusters, count)) %>%
      add_row("Number of Schools" = length(solution$clusters), .before=1)

    Clusters <- "Population"
    for(i in 2:(n_clusters+1)){
      Clusters[i] <- paste("Stratum", (i - 1))
    }

    simtab_m <- data %>%
      select(-clusters) %>%
      summarize_all(base::mean)

    simtab_sd <- data %>% select(-clusters) %>% summarize_all(sd)
    simtab_sd_pop <- data %>% group_by(clusters) %>% summarize_if(is.numeric, sd) %>%
      mutate(clusters = factor(clusters)) %>% select(-clusters)

    meantab <- bind_rows(simtab_m, simtab_pop)
    sdtab <- bind_rows(simtab_sd, simtab_sd_pop)
    final_table <- data.frame(matrix(NA, ncol=(ncol(meantab)*2), nrow=(nrow(meantab))))
    odd_vals <- seq(1, ncol(final_table), by=2)
    even_vals <- seq(2, ncol(final_table), by=2)
    final_table[,odd_vals] <- meantab
    final_table[,even_vals] <- sdtab
    final_table <- data.frame(Clusters, final_table, num_schools)
    colnames(final_table)[(even_vals+1)] <- "SD"
    colnames(final_table)[(odd_vals+1)] <- colnames(simtab_m)
    colnames(final_table)[ncol(final_table)] <- "# of Schools"
    print(final_table, digits = 4)

    #Heatmap function

      mean <- simtab_m %>%
        tidyr::gather(key = Stratifying_Variable, value = Pop_Mean)

      sd_heat <- sdtab %>%
        mutate(Clusters = Clusters) %>%
        gather(key = Stratifying_Variable, value = SD, - Clusters)

      schools <- num_schools %>%
        mutate(Clusters = Clusters) %>%
        rename(Schools = `Number of Schools`)

      mean_heat <- meantab %>%
        mutate(Clusters = Clusters) %>%
        gather(key = Stratifying_Variable, value = Mean, - Clusters)

      heatdata <- mean_heat %>%
        left_join(sd_heat, by = c("Clusters", "Stratifying_Variable")) %>%
        left_join(mean, by = "Stratifying_Variable") %>%
        left_join(schools, by = "Clusters") %>%
        mutate(Deviation = case_when((Mean - Pop_Mean)/Pop_Mean >= 0.7 ~ 0.7,
                                     (Mean - Pop_Mean)/Pop_Mean <= -0.7 ~ -0.7,
                                     TRUE ~ (Mean - Pop_Mean)/Pop_Mean)) %>%
        mutate(Clusters = ifelse(Clusters == "Population", "Total", Clusters))
      
      #Preserve levels
      heatdata$Stratifying_Variable <- factor(heatdata$Stratifying_Variable,
                                              levels = rev(unique(heatdata$Stratifying_Variable)))
      
      #Heatmap
            heat <- ggplot(data = heatdata) +
        geom_tile(aes(x = Clusters, y = Stratifying_Variable, fill = Deviation), width = 0.95) +
        geom_text(aes(x = Clusters, y = (ncol(final_table)/2 - 0.15), label = paste(Schools, "\nschools")),
                  size = 3.4) +
        geom_label(aes(x = Clusters, y = Stratifying_Variable,
                       label = paste0(round(Mean, 1), "\n(", round(SD, 1), ")")),
                   colour = "black", alpha = 0.7, size = ifelse(ncol(final_table)/2 > 7, 2, 3.5)) +
        geom_hline(yintercept = seq(1.5,ncol(final_table) - 2 ,1), linetype = "dotted", colour = "white") +
        scale_fill_gradientn(name = NULL,
                             breaks=c(-0.5, 0, 0.5), labels = c("50% \nBelow Mean","Population\nMean","50% \nAbove Mean"),
                             colours=c("#990000","#CC0000", "white", "#3D85C6", "#0B5294"),
                             limits = c(-0.7, 0.7)) +
        scale_x_discrete(position = "top", expand = c(0, 0), labels = c(Clusters[-1], "Population")) +
        expand_limits(y = c(0, ncol(final_table)/2 + 0.1)) +
        labs(y = NULL, x = NULL) +
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(size = 10, colour = "grey15"),
              legend.key.height = unit(1, "cm"),
              legend.text = element_text(size = 10),
              legend.position = "right")

      print(heat)
      
      
      cat("\nThis heat map compares the average values of the covariates in \nthe strata to the average values in the population.", 
"Strata with \nhigher average values are blue; strata with lower average values \nare red.",
"Strata whose average values are close to that of the \npopulation are white. The number in each tile is the actual mean value. \n\n")
      
      cat(blue("We've saved your stratification output as"), blue$bold("'generalizer_output'"), 
          blue("in the Global Environment. To generate recruitment lists, run recruitment_list(generalizer_output)."))
      
      return(invisible(generalizer_output))
  
}

