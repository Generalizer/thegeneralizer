#'Calculate and visualize the means of stratifying variables across strata.
#'
#'This function is designed to be run after using \code{stratifier} on the output that is produced. It produces a table of the population means and standard deviations of the stratification variables selected in each cluster. It also presents some visualizations of the population cluster information, which can help users describe the clusters.
#'
#'The function by default does not store this information, but users can save the plots manually, or specify the argument \code{store} to export the table and plots to files in their current working directory.
#'
#'@param solution object storing the output of \code{stratifier}; usually called \code{generalizer_output}
#'@param plots defaults to \code{TRUE}; whether or not to create plots
#'@param store defaults to \code{FALSE}; whether to export table and plots to files
#'@return a table of the mean values of selected stratifying variables across strata
#'@seealso \url{http://thegeneralizer.org/}, also add other resources
#' @examples
#' \dontrun{
#' # To view a table of means along with graphs:
#' mean_table(solution)
#' }

mean_table <- function(solution, plots = TRUE, store = FALSE){

  # allow the option for color choice
  # look around for other visualizations

  data <- solution[[3]]

  n_clusters <- max(solution[[1]]$clusters)

  cat("\n\nYou have specified ")
  cat(bold (n_clusters))
  cat(" strata, which explains ")
  cat(bold(100*round(solution[[1]]$between.SS_DIV_total.SS, 4), "%"))
  cat(" of the total variation in the population")

  cat("The following table presents \nthe average value (mean) for each covariate for each stratum. \nThe first row, 'All,' presents the average values for the \nentire inference population. The last column, '# of \nSchools,' lists the total number of schools in the inference \npopulation that fall within each stratum.\n\n")

  data <- data.frame(data, clusters=as.character(solution[[1]]$clusters))

  simtab_pop <- data %>%
    dplyr::group_by(clusters) %>%
    dplyr::summarize_if(is.numeric, base::mean) %>%
    select(-clusters)

  num_schools <- data %>%
    group_by(clusters) %>%
    summarize(count = n()) %>%
    mutate("Number of Schools" = count) %>%
    select(-c(clusters, count)) %>%
    add_row("Number of Schools" = length(solution[[1]]$clusters), .before=1)

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

  if(plots == TRUE){

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

    par(ask = TRUE)
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

    cat("\nThis heat map compares the average values of the covariates in \nthe strata to the average values in the population. Strata with \nhigher average values are blue; strata with lower average values \nare red. Strata whose average values are close to that of the \npopulation are white. The number in each tile is the actual mean value. \n\n")

    grayheat <- ggplot(data = heatdata) +
      geom_tile(aes(x = Clusters, y = Stratifying_Variable, fill = Deviation), width = 0.95) +
      geom_text(aes(x = Clusters, y = (ncol(final_table)/2 - 0.15), label = paste(Schools, "\nschools")),
                size = 3.2) +
      geom_label(aes(x = Clusters, y = Stratifying_Variable,
                     label = paste0(round(Mean, 1), "\n(", round(SD, 1), ")")),
                 colour = "black", alpha = 0.7, size = ifelse(ncol(final_table)/2 > 7, 2, 3.5)) +
      geom_hline(yintercept = seq(1.5,ncol(final_table) - 2 ,1), linetype = "dotted", colour = "white") +
      scale_fill_gradientn(name = NULL,
                           breaks = c(-0.5, 0, 0.5), labels = c("50% \nBelow Mean","Population\nMean","50% \nAbove Mean"),
                           colours = c("white","grey70", "grey50", "grey20", "black"),
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

    print(grayheat)

    cat("\nThis heat map presents the same information without using color.\n\n")

    heatdata <- heatdata %>%
      mutate(fillcolor = ifelse(Deviation < 0, "red3", "royalblue3"))

    facetwrappedbars <- ggplot(heatdata) +
      geom_bar(aes(x = "", y = Deviation, fill = fillcolor),
               stat = "identity", show.legend = FALSE) +
      geom_text(aes(x = "", y = ifelse(Clusters == "Total", 0.25, 0.55),
                    label = paste0(round(Mean, 1), "\n(", round(SD, 1), ")"))) +
      geom_hline(yintercept = 0) +
      ylim(c(-0.7, 0.7)) +
      labs(x = NULL, y = NULL) +
      facet_grid(Stratifying_Variable ~ Clusters, switch = "y") +
      theme(axis.ticks = element_blank(), axis.text = element_blank()) +
      scale_fill_identity()

    print(facetwrappedbars)

    cat("\nThis plot presents how much the average value of each strata deviates from the population average, per variable. Below average values are blue, while above average values are red. The black line represents the population average. \n\n")

    par(ask = FALSE)

  }
  return(invisible(final_table))

}
