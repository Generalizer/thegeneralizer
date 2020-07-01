assess_new <- function(sample, population, join_var = NULL, grouping_var = NULL){

  # Note: join_var and grouping_var have to be in quotes; join_var = "unitid"
  # Also, the variable being joined on has to be same type in each
  # Sample should just be a tibble with column of ids
  # Population should be a tibble with ids, stratifying variables, and (if applicable)
  # a grouping variable
  sample <- sample %>%
    clean_names() %>%
    mutate(trial = rep(1))

  sample_and_pop <- sample %>%
    full_join((population %>% clean_names()), by = join_var) %>%
    replace_na(list(trial = 0)) %>%
    data.frame()

  if(is.null(grouping_var)){
    selection_vars <- colnames(
      (sample_and_pop %>% select(-all_of(join_var), -trial))
    )

    output <- assess(data = sample_and_pop, trial = "trial",
              is_data_disjoint = TRUE, selection_covariates = selection_vars)
  }
  if(!is.null(grouping_var)){
    selection_vars <- colnames(
      (sample_and_pop %>% select(-all_of(join_var), -trial, -all_of(grouping_var)))
    )

    output <- sample_and_pop %>%
      group_by_at(grouping_var) %>%
      group_map(~ suppressWarnings(tryCatch(assess(data = data.frame(.x), trial = "trial", is_data_disjoint = FALSE, selection_covariates = selection_vars), error = function(err) NA)))

    g_indexes <- unlist(map(output, function(x){x["g_index"][[1]]}))
    cov_matrices <- map(output, function(x){x["covariate_table"][[1]]})

    output <- list(output, g_indexes, cov_matrices)

    # output2 <- map(output, function(x) x$g_index)
    # print(map(output, function(x) x$covariate_table))
    # return(output2)
  }

  # class(output) <- "generalizeAssess"
  return(output)

}
#
# print.generalizeAssess <- function(x,...){
#   # cat("A generalize_assess object: \n")
#   # cat(paste0(" - probability of trial participation method: ", x$selection_method, "\n"))
#   # cat(paste0(" - common covariates included: ", paste(x$selection_covariates, collapse = ", "), "\n"))
#   # cat(paste0(" - sample size of trial: ", x$n_trial, "\n"))
#   # cat(paste0(" - size of population: ", x$n_pop, "\n"))
#   # cat(paste0(" - was population trimmed according to trial covariate bounds?: ", ifelse(x$trim_pop == TRUE, "Yes", "No"), "\n"))
#   # if(x$trim_pop == TRUE){
#   #   cat(paste0("    - number excluded from population data: ", x$n_excluded, "\n"))
#   # }
#   #
#   # invisible(x)
#   x[[2]]
# }
