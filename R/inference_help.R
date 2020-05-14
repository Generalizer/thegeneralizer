#' Inference Help
#'
#' This function introduced the dataset and gives filtering recommendations for categorical and continuous variables in the \code{thegeneralizer} package.
#' @param N/A no parameters
#' @return Console text teaching people how to filter
#' @examples Run inference_help() to see what it does
#' inference_help()

inference_help <- function(){

  pop <- menu(choices = (c("K-12", "Post-secondary")),
              title = cat("First, we need to define your inference population. This is the population which you believe your intervention will be applicable to. Are you looking at K-12 education or post-secondary education? "))
  if(pop == 1){
    cat("We have a built-in dataset 'cc', which is taken from Common Core Data 2016-2017. This is population data on all public elementary and secondary schools in the US. ")
  }
  if(pop == 2){
    cat("We have a built-in dataset 'ipeds', which is taken from IPEDS 2017. This is population data on more than 7,500 post-secondary institutions in the US.")
  }
  cat("\n\nYour study will recruit select schools to form a sample, with the aim that the selected schools are representative of the population. The choice of an appropriate inference population may be based on a study's resources, focus, or implementation aims. For example, the target group for your intervention may be limited to schools in California, a certain grade, or students of a certain income background.")

  invisible(readline(prompt="Press [enter] to continue"))

  cat("These are the categorical variables available for filtering: \n")

  cat("\nCollege Characteristics\n")
  print(c("Control_Level (Public/Private/For Profit)", "Degree_Level (4 yr/ 2 yr/ < 2 yr)"))
  cat("\nLocation\n")
  print(c("State", "Urbanicity", "Parentsystem"))
  cat("\nStudent Achievement\n")
  print(c("HS_Remedial_Services (High School/Adult School)", "Remedial_Services"))
  cat("\nFinancial Aid\n")
  print(c("TitleIV_Status"))

  cat("\nSelect the values that you are interested in within these variables. For example, you may only be interested in schools in California: \n\n",

      "my_inference_pop <- ipeds %>% filter(State == 'CA')\n\n")

  invisible(readline(prompt="Press [enter] to continue to continuous variables"))

  cat("These are the continuous variables available for filtering: \n")

  cat("\nCollege Characteristics\n")
  print(c("Total_Undergrad", "Admit_rate", "Retention_Fullt", "Retention_Partt"))
  cat("\nStudent Demographics\n")
  print(c("PCT_Female", "PCT_Male", "PCT_NatAA", "PCT_Asian", "PCT_Black", "PCT_Hispanic",
          "PCT_NatHPI", "PCT_White", "PCT_2more", "PCT_unknown", "PCT_Foreign", "PCT_LowInc",
          "PCT_LowMidInc", "PCT_MidInc", "PCT_MidHighInc", "PCT_HighInc" ))
  cat("\nStudent Achievement\n")
  print(c("SAT_RW_25", "SAT_RW_75","SAT_Math_25", "SAT_Math_75",
          "ACT_25", "ACT_75", "ACT_En_25", "ACT_En_75", "ACT_Math_25", "ACT_Math_75"))
  cat("\nFinancial Aid\n")
  print(c("PCT_GrantAid", "PCT_Pell", "PCT_FedLoans", "PCT_AnyAid"))

  cat("\nLimit the range of the variables to define the inference population you're interested in. For example, you may only be interested in schools that are within the top 75th percentile of % Low Income stundents served: \n\n",

      "my_inference_pop <- ipeds %>% set_percentile_limits(PCT_LowInc, min_pct = 75, max_pct = 100)\n\n")

  cat("When you're done, run solution <- stratifier() on your filtered dataset (my_inference_pop) ")

}

