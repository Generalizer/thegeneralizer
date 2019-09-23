library(tidyverse)

ipeds %>%
  select(Total_Undergrad) %>%
  summary()

ipeds_new <- ipeds %>%
  mutate(Total_Undergrad_pct = (1/2)*percentile_rank(Total_Undergrad),
         Total_Undergrad_top = case_when(Total_Undergrad < 500 ~ 500,
                                         Total_Undergrad > 30000 ~ 30000,
                                         TRUE ~Total_Undergrad))


ipeds_new %>% pull(Admit_rate) %>% summary()
