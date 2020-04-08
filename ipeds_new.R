library(tidyverse)


scores_n <- scores %>%
  transmute(UNITID = UNITID,
    SAT = 0.5*(SATVR25 + SATMT25 + SATVR75 + SATMT75),
         ACT = 0.5*(ACTCM25 + ACTCM75))

ipeds_new <- ipeds %>%
  mutate(Total_Undergrad_lg = log10(Total_Undergrad)) %>%
  left_join(scores_n)

age <- read_csv(file = "/Users/beatricechao/Dropbox/Generalizer Project/data/ef2017b.csv")

RRPTCT

age <- age %>%
  filter(EFBAGE == "7") %>%
  filter(LSTUDY == "2") %>%
  select(UNITID, EFAGE05, EFAGE09)

age <- age %>%
  rename(Over25_fullt = EFAGE05,
         Over25_total = EFAGE09)

char <- read_csv(file = "/Users/beatricechao/Dropbox/Generalizer Project/data/hd2017.csv") %>%
  select(UNITID, GROFFER, HBCU, TRIBAL)

carnegie <- read_csv("/Users/beatricechao/Dropbox/Generalizer Project/data/hd2017.csv") %>%
  select(UNITID, CCBASIC)

ipeds_new <- ipeds_new %>%
  left_join(char) %>%
  left_join(age) %>%
  left_join(carnegie)

ipeds_new <- ipeds_new %>%
  mutate(PCT_Over25_fullt = Over25_fullt/Total_Undergrad,
         PCT_Over25_total = Over25_total/Total_Undergrad)


retention <- read_csv(file = "/Users/beatricechao/Dropbox/Generalizer Project/data/ef2017d.csv") %>%
  select(UNITID, RRPTCTA)


ipeds_ccn <- ipeds_ccn %>%
  left_join(carnegie) %>%
  left_join(retention) %>%
  mutate(GROFFER = recode(GROFFER, 1, 0),
         HBCU = recode(HBCU, 1, 0),
         TRIBAL = recode(TRIBAL, 1, 0),
         PCT_Parttime = RRPTCTA/Total_Undergrad)


