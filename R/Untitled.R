
# Reading in the dataset --------------------------------------------------

# This is a dataset from ipeds that contains 6440 schools.
# There are a total of 7153 schools in IPEDS, and these 6440
# are all those that report to be primarily post-secondary institutions
# and who report gender demographic information (as this seemed basic)

# A large collection of variables have been selected across available datasets

load("ipeds_ccn.rda")


# Filtering the dataset ---------------------------------------------------

# We filter out all for-profit schools, online universities, special-focus institusions,
# and only schools with acceptance rate > 75%, or who don't report an acceptance rate
# (the latter tend to be community colleges)

ccn <- ipeds_ccn %>%
  # for-profit schools
  filter(Control_Level != "3") %>%
  # online universities
  filter(INSTNM != "Southern New Hampshire University") %>%
  filter(INSTNM != "Western Governors University") %>%
  filter(INSTNM != "Ashford University") %>%
  # special-focus institutions
  # filter(CCBASIC < 24) %>%
  # admit rate
  filter(Admit_rate >= 0.75 | is.na(Admit_rate)) %>%

  # I also decided you need at least 1000 students for me not to be suspicious
  # that you are an online, mainly graduate or special focus school
  filter(Total_Undergrad > 1000)

# This gives us a total of 1564 schools in the end


# Stratifier --------------------------------------------------------------

# We then load in thegeneralizer package

##### INSERT INSTRUCTIONS HERE

# and run stratifier
# with these instructions :

# Custom dataset: 3
# ID Variable name : UNITID
# stratifying variables: 10 15 18 30 34 43 46 47 48 49 50 51 52 53 59,
# 9 24 24 37 40 41 42 43 44 45 46 47 53

stratifier(ccn)
