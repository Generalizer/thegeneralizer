## Beth has suggested -- and I've verified -- that we can just use private-for-profit as a proxy for schools like this
## 34 out ouf 37 construction schools are private-for-profit
## 1142 out of 1151 cosmetology schools are private-for-profit
## and according to Beth people don't generally do studies on private-for-profit schools anyway.
##

ipeds <- ipeds %>%
  filter(private_profit == 0)
ipeds

cosmetology_terms <- c("Beauty", "Salon", "Paul Mitchell", "Aveda", "Cosmetology", "Makeup", "Hair", "Nail", "Barber", "Esthetics", "Massage", "Personal Training", "The Training Domain")

construction_terms <- c("Fab", "Welding", "Boat", "Truck", "Automotive", "Motor", "Construction", "Universal Technical Institute", "MotoRing")
# Fab = fabrication

ipeds %>%
  select(instnm) %>%
  filter(any(instnm) %in% cosmetology_terms)

ipeds %>%
  filter(any(instnm %in% cosmetology_terms))

library(data.table)
?str_match
names <- ipeds$instnm
names
str_extract(names, cosmetology_terms)
str_extract(names, construction_terms)
ipeds[instnm %like% construction_terms]
data.table::like(ipeds$instnm, cosmetology_terms)
?like
grep(ipeds$instnm, any(cosmetology_terms))

matches_cosm <- unique(grep(paste(cosmetology_terms,collapse="|"),
                        ipeds$instnm, value=TRUE))
matches_const <- unique(grep(paste(construction_terms,collapse="|"),
                             ipeds$instnm, value=TRUE))

ipeds %>%
  filter((instnm %in% matches_const)) %>%
  select(webaddr)
View(ipeds %>%
       select(instnm, webaddr))

# Notes: Concordia College Alabama	closed in 2018
# New Beginning College of Cosmetology
#
