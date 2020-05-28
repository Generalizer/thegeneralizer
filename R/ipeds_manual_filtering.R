ipeds
cosmetology_terms <- c("Beauty", "Salon", "Paul Mitchell", "Aveda", "Cosmetology", "Makeup", "Hair", "Nail", "Barber", "Esthetics", "Massage", "Personal Training", "The Training Domain")
# Original version from Katie M.: construction_terms <- c("Fab", "Welding", "Boat", "Truck", "Automotive", "Motor", "Construction", "Universal Technical Institute", "MotoRing")

construction_terms <- c("Welding", "Boat", "Truck", "Automotive", "Motor", "Construction", "Universal Technical Institute", "MotoRing")
ipeds %>%
  filter(instnm %in% cosmetology_terms) %>%
  select(instnm)
