##Generalizability Index ##
##Tipton (2014) ###

Bindex <- function(dat1B,dat2B) {
  ##Baklizi and Eidous (2006) estimator
  # bandwidth
  h = function(x){
    n = length(x)
    return((4*sqrt(var(x))^5/(3*n))^(1/5))
  }

  # kernel estimators of the density and the distribution
  kg = function(x,data){
    hb = h(data) #bin width
    k = r = length(x)
    for(i in 1:k) r[i] = mean(dnorm((x[i]-data)/hb))/hb
    return(r )
  }

  return( as.numeric(integrate(function(x) sqrt(kg(x,dat1B)*kg(x,dat2B)),-Inf,Inf)$value))

}

gen_index <- function(popdata, sample, var1, var2){

  sample <- data.frame(sample, in_sample = rep(1, dim(sample)[1]))

  fulldata <- popdata %>%
    full_join(sample) %>%
    mutate(in_sample = replace(in_sample, is.na(in_sample), 0))

  pr_model <- glm(fulldata$in_sample ~ as.matrix(fulldata[var1]) + as.matrix(fulldata[var2]), family = binomial())

  fulldata <- fulldata %>%
    mutate(prop_score = predict(pr_model, type = "response"))

  dat1b <- fulldata %>%
    filter(in_sample == 1) %>%
    select(prop_score)
  dat1b <- as.numeric(unlist(dat1b))

  dat2b <- fulldata %>%
    filter(in_sample == 0) %>%
    select(prop_score)
  dat2b <- as.numeric(unlist(dat2b))

  gen_ind <- Bindex(dat1b, dat2b)
  return(gen_ind)

}
