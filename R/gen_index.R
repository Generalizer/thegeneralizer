##### CITE BETH TIPTON'S PAPER IN THE DOCUMENTATION #####

#' Calculate `generalizability index` to describe how similar or different trial is from population
#'
#' @param dat1B vector of probabilities of trial participation among individuals in the trial
#' @param dat2B vector of probabilities of trial participation among individuals in the population
#' @return the generalizability index, a value between 0 and 1, where scores greater than 1 indicate greater similarity (see Tipton paper for description)

gen_index <- function(dat1B,dat2B) {
  ##Baklizi and Eidous (2006) estimator
  # bandwidth
  h = function(x){
    n = length(x)
    optim_binwidth = (4*sqrt(var(x))^5/(3*n))^(1/5)
    if(is.na(optim_binwidth) | is.nan(optim_binwidth)){
      optim_binwidth = 0
    }
    if(optim_binwidth < 0.001){ # this yielded a b index of 0.9999501 for (at least one specific case of) "perfectly" stratified data
      optim_binwidth = 0.001
    }
    return(optim_binwidth)
  }

  # kernel estimators of the density and the distribution
  kg = function(x,data){
    hb = h(data) #bin width
    k = r = length(x)
    for(i in 1:k) r[i] = mean(dnorm((x[i]-data)/hb))/hb # we divide by bin width, which is a problem when bin width goes to zero
    return(r)
  }

  return( as.numeric(integrate(function(x) sqrt(kg(x,dat1B)*kg(x,dat2B)),-Inf,Inf)$value))
}
