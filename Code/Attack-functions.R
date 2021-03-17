

# Define attack ----------------------------------------------------------------
set.seed(123)
AttackElement <- function(e, d, p){
  if(runif(1) <= p){
    i = round(runif(1, max = d, min = 1), digits = 0)
    j = i
    while(j == i){
      j = round(runif(1, max = d, min = 1), digits = 0)
    }
    e[j] = e[i] 
  }
  return(e)
}

AttackTimeSeries <- function(time_series, d, tau, p) {
  n.series = dim(time_series)[1]
  attacked_elements = array(list(), n.series)
  for(i in 1:n.series){
    #cat("Attacked elements: ", i, "\n")
    elements = formationPattern(time_series[i,], d, tau, 1)
    attacked_elements[[i]] = AttackElement(elements, d, p)
  }
  return(attacked_elements)
}