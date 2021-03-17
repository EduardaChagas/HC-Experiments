########################################################################################################
# Author: Eduarda Chagas
# Date : Nov 15, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("testPoint.R")
source("Bandt-Pompe.R")
source("Attack-functions.R")
if(!require(foreach)){
  install.packages("foreach")
  require(foreach)
} 

# Reading white noise data -----------------------------------------------------
white.noise.samples <- function(N){
  filenames = list.files(path = "../Data/random.org/")
  names = substr(filenames, 1, 10)
  j = 0
  dados = vector()
  for(i in names){
    j = j + 1
    filepath = file.path(paste("../Data/random.org/", i,".bin",sep=""))
    assign("Data", readBin(filepath, n = 1e8, size = "4", what ='integer'))
    dados = c(dados, Data)
  }
  dados = abs(dados/max(dados))
  n.series = round((length(dados)/N), digits = 0)
  cat(n.series, ' time series were formed\n')
  split_seq = matrix(nrow = n.series, ncol = N)
  for(i in 1:n.series){
    split_seq[i,] = dados[sample(1:length(dados), N, replace = F)]
  }
  write.csv(split_seq, file = paste0("../Data/random_sampleN", N, ".csv"))
  return(split_seq)
}

tau = 1
region = 90
N = c(1000, 50000) # lengths of the time series to be considered
D = c(3, 4, 5, 6) # embedding dimensions to be considered
P = c(0.1, 0.3, 0.5, 0.7) # probabilities of attack to be considered

# Store all the points in the HxC plane of the following loop
for(n in N){
  x = white.noise.samples(n)
  xx = x[1:1000, ]
  for(d in D) {
    hcx = hc.samples(xx, d, tau)
    write.csv(hcx, paste0("../Data/originalD", d, 'N', n, ".csv"))
    for(p in P) {
      cat("N: ", n, " D: ", d, " P: ", p, "\n")
      x_attack = AttackTimeSeries(xx, d, tau, p)
      
      hc_x_timeordered = hc.time.ordered.samples(x_attack, d)
      hc_x_datadriven = hc.data.driven.samples(x_attack, d)
      write.csv(hc_x_timeordered, paste0("../Data/timeorderedD", d, 'N', n, "P", p, ".csv"))
      write.csv(hc_x_datadriven, file = paste0("../Data/datadrivenD", d, 'N', n, "P", p, ".csv"))
      
      test_timeordered = test.set.point(hc_x_timeordered, d, n, region)
      cat("Time Ordered: Points inside the regions of", region, ": ", 
          (length(test_timeordered[test_timeordered == 1])/length(test_timeordered))*100, '%\n')
      
      test_datadriven = test.set.point(hc_x_datadriven, d, n, region)
      cat("Data-driven: Points inside the regions of", region, ": ", 
          (length(test_datadriven[test_datadriven == 1])/length(test_datadriven))*100, '%\n\n')
    }
  }
}
