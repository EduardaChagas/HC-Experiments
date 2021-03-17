################################################################################
# Author: Eduarda Chagas
# Date : Nov 28, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
if(!require(fftw)){
  install.packages("fftw")
  require(fftw)
} 
source("Bandt-Pompe.R")
source("testPoint.R")

#Analysis Function -------------------------------------------------------------
series.generator.fk <- function(n.series, len.series, k){
  set.seed(seed = 1234567890, kind = "Mersenne-Twister")
  sequencies = matrix(nrow = n.series, ncol = len.series)
  for(i in 1:n.series){
    x = rnorm(len.series)
    x = x - mean(x)
    pp = planFFT(len.series)
    y = FFT(x, plan = pp)
    filter = (1:len.series)^-(k/2)
    filter = filter/sum(filter)
    y1 = y * filter    
    x1 = IFFT(y1, plan = pp)  
    sequencies[i,] = c(Re(x1)) 
  }
  return(sequencies)
}

#Global variables --------------------------------------------------------------
#len.series = n.series = 10^4

#series.k05 = series.generator.fk(n.series, len.series, 0.5)
#write.csv(series.k05, file = "../../series-k05.csv")

#series.k1.5 = series.generator.fk(n.series, len.series, 1.5)
#write.csv(series.k1.5, file = "../../series-k15.csv")

#series.k3 = series.generator.fk(n.series, len.series, 3)
#write.csv(series.k3, file = "../../series-k3.csv")