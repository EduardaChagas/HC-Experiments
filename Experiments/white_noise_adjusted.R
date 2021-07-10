require("EnvStats")
require("goftest")

setwd("/home/eduarda/Desktop/Codes/HC-Experiments-master/Experiments")
source('../Code/Bandt-Pompe.R')

random_50k <- readBin('data/random_50k.bin', n = 1e8, size = "4", what = 'integer')
seq_50k <- abs(random_50k/max(random_50k))
split_seq <- as.matrix(split(seq_50k, ceiling(seq_along(seq_50k)/50000)))

D = 3
tau = 1
N = 100

metrics = data.frame('shape1' = numeric(length = N),
                     'shape2' = numeric(length = N),
                     'p_value' = numeric(length = N))

for(i in 1:N){
  cat('i: ', i, ' of ', N, '\n')
  
  ts = unlist(split_seq[i, 1]) 
  probs = bandt.pompe(ts, D, tau)
  probs_sort = sort(probs, decreasing = FALSE)
  fit = ebeta(probs_sort, method = "mle")
  metrics$shape1[i] = fit$parameters[1]
  metrics$shape2[i] = fit$parameters[2]
  metrics$p_value[i] = ad.test(probs_sort, "pbeta", shape1 = fit$parameters[1], shape2 = fit$parameters[2], estimated = TRUE)$p.value
}

write.csv(metrics, 'beta_adjusting.csv')
