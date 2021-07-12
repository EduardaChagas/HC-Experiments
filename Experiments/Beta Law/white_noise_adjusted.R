require("EnvStats")
require("goftest")

#setwd("/home/eduarda/Desktop/Codes/HC-Experiments/HC-Experiments/Experiments/Beta Law")
source('Bandt-Pompe.R')

random_50k <- readBin('random_50k.bin', n = 1e8, size = "4", what = 'integer')
seq_50k <- abs(random_50k/max(random_50k))
split_seq <- as.matrix(split(seq_50k, ceiling(seq_along(seq_50k)/50000)))

metrics = data.frame('p' = numeric(length = N),
                     'q' = numeric(length = N),
                     'p_value' = numeric(length = N))

estBetaParams <- function(mu, varh) {
  v <- ((mu - mu^2)/varh) - 1
  alpha <- mu * v
  beta <- (1 - mu) * v
  return(params = list(alpha = alpha, beta = beta))
}

D = 6
tau = 1
N = 100

n_bins = factorial(D)
x = seq(1/(2*n_bins), 1-1/(2*n_bins), length.out = n_bins) 

for(i in 1:N){
  cat('i: ', i, ' of ', N, '\n')
  
  ts = unlist(split_seq[i, 1]) 
  probs = bandt.pompe(ts, D, tau)
  probs_sort = sort(probs, decreasing = FALSE)
  
  # Aqui está o problema. As probabilidades ordenadas não são observações;
  # elas são agregados de observações.
  # Precisamos calcular estimativas tanto da média amostral quanto da variância amostral
  
  mean_h <- sum(x * probs_sort)
  secm_h <- sum((x*x) * probs_sort)
  var_h <- secm_h - mean_h^2
  fit = estBetaParams(mean_h, var_h)
  
  metrics$p[i] = fit$alpha
  metrics$q[i] = fit$beta
  metrics$p_value[i] = chisq.test(probs_sort, dbeta(x, fit$alpha, fit$beta), simulate.p.value = FALSE)$p.value
}

write.csv(metrics, '/Results/beta_adjusting.csv')
