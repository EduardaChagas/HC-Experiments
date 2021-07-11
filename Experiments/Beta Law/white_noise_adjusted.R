require("EnvStats")
require("goftest")

#setwd("/home/eduarda/Desktop/Codes/HC-Experiments/HC-Experiments/Experiments/Beta Law")
source('../../Code/Bandt-Pompe.R')

random_50k <- readBin('random_50k.bin', n = 1e8, size = "4", what = 'integer')
seq_50k <- abs(random_50k/max(random_50k))
split_seq <- as.matrix(split(seq_50k, ceiling(seq_along(seq_50k)/50000)))

D = 6
tau = 1
N = 100

metrics = data.frame('p' = numeric(length = N),
                     'q' = numeric(length = N),
                     'p_value' = numeric(length = N))

estBetaParams <- function(mu, varh) {
  v <- ((mu - mu^2)/varh) - 1
  alpha <- mu * v
  beta <- (1 - mu) * v
  return(params = list(alpha = alpha, beta = beta))
}

for(i in 1:N){
  cat('i: ', i, ' of ', N, '\n')
  
  ts = unlist(split_seq[i, 1]) 
  probs = bandt.pompe(ts, D, tau)
  probs_sort = sort(probs, decreasing = FALSE)
  
  # Aqui está o problema. As probabilidades ordenadas não são observações;
  # elas são agregados de observações.
  # Precisamos calcular estimativas tanto da média amostral quanto da variância amostral
  
  (meanh <- mean(probs_sort)) # estimativa da média amostral
  (varh <- var(probs_sort)) # estimativa da variância amostral
  
  fit = estBetaParams(meanh, varh)
  metrics$p[i] = fit$alpha
  metrics$q[i] = fit$beta
  
  metrics$p_value[i] = ad.test(probs_sort, "pbeta", shape1 = metrics$p[i], shape2 = metrics$q[i], estimated = TRUE)$p.value
}

write.csv(metrics, 'beta_adjusting.csv')
