require(ggplot2)
require(ggthemes)

D = 4
n_bins = factorial(D)
mean_h = seq(1/(2*n_bins), 1-1/(2*n_bins), length.out = n_bins) 

# Histogram
unifs <- runif(n = 10000, min = 0, max = 1)
ggplot(data = NULL, aes(x = unifs)) + 
  geom_histogram(binwidth = 1/n_bins, boundary = 1)  +
  geom_vline(xintercept = mean_h, color = 'red') +
  theme_classic()



