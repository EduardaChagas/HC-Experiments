### Packages
require(ggplot2)
require(ggthemes)
require(cowplot)

### Functions

Hhpi <- function(epsilon, D) {
  u <- 1/factorial(D)
  upepsilon <- u+epsilon
  umepsilon <- u-epsilon
  
 return(
   (upepsilon * log(upepsilon) + umepsilon * log(umepsilon)) / log(u) + (1-2*u)
        )
}

Dhu <- function(epsilon, D) {
  
  u <- 1/factorial(D)
  
  return(
    -(
      (2*u+epsilon) * log(u+epsilon/2) +
        (2*u-epsilon) * log(u-epsilon/2) -
        (u+epsilon) * log(u+epsilon) -
        (u-epsilon) * log(u-epsilon) +
        (1-2*u)* log(u) - log(u)
    )/2
  )
}


### Plots
eps3 = seq(0, 1/factorial(3), length=1000)
eps4 = seq(0, 1/factorial(4), length=1000)
eps5 = seq(0, 1/factorial(5), length=1000)
eps6 = seq(0, 1/factorial(6), length=1000)
Hhp3 = Hhpi(eps3, 3)
Hhp4 = Hhpi(eps4, 4)
Hhp5 = Hhpi(eps5, 5)
Hhp6 = Hhpi(eps6, 6)
C3 = Hhp3 * Dhu(eps3, 3) / log(3)
C4 = Hhp4 * Dhu(eps4, 4) / log(4)
C5 = Hhp5 * Dhu(eps5, 5) / log(5)
C6 = Hhp6 * Dhu(eps6, 6) / log(6)

Hppi3456 <- data.frame(eps3, eps4, eps5, eps6, Hhp3, Hhp4, Hhp5, Hhp6, C3, C4, C5, C6)
rm(eps3, eps4, eps5, eps6, Hhp3, Hhp4, Hhp5, Hhp6, C3, C4, C5, C6)

ggplot(data=Hppi3456) +
  geom_line(aes(x=Hhp3, y=C3), col="darkgoldenrod4") +
  geom_line(aes(x=Hhp4, y=C4), col="deeppink1") +
  geom_line(aes(x=Hhp5, y=C5), col="chartreuse4") +
  geom_line(aes(x=Hhp6, y=C6), col="mediumblue") +
  labs(x=expression(italic(H)), 
       y=expression(italic(C))) +
  annotate('text', x = Hppi3456$Hhp3[999], y = Hppi3456$C3[999], 
           label = "epsilon==1/6", parse = TRUE, size=2, hjust=0.5, vjust=0) +
  annotate('text', x = Hppi3456$Hhp4[999], y = Hppi3456$C4[999], 
           label = "epsilon==1/24", parse = TRUE, size=2, hjust=0.5, vjust=0) +
  annotate('text', x = Hppi3456$Hhp5[999], y = Hppi3456$C5[999], 
           label = "epsilon==1/120", parse = TRUE, size=2, hjust=0, vjust=0) +
  theme_pander() -> p1

ggplot(data=Hppi3456) +
  geom_line(aes(x=Hhp3, y=C3), col="darkgoldenrod4") +
  geom_line(aes(x=Hhp4, y=C4), col="deeppink1") +
  geom_line(aes(x=Hhp5, y=C5), col="chartreuse4") +
  geom_line(aes(x=Hhp6, y=C6), col="mediumblue") +
  xlim(.9975, 1) +
  ylim(0, 1-.9975) +
  labs(x=expression(italic(H)), 
       y=expression(italic(C))) +
  annotate('text', x = Hppi3456$Hhp5[999], y = Hppi3456$C5[999], 
           label = "epsilon==1/120", parse = TRUE, size=2, hjust=0, vjust=0) +
  annotate('text', x = Hppi3456$Hhp6[999], y = Hppi3456$C6[999], 
           label = "epsilon==1/720", parse = TRUE, size=2, hjust=0, vjust=0) +
  theme_pander() -> p2

plot_grid(p1, p2) -> grid
save_plot("Deviations.pdf", grid)
