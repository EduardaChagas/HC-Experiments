################################################################################
# Author: Eduarda Chagas
# Date : Nov 28, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
source("Attack-functions.R")
source("fkAnalysis.R")

#Global variables --------------------------------------------------------------
k = 3
D = 3
tau = 1
n.series = len.series = 10^4
P = c(0.1, 0.5, 0.7) # probabilities of attack to be considered

# Store all the points in the HxC plane of the following loop
#sequencies = read.csv("../../series-k3.csv")
sequencies = series.generator.fk(n.series, len.series, 3)
hcx = hc.samples(sequencies, D, tau)
write.csv(hcx, paste0("../../originalk", k, ".csv"))
for(p in P) {
      cat(" P: ", p, "\n")
      x_attack = AttackTimeSeries(sequencies, D, tau, p)
      
      hc_x_timeordered = hc.time.ordered.samples(x_attack, D)
      hc_x_datadriven = hc.data.driven.samples(x_attack, D)
      write.csv(hc_x_timeordered, paste0("../../timeorderedk", k, "P", p, ".csv"))
      write.csv(hc_x_datadriven, file = paste0("../../datadrivenk", k, "P", p, ".csv"))
}


