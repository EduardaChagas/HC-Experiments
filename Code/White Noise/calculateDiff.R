# Packages and sources ---------------------------------------------------------------------------------
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
} 
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}  
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
} 

# ------------------------------------------------------

n = 1000
D = c(3, 4) # embedding dimensions to be considered
P = c(0.1, 0.3, 0.5, 0.7) # probabilities of attack to be considered

diff.timeordered = diff.datadriven = data.frame(HD3P0.1 = numeric(1000),
                                               HD3P0.3 = numeric(1000),
                                               HD3P0.5 = numeric(1000),
                                               HD3P0.7 = numeric(1000),
                                               HD4P0.1 = numeric(1000),
                                               HD4P0.3 = numeric(1000),
                                               HD4P0.5 = numeric(1000),
                                               HD4P0.7 = numeric(1000),
                                               CD3P0.1 = numeric(1000),
                                               CD3P0.3 = numeric(1000),
                                               CD3P0.5 = numeric(1000),
                                               CD3P0.7 = numeric(1000),
                                               CD4P0.1 = numeric(1000),
                                               CD4P0.3 = numeric(1000),
                                               CD4P0.5 = numeric(1000),
                                               CD4P0.7 = numeric(1000))

for(d in D) {
  hc = read.csv(paste0("../Data/originalD", d, 'N', n, ".csv"))
  for(p in P) {
    cat("N: ", n, " D: ", d, " P: ", p, "\n")
    hc_x_timeordered = read.csv(paste0("../Data/timeorderedD", d, "N1000P", p, ".csv"))
    hc_x_datadriven = read.csv(paste0("../Data/datadrivenD", d, "N1000P", p, ".csv"))
    
    diff.timeordered[,paste0('HD', d, 'P', p)] = abs(hc$V1 - hc_x_timeordered$V1)
    diff.datadriven[,paste0('HD', d, 'P', p)] = abs(hc$V1 - hc_x_datadriven$V1)
    diff.timeordered[,paste0('CD', d, 'P', p)] = abs(hc$V2 - hc_x_timeordered$V2)
    diff.datadriven[,paste0('CD', d, 'P', p)] = abs(hc$V2 - hc_x_datadriven$V2)
  }
}

diff.P01 = data.frame(H = c(diff.timeordered$HD3P0.1, 
                            diff.datadriven$HD3P0.1,
                            diff.timeordered$HD4P0.1, 
                            diff.datadriven$HD4P0.1),
                      C = c(diff.timeordered$CD3P0.1, 
                            diff.datadriven$CD3P0.1,
                            diff.timeordered$CD4P0.1, 
                            diff.datadriven$CD4P0.1),
                      D = as.factor(c(rep(3, 2000), rep(4, 2000))),
                      Algorithm = as.factor(rep(c(rep("Time Ordered", 1000), rep("Data-Driven", 1000)), 2)))

diff.P03 = data.frame(H = c(diff.timeordered$HD3P0.3, 
                            diff.datadriven$HD3P0.3,
                            diff.timeordered$HD4P0.3, 
                            diff.datadriven$HD4P0.3),
                      C = c(diff.timeordered$CD3P0.3, 
                            diff.datadriven$CD3P0.3,
                            diff.timeordered$CD4P0.3, 
                            diff.datadriven$CD4P0.3),
                      D = as.factor(c(rep(3, 2000), rep(4, 2000))),
                      Algorithm = as.factor(rep(c(rep("Time Ordered", 1000), rep("Data-Driven", 1000)), 2)))

diff.P05 = data.frame(H = c(diff.timeordered$HD3P0.5, 
                            diff.datadriven$HD3P0.5,
                            diff.timeordered$HD4P0.5, 
                            diff.datadriven$HD4P0.5),
                      C = c(diff.timeordered$CD3P0.5, 
                            diff.datadriven$CD3P0.5,
                            diff.timeordered$CD4P0.5, 
                            diff.datadriven$CD4P0.5),
                      D = as.factor(c(rep(3, 2000), rep(4, 2000))),
                      Algorithm = as.factor(rep(c(rep("Time Ordered", 1000), rep("Data-Driven", 1000)), 2)))

diff.P07 = data.frame(H = c(diff.timeordered$HD3P0.7, 
                            diff.datadriven$HD3P0.7,
                            diff.timeordered$HD4P0.7, 
                            diff.datadriven$HD4P0.7),
                      C = c(diff.timeordered$CD3P0.7, 
                            diff.datadriven$CD3P0.7,
                            diff.timeordered$CD4P0.7, 
                            diff.datadriven$CD4P0.7),
                      D = as.factor(c(rep(3, 2000), rep(4, 2000))),
                      Algorithm = as.factor(rep(c(rep("Time Ordered", 1000), rep("Data-Driven", 1000)), 2)))

# ------------------------------------------------------

plot.diff <- function(diff){
  
  shape.select = c(17,18,19,8)
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  Color = rainbow.colors[as.factor(diff$Algorithm)]
  Shape = shape.select[as.factor(diff$Algorithm)]
  
  signal.values = data.frame(H = diff$H, C = diff$C, Color = Color, Shape = Shape)
  
  p = ggplot() +
    geom_point(data = signal.values, aes(x = H, y = C, color = Color), shape = Shape, size = 2) +
    theme_few(base_size = 14, base_family = "serif")  + theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  return(p)
}

diff = diff.P05[2001:4000,]
#diff = diff.P05[1:2000,]
p = plot.diff(diff)
print(p)