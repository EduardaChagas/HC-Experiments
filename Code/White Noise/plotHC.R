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
if(!require(latex2exp)){
  install.packages("latex2exp")
  require(latex2exp)
} 
source('Bandt-Pompe.R')

HC.Plane.zoom <- function(d, types, hc){
  shape.select <- c(17,18,19,8)
  XMIN = min(hc[,2]) + 0.000005
  XMAX = min(max(hc[,2]) + 0.000005, 1)
  YMIN = max(0,min(hc[,3]) - 0.000005)
  YMAX = max(hc[,3]) + 0.000005
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  
  Color = rainbow.colors[types]
  Shape = shape.select[types]
  Regions =  c("Time Ordered", "Data-driven")[types]
  signal.values = data.frame("H" = hc[,2], "C" = hc[,3], "Color" = Color, "Shape" = Shape, "Regions" = Regions)
  
  p = cotas(d)
  p = p + 
    geom_point(data = signal.values, aes(x = H, y = C, color = Regions), shape = Shape, size = 2) +
    xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) +  
    theme_few(base_size = 14, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  return(p)
}

n = 1000
D = c(3, 4) # embedding dimensions to be considered
P = c(0.1, 0.3, 0.5, 0.7) # probabilities of attack to be considered

for(d in D) {
  index = 1
  plots = array(list(), 4)
  for(p in P) {
    cat("N: ", n, " D: ", d, " P: ", p, "\n")
    hc_x_timeordered = read.csv(paste0("../Data/timeorderedD", d, "N1000P", p, ".csv"))
    hc_x_datadriven = read.csv(paste0("../Data/datadrivenD", d, "N1000P", p, ".csv"))
    hc = rbind(hc_x_timeordered, hc_x_datadriven)
    types = c(rep(1, dim(hc_x_timeordered)[1]), rep(2, dim(hc_x_timeordered)[1]))
    plots[[index]] = HC.Plane.zoom(d, types, hc)
    print(plots[[index]])
    index = index + 1
  }
  pdf(paste0("../Data/HCD", d, "N1000.pdf"), width = 14, height = 8)  
  plot.general = ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                            ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
                ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
                xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
                labs(colour=expression(italic(Regions))) +
                theme_clean() + theme(text = element_text(size = 14, family = "Times"), 
                                      axis.text.x = element_blank(), 
                                      axis.text.y = element_blank(),
                                      plot.title = element_text(hjust = 0.5)) + 
                guides(colour = guide_legend(override.aes = list(size=3)))
  print(plot.general)
  dev.off()
}