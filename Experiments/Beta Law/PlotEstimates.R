require(ggplot2)
require(ggthemes)

beta_estimates <- data.frame(read_csv("Results/beta_adjusting.csv", 
                                      col_types = cols(X1 = col_skip())))

ggplot(beta_estimates, aes(x=p, y=q)) +
  geom_point() +
  geom_point(aes(x=mean(p), y=mean(q)), col="red", size=3) +
  geom_segment(aes(x=mean(p)-sd(p), 
                   xend=mean(p)+sd(p),
                   y=mean(q), yend=mean(q)
                   ),
               col="red"
               ) +
  geom_segment(aes(x=mean(p), 
                   xend=mean(p),
                   y=mean(q)-sd(q), 
                   yend=mean(q)+sd(q)
                   ),
               col="red"
               ) +
  stat_ellipse(level=.90) +
  xlab(expression(hat(italic(p)))) +
  ylab(expression(hat(italic(q)))) +
  annotate("text", x = 1.085, hjust=0, y = 0.936, 
           label = "hat(rho)==0.49", parse="TRUE") +
  theme_pander()
