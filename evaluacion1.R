library(ggplot2)

xx <- rnorm(100, mean = 12, sd = 4)
yy <- rnorm(100, mean = 7, sd=2)

dat <- data.frame("Zona" = c(rep("Texas", 100), rep("Yucatán", 100)), "Cadmio" = c(xx,yy))

ggplot(dat, aes(x = Cadmio, fill = Zona)) +
  geom_density(alpha = .3)+
  theme_classic()+
  scale_x_continuous(breaks = seq(0,30,2))+
  xlab("Cadmio (mg/kg)")+
  ylab("Densidad")


ggplot(dat, aes(x= Zona, y=Cadmio, fill = Zona))+
  geom_boxplot()+
  theme_bw()+
  ylab("Cadmio (mg/kg)")
