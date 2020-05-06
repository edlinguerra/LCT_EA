#Paquetes para diversidad (alfa, gama) de especies
library(vegan)
library(iNEXT)

#Paquetes para beta diversidad
library(vegan)
library(betapart)

#paquetes para optimizar tratamiento de datos
library(ggplot2) #gráficos
library(dplyr) #reordenar datos
library(gt)

library(patchwork) #gráfico múltiple
library(readxl) #importar archivos excel

####################################################################

#import data file (SB_yucatan_oct2018.csv) as data
full_data <- read_excel("SB_yucatan_mexico_march2019_final.xlsx", sheet = "Hoja1")


dat<-full_data[,22:52] #subset to a matrix of species only

ave.sd <- function(x){
  ave <- round(mean(x, na.rm = TRUE),1)
  des <- round(sd(x, na.rm = TRUE),2)
  xx <- paste(ave, des, sep = " ± ")
}

Appendixe <- full_data %>% 
  select(City, Beach, Zone, 22:52) %>% 
  group_by(City, Beach, Zone) %>%
  summarise_if(is.numeric, ave.sd)

Appendixe%>% 
  gt()


full_data$City <-  factor(full_data$City, levels = c("Celestun", "Sisal", "Progreso", "Telchac", "Dzilam", "El Cuyo"))
full_data$richness <- specnumber(dat)
full_data$N <- apply(dat, 1, sum)


fig4a <- full_data %>% 
  select(City, Beach, richness) %>% 
  group_by(City, Beach) %>% 
  summarise("aves" = mean(richness)) %>% 
  group_by(City) %>% 
  summarise("ave" = mean(aves), "sd" = sd(aves)) %>% 
  ggplot(aes(x = City, y = ave))+
  ylim(0,5)+
  geom_errorbar(aes(x = City, ymin = ave-sd, ymax = ave+sd), width = .2)+
  geom_point()+
  theme_classic()+
  ylab("Average number of species per core (± Stand. Error)")+
  xlab("Localities")+
  ggtitle("A")
fig4a


fig4b <- full_data %>% 
  select(City, Beach, N) %>% 
  group_by(City, Beach) %>% 
  summarise("aves" = mean(N)) %>% 
  group_by(City) %>% 
  summarise("ave" = mean(aves), "sd" = sd(aves)) %>% 
  ggplot(aes(x = City, y = ave))+
  geom_errorbar(aes(x = City, ymin = ave-sd, ymax = ave+sd), width = .2)+
  geom_point()+
  scale_y_continuous(breaks = seq(0,300, 50))+
  theme_classic()+
  ylab("Average number of organisms per core (± Stand. Error)")+
  xlab("Localities")+
  ggtitle("B")
fig4b

fig4a | fig4b

###iNEXT

sites <- levels(factor(full_data$City, levels = c("Celestun", "Sisal", "Progreso", "Telchac", "Dzilam", "El Cuyo")))
dat.list <- vector(mode = "list", length = 6)

for (i in 1:6){
  site <- sites[i]
  x <- full_data[full_data$City==site,22:53]
  x <- 1 * (x > 0)
  dat.list[[i]] <- t(x)
}

names(dat.list) <- c("1Celestun", "2Sisal", "3Progreso", "4Telchac", "5Dzilam", "6El Cuyo")

alfa <- iNEXT(dat.list, datatype="incidence_raw", endpoint=100)
gamma <- iNEXT(t(dat), datatype="incidence_raw", endpoint=3500)
gamma.sc90 <- estimateD(t(dat), datatype="incidence_raw", base="coverage", level= 0.95, conf=0.95)

######################################
#fig5a SAC richness
df.a <- fortify(alfa, type = 1)
df.a$site <- factor(df.a$site, labels = c("Celestun", "Sisal", "Progreso", "Telchac", "Dzilam", "El Cuyo"))
df.a.point <- df.a[which(df.a$method=="observed"),]
df.a.line <- df.a[which(df.a$method!="observed"),]
df.a.line$method <- factor(df.a.line$method, 
                           c("interpolated", "extrapolated"),
                           c("Interpolation", "Extrapolation"))

fig5a<- ggplot(df.a, aes(x=x, y=y, colour=site)) + 
  geom_point(aes(colour = site), size=6, data=df.a.point) +
  geom_line(aes(linetype=method), lwd=1.5, data=df.a.line) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 20, 2))+
  scale_shape_manual(values = c(20,20,20,20,20,20)) +
  scale_colour_manual(values = c("cyan","blue","red3","darkorange","yellow","darkgreen"))+
  labs(x="Number of sampling units", y="Species richness") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.title=element_blank(),
        text=element_text(size=14),
        legend.box = "vertical")+
  ggtitle("A")
fig5a

#fig5b SAC sample coverage
df.b <- fortify(alfa, type = 2)
df.b$site <- factor(df.b$site, labels = c("Celestun", "Sisal", "Progreso", "Telchac", "Dzilam", "El Cuyo"))
df.b.point <- df.b[which(df.b$method=="observed"),]
df.b.line <- df.b[which(df.b$method!="observed"),]
df.b.line$method <- factor(df.b.line$method, 
                           c("interpolated", "extrapolated"),
                           c("Interpolation", "Extrapolation"))

fig5b<- ggplot(df.b, aes(x=x, y=y, colour=site)) + 
  geom_point(aes(colour = site), size=6, data=df.b.point) +
  geom_line(aes(linetype=method), lwd=1.5, data=df.b.line) +
  scale_y_continuous(breaks = seq(0.4, 1.0, 0.1))+
  theme_bw() +
  scale_shape_manual(values = c(20,20,20,20,20,20)) +
  scale_colour_manual(values = c("cyan","blue","red3","darkorange","yellow","darkgreen"))+
  labs(x="Number of sampling units", y="Sample coverage") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.title=element_blank(),
        text=element_text(size=14),
        legend.box = "vertical")+
  ggtitle("B")
fig5b


#fig5c SAC sample coverage
df.c <- fortify(alfa, type = 3)
df.c$site <- factor(df.c$site, labels = c("Celestun", "Sisal", "Progreso", "Telchac", "Dzilam", "El Cuyo"))
df.c.point <- df.c[which(df.c$method=="observed"),]
df.c.line <- df.c[which(df.c$method!="observed"),]
df.c.line$method <- factor(df.c.line$method, 
                           c("interpolated", "extrapolated"),
                           c("Interpolation", "Extrapolation"))

fig5c<- ggplot(df.c, aes(x=x, y=y, colour=site)) + 
  geom_point(aes(colour = site), size=6, data=df.c.point) +
  geom_line(aes(linetype=method), lwd=1.5, data=df.c.line) +
  scale_x_continuous(breaks = seq(0.4, 1.0, 0.1))+
  scale_y_continuous(breaks = seq(0, 20, 2))+
  theme_bw() +
  scale_shape_manual(values = c(20,20,20,20,20,20)) +
  scale_colour_manual(values = c("cyan","blue","red3","darkorange","yellow","darkgreen"))+
  labs(x="Sample coverage", y="Species richness") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.title=element_blank(),
        text=element_text(size=14),
        legend.box = "vertical")+
  ggtitle("C")
fig5c

#fig5c SAC gamma diversity

df.d <- fortify(gamma, type = 1)
df.d.point <- df.d[which(df.d$method=="observed"),]
df.d.line <- df.d[which(df.d$method!="observed"),]
df.d.line$method <- factor(df.d.line$method, 
                           c("interpolated", "extrapolated"),
                           c("Interpolation", "Extrapolation"))

fig5d<- ggplot(df.d, aes(x=x, y=y)) + 
  #geom_ribbon(aes(ymin = y.lwr, ymax =y.upr, colour = "gray", alpha = 0.2)) +
  geom_point(size=6, data=df.d.point) +
  geom_line(aes(linetype=method), lwd=1.5, data=df.d.line) +
  scale_y_continuous(breaks = seq(5, 70, 5))+
  scale_x_continuous(breaks = seq(0, 3500, 250))+
  theme_bw() +
  labs(x="Number of sampling units", y="Species richness") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.title=element_blank(),
        text=element_text(size=14),
        legend.box = "vertical")+
  ggtitle("D")
fig5d

#patchwork todos los gráficos
(fig5a | fig5b | fig5c) / fig5d + plot_layout(guides = 'collect')


esfuerzo <- full_data %>% 
  group_by(City, Beach) %>% 
  summarise("effort" = length(Station))







