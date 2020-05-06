#Actividad Beta diversidad Parte 2: Ordenación

#Paquetes para diversidad beta
library(vegan)
library(betapart)

#paquetes para optimizar gráficos
library(ggplot2)


####################################################################


# Se obtuvieron muestras de organismos macrobénticos de fondos blandos en N = 101 sitios, en 5 grandes áreas a lo largo de una transecta
# de 15 grados de latitud en la costa de Noruega. Un total de p = 809 taxones fueron registrados, y las muestras consistieron en 
# abundancias sumadas en cinco dragas bentónicas obtenidos en cada sitio. El interés era comparar la beta diversidad (el grado de heterogeneidad
# composicional) para cada una de estas cinco áreas, dispuestas en un gradiente latitudinal. Los datos están en el archivo macrofauna.csv.

macrofauna <- read.csv("macrofauna.csv")

# 1.	Calcule la matriz de similitud de Jaccard a partir de estos datos usando la librería vegan.
library(vegan)

dat<-macrofauna[,1:809]
jac<-vegdist(dat, method = "jaccard", binary = TRUE)

# 2.	Genere un MDS con la librería vegan luego mejore el gráfico usando ggplot para asignar colores a 
#     los símbolos de cada áres.

#MDS con vegan
val.mds<-metaMDS(jac)
plot(val.mds, type = "t")

#MDS con ggplot2
scores.mds<-as.data.frame(val.mds$points)
scores.mds$Area<-as.factor(macrofauna$Area)

MDS<-ggplot(scores.mds, aes(x=MDS1, y=MDS2, color = Area)) +
  geom_point(size=6)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x=0.3, y=0.43, label= "2D Stress = 0.117")

MDS            



# 3.  Estime la dispersión multivariada de cada área usando la función betadisper de vegan. Grafique las dispersiones por área y someta a prueba
#	la hipótesis de igual despersión usando permutest.

dispercion<-betadisper(jac, group = macrofauna$Area)
permutest(dispercion)
boxplot(dispercion)

# 4.	¿Qué tan similares son las áreas en composición de especies? #Alternativamente podemos evaluar qué tan diferentes son las regiones. 
# Ahora la pregunta no es sobre qué tan dispersas son, si qué tan diferentes son entre ellas
anosim(jac, group = macrofauna$Area)
 
# 5.	¿Del total de beta diversidad entre las áreas, cuánto corresponde a verdadero
# recambio de especies, y cuánto a perdida por el gradiente	latitudinal?  use la función beta.multi del paquete betapart

library(betapart)
core<-betapart.core(dat)
beta.m<-beta.multi(core, index.family = "jaccard")
beta.m

# Densidad de especies por sitio

scores.mds$riqueza <- specnumber(dat)

ggplot(scores.mds, aes(x= Area, y = riqueza, color = Area))+
  geom_boxplot()+
  theme_classic()



