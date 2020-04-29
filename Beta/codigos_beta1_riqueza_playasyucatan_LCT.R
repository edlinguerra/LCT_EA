#Actividad Beta diversidad Parte 1

#Paquetes para diversidad beta
library(vegan)
library(betapart)

#paquetes para optimizar tratamiento de datos
library(ggplot2) #gráficos
library(dplyr) #reordenar datos
library(gt)

library(patchwork) #gráfico múltiple
library(readxl) #importar archivos excel

####################################################################

#importar archivo de datos (SB_yucatan_oct2018.csv) como full_data
full_data <- read_excel("SB_yucatan_mexico_march2019_final.xlsx", sheet = "Hoja1")

#divide la matriz que incluya solo especies, nómbrala dat
dat<-full_data[,22:52] 

#Estima la riqueza de especies por localidad 
alfa <- specpool(dat, pool = full_data$City)
alfa.p <- mean(alfa$Species)

#Estima la riqueza de especies por lregión
gamma <- specpool(dat)

#Estima Beta diversidad usando Whittaker (1960)
# La fórmula original es βw = S / α
Bw <- gamma$Species / alfa.p

#apliquemos los índices revisados por Koleff

#Primero preparemos los datos
dat$localidad <- full_data$City

#función necesaria para transformar los datos a Presencia/Ausencia (1/0)
pa <- function(x){
  suma <- sum(x)
  y <- 1 * (suma > 0)
  return(y)
}

#Apliquemos una serie de comandos para reducir la matriz de datos de 225 observaciones a 6 (una resumen por localidad)
dat.t <- dat %>% 
          group_by(localidad) %>%
            summarise_all(pa)
dat.t

dat.red <- as.matrix(dat.t[,2:32])


####Tipos de medidas de Beta diversidad según Koleff (2005). Evalúe algunos de los 24 índices enlistados

#Aplique la siguiente función para que identifique el texto que debe colocar en method = " "
betadiver(help = TRUE)

#Medidas de continuidad βRLB
b.rlb <- betadiver(dat.t[,2:32], method = "rlb")


#Medidas de gradiente βgl 
b.gl <- betadiver(dat.t[,2:32], method = "gl")



#Medidas de continuidad
#Calculemos el índice pareado de Whittaker (1960)
b.w <- betadiver(dat.t[,2:32], method = "w")

#Calculemos el índice pareado de Jaccard (1912)
b.j <- betadiver(dat.t[,2:32], method = "j")

#Calculemos el índice pareado de Sorensen (1948)
b.sor <- betadiver(dat.t[,2:32], method = "sor")

#Medidas de ganancia y pérdida
#Calculemos el índice pareado de Simpson (1943)
b.sim <- betadiver(dat.t[,2:32], method = "sim")

###Ahora comparemos los índices para ver qué dicen respecto a otros

#Lista para acumular las matrices de distancia
beta.list <- list(b.rlb, b.gl, b.w, b.j, b.sor, b.sim)

#Matriz para albergar resultados de correlación
matriz <- matrix(data = NA, nrow = 6, ncol = 6)
row.names(matriz) <- c("Brlb","Bgl", "Bw", "Bj", "Bsor", "Bsim") 
colnames(matriz) <- c("Brlb","Bgl", "Bw", "Bj", "Bsor", "Bsim") 

#Estimación de correlaciones
for (i in 1:6){
  for (j in 1:6){
  matriz[j,i] <- cor(beta.list[[j]], beta.list[[i]]) 
  }
}

#Correlacions como objeto de tipo distancia
cor.betas <- abs(as.dist(matriz))
cor.betas <- 1 - cor.betas

#Veamos un gráfico tipo MDS para comparar visualmente los índices
mds <- metaMDS(cor.betas)
plot(mds, type = "t")


