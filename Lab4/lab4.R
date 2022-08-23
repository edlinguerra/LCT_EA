#Script del laboratorio  de la Unidad 4
#Nombre y Apellido:

#Preguntas:

# 1. Importa los datos en R, y verifica sus características y estructura. ¿Cuántas dimensiones tiene la
#tabla que importaste? ¿En qué difiere esta de aquellas usada en las pruebas de *t* para dos muestras? 

# 
# 
# 2. Lee el problema con atención y responde a las siguientes preguntas:
#   a) Identifica la relación causa-efecto que se pretende corroborar con este experimento.

#   b) Identifica la variable de respuesta, y las unidades en que ésta fue medida. ¿Es una variable 
#continua o discreta?

#   c) Identifica la variable que explicaría la respuesta si se corrobora la relación causa-efecto que
#fue propuesta. ¿Es una variable continua o categórica?

#   d) Identifica la unidad experimental. ¿Cuál es el valor de n (i.e., réplicas por nivel)?

#   f) Formula la hipótesis nula de este análisis en un enunciado simple.

# 
# 3. Lleva a cabo una breve exploración gráfica y numérica de los datos que te permita responder a las 
#siguientes preguntas. Para esto usa las funciones `boxplot` y `aggregate`. Este último para calcular 
#las medias y desviaciones estándar de cada localidad con las funciones `mean` y `sd`, respectivamente.
#Note que no podrá estimar ambos parámteros simultáneamente, por lo que deberá calcularlos separadamente,
#y luego combinar resultados para tener una sola tabla. Recomiendo que para ambas funciones (`boxplot` y 
#`aggregate`) use el método *S3 method for class 'formula'*.
# 
##Completa los argumentos para la construcción de el gráfico de cajas
boxplot()
 
##Completa los argumentos para la estimación de promedio y desviación estándar
 promedio<-aggregate()
 promedio
 
desv.est<-aggregate()
desv.est
 
# Combinar ambos resultados en una tabla
tabla1<-data.frame("localidades" = promedio$localidades, "DBO" = c(NA,NA,NA,NA), "Desv.Est" = c(NA,NA,NA,NA))
tabla1[,2]<-promedio[,2]
tabla1[,3]<-desv.est[,2]
tabla1
# 

# Responde a las siguientes preguntas:
#   a) ¿Son similares o diferentes los valores promedios de las 4 localidades?

#   b) ¿Son similares o diferentes las dispersiones de las 4 localidades?

#   c) ¿Cómo es la distribución de la variable de respuesta?

#   d) ¿Es esta distribución similar entre los distintos niveles?

#   
#   4. Aplica un **ANOVA** a los datos. Para ello se requiere primero obtener un modelo lineal usando la función
#`lm`. Esta función ajusta un modelo lineal de la variable de respuesta en función de la variable explicativa.
#Como en este caso la variable explicativa es un factor (categórico), es conveniente hacerlo explícito. Puedes 
#ajustar el modelo usando la *localidad* como variable explicativa. Copia el siguiente comando y analiza la respuesta 
#que R devuelve (PISTA: la primera linea de la respuesta es el modelo). 

##Especificar que localidades son una variable expicativa (factor)
datos$localidades<-as.factor(datos$localidades)
# 
#Preguntamos si localidades son reconocidas como factor en R
is.factor(datos$localidades)
# 
#Modelo lineal
mod<-lm(DBO ~ 0 + localidades, data = datos)
mod2<-lm(DBO ~ localidades, data = datos)
# 

# Responde a las siguientes preguntas:
#   a) ¿Reconoces alguno de los valores bajo el título de *Coefficients*? ¿Qué crees que son éstos valores?

#   b) ¿qué representa el primer coeficiente generado por `lm`? 

#   
#   
#   5. Guarda el modelo que acabas de ajustar bajo un objeto con el nombre *mod1*, y aplica la función `anova` a dicho objeto.
 


# Responde a las siguientes preguntas:
#   a) ¿Qué hace la función `anova`?

#   b) ¿Qué es la *Sum Sq* correspondiente al factor *localidades* y a los residuales?¿qué es *Df*?

#   c) ¿Cuánto vale la *Sum Sq* total?

#   d) ¿Corresponden los valores de *Sum Sq* y *Df* que aparecen en la consola con aquéllos calculados en clase?

#   e) ¿Qué es la *Mean Sq*?

#   f) ¿Qué representa el valor de *F* de la tabla? ¿Es un valor grande o pequeño? ¿Cómo lo sabes?

#   g) ¿Qué representa el valor de probabilidad? ¿Es un valor grande o pequeño? ¿Cómo lo sabes?

#   h) Calcula la proporción de la variación total de la variable *DBO* que es debida al factor *localidades*.

#   i) ¿Es grande o pequeña esta proporción? ¿Cómo lo sabes?

#   j) A partir de este resultado, concluye si tienes evidencias suficientes para rechazar la Ho que formulaste antes.

#   k) ¿Cuál es la probabilidad de equivocarte en esta aseveración?
#   
#   6. Utilizando la función `qf` obtén el valor crítico de *F* bajo la hipótesis nula. Los argumentos de la función están 
#en el 'help'. Busca valores de los grados de libertad para el numerador y el denominador en la tabla anterior, y considera
#un valor de $\alpha = 0.05$. ¿Qué representa este valor?

##Completa los argumentos de la función
qf(p = , df1 =, df2 =, lower.tail=F)
#   
#   
#   
#7. Intenta predecir lo que sucedería con el valor crítico de *F* bajo las siguientes situaciones. Después modifica el 
# comando que escribiste en el inciso 6 para corroborar tus predicciones.

#   a) si se aumenta el valor de $\alpha = 0.10$ (uno en diez chances de equivocarme).

#   b) si se disminuye el valor de alfa a  $\alpha = 0.001$ (uno en mil chances de equivocarme).

#   c) si aumentas el número de réplicas en este experimento a *n* = 30 réplicas por cada nivel del factor, manteniendo $\alpha = 0.05$.

#   
#   8. Aplica la función `summary` al modelo lineal que ajustaste, y responde a las siguientes preguntas:

#   #Completa los argumentos de la función
summary()
#   
 
#   Responde a las siguientes preguntas:
#     a) ¿Reconoces algún valor ya obtenido o calculado en el resultado que R devuelve? 

#   b) ¿Qué crees que sea el valor dado en 'Residual Standard Error'? 
   
#   9. Aplica la función `fitted` al modelo lineal que ajustaste. ¿Qué hace la función `fitted`?

#   ¿Qué pasa si aplicas la función `predict` al modelo lineal? ¿Cuántos hay?
#Completa los argumentos de la función
fitted()
predict()

# 
# 10. Para obtener una visualización prolija del modelo con los datos observados, copia los siguientes
#códigos del paquete `ggplot2`. Estos códigos representarán los valores por localidad, los promedios
#y desviaciones estándar. Explora cada uno y trata de identificar qué se va ganando a medida que agregas capas.
library(ggplot2)
# 
# #Figura básica
# fig1 <- ggplot(datos, aes(y=DBO, x=localidades))+
#   geom_point()
# fig1
# 
# #Figura básica con los promedios  
# fig1.1 <- fig1 +
#   geom_point(data=tabla1, aes(x=localidades, y=DBO, col=localidades), size=3) 
# 
# fig1.1
# #Figura básica con promedios y barras de desviación estándar
# fig1.2 <- fig1.1 +
#   geom_point(data=tabla1, aes(x=localidades, y=DBO, col=localidades), size=3) +
#   geom_errorbar(data = tabla1, aes(x= localidades, ymin = DBO - Desv.Est, ymax = DBO + Desv.Est), width = 0.2)
# fig1.2
# 
# #figura básica con promedios, barras de desviación estándar y cambios en la estética de la figura
# 
# fig1.3 <- fig1.2 +
#   theme_bw() + 
#   ylab(expression(paste("DBO ", "(mg ", O[2], "/l/d)")))+
#   xlab("Localidades")
# 
# #figura sólo con promedios, barras de desviación estándar y cambios en la estética de la figura
# fig1.4 <- ggplot(data = tabla1, aes(y=DBO, x=localidades)) +
#   geom_errorbar(data = tabla1, aes(x= localidades, ymin = DBO - Desv.Est, ymax = DBO + Desv.Est), width = 0.2) +
#   geom_point(aes(col = localidades), size = 3)+        
#   theme_bw() + 
#   ylab(expression(paste("DBO ", "(mg ", O[2], "/l/d)")))+
#   xlab("Localidades")
# 
# ```
# Responde a las siguientes preguntas:
#   a) ¿Qué representan los puntos de color? 
#   b) ¿Qué representan los puntos negros?
#   c) ¿Qué representan las barras?
#   d) Desde el punto de vista gráfico ¿qué se gana al pasar de fig1 a fig1.1, luego a fig1.2, a fig1.3 y fig1.4?
#   e) En el contexto del seguimiento ambiental ¿qué sugiere el resultado?
#   f) ¿existen diferencias significativas entre loc1 con loc2? ¿y entre loc1 con loc3? ¿loc3 respecto loc4?
#   
#   Por ahora es suficiente. Salve el proyecto con el nombre "laboratorio 3".