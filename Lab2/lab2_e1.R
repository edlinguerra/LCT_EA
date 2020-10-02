#Script del ejercicio 1 del laboratorio 2
#Nombre y Apellido:

#Preguntas: 

#1. Importa los datos al ambiente con el nombre **datos** y verifica sus características y estructura. 


#2. Calcula la media (con la función `mean`) y desviación estándar (con la función `sd`) de la DBO en la muestra y 
#   obtén un histograma (`hist`) para ver su distribución. ¿Te parece que tienen una distribución normal?

#  ¿cuál es el error estándar de la media? ¿Es un valor grande o pequeño?

#3. Define la hipótesis alternativa y luego la Hipótesis nula. Aplica una prueba de t para una sola muestra, usando el 
#   comando `t.test`, pero antes de efectuar la prueba debes definir el alfa para rechazar la Hipótesis nula.
#   Utiliza la ayuda `help` para escribir los argumentos correctos de `t.test` (tip: los mas importantes son *mu* y *alternative*)


#4. A partir del resultado obtenido (output o salida) responde a las siguientes preguntas:
#    a) ¿Cuál fue el valor de *t* que obtuviste? 

#    b) ¿Cuál es el valor de probabilidad asociado a ese valor de *t*?

#    c) ¿Existen evidencias suficientes para considerar que DBO de las aguas redisuales superan los valores de la norma?

#    d) ¿Cuáles son los límites inferior y superior del intervalo de confianza al 95%? 


#5. Vamos a calcular el intervalo de confianza paso por paso. Para ello requerimos contar con el valor de *t* que separa la curva en zonas de baja y alta probabilidad de ocurrir (dos colas, cada una de prob alfa/2). Este valor se obtiene con la función `qt` y requiere de tres argumentos esenciales (p = 0.025, df = 49, lower.tail = TRUE. Usa el `help` para tener más detalles de esta función).
#    a) ¿Son un error los valores negativos de esta función? ¿Por qué? 

#    b) ¿Cómo puedes obtener un valor positivo?

#    c) ¿Por qué razón usamos una probabilidade de 0.025, si el intervalo de confianza es de 95%?

#    d) ¿Por qué usamos n-1 en vez de n?

#    Ahora, multiplica el valor de t-student (absoluto) por el valor de error estándar calculado antes.

#    e) ¿Qué obtuviste con esta operación? 

#    f) ¿Cuáles son las unidades de esta medida?

#    Finalmente suma y resta el intervalo (al 95%) a la media para conocer los límites superior e inferior. 

#    g) Compara los valores obtenidos con el resultado de la función usada para hacer la prueba de hipótesis.

#8. Calcula el intervalo de confianza al 99%? 
#    a) ¿En cuál de los dos intervalos (95% o 99%) tengo más confianza de encontrar la media si repito el experimento?

#    b) ¿Cuál de los dos intervalos es más ancho?

#    c) ¿Qué le sucedería al intervalo si en vez de 50 mediciones, tuviese solo 5 mediciones?

#    d) Verifica tu conclusión (PISTA: usa la función `qt`)

#9. ¿Qué sucedería con el IC al 95% si la DBO de los efluentes fuese mas variable? 

#    Obtén un histograma y verifica tu conclusión mediante una simulación usando la función `rnorm` (PISTA: mantén la muestra simulada en n=50, y la media en 40).