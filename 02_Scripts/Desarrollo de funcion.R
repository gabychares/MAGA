# Proyecto final 
# Se tiene como idea que a partir de una función en la cual el usuario introduzca los datos de las variables 
# que esta estudiando se determine el análisis estadístico que mejor se acople.


maga <- function (data) {
  
  # Preguntas para determinar el analisis estadistico que mejor se acople 
  
  readline(prompt = "¿Qué tipo de variable dependiente estas usando? (c = continua / d = discreta) ") -> dependiente
  readline(prompt= "¿Qué tipo de variable independiente estas usando? (c = continua / d = discreta) ") -> independiente
  readline (prompt = "¿Estás usando una covariable? (s/n) ") -> covariable

    if (dependiente == "c" & independiente == "d"){
    readline (prompt= "¿cantidad de variables dependientes?")-> vnum
    if(vnum >=2 ){
      print("Te recomendamos hacer un ANOVA o glm")
    }else if (vnum== 1){
      print ("Te recomendamos hacer una prueba de t")}
  }else if  (dependiente == "c" & independiente == "c"){
    print("Te recomendamos hacer un glm, correlación o regresión")
  }else if (dependiente == "d" & independiente == "d" ){
    print("Te recomendamos hacer un análisis de tablas de contingencia")
  }
  
  # Después de que se le dan las mejores opciones al usuario, se hace el análisis que este indique
  
  message ("anova = a, ancova = c, glm = g, prueba de t = t, correlación = cr, regresión = r, tablas de contingencia = tc")
  readline (prompt = "¿Qué analisis vas hacer? ") -> analisis
  
  if(analisis == "t"){
  #Introducir bases de datos
    
   readline (prompt = "Introduce tu variable dependiente, tal como aparece en tu base de datos ") -> dep 
   readline (prompt = "Introduce tu variable independiente, tal como aparece en tu base de datos ") -> ind 
   dep <- as.numeric (dep)
   ind <- as.numeric (ind)
   t.test (data [, dep] ~ data [ , ind], data)
   
   
   } else if (analisis == "a") {
     
     readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos ") -> dep
     readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos ") -> ind
     
     dep <- as.numeric (dep)
     ind <- as.numeric (ind)
     
     anova <- aov (data [, dep] ~ data [ , ind], data)
     
     message ("Resultados del análisis:",summary(anova));
     readline (prompt = "Presiona enter para continuar")
     message("Ahora hay que checar los supuestos del anova:")
     # nos da las gráficas por separado
     #Ver todas las gráficas en un mismo lugar
     plot(anova)
     layout(matrix(c(1,2,3,4),2,2,))
     plot(anova)
     message("Con Normal Q-Q  se ve si se cumple el supuesto de la distribución de residuales en gaussiana (normal):")
     print("La línea punteada debe estar de esquina a esquina y cada uno de los residuales debe de caer en la línea punteada o cerca de la línea punteada") 
     message("Para el segundo supuesto sirven Residuals vs Fitted y Scale Location para ver la homogeneidad de varianzas, los números que aparecen en la  gráfica a lado de la bolita representa")
     print("el renglón en el que se encuentra ese residual; Scale Location debería tener pendiente de con tendencia a 0")
     message ("Residuals vs Leverange (influencia) de loa valores en el análisis o son pseudoréplicas; es para el tercer supuesto (Hay independencia entre los errores y las observaciones)")
     message("Si no se observa el enunciado de Distancia de Cook: nos representa que no hay como un valor que sobresalga en la influencia de los valores.")
     
     
  }
    
  #Supuestos de los análisis
   
   
  #Hacer analisis 
}
ChickWeight
maga (ChickWeight) # Ejemplo de uso de la función
