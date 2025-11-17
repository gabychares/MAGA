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
    
   readline (prompt = "Introduce tu variable dependiente, tal como aparece en tu base de datos ") 
   readline (prompt = "Introduce tu variable independiente, tal como aparece en tu base de datos ") 
   print ("Ahora introduce en la siguiente función de R tus datos de esta manera:Base de datos$variable dependiente, mu= el promedio de tu hipotesis nula: t.student ()")
 
   } else if (analisis == "a") {
     
     readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos ") -> dep
     readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos ") -> ind
     
     dep <- as.numeric (dep)
     ind <- as.numeric (ind)
     
     anova <- aov (data [, dep] ~ data [ , ind], data)
     
     message ("Resultados del análisis:")
    
     summary (anova)
     
  }
    
  #Supuestos de los análisis
   
   
  #Hacer analisis 
}

maga (ChickWeight) # Ejemplo de uso de la función
