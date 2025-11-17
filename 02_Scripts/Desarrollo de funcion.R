# Proyecto final 
# Se tiene como idea que a partir de una función en la cual el usuario introduzca los datos de las variables 
# que esta estudiando se determine el análisis estadístico que mejor se acople.


maga <- function(){
  
  # Preguntas para determinar el analisis estadistico que mejor se acople 
  
  readline(prompt = "que tipo de variable dependiente estas usando (c = continua / d = discreta)?") -> dependiente
  readline(prompt= "que tipo de variable independiente estas usando? (c = continua / d = discreta)") -> independiente
  readline (prompt = "¿Estás usando una covariable? (s/n)") -> covariable

    if (dependiente == "c" & independiente == "d"){
    readline (prompt= "¿cantidad de variables dependientes?")-> vnum
    if(vnum >=2 ){
      print("anova o glm")
    }else if (vnum== 1){
      print ("prueba de t")}
  }else if  (dependiente == "c" & independiente == "c"){
    print("glm, correlación o regresión")
  }else if (dependiente == "d" & independiente == "d" ){
    print("tablas de contingencia")
  }
  
  # Después de que se le dan las mejores opciones al usuario, se hace el análisis que este indique
  
  message ("anova = a, ancova = c, glm = g, prueba de t = t, correlación = cr, regresión = r, tablas de contingencia = tc")
  readline (prompt = "¿Qué analisis vas hacer?") -> analisis
  
  if(analisis == "t"){
  #Introducir bases de datos
   readline (prompt = "Una vez precargada tu base de datos en tu Rstudio, introduce el nombre de tu base de datos ") -> Data 
   readline (prompt = "Introduce tu variable dependiente, tal como aparece en tu base de datos") 
   readline (prompt = "Introduce tu variable independiente, tal como aparece en tu base de datos") 
   print ("Ahora introduce en la siguiente función de R tus datos de esta manera:Base de datos$variable dependiente, mu= el promedio de tu hipotesis nula: t.student ()")
 
   } else if (analisis == "a") {
    readline (prompt = "Una vez precargada tu base de datos en tu Rstudio, introduce el nombre de tu base de datos ") -> Data 
    readline (prompt = "Introduce tu variable dependiente, tal como aparece en tu base de datos") -> dep
    readline (prompt = "Introduce tu variable independiente, tal como aparece en tu base de datos") -> ind
    
    anova <- aov (dep ~ ind, Data)
    summary (anova)
  }
    
  #Supuestos de los análisis
   
   
  #Hacer analisis 
}

maga()






