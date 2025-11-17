# Proyecto final 
# Se tiene como idea que a partir de una función en la cual el usuario introduzca los datos de las variables 
# que esta estudiando se determine el análisis estadístico que mejor se acople.


maga <- function(){
  #Preguntas para determinar el analisis estadistico que mejor se acople 
  readline(prompt = "que tipo de variable dependiente estas usando (continua/discreta)?") -> dependiente
  readline(prompt= "que tipo de variable independiente estas usando? (continua/discreta)") -> independiente
  readline (prompt = "¿Estás usando una covariable? (S/N)") -> covariable
  if (dependiente == "continua" & independiente == "discreta"){
    readline (prompt= "¿cantidad de variables dependientes?")-> vnum
    if(vnum >=2 ){
      print("anova o glm")
    }else if (vnum== 1){
      print ("prueba de t")}
  }else if  (dependiente == "continua" & independiente == "continua"){
    print("glm, correlación o regresión")
  }else if (dependente == "discreta" & independiente == "discreta" ){
    print("tablas de contingencia")
  }
  # Después se hace el análisis
  readline(prompt = "que analisis vas hacer?") -> analisis
  if(analisis == "prueba de t"){
  #Introducir bases de datos
   readline (prompt = "Una vez precargada tu base de datos en tu Rstudio, introduce tu base de datos ") -> Data 
   readline (prompt = "Introduce tu variable dependiente") 
   readline (prompt = "Introduce tu variable independiente") 
   print ("Ahora introduce en la siguiente función de R tus datos de esta manera:Base de datos$variable dependiente, mu= el promedio de tu hipotesis nula: t.student ()")
  }else if ( analisis == )
  #Supuestos de los análisis
   
   
  #Hacer analisis 
}
maga()






