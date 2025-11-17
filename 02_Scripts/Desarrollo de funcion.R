# Proyecto final 
# Se tiene como idea que a partir de una función en la cual el usuario introduzca los datos de las variables 
# que esta estudiando se determine el análisis estadístico que mejor se acople.

library (ggplot2)
library (gmodels)

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
  
  if(analisis == "t"){ # Hace la prueba de t
    
   readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos ") -> dep 
   readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos ") -> ind 
   
   dep <- as.numeric (dep) # Esto hace coerción del objeto para que sea considerado numérico, y podamos utilizarlo en la función
   ind <- as.numeric (ind)
  
    t.test (data [, dep] ~ data [ , ind], data) # Indica que la primera variable se encuentra en la base de datos "data", y abarca todos los renglones de la columna indicada por el usuario
   
   
   } else if (analisis == "a") { # Hace el ANOVA y genera las gráficas para verificar el cumplimiento de los supuestos
     
     readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos ") -> dep
     readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos ") -> ind
     
     dep <- as.numeric (dep)
     ind <- as.numeric (ind)
     
     anova <- aov (data [, dep] ~ data [ , ind], data) # aov hace el anova
     
     message ("Resultados del análisis:")
     
     summaryanova <- summary (anova) # Muestra los resultados del anova de manera más fácil de visualizar
     print (summaryanova)
     
     readline (prompt = "Presiona enter para continuar")
     
     print ("A continuación se mostrarán 4 gráficas, que deben cumplir lo siguiente:")
     
     message("Residuals vs Fitted y Scale Location permiten observar la homogeneidad de varianzas:")
     print("Los números que aparecen en la  gráfica a lado de la bolita representa el renglón en el que se encuentra ese residual; Scale Location debería tener pendiente de con tendencia a 0")
     
     message("Con Normal Q-Q  se ve si se cumple el supuesto de la distribución gaussiana de los residuales (distribución normal):")
     print("La línea punteada debe estar de esquina a esquina y cada uno de los residuales debe de caer en la línea punteada o cerca de la línea punteada") 
     
     message ("Residuals vs Leverage (influencia) de los valores en el análisis o son pseudoréplicas; es para el tercer supuesto (Hay independencia entre los errores y las observaciones):")
     print ("Si no se observa el enunciado de Distancia de Cook: nos representa que no hay un valor que sobresalga en la influencia de los valores, por lo que se cumple el supuesto.")
     
     # nos da las gráficas por separado
     #Ver todas las gráficas en un mismo lugar
     plot(anova)
     layout(matrix(c(1,2,3,4),2,2,))
     plot(anova)
     
     message ("Si todos los supuestos se cumplen, el resultado de tu análisis es confiable")
     
  } else if (analisis == "r") { # Hace el análisis de regresión lineal y genera la gráfica del análisis
    
    readline (prompt = "Introduce el nombre de tu variable dependiente, tal como aparece en tu base de datos ") -> dep
    readline (prompt = "Introduce el nombre de tu variable independiente, tal como aparece en tu base de datos ") -> ind
    
    formula <- as.formula (paste (dep, "~", ind)) # Esto genera la fórmula que se utilizará en el análisis de regresión. Es importante hacer la coerción porque la función lm no reconoce el texto obtenido del prompt como un argumento.
    
    regresion <- lm (formula, data) # lm hace la regresión
    
    sreg <- summary (regresion) # Da un resumen estadístico de los resultados del lm
    print (sreg)

    
    figuraregresion <- ggplot (data, aes (.data [[ind]], .data [[dep]])) + # Basado en la página de ggplot2 (Define Aesthetic Mappings Programmatically — Aes_, s. f.). Referencias disponibles en el archivo README_Scripts.
      geom_point () + # Agrega los puntos de la regresión en el formato predeterminado
      geom_smooth (method = "lm", se = T) # Añade la línea de la regresión
    print (figuraregresion)
    
  } else if (analisis == "tc") { # Hace el análisis de tabla de contingencia
    
    readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos ") -> dep
    readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos ") -> ind
    
    dep <- as.numeric (dep)
    ind <- as.numeric (ind)
    
    CrossTable (data [, dep], data [, ind], digits = 5, expected = T, chisq = T)
    
    figuratc <- ggplot (data, aes (.data [[dep]], fill = .data [[ind]])) +
      geom_bar (stat = "count", position = "stack") +
    figuratc
  
  }
    
  
}

maga () # Ejemplo de uso de la función

micofrijoles <- read.csv ("01_RawData/micofrijoles.csv")
