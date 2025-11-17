# Proyecto final 

# Se creó una función (MAGA) en la cual el usuario puede introducir los datos de las variables 
# que esta estudiando, y se determina el análisis estadístico que mejor se acople.

maga <- function (data) {
  
  # Preguntas para determinar el análisis estadístico que mejor se acople 
  
  readline(prompt = "¿Qué tipo de variable dependiente estas usando? (c = continua / d = discreta) ") -> dependiente
  readline(prompt= "¿Qué tipo de variable independiente estas usando? (c = continua / d = discreta) ") -> independiente
  readline (prompt = "¿Estás usando una covariable? (s/n) ") -> covariable

    if (dependiente == "c" & independiente == "d" & covariable == "s"){ # Código para recomendar ANCOVA
      print("Te recomendamos hacer un ANCOVA")
      
      } else if (dependiente == "c" & independiente == "d"){ # Código para recomendar ANOVA o prueba de t
        readline (prompt= "¿Cuántas muestras estás comparando?")-> vnum # Permite decidir si se usa ANOVA o prueba de t
        if(vnum >= 2 ){ 
          print("Te recomendamos hacer un ANOVA")
        }else if (vnum == 1){
            print ("Te recomendamos hacer una prueba de t")} else {
         print ("Vuelve a intentarlo")
        }
        
      }else if  (dependiente == "c" & independiente == "c"){ # Código para recomendar regresión lineal
        print("Te recomendamos hacer una regresión lineal")
        
      }else if (dependiente == "d" & independiente == "d" ){ # Código para recomendar tablas de contingencia
        print("Te recomendamos hacer un análisis de tablas de contingencia")
      
      } # Fin del código para generar recomendaciones
  
  # Después de que se le dan las mejores opciones al usuario, se hace el análisis que este indique
  
  message ("anova = a, ancova = c, prueba de t = t, regresión = r, tablas de contingencia = tc")
  readline (prompt = "¿Qué analisis vas hacer? ") -> analisis
  
  if(analisis == "t"){ # Hace la prueba de t
    
   # En algunos análisis, se optó por utilizar el número de columna de la variable en lugar del nombre, debido a que eso generó los resultados adecuados. En otros casos fue mejor utilizar el nombre en lugar del número de columna.
   
   readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos:  ") -> dep 
   readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos:  ") -> ind 
   
   dep <- as.numeric (dep) # Esto hace coerción del objeto para que sea considerado numérico, y podamos utilizarlo en la función
   ind <- as.numeric (ind)
  
    t.test (data [, dep] ~ data [ , ind], data) # Indica que la primera variable se encuentra en la base de datos "data", y abarca todos los renglones de la columna indicada por el usuario
   
   
   } else if (analisis == "a") { # Hace el ANOVA y genera las gráficas para verificar el cumplimiento de los supuestos
     
     readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos:  ") -> dep
     readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos:  ") -> ind
     
     dep <- as.numeric (dep)
     ind <- as.numeric (ind)
     
     anova <- aov (data [, dep] ~ data [ , ind], data) # aov hace el anova
     
     message ("Resultados del análisis:")
     
     summaryanova <- summary (anova) # Muestra los resultados del anova de manera más fácil de visualizar
     print (summaryanova)
     
     readline (prompt = "Presiona enter para continuar") # Permite al usuario decidir cuando iniciar el siguiente paso de la función.
     
     print ("A continuación se mostrarán 4 gráficas, que deben cumplir lo siguiente:")
     
     # Muestra los supuestos del anova para que este análisis tenga confiabilidad
     
     message("Residuals vs Fitted y Scale Location permiten observar la homogeneidad de varianzas:")
     print("Los números que aparecen en la  gráfica a lado de cada punto representan el renglón en el que se encuentra ese residual; Ambas gráficas deben tener una línea con pendiente con tendencia a 0, y los puntos deben estar bien distribuidos en los ejes X y Y")
     
     message("Con Normal Q-Q  se ve si se cumple el supuesto de la distribución gaussiana de los residuales (distribución normal):")
     print("La línea punteada debe estar de esquina a esquina y cada uno de los residuales debe de caer en la línea punteada o cerca de la línea punteada") 
     
     message ("Residuals vs Leverage: determina la influencia de los valores en el análisis o si hay pseudoréplicas; es para el tercer supuesto (Hay independencia entre los errores y las observaciones):")
     print ("Si no se observa el enunciado de Distancia de Cook: nos representa que no hay un valor que sobresalga en la influencia de los valores, por lo que se cumple el supuesto.")
     
     # Primero nos da las gráficas por separado
     # Y luego muestra todas las gráficas en un mismo plot
     
     plot(anova)
     layout(matrix(c(1,2,3,4),2,2,))
     plot(anova)
     
     message ("Si todos los supuestos se cumplen, el resultado de tu análisis es confiable") # Fin del análisis
     
  } else if (analisis == "r") { # Hace el análisis de regresión lineal y genera la gráfica del análisis
    
    message("Requieres el paquete ggplot2 precargado")
    
    readline (prompt = "Introduce el nombre de tu variable dependiente, tal como aparece en tu base de datos:  ") -> dep
    readline (prompt = "Introduce el nombre de tu variable independiente, tal como aparece en tu base de datos:  ") -> ind
    
    formula <- as.formula (paste (dep, "~", ind)) # Esto genera la fórmula que se utilizará en el análisis de regresión. Es importante hacer la coerción a fórmula porque la función lm no reconoce el texto obtenido del prompt como un argumento.
    
    regresion <- lm (formula, data) # lm hace la regresión, con la fórmula generada en la línea anterior y con el archivo que se agregó en los argumentos.
    
    sreg <- summary (regresion) # Da un resumen estadístico de los resultados del lm
    print (sreg)

    
    figuraregresion <- ggplot (data, aes (.data [[ind]], .data [[dep]])) + # Basado en la página de ggplot2 (Define Aesthetic Mappings Programmatically — Aes_, s. f.). Referencias disponibles en el archivo README_Scripts.
      geom_point () + # Agrega los puntos de la regresión en el formato predeterminado
      geom_smooth (method = "lm", se = T) # Añade la línea de la regresión
    print (figuraregresion)
    
    # Supuestos del análisis
    
    print ("Ahora hay que checar los supuestos:")
    message ("La varianza en Y es constante, el efecto de x es proporcionalmente igual en cualquier punto de y")
    message ("Los residuales del modelos (error) se ajustan a una distribución gaussiana")
    
    print ("A continuación se mostrarán 3 gráficas, que deben cumplir lo siguiente:")
    
    message("Residuals vs Fitted y Scale Location permiten observar la homogeneidad de varianzas:")
    print("Los números que aparecen en la  gráfica a lado de cada punto representan el renglón en el que se encuentra ese residual; Ambas gráficas deben tener una línea con pendiente con tendencia a 0, y los puntos deben estar bien distribuidos en los ejes X y Y")
    
    message("Con Normal Q-Q  se ve si se cumple el supuesto de la distribución gaussiana de los residuales (distribución normal):")
    print("La línea punteada debe estar de esquina a esquina y cada uno de los residuales debe de caer en la línea punteada o cerca de la línea punteada") 
    
    message ("En el ANCOVA, no es necesario que se cumpla el supuesto de pseudoréplicas, ignorar gráfica 4")
    
    layout(matrix (c(1,2,3), 1, 3))
    plot (ancova)
    
   message("Si no se cumple uno de los supuestos, los resultados no son confiables y se debe buscar otro tipo de análisis")
    
  } else if (analisis == "tc") { # Hace el análisis de tabla de contingencia
    
    message("Requieres el paquete gmodels precargado")
    
    readline (prompt = "Introduce el nombre de tu variable dependiente, tal como aparece en tu base de datos:  ") -> dep
    readline (prompt = "Introduce el nombre de tu variable independiente, tal como aparece en tu base de datos:  ") -> ind
    
    CrossTable (data [, dep], data [, ind], digits = 5, expected = T, chisq = T) # Este análisis es muy sensible a decimales, por lo que se usaron 5. expected y chisq son T para que el resultado muestre los valores esperados y el valor del estadístico chi^2
    
    figuratc <- ggplot (data, aes (.data [[dep]], fill = .data [[ind]])) + # La variable dependiente es el eje X, y el eje Y es el conteo de esta. La variable independiente se usa en fill para separar las barras de la variable independiente según la segunda variable.
      geom_bar (stat = "count", position = "stack") # Indica que el eje Y tiene el conteo, y las barras que se separaron por la variable independiente se apilarán.
    figuratc # Visualización de la gráfica con la tabla de contingencia
  
  } else if (analisis == "c") { # Hace el ANCOVA y genera las gráficas para verificar el cumplimiento de los supuestos
    
    readline (prompt = "Introduce el número de columna de tu variable dependiente, tal como aparece en tu base de datos:  ") -> dep
    readline (prompt = "Introduce el número de columna de tu variable independiente, tal como aparece en tu base de datos:  ") -> ind
    readline (prompt = "Introduce el número de columna de tu covariable, tal como aparece en tu base de datos:  ") -> cov
    
    dep <- as.numeric (dep)
    ind <- as.numeric (ind)
    cov <- as.numeric (cov)
    
    ancova <- aov (data [, dep] ~ data [ , ind] + data [, cov], data) # aov hace el ancova
    
    message ("Resultados del análisis:")
    
    summaryancova <- summary (ancova) # Muestra los resultados del ancova de manera más fácil de visualizar
    print (summaryancova)
    
    readline (prompt = "Presiona enter para continuar")
    
    print ("A continuación se mostrarán 4 gráficas, que deben cumplir lo siguiente:")
    
    message("Residuals vs Fitted y Scale Location permiten observar la homogeneidad de varianzas:")
    print("Los números que aparecen en la  gráfica a lado de cada punto representan el renglón en el que se encuentra ese residual; Ambas gráficas deben tener una línea con pendiente con tendencia a 0, y los puntos deben estar bien distribuidos en los ejes X y Y")
    
    message("Con Normal Q-Q  se ve si se cumple el supuesto de la distribución gaussiana de los residuales (distribución normal):")
    print("La línea punteada debe estar de esquina a esquina y cada uno de los residuales debe de caer en la línea punteada o cerca de la línea punteada") 
    
    message ("Residuals vs Leverage (influencia) de los valores en el análisis o son pseudoréplicas; es para el tercer supuesto (Hay independencia entre los errores y las observaciones):")
    print ("Si no se observa el enunciado de Distancia de Cook: nos representa que no hay un valor que sobresalga en la influencia de los valores, por lo que se cumple el supuesto.")
    
    # Nos da las gráficas por separado
    # Y después muestra todas las gráficas en un mismo plot
    
    plot(ancova)
    layout(matrix(c(1,2,3,4),2,2,))
    plot(ancova)
    
    message ("Por último, se debe cumplir el supuesto: Los factores no afectan a la covariable. Esto se verifica con un anova de una vía. Si el valor de p es mayor a 0.05, el supuesto se cumple")
    
    supuesto4 <- aov (data [, cov] ~ data [ , ind], data) # Analiza si la variable independiente tiene un efecto sobre la variación en la covariable
    summarysup4 <- summary (supuesto4)
    print (summarysup4)
    
    message ("Si todos los supuestos se cumplen, el resultado de tu análisis es confiable")
    
  }  else {
  print ("Lo sentimos, no tenemos disponible ese análisis. Vuelve a cargar la función para reintroducir tus datos si crees que esto es un error.")
    } # Esta línea permite al usuario finalizar la operación en caso de haber cometido un error al introducir los datos
}

maga () # Como argumento debe ponerse el nombre de la base de datos, previamente asignada a un objeto en R.
