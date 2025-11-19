# MAGA

Proyecto final de Bioinformática

Incluye la función MAGA, que permite seleccionar análisis estadísticos basándose en las variables de la base de datos generada.
La función genera recomendaciones, hace el análisis, y verifica el cumplimiento de los supuestos.

Los análisis disponibles dentro de la función, hasta el momento son:

* ANOVA de una vía
* ANCOVA
* Prueba de t
* Regresión lineal
* Tablas de contingencia


El repositorio se encuentra organizado en carpetas, para permitirle al usuario tener una investigación reproducible.


#########################################################################################

* 01_Raw data

La carpeta 01_RawData es específica para guardar las bases de datos sin procesar. Esto permite reproducibilidad de la investigación del usuario.

# Las siguientes líneas de código añaden las bases de datos a las carpetas y permiten su uso en la función.

write.csv (Orange, file = "01_RawData/Orange.csv", row.names = F) # Agrega el archivo a la carpeta (Para que el usuario tenga el archivo al clonar el repositorio)
Orange <- read.csv ("01_RawData/Orange.csv") # Asigna la base de datos a un objeto para utilizarlo en la función

write.csv (ChickWeight, file = "01_RawData/ChickWeight.csv", row.names = F)
ChickWeight <- read.csv ("01_RawData/ChickWeight.csv")

# Esta base de datos fue importada antes de correr el siguiente código
write.csv (frijoles, file = "01_RawData/frijoles.csv", row.names = F)
frijoles <- read.csv ("01_RawData/frijoles.csv")

#########################################################################################

* 02_Scripts

La carpeta 02_Scripts es específica para guardar el código utilizado para el análisis de los datos, pero los resultados no se guardan en esta carpeta.

Para el uso de la función MAGA es necesario que se instalen y carguen los paquetes "ggplot2" y "gmodels"

install.packages ("ggplot2")
install.packages ("gmodels")
library (ggplot2)
library (gmodels)

Incluimos algunas bases de datos que el usuario puede utilizar para probar el funcionamiento de MAGA.
Precargar las bases de datos para conocer las variables.

#########################################################################################

* 03_Results

En la carpeta 03_Results, se guardarán todos los resultados generados con el código de la carpeta 02_Scripts.

#########################################################################################

* 04_Docs

En la carpeta 04_Docs se encuentra una presentación introductoria a la función MAGA e información sobre los contribuidores.

#########################################################################################

* Referencias

1. Define aesthetic mappings programmatically — aes_. (s. f.). https://ggplot2.tidyverse.org/reference/aes_.html
2. Etzel Garrido.(2023).frijoles
3. Wickham, H. (2025). ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics (versión 4.0.1). https://github.com/tidyverse/ggplot2
4. Warnes G. R. et al (2024). gmodels: Various R Programming Tools for Model Fitting (Versión 2.19.1). https://github.com/r-gregmisc/gmodels

#########################################################################################

* Información adicional

Autoras: Gabriela Chávez Reséndiz y Mariana Gómez Becerra

Versión 1.0.0

Fecha de publicación: 19-11-2025

URL: https://github.com/gabychares/MAGA

