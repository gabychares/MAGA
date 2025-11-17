# Aquí pondremos datos crudos.

write.csv (Orange, file = "01_RawData/Orange.csv") # Agrega el archivo a la carpeta (Para que el usuario tenga el archivo al clonar el repositorio)
Orange <- read.csv ("01_RawData/Orange.csv") # Asigna la base de datos a un objeto para utilizarlo en la función

write.csv (ChickWeight, file = "01_RawData/ChickWeight.csv")
ChickWeight <- read.csv ("01_RawData/ChickWeight.csv")

micofrijoles <- read.csv ("01_RawData/micofrijoles.csv")
