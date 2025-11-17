
# La carpeta 01_RawData es específica para guardar las bases de datos sin procesar. Esto permite reproducibilidad de la investigación del usuario.

# Las siguientes líneas de código añaden las bases de datos a las carpetas y permiten su uso en la función.

write.csv (Orange, file = "01_RawData/Orange.csv", row.names = F) # Agrega el archivo a la carpeta (Para que el usuario tenga el archivo al clonar el repositorio)
Orange <- read.csv ("01_RawData/Orange.csv") # Asigna la base de datos a un objeto para utilizarlo en la función

write.csv (ChickWeight, file = "01_RawData/ChickWeight.csv", row.names = F)
ChickWeight <- read.csv ("01_RawData/ChickWeight.csv")

# Esta base de datos fue importada antes de correr el siguiente código
write.csv (frijoles, file = "01_RawData/frijoles.csv", row.names = F)
frijoles <- read.csv ("01_RawData/frijoles.csv")

