# AN�LISIS DE GRADO DE INFLUENCIA DE ERRORES EN DATOS DE TSE 
# Autor: Jos� Miguel Molina Fern�nez
# Fecha: 24/10/2019

# Versi�n de la base de datos de c�mputo de TSE: acta.2019.10.24.20.01.43

# Importamos librer�as necesarias
library(readxl)

# TODO: Importar la base de datos con el nombre "actas"

# Filtramos solo los votos para Presidente y Vicepresidente
actas <- actas[actas$Elecci�n=="Presidente y Vicepresidente", ]

# Creamos algunas variables auxiliares
actas$actas_partidos <- actas$CC + actas$FPV + actas$MTS + actas$UCS + actas$`MAS - IPSP` + actas$`21F` + actas$PDC + actas$MNR + actas$`PAN-BOL`
actas$error <- ifelse(actas$actas_partidos != actas$`Votos V�lidos`, 1, 0)

# En todas las actas con error, si sobran votos se los quitamos al MAS y si faltan votos se los asignamos a CC
actas$mas_corregido <- ifelse(actas$actas_partidos > actas$`Votos V�lidos`, actas$`MAS - IPSP` - (actas$actas_partidos - actas$`Votos V�lidos`), actas$`MAS - IPSP`)
actas$cc_corregido <- ifelse(actas$actas_partidos < actas$`Votos V�lidos`, actas$CC + (actas$`Votos V�lidos` - actas$actas_partidos), actas$CC)

# Nueva variable de n�mero de votos v�lidos con las "correcciones" y verificaci�n de que ya no hay diferencias con el n�mero de votos v�lidos
actas$actas_partidos_corregido <- actas$cc_corregido + actas$FPV + actas$MTS + actas$UCS + actas$mas_corregido + actas$`21F` + actas$PDC + actas$MNR + actas$`PAN-BOL`
sum(actas$actas_partidos_corregido != actas$`Votos V�lidos`) # Suma a cero con diferencias

# Resultados del experimento
sum(actas$mas_corregido) / sum(actas$`Votos V�lidos`) # porcentaje hipot�tico de votos para el MAS
sum(actas$cc_corregido) / sum(actas$`Votos V�lidos`) # porcentaje hipot�tico de votos para CC
