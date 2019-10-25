# ANÁLISIS DE GRADO DE INFLUENCIA DE ERRORES EN DATOS DE TSE 
# Autor: José Miguel Molina Fernánez
# Fecha: 24/10/2019

# Versión de la base de datos de cómputo de TSE: acta.2019.10.24.20.01.43

# Importamos librerías necesarias
library(readxl)

# TODO: Importar la base de datos con el nombre "actas"

# Filtramos solo los votos para Presidente y Vicepresidente
actas <- actas[actas$Elección=="Presidente y Vicepresidente", ]

# Creamos algunas variables auxiliares
actas$actas_partidos <- actas$CC + actas$FPV + actas$MTS + actas$UCS + actas$`MAS - IPSP` + actas$`21F` + actas$PDC + actas$MNR + actas$`PAN-BOL`
actas$error <- ifelse(actas$actas_partidos != actas$`Votos Válidos`, 1, 0)

# En todas las actas con error, si sobran votos se los quitamos al MAS y si faltan votos se los asignamos a CC
actas$mas_corregido <- ifelse(actas$actas_partidos > actas$`Votos Válidos`, actas$`MAS - IPSP` - (actas$actas_partidos - actas$`Votos Válidos`), actas$`MAS - IPSP`)
actas$cc_corregido <- ifelse(actas$actas_partidos < actas$`Votos Válidos`, actas$CC + (actas$`Votos Válidos` - actas$actas_partidos), actas$CC)

# Nueva variable de número de votos válidos con las "correcciones" y verificación de que ya no hay diferencias con el número de votos válidos
actas$actas_partidos_corregido <- actas$cc_corregido + actas$FPV + actas$MTS + actas$UCS + actas$mas_corregido + actas$`21F` + actas$PDC + actas$MNR + actas$`PAN-BOL`
sum(actas$actas_partidos_corregido != actas$`Votos Válidos`) # Suma a cero con diferencias

# Resultados del experimento
sum(actas$mas_corregido) / sum(actas$`Votos Válidos`) # porcentaje hipotético de votos para el MAS
sum(actas$cc_corregido) / sum(actas$`Votos Válidos`) # porcentaje hipotético de votos para CC
