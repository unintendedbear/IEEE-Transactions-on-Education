# Reading responses from Z-V
responsesESOZV_csv <- "/home/osica/Github/IEEE-Transactions-on-Education/Respuestas - ESO.csv"
responsesBachZV_csv <- "/home/osica/Github/IEEE-Transactions-on-Education/Respuestas - Bachillerato.csv"
responsesCFZV_csv <- "/home/osica/Github/IEEE-Transactions-on-Education/Respuestas - Ciclos Formativos.csv"

responsesESOZV <- read.table(file = responsesESOZV_csv, header = TRUE, sep = ",")
responsesBachZV <- read.table(file = responsesBachZV_csv, header = TRUE, sep = ",")
responsesCFZV <- read.table(file = responsesCFZV_csv, header = TRUE, sep = ",")

# Reading responses from La Madraza
responsesBachLM_csv <- "/home/osica/Github/IEEE-Transactions-on-Education/Respuestas - La Madraza Bachillerato.csv"

responsesBachLM <- read.table(file = responsesBachLM_csv, header = TRUE, sep = ",")

theFuture <- factor(ifelse(responsesESOZV$Futuro == "Bachillerato de ciencias", "Yes", "No"))
class(theFuture)
plot(responsesESOZV$Mujer, theFuture)

