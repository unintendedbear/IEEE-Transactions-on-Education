require(ggplot2)

# Reading responses from Z-V
responsesESOZV_csv <- "Respuestas - ESO_processed.csv"
responsesBachZV_csv <- "Respuestas - Bachillerato_processed.csv"
responsesCFZV_csv <- "Respuestas - Ciclos Formativos_processed.csv"

responsesESOZV <- read.table(file = responsesESOZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
responsesBachZV <- read.table(file = responsesBachZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
responsesCFZV <- read.table(file = responsesCFZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))

# Reading responses from La Madraza
responsesBachLM_csv <- "Respuestas - La Madraza Bachillerato_processed.csv"

responsesBachLM <- read.table(file = responsesBachLM_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))

#Bar chart
print(ggplot(data=subset(responsesESOZV, !is.na(Social_acceptance)), aes(Social_acceptance) ) +
        geom_bar() + facet_grid(Girl ~ .))
#Dot chart
print(ggplot(data=subset(responsesESOZV, !is.na(Social_acceptance)), aes(Social_acceptance, ..count.. ) ) +
        geom_point(stat = "count", size = 3) + coord_flip() +
        facet_grid(Girl ~ .))
