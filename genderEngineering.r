require(ggplot2)
require(dplyr)

setwd('~/Github/IEEE-Transactions-on-Education/respuestas/')

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

# Adding new column to identofy the course
responsesESOZV$Course <- "Compulsory secondary ed."
responsesBachZV$Course <- "Upper secondary ed."
responsesCFZV$Course <- "Vocational courses"
responsesBachLM$Course <- "Upper secondary ed. LM"

# All data together
allResponses <- rbind(responsesESOZV, responsesBachZV, responsesCFZV, responsesBachLM)

# Analysing opinions about engineers
opinionsESOZV <- melt(responsesESOZV,id.vars=names(responsesESOZV)[2],measure.vars = names(responsesESOZV)[18:23])
opinionsBachZV <- melt(responsesBachZV,id.vars=names(responsesBachZV)[2],measure.vars = names(responsesBachZV)[18:23])
opinionsCFZV <- melt(responsesCFZV,id.vars=names(responsesCFZV)[2],measure.vars = names(responsesCFZV)[18:23])
opinionsBachLM <- melt(responsesBachLM,id.vars=names(responsesBachLM)[2],measure.vars = names(responsesBachLM)[18:23])
allOpinions <- melt(allResponses,id.vars=names(allResponses)[c(2,43)],measure.vars = names(allResponses)[18:23])

# Cleaning
opinionsESOZV <- opinionsESOZV[!is.na(opinionsESOZV$Girl),]
opinionsESOZV <- opinionsESOZV[!is.na(opinionsESOZV$variable),]
opinionsESOZV <- opinionsESOZV[!is.na(opinionsESOZV$value),]
opinionsBachZV <- opinionsESOZV[!is.na(opinionsBachZV$Girl),]
opinionsBachZV <- opinionsESOZV[!is.na(opinionsBachZV$variable),]
opinionsBachZV <- opinionsESOZV[!is.na(opinionsBachZV$value),]
opinionsCFZV <- opinionsESOZV[!is.na(opinionsCFZV$Girl),]
opinionsCFZV <- opinionsESOZV[!is.na(opinionsCFZV$variable),]
opinionsCFZV <- opinionsESOZV[!is.na(opinionsCFZV$value),]
opinionsBachLM <- opinionsESOZV[!is.na(opinionsBachLM$Girl),]
opinionsBachLM <- opinionsESOZV[!is.na(opinionsBachLM$variable),]
opinionsBachLM <- opinionsESOZV[!is.na(opinionsBachLM$value),]
allOpinions <- allOpinions[!is.na(allOpinions$Girl),]
allOpinions <- allOpinions[!is.na(allOpinions$variable),]
allOpinions <- allOpinions[!is.na(allOpinions$value),]

# Variables and Values into factors
opinionsESOZV$value <- factor(opinionsESOZV$value,levels = c("Sí","No","No lo sé"))
opinionsESOZV$variable <- factor(opinionsESOZV$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
opinionsBachZV$value <- factor(opinionsBachZV$value,levels = c("Sí","No","No lo sé"))
opinionsBachZV$variable <- factor(opinionsBachZV$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
opinionsCFZV$value <- factor(opinionsCFZV$value,levels = c("Sí","No","No lo sé"))
opinionsCFZV$variable <- factor(opinionsCFZV$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
opinionsBachLM$value <- factor(opinionsBachLM$value,levels = c("Sí","No","No lo sé"))
opinionsBachLM$variable <- factor(opinionsBachLM$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
allOpinions$value <- factor(allOpinions$value,levels = c("Sí","No","No lo sé"))
allOpinions$variable <- factor(allOpinions$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
allOpinions$Course <- factor(allOpinions$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses","Upper secondary ed. LM"))

# Opinion Graphs
graphESOZV <- ggplot(opinionsESOZV[!is.na(opinionsESOZV$variable),],aes(x=value,y=..density..,group=Girl,fill=Girl))+ stat_density() + facet_grid(. ~ variable, scales = "free_y")
graphBachZV <- ggplot(opinionsBachZV[!is.na(opinionsBachZV$variable),],aes(x=value,y=..density..,group=Girl,fill=Girl))+ stat_density() + facet_grid(. ~ variable, scales = "free_y")
graphCFZV <- ggplot(opinionsCFZV[!is.na(opinionsCFZV$variable),],aes(x=value,y=..density..,group=Girl,fill=Girl))+ stat_density() + facet_grid(. ~ variable, scales = "free_y")
graphBachLM <- ggplot(opinionsBachLM[!is.na(opinionsBachZV$variable),],aes(x=value,y=..density..,group=Girl,fill=Girl))+ stat_density() + facet_grid(. ~ variable, scales = "free_y")
graphAll <- ggplot(allOpinions[!is.na(allOpinions$variable),],aes(x=value,y=..density..,group=Girl,fill=Girl))+ stat_density() + facet_grid(Course ~ variable, scales = "free_y")
print(graphESOZV)
print(graphBachZV)
print(graphCFZV)
print(graphBachLM)
print(graphAll)
