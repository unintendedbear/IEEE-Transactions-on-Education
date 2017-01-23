setwd('~/Github/IEEE-Transactions-on-Education/respuestas/')

require(ggplot2)
require(dplyr)
require(reshape2)

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

# Adding new column to identify the course
responsesESOZV$Course <- "Compulsory secondary ed."
responsesBachZV$Course <- "Upper secondary ed."
responsesCFZV$Course <- "Vocational courses"
responsesBachLM$Course <- "Upper secondary ed. LM"

# All data together
allResponses <- rbind(responsesESOZV, responsesBachZV, responsesCFZV, responsesBachLM)

# Analysing opinions about engineers
allOpinions <- melt(allResponses,id.vars=names(allResponses)[c(2,43)],measure.vars = names(allResponses)[18:23])

# Cleaning
allOpinions <- allOpinions[!is.na(allOpinions$Girl),]
allOpinions <- allOpinions[!is.na(allOpinions$variable),]
allOpinions <- allOpinions[!is.na(allOpinions$value),]

# Translating answers
allOpinions[allOpinions$value == unique(allOpinions$value)[1],"value"] <- "Agree"
allOpinions[allOpinions$value == unique(allOpinions$value)[2],"value"] <- "Neutral"
allOpinions[allOpinions$value == unique(allOpinions$value)[3],"value"] <- "Disagree"
allOpinions$Gender <- "None"
allOpinions[allOpinions$Girl == unique(allOpinions$Girl)[1],"Gender"] <- "Male"
allOpinions[allOpinions$Girl == unique(allOpinions$Girl)[2],"Gender"] <- "Female"

# Variables and Values into factors
allOpinions$value <- factor(allOpinions$value,levels = c("Agree","Neutral", "Disagree"))
allOpinions$variable <- factor(allOpinions$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
allOpinions$Course <- factor(allOpinions$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses","Upper secondary ed. LM"))

# Opinion Graphs
graphAll <- ggplot(allOpinions,aes(x=value,y=..density..,group=Gender,fill=Gender)) +
  stat_density() +
  facet_grid(Course ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Density") +
  ggtitle("Do you agree with these statements about engineers?")
ggsave("engineer_opinions.png", plot = graphAll)