setwd('~/Github/IEEE-Transactions-on-Education/')

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
responsesBachZV <- rbind(responsesBachZV, responsesBachLM)

# Copying responsesBach and responsesESO to work with them later on
responsesBach_aux <- responsesBachZV
responsesESO_aux <- responsesESOZV

# Adding new column to identify the course
responsesESOZV$Course <- "Compulsory secondary ed."
responsesBachZV$Course <- "Upper secondary ed."
responsesCFZV$Course <- "Vocational courses"

# All data together
allResponses <- rbind(responsesESOZV, responsesBachZV, responsesCFZV)

# Adding columns to group feelings about engineering
allResponses$I_am_capable <- "Neutral"
allResponses[allResponses$Eng_im_capable == 1 & allResponses$Eng_im_not_capable == 0,"I_am_capable"] <- "Agree"
allResponses[allResponses$Eng_im_capable == 0 & allResponses$Eng_im_not_capable == 1,"I_am_capable"] <- "Disagree"
allResponses$Eng_Good_Degree_Choice <- "Neutral"
allResponses[(allResponses$Eng_easy_access +
                allResponses$Eng_easy_study +
                allResponses$Eng_opportunities_good +
                allResponses$Eng_job_fast) >
               (allResponses$Eng_not_easy_access +
                  allResponses$Eng_not_easy_study +
                  allResponses$Eng_opportunities_bad +
                  allResponses$Eng_job_not_fast),"Eng_Good_Degree_Choice"] <- "Agree"
allResponses[(allResponses$Eng_easy_access +
                allResponses$Eng_easy_study +
                allResponses$Eng_opportunities_good +
                allResponses$Eng_job_fast) <
               (allResponses$Eng_not_easy_access +
                  allResponses$Eng_not_easy_study +
                  allResponses$Eng_opportunities_bad +
                  allResponses$Eng_job_not_fast),"Eng_Good_Degree_Choice"] <- "Disagree"
allResponses$Eng_is_for_men <- "Neutral"
allResponses[allResponses$Eng_for_men_good == 0 &
               allResponses$Eng_for_women_good == 0 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 0,"Eng_is_for_men"] <- "Agree"
allResponses[allResponses$Eng_for_men_good == 1 &
               allResponses$Eng_for_women_good == 0 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 0,"Eng_is_for_men"] <- "Agree"
allResponses[allResponses$Eng_for_men_good == 0 &
               allResponses$Eng_for_women_good == 1 &
               allResponses$Eng_for_men_bad == 0 &
               allResponses$Eng_for_women_bad == 0,"Eng_is_for_men"] <- "Disagree"
allResponses[allResponses$Eng_for_men_good == 0 &
               allResponses$Eng_for_women_good == 0 &
               allResponses$Eng_for_men_bad == 0 &
               allResponses$Eng_for_women_bad == 1,"Eng_is_for_men"] <- "Disagree"
allResponses[allResponses$Eng_for_men_good == 1 &
               allResponses$Eng_for_women_good == 1 &
               allResponses$Eng_for_men_bad == 0 &
               allResponses$Eng_for_women_bad == 0,"Eng_is_for_men"] <- "Disagree"
allResponses[allResponses$Eng_for_men_good == 0 &
               allResponses$Eng_for_women_good == 0 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 1,"Eng_is_for_men"] <- "Disagree"
allResponses[allResponses$Eng_for_men_good == 1 &
               allResponses$Eng_for_women_good == 1 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 1,"Eng_is_for_men"] <- "Disagree"
allResponses$Eng_is_for_Geeks <- "Neutral"
allResponses[allResponses$Eng_for_geeks_good == 1, "Eng_is_for_Geeks"] <- "Agree"
allResponses[allResponses$Eng_for_geeks_bad == 1, "Eng_is_for_Geeks"] <- "Agree"

# Adding columns to group choices about future studies
responsesBach_aux <- responsesBach_aux[!is.na(responsesBach_aux$Future_studies),] # Cleaning, first
responsesBach_aux$Immediate_future_plans <- "Work/Others"
responsesBach_aux[responsesBach_aux$Future_studies == unique(responsesBach_aux$Future_studies)[1],"Immediate_future_plans"] <- "Tech related studies"
responsesBach_aux[responsesBach_aux$Future_studies == unique(responsesBach_aux$Future_studies)[5],"Immediate_future_plans"] <- "Tech related studies"
responsesBach_aux[responsesBach_aux$Future_studies == unique(responsesBach_aux$Future_studies)[2],"Immediate_future_plans"] <- "Other studies (not tech)"
responsesBach_aux[responsesBach_aux$Future_studies == unique(responsesBach_aux$Future_studies)[3],"Immediate_future_plans"] <- "Other studies (not tech)"

# Grouping and translating STEM courses scoring
responsesBach_aux <- responsesBach_aux[!is.na(responsesBach_aux$Maths),]
responsesBach_aux <- responsesBach_aux[!is.na(responsesBach_aux$Physics),]
responsesBach_aux <- responsesBach_aux[!is.na(responsesBach_aux$Tech),]
responsesBach_aux <- responsesBach_aux[!is.na(responsesBach_aux$Computer_science),] # Cleaning, first
levels(responsesBach_aux$Maths) <- c(levels(responsesBach_aux$Maths), "Fail", "Average", "A+")
levels(responsesBach_aux$Physics) <- c(levels(responsesBach_aux$Physics), "Fail", "Average", "A+")
levels(responsesBach_aux$Tech) <- c(levels(responsesBach_aux$Tech), "Fail", "Average", "A+")
levels(responsesBach_aux$Computer_science) <- c(levels(responsesBach_aux$Computer_science), "Fail", "Average", "A+")
responsesBach_aux$Maths[responsesBach_aux$Maths == "Suspenso"] <- "Fail"
responsesBach_aux$Maths[responsesBach_aux$Maths == "Aprobado bajo" | responsesBach_aux$Maths == "Aprobado alto"] <- "Average"
responsesBach_aux$Maths[responsesBach_aux$Maths == "Notable" | responsesBach_aux$Maths == "Sobresaliente"] <- "A+"
responsesBach_aux$Physics[responsesBach_aux$Physics == "Suspenso"] <- "Fail"
responsesBach_aux$Physics[responsesBach_aux$Physics == "Aprobado bajo" | responsesBach_aux$Physics == "Aprobado alto"] <- "Average"
responsesBach_aux$Physics[responsesBach_aux$Physics == "Notable" | responsesBach_aux$Physics == "Sobresaliente"] <- "A+"
responsesBach_aux$Tech[responsesBach_aux$Tech == "Suspenso"] <- "Fail"
responsesBach_aux$Tech[responsesBach_aux$Tech == "Aprobado bajo" | responsesBach_aux$Tech == "Aprobado alto"] <- "Average"
responsesBach_aux$Tech[responsesBach_aux$Tech == "Notable" | responsesBach_aux$Tech == "Sobresaliente"] <- "A+"
responsesBach_aux$Computer_science[responsesBach_aux$Computer_science == "Suspenso"] <- "Fail"
responsesBach_aux$Computer_science[responsesBach_aux$Computer_science == "Aprobado bajo" | responsesBach_aux$Computer_science == "Aprobado alto"] <- "Average"
responsesBach_aux$Computer_science[responsesBach_aux$Computer_science == "Notable" | responsesBach_aux$Computer_science == "Sobresaliente"] <- "A+"

# Analysing opinions about engineers
allOpinions <- melt(allResponses,id.vars=names(allResponses)[c(2,43)],measure.vars = names(allResponses)[18:23])
# Analysing opinions about engineering
allOpinionsEng <- melt(allResponses,id.vars=names(allResponses)[c(2,43)],measure.vars = names(allResponses)[44:47])
# All future choices in Bachillerato (upper secondary education)
futureChoiceBach <- melt(responsesBach_aux, id.vars = names(responsesBach_aux)[c(2,43)], measure.vars = names(responsesBach_aux)[9:12])

# Cleaning
allOpinions <- allOpinions[!is.na(allOpinions$Girl),]
allOpinions <- allOpinions[!is.na(allOpinions$variable),]
allOpinions <- allOpinions[!is.na(allOpinions$value),]
allOpinionsEng <- allOpinionsEng[!is.na(allOpinionsEng$Girl),]
allOpinionsEng <- allOpinionsEng[!is.na(allOpinionsEng$variable),]
allOpinionsEng <- allOpinionsEng[!is.na(allOpinionsEng$value),]

# Translating answers
allOpinions[allOpinions$value == unique(allOpinions$value)[1],"value"] <- "Agree"
allOpinions[allOpinions$value == unique(allOpinions$value)[2],"value"] <- "Neutral"
allOpinions[allOpinions$value == unique(allOpinions$value)[3],"value"] <- "Disagree"
allOpinions$Gender <- "None"
allOpinions[allOpinions$Girl == unique(allOpinions$Girl)[1],"Gender"] <- "Male"
allOpinions[allOpinions$Girl == unique(allOpinions$Girl)[2],"Gender"] <- "Female"
allOpinionsEng$Gender <- "None"
allOpinionsEng[allOpinionsEng$Girl == unique(allOpinionsEng$Girl)[1],"Gender"] <- "Male"
allOpinionsEng[allOpinionsEng$Girl == unique(allOpinionsEng$Girl)[2],"Gender"] <- "Female"
futureChoiceBach$Gender <- "None"
futureChoiceBach[futureChoiceBach$Girl == unique(futureChoiceBach$Girl)[1],"Gender"] <- "Male"
futureChoiceBach[futureChoiceBach$Girl == unique(futureChoiceBach$Girl)[2],"Gender"] <- "Female"

# Variables and Values into factors
allOpinions$value <- factor(allOpinions$value,levels = c("Agree","Neutral", "Disagree"))
allOpinions$variable <- factor(allOpinions$variable,levels = c("Social_acceptance","Wealth","Creative_job","Easy_job","Good_schedule", "Job_impact"))
allOpinions$Course <- factor(allOpinions$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses"))
allOpinionsEng$value <- factor(allOpinionsEng$value,levels = c("Agree","Neutral", "Disagree"))
allOpinionsEng$variable <- factor(allOpinionsEng$variable,levels = c("Eng_Good_Degree_Choice","I_am_capable","Eng_is_for_men","Eng_is_for_Geeks"))
allOpinionsEng$Course <- factor(allOpinionsEng$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses"))

# Opinion Graphs
graphAll <- ggplot(allOpinions,aes(x=value,y=..density..,group=Gender,fill=Gender)) +
  stat_density() +
  facet_grid(Course ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Density") +
  ggtitle("Do you agree with these statements about engineers?")

graphAll2 <- ggplot(allOpinionsEng[allOpinionsEng$value != "Neutral",],aes(x=value,y=..count..,group=Gender,fill=Gender)) +
  geom_bar() +
  facet_grid(Course ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Density") +
  ggtitle("Feelings about studying an engineering")

graphFuture <- ggplot(futureChoiceBach,aes(x=value,y=..density..,group=Gender,fill=Gender)) +
  stat_density() +
  facet_grid(Immediate_future_plans ~ variable, scales = "free_y") +
  xlab("Scoring") +
  ylab("Density") +
  ggtitle("What will students do in the future vs. their scoring in STEM")

print(graphAll)
print(graphAll2)
print(graphFuture)
ggsave("engineer_opinions.png", plot = graphAll, scale = 1.75)
ggsave("engineering_opinions.png", plot = graphAll2, scale = 1.75)
ggsave("future_vs_scoringSTEM.png", plot = graphAll2, scale = 1.5)
