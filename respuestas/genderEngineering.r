setwd('~/Github/IEEE-Transactions-on-Education/respuestas/')

require(ggplot2)
require(dplyr)
require(reshape2)
require(xtable)

# Reading responses from Z-V
responsesESOZV_csv <- "Respuestas - ESO_processed.csv"
responsesBachZV_csv <- "Respuestas - Bachillerato_processed.csv"
responsesCFZV_csv <- "Respuestas - Ciclos Formativos_processed.csv"
enrolled_csv <- "Matriculas.csv"
responsesESOZV <- read.table(file = responsesESOZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
responsesBachZV <- read.table(file = responsesBachZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
responsesCFZV <- read.table(file = responsesCFZV_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
enrolled <- read.table(file = enrolled_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))

# Reading responses from La Madraza
#responsesBachLM_csv <- "Respuestas - La Madraza Bachillerato_processed.csv"
#responsesBachLM <- read.table(file = responsesBachLM_csv, header = TRUE, sep = ",", na.strings=c(""," ","NA"))
#responsesBachZV <- rbind(responsesBachZV, responsesBachLM)

# Adding new column to identify the course
responsesESOZV$Course <- "Compulsory secondary ed."
responsesBachZV$Course <- "Upper secondary ed."
responsesCFZV$Course <- "Vocational courses"
enrolled$Type <- factor("Enrolled", levels = c("Enrolled", "Surveyed"))

# All data together
allResponses <- rbind(responsesESOZV, responsesBachZV, responsesCFZV)
# Translating "Girl" column into "Gender" column
allResponses <- allResponses[!is.na(allResponses$Girl),]
allResponses$Gender <- "None"
allResponses[allResponses$Girl == unique(allResponses$Girl)[1],"Gender"] <- "Female"
allResponses[allResponses$Girl == unique(allResponses$Girl)[2],"Gender"] <- "Male"
# Translating more answers
levels(allResponses$Engineering) <- c(levels(allResponses$Engineering), "Yes")
allResponses$Engineering[allResponses$Engineering == "Sí"] <- "Yes"
allResponses$Engineering <- factor(allResponses$Engineering, levels = c("Yes", "No"))
allResponses <- allResponses[!is.na(allResponses$Class),]
levels(allResponses$Class) <- c(levels(allResponses$Class), "Equity or mainly female", "Mainly male")
allResponses[allResponses$Class == "Más chicas que chicos" | allResponses$Class == "Todo chicas", "Class"] <- "Equity or mainly female"
allResponses[allResponses$Class == "Más o menos igual", "Class"] <- "Equity or mainly female"
allResponses[allResponses$Class == "Más chicos que chicas" | allResponses$Class == "Todo chicos", "Class"] <- "Mainly male"
allResponses$Class <- factor(allResponses$Class, levels = c("Equity or mainly female", "Mainly male"))
allResponses <- allResponses[!is.na(allResponses$Professors),]
levels(allResponses$Professors) <- c(levels(allResponses$Professors), "Equity or mainly female", "Mainly male")
allResponses[allResponses$Professors == "Más mujeres que hombres" | allResponses$Professors == "Todo mujeres", "Professors"] <- "Equity or mainly female"
allResponses[allResponses$Professors == "Más o menos igual", "Professors"] <- "Equity or mainly female"
allResponses[allResponses$Professors == "Más hombres que mujeres" | allResponses$Professors == "Todo hombres", "Professors"] <- "Mainly male"
allResponses$Professors <- factor(allResponses$Professors, levels = c("Equity or mainly female", "Mainly male"))

surveyed <- allResponses[,c(43, 44)]
surveyed$Course <- factor(surveyed$Course, levels = unique(surveyed$Course))
surveyed$Gender <- factor(surveyed$Gender, levels = unique(surveyed$Gender))
surveyed$Type <- factor("Surveyed", levels = c("Enrolled", "Surveyed"))
allStudents <- rbind(enrolled, surveyed)

# Gender distribution
graphGender <- ggplot(allStudents, aes(Course)) +
  geom_bar(aes(fill = Gender), position = "fill") +
  facet_grid(. ~ Type) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  xlab("Course") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom", axis.text.x = element_text(size=7)) +
  ggtitle("Gender distribution among the courses")
print(graphGender)
ggsave("../img/gender_distribution.pdf", plot = graphGender, scale = 2.25,units = "mm", height = 70, width = 90, dpi = 600)

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
               allResponses$Eng_for_women_bad == 0,"Eng_is_for_men"] <- "Neutral"
allResponses[allResponses$Eng_for_men_good == 0 &
               allResponses$Eng_for_women_good == 0 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 1,"Eng_is_for_men"] <- "Neutral"
allResponses[allResponses$Eng_for_men_good == 1 &
               allResponses$Eng_for_women_good == 1 &
               allResponses$Eng_for_men_bad == 1 &
               allResponses$Eng_for_women_bad == 1,"Eng_is_for_men"] <- "Neutral"
allResponses$Eng_is_for_Geeks <- "Disagree"
allResponses[allResponses$Eng_for_geeks_good == 1, "Eng_is_for_Geeks"] <- "Agree"
allResponses[allResponses$Eng_for_geeks_bad == 1, "Eng_is_for_Geeks"] <- "Agree"

# Analysing opinions about engineers
allOpinions <- melt(allResponses,id.vars=names(allResponses)[c(44,43)],measure.vars = names(allResponses)[c(18,19,21,22,23,46)])
# Analysing opinions about engineering
allOpinionsEng <- melt(allResponses,id.vars=names(allResponses)[c(44,43)],measure.vars = names(allResponses)[45:48])

# Cleaning
allOpinions <- allOpinions[!is.na(allOpinions$variable),]
allOpinions <- allOpinions[!is.na(allOpinions$value),]
allOpinionsEng <- allOpinionsEng[!is.na(allOpinionsEng$variable),]
allOpinionsEng <- allOpinionsEng[!is.na(allOpinionsEng$value),]

# Translating answers
allOpinions[allOpinions$value == unique(allOpinions$value)[1],"value"] <- "Agree"
allOpinions[allOpinions$value == unique(allOpinions$value)[2],"value"] <- "Neutral"
allOpinions[allOpinions$value == unique(allOpinions$value)[3],"value"] <- "Disagree"

# Variables and Values into factors
allOpinions$value <- factor(allOpinions$value,levels = c("Agree","Neutral", "Disagree"))
allOpinions$variable <- factor(allOpinions$variable,levels = c("Social_acceptance","Wealth","Easy_job","Good_schedule","Job_impact", "Eng_Good_Degree_Choice"))
allOpinions$Course <- factor(allOpinions$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses"))
allOpinionsEng$value <- factor(allOpinionsEng$value,levels = c("Agree","Neutral", "Disagree"))
allOpinionsEng$variable <- factor(allOpinionsEng$variable,levels = c("Eng_Good_Degree_Choice","I_am_capable","Eng_is_for_men","Eng_is_for_Geeks"))
allOpinionsEng$Course <- factor(allOpinionsEng$Course,levels = c("Compulsory secondary ed.","Upper secondary ed.","Vocational courses"))

# Opinion Graphs
graphAll <- ggplot(allOpinions[allOpinions$variable != "Social_acceptance",],aes(x=Gender,y=..count..,group=value,fill=value)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  facet_grid(Course ~ variable, scales = "free_y") +
  xlab("Gender") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Do you agree with these statements about engineers?") +
  theme(legend.position = "bottom", strip.text = element_text(size=7))

graphAll2 <- ggplot(allOpinionsEng,aes(x=value,y=..count..,group=Gender,fill=Gender)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  facet_grid(Course ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Feelings about studying an engineering")

print(graphAll)
ggsave("../img/engineer_opinions.pdf", plot = graphAll, scale = 2,units = "mm", height = 80, width = 90, dpi = 600 )
ggsave("engineering_opinions.png", plot = graphAll2)

# Adding columns to group choices about future studies
allResponses <- allResponses[!is.na(allResponses$Future_studies),] # Cleaning, first
allResponses$Immediate_future_plans <- "Work/Others"
allResponses[allResponses$Future_studies == unique(allResponses$Future_studies)[1] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[5] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[8] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[9] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[11],"Immediate_future_plans"] <- "Tech related studies"
allResponses[allResponses$Future_studies == unique(allResponses$Future_studies)[3] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[4] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[6] |
               allResponses$Future_studies == unique(allResponses$Future_studies)[7],"Immediate_future_plans"] <- "Other studies (not tech)"

# All future choices in ESO (compulsory secondary education)
futureChoiceESO <- melt(allResponses[allResponses$Course == "Compulsory secondary ed.",], id.vars = names(allResponses)[c(44,49)], measure.vars = names(allResponses)[c(20,45,47,48)])
futureChoiceESO_alt <- melt(allResponses[allResponses$Course == "Compulsory secondary ed.",], id.vars = names(allResponses)[c(44,24)], measure.vars = names(allResponses)[c(20,45,47,48)])

# Translating answers
futureChoiceESO[futureChoiceESO$value == unique(futureChoiceESO$value)[1],"value"] <- "Agree"
futureChoiceESO[futureChoiceESO$value == unique(futureChoiceESO$value)[2],"value"] <- "Neutral"
futureChoiceESO[futureChoiceESO$value == unique(futureChoiceESO$value)[3],"value"] <- "Disagree"
futureChoiceESO_alt[futureChoiceESO_alt$value == unique(futureChoiceESO_alt$value)[1],"value"] <- "Agree"
futureChoiceESO_alt[futureChoiceESO_alt$value == unique(futureChoiceESO_alt$value)[2],"value"] <- "Neutral"
futureChoiceESO_alt[futureChoiceESO_alt$value == unique(futureChoiceESO_alt$value)[3],"value"] <- "Disagree"

# Variables and Values into factors
#futureChoiceESO$value <- factor(futureChoiceESO$value,levels = c("Agree","Neutral", "Disagree"))
futureChoiceESO_alt$value <- factor(futureChoiceESO_alt$value,levels = c("Agree","Neutral", "Disagree"))

# Graphs comparing future choice vs scoring and opinions
graphFuturevsOpinion <- ggplot(futureChoiceESO,aes(x=value,y=..count..,group=Gender,fill=Gender)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(Immediate_future_plans ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("What will students do in the future vs. their opinions")
print(graphFuturevsOpinion)

graphFuturevsOpinion_alt <- ggplot(futureChoiceESO_alt,aes(x=Gender,y=..count..,group=value,fill=value)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  facet_grid(Engineering ~ variable, scales = "free_y") +
  xlab("Opinion") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("What will students do in the future vs. their opinions") + theme(legend.position = "bottom")
print(graphFuturevsOpinion_alt)
ggsave("future_vs_opinion.png", plot = graphFuturevsOpinion_alt)

# Grouping and translating STEM courses scoring
#allResponses <- allResponses[!is.na(allResponses$Maths),]
#allResponses <- allResponses[!is.na(allResponses$Physics),]
#allResponses <- allResponses[!is.na(allResponses$Tech),]
#allResponses <- allResponses[!is.na(allResponses$Computer_science),] # Cleaning, first
levels(allResponses$Maths) <- c(levels(allResponses$Maths), "Fail", "Average", "A+")
levels(allResponses$Physics) <- c(levels(allResponses$Physics), "Fail", "Average", "A+")
levels(allResponses$Tech) <- c(levels(allResponses$Tech), "Fail", "Average", "A+")
levels(allResponses$Computer_science) <- c(levels(allResponses$Computer_science), "Fail", "Average", "A+")
allResponses$Maths[allResponses$Maths == "Suspenso"] <- "Fail"
allResponses$Maths[allResponses$Maths == "Aprobado bajo" | allResponses$Maths == "Aprobado alto"] <- "Average"
allResponses$Maths[allResponses$Maths == "Notable" | allResponses$Maths == "Sobresaliente"] <- "A+"
allResponses$Physics[allResponses$Physics == "Suspenso"] <- "Fail"
allResponses$Physics[allResponses$Physics == "Aprobado bajo" | allResponses$Physics == "Aprobado alto"] <- "Average"
allResponses$Physics[allResponses$Physics == "Notable" | allResponses$Physics == "Sobresaliente"] <- "A+"
allResponses$Tech[allResponses$Tech == "Suspenso"] <- "Fail"
allResponses$Tech[allResponses$Tech == "Aprobado bajo" | allResponses$Tech == "Aprobado alto"] <- "Average"
allResponses$Tech[allResponses$Tech == "Notable" | allResponses$Tech == "Sobresaliente"] <- "A+"
allResponses$Computer_science[allResponses$Computer_science == "Suspenso"] <- "Fail"
allResponses$Computer_science[allResponses$Computer_science == "Aprobado bajo" | allResponses$Computer_science == "Aprobado alto"] <- "Average"
allResponses$Computer_science[allResponses$Computer_science == "Notable" | allResponses$Computer_science == "Sobresaliente"] <- "A+"

# All future choices in Bachillerato (upper secondary education)
#futureChoiceBach <- melt(allResponses[allResponses$Course == "Upper secondary ed.",], id.vars = names(allResponses)[c(44,49)], measure.vars = names(allResponses)[9:12])
futureChoiceBach_alt <- melt(allResponses, id.vars = names(allResponses)[c(43,44,24)], measure.vars = names(allResponses)[9:12])
futureChoiceBach_alt <- melt(allResponses, id.vars = names(allResponses)[c(43,44,24)], measure.vars = names(allResponses)[c(20,45,47,48)])
futureChoiceBach_alt <- futureChoiceBach_alt[!is.na(futureChoiceBach_alt$Engineering),]

# Graphs comparing future choice vs scoring and opinions
graphFuture <- ggplot(futureChoiceBach,aes(x=value,y=..count..,group=Gender,fill=Gender)) +
  #geom_density() +
  geom_bar(position = "fill") +
  #scale_fill_brewer(palette="Set1",direction = -1) +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(Immediate_future_plans ~ variable, scales = "free_y") +
  xlab("Scoring") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("What will students do in the future vs. their scoring in STEM")

graphFuture_alt <- ggplot(futureChoiceBach_alt,aes(x=Gender,y=..count..,group=value,fill=value)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Score") +
  facet_grid(Engineering ~ variable, scales = "free_y") +
  xlab("Percentage") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Whether the students will choose an engineering or not vs. their scoring in STEM") + 
  theme(legend.position = "bottom")

futureChoiceBach_alt$temp <- paste(futureChoiceBach_alt$variable,futureChoiceBach_alt$Engineering,sep = " - ")

graphFuture_alt <- ggplot(futureChoiceBach_alt,aes(x=Gender,y=..count..,group=value,fill=value)) +
  geom_bar(stat="count",position = "fill") + #geom_text(aes(label = ..count..), stat= "count") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Score") +
  facet_wrap(~ Course+temp,ncol=8) +
  xlab("Gender") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Whether the students will choose an engineering or not vs. their scoring in STEM") + 
  theme(legend.position = "bottom",strip.text = element_text(size=8))

print(graphFuture_alt)
#print(graphFuturevsOpinion)
ggsave("../img/future_vs_scoringSTEM.pdf", plot = graphFuture_alt,scale = 1.75,units = "mm", height = 100, width = 180, dpi = 600 )

influences <- melt(allResponses, id.vars = names(allResponses)[c(43,44,24)], measure.vars = names(allResponses)[c(3,7)])
influences <- influences[!is.na(influences$Engineering),]
influences$temp <- paste(influences$variable,influences$Engineering,sep = " - ")

graphInfluences <- ggplot(influences,aes(x=Gender,y=..count..,group=value,fill=value)) +
  geom_bar(stat="count",position = "fill") + #geom_text(aes(label = ..count..), stat= "count") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Score") +
  facet_wrap(Course ~ temp,ncol=4) +
  xlab("Gender") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Whether the students will choose an engineering or not vs. their environment") + 
  theme(legend.position = "bottom",strip.text = element_text(size=10))

print(graphInfluences)
ggsave("../img/future_vs_influence.pdf", plot = graphInfluences,scale = 2.25,units = "mm", height = 100, width = 90, dpi = 600 )
