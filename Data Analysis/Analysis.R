library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggExtra)
library(gridExtra)
library(stringr)
library(scales)
library(dplyr)
library('cowplot') #not in use now
library(egg)
library(tidyr)
library(dplyr)
library(ggpattern)
library(treemapify)
library(rstan)
library(brms)
library(ggpubr)  
library(grid)
library(stringi)
library(car)

rm(list = ls())

switch(Sys.info()[['sysname']],
       Windows= {setwd("E:/OneDrive - Aalborg Universitet/PhD/Projects/2023 - CI Concert/Data Analysis")
                library('Cairo')},
       Linux  = {print("I'm a penguin, i don't know what to do.")},
       Darwin = {setwd("~/OneDrive - Aalborg Universitet/PhD/Projects/2023 - CI Concert/Data Analysis")})

load("Data Processing for March & October.RData")
#################################################################################
### Custom theme for publication modified from https://rpubs.com/Koundy/71792 ###
#################################################################################
theme_Publication <- function(base_size=14, base_family="helvetica") {
  
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.2, "cm"),
           legend.margin = unit(0, "cm"),
           legend.title = element_text(face="italic"),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
  
}

scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","tomato","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)}

scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","tomato","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)}


###############################
### Data curation functions ###
###############################
loadTable <- function(month)
{
  if (month == "March"){
    print("Loading March")
    Table <- read_excel("Concerts Cummulated.xlsx", 
                        sheet = "Voters March", 
                        col_types = c("skip", "skip","numeric","text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "numeric", "numeric"), n_max = 53)
    Table <- Table[-1,]; #Remove first row
    #Change collum names
    colnames(Table)[1] <- "Participant_ID" 
    colnames(Table)[2] <- "Hearing_Profile"
    colnames(Table)[52]<- "Age"
    colnames(Table)[53]<- "Yearly_Concerts"
    
    for(i in 3:(ncol(Table) - 3))
    {
      colnames(Table)[i] <- sapply(Table[i], function(x) gsub("[^0-9]*([0-9]{1,2}).*", "\\1", as.character(x[1])))
      Table[52,i] <- substr(Table[50,i], nchar(Table[50,i]), nchar(Table[50,i]))
      Table[50,i] <- gsub("^\\d+\\s+([[:alpha:]]+).*", "\\1", Table[50,i])
    }
    
    Table <- Table[-c(1, 15, 35), ] #Remove empty rows
    Table[47,2] <- "Contours" #change cell value
    Table[49,2] <- "Key" 
    
    Table$`Participant_ID` <- Table$`Participant_ID` - 19; #Normalize particiapant iDs
    
    colnames(Table)[3:50] <- paste0("Question_", 1:48) #Rename columns to Question_1...
    
    Table <- subset(Table, select = -51)
    return(Table)
  }
  else if (month == "October") {
    print("Loading October")
    Table <- read_excel("Concerts Cummulated.xlsx", 
                        sheet = "Voters October", 
                        col_types = c("skip", "skip","numeric","text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "numeric","numeric" ), n_max = 35)
    Table <- Table[-c(1, 2, 9, 11, 28, 30), ] #Remove empty rows
    #Change collum names
    colnames(Table)[1] <- "Participant_ID" 
    colnames(Table)[2] <- "Animal"
    colnames(Table)[40]<- "Age"
   # colnames(Table)[43]<- "Yearly_Concerts"
    colnames(Table)[3:38] <- paste0("Question_", 1:36) #Rename columns to Question_1...
    
    Table <- subset(Table, select = -39) #Exclude collumn 39(animal confirmation collumn)
    return(Table)
    }
  else{return(NULL)
  }
  
    #Check data types in each collumn
    #sapply(Table, mode)
}
loadDemographics <- function(Table, month)
{
  if (month == "March"){
    month = "Demographics March"
    Demographics <- read_excel("Concerts Cummulated.xlsx", 
                               sheet = month, 
                               col_types = c("text", "text","numeric","text", "numeric", 
                                             "numeric", "numeric", "text", "numeric", "text", 
                                             "numeric", "numeric"), n_max = 47)
    Demographics <- Demographics[-c(14,34, 47),]; # Remove last row since the "Ål" did not participate in the quiz
    Demographics$"Hearing profile" <- lapply(X = Demographics$"Hearing profile", FUN  = classifyHearingProfile);
    Demographics$`Hearing profile` <- as.character(Demographics$`Hearing profile`)
    colnames(Table)[4] <- "Hearing_Profile"
  }
  
  else if (month == "October"){
    month = "Demographics October"
    Demographics <- read_excel("Concerts Cummulated.xlsx", 
                               sheet = month, 
                               col_types = c("text", "text", "numeric", "text", "numeric", 
                                             "numeric", "text", "numeric", "text", "numeric", 
                                             "text", "numeric", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric", "numeric", "skip",
                                             "skip", "numeric", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric", "numeric", "numeric",
                                             "numeric", "numeric"), n_max = 47) #33 collumns
    
    order_in_table <- match(Table$Animal, Demographics$Animal)
    Demographics <- Demographics[order_in_table, ]
    
    Demographics <- Demographics[-c(27, 28, 29),]; # Remove last row since the "Ål" did not participate in the quiz
    Demographics$"Hearing profile" <- lapply(X = Demographics$"Hearing profile", FUN  = classifyHearingProfile);
    Demographics$`Hearing profile` <- as.character(Demographics$`Hearing profile`)
    colnames(Table)[4] <- "Hearing_Profile"
  }
    return(Demographics)
}
classifyHearingProfile <- function(profile)
{
    ifelse(grepl("^Normal$", profile), return("NH"), profile)
    ifelse(grepl("Two hearing aids", profile), return("DSHA"), profile)
    ifelse(grepl("Hearing aid.*CI|CI.*Hearing aid", profile), return("Bimodal"), profile)
    ifelse(grepl("One CI", profile), return("SSCI"), profile)
    ifelse(grepl("Hearing aid", profile), return("SSHA"), profile)
    ifelse(grepl("Two CIs", profile), return("Bilateral"), profile)
    ifelse(grepl("^Deaf$", profile), return("Deaf"), return("Unknown"))
}
combineData <- function(month, Table, Demographics)
{
  if (month == "March"){
    Table_long <- Table %>%
      gather(Question, Response, -1)
    Table_long <- Table_long[-c(1:49), ]
    Table_long <- Table_long[-c(2353:2401), ]
    
    n_rows <- nrow(Table_long)
    # Create a vector of row indices to remove
    rows_to_remove <- c( seq(45, n_rows, 49), seq(46, n_rows, 49),
                         seq(47, n_rows, 49), seq(48, n_rows, 49), seq(49, n_rows, 49))
    # Remove the rows from the dataset
    Table_long <- Table_long[-rows_to_remove, ]
    
    participantID <- head(Table[,1], 44)
    Demographics_long <- cbind(participantID, Demographics)

    colnames(Demographics_long)[1] <- "Participant_ID"
    colnames(Table_long)[1] <- "Participant_ID"
    
    merged <- Demographics_long %>%
      inner_join(Table_long, by = "Participant_ID")
    
    MelodyInfo <- t(Table[47:49,])
    colnames(MelodyInfo) <- c("Correct_Contours", "Instruments", "Keys")
    MelodyInfo <- as.data.frame(MelodyInfo)
    MelodyInfo <- MelodyInfo[-c(1, 2), ]
    MelodyInfo <- MelodyInfo[-c(nrow(MelodyInfo)),]
    MelodyInfo_repeated <- MelodyInfo[rep(1:nrow(MelodyInfo), 44), ]
    
    merged<- cbind(merged, MelodyInfo_repeated)
    
    return(merged)
  }
  
  else if (month == "October"){
    Table_long <- octoberTable %>%
      gather(Question, Response, -1)
    Table_long <- Table_long[-c(1:29), ]
    Table_long <- Table_long[-c(1045:1102), ]
    n_rows <- nrow(Table_long)
    # Create a vector of row indices to remove(for key, instrument, answer)
    rows_to_remove <- c( seq(27, n_rows, 29), seq(28, n_rows, 29),
                         seq(29, n_rows, 29))
    # Remove the rows from the dataset
    Table_long <- Table_long[-rows_to_remove, ]
    
    participantID <- head(Table[,1], 26)
    Demographics_long <- cbind(participantID, Demographics)
    
    colnames(Demographics_long)[1] <- "Participant_ID"
    colnames(Table_long)[1] <- "Participant_ID"
  
    merged <- Demographics_long %>%
      inner_join(Table_long, by = "Participant_ID")
    
    MelodyInfo <- t(Table[27:29,])
    MelodyInfo <- MelodyInfo[-c(1, 2, 39, 40), ]
    colnames(MelodyInfo) <- c("Correct_Contours", "Instruments", "Keys")
    MelodyInfo <- as.data.frame(MelodyInfo)

    MelodyInfo_repeated <- MelodyInfo[rep(1:nrow(MelodyInfo), 26), ]

    merged<- cbind(merged, MelodyInfo_repeated)
    
    return(merged)
  }
  else return(NULL)
  
}
#can be called for : BiLateral, Bimodal, SSCI, NH, DSHA
generateContourPercentageGraph <- function(month, df, hearingProfile, save, legend)
{
  if(month == "March"){
    nrQuestions = 4
  }
  else if(month == "October"){
    nrQuestions = 3
    }
    
  
  #filter the complete dataframe based on the hearing profile from the collumn "Hearing profile"
    hearingProfileDf <- df[df$`Hearing profile` == hearingProfile,]
  
    percentagePlot <- ggplot(hearingProfileDf, aes(x = Response, fill = Correct_Contours)) + 
      geom_bar(aes(y = (..count..) / sum(..count..) * nrQuestions), position = "dodge", stat = "count", show.legend = legend) +
      scale_fill_discrete(name = "Contour") +
      labs(x = "Response", y = "Percentage of answers", title = paste("Percentage of answers for:", hearingProfile, "participants")) +
      facet_wrap(~Correct_Contours, nrow = 1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, 1), breaks = seq(0, 9, by = 0.1), expand = c(0, 0)) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (save==TRUE)
  {
    ggsave(paste(hearingProfile,".png",sep=""), plot=percentagePlot, height=700, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')
    print("saved")
  }
  return(percentagePlot)
}
MOM <- 0
#can be called for : BiLateral, Bimodal, SSCI, NH, DSHA, all and Accordion, Piano, Accordion & Bass, Piano & Bass, all
generateInstrumentGraph <- function(month, completeDf, hearingProfile, instrument, detailed, save, legend)
{
  if (hearingProfile == "all") {
    hearingProfileDf <- completeDf
  } else {
    hearingProfileDf <- completeDf[completeDf$`Hearing profile` == hearingProfile,]
  }
  
  if (instrument == "all") {
    instrumentsDf <- hearingProfileDf
  } else {
    instrumentsDf <- hearingProfileDf[hearingProfileDf$Instruments == instrument,]
  }
  
  if(month == "March"){
    nrQuestions = 4
  }
  else if(month == "October"){
    nrQuestions = 3
  }
  
  #naesehorn_correct_percentage <- sum(instrumentsDf$Animal == "Næsehorn" & instrumentsDf$Correct_Contours == instrumentsDf$Response, na.rm = TRUE) / sum(instrumentsDf$Animal == "Næsehorn", na.rm = TRUE)
  #print(naesehorn_correct_percentage)
  
  if (detailed == TRUE) {
    print("detailed")
    
    # Exclude rows with "NA" in Correct_Contours
    instrumentsDf_filtered <- instrumentsDf %>% 
      filter(!is.na(Correct_Contours)) 
    
    instrumentPlot <- ggplot(instrumentsDf_filtered, aes(x = Response, fill = Correct_Contours)) + 
      geom_bar(aes(y = (..count..) / sum(..count..) * nrQuestions), position = "dodge",  stat = "count",  show.legend = legend) +
      scale_fill_discrete(name = "Contour") +
      labs(x = "Response", y = "Percentage of answers", title = paste(hearingProfile, instrument)) +
      facet_wrap(~Correct_Contours, nrow = 1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, 1), breaks = seq(0, 9, by = 0.1), expand = c(0, 0)) + 
      theme_Publication() +
      scale_fill_Publication() +
      scale_colour_Publication() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
      #geom_segment(data = df.mean,aes(x = as.integer(1), xend = as.integer(4),y = mean_correct, yend = mean_correct),
                   #size=1, linetype = "longdash", inherit.aes = F)+
      #geom_errorbar(data = df.mean,
                    #aes(Correct_Contours, ymin = mean_correct - std/10,  ymax = mean_correct + std/10), width = 0.9)+
      #geom_text(data = df.mean, aes(x = Position, y =mean_correct + std/10, label = paste("sd =", round(std, 2))),
                #vjust = -0.5, hjust = 0.5, size = 3)
  }
else {
  print("simple")
  instrumentsDf_filtered <- instrumentsDf %>% 
    filter(Correct %in% c("Correct", "Incorrect"))  # Exclude rows with "Not Answered" in Correct
  
  df.mean <- instrumentsDf_filtered %>%
    group_by(Correct_Contours) %>%
    summarize(
      mean_correct = mean(Response == Correct_Contours, na.rm = TRUE),
      std_correct = sd(Response == Correct_Contours, na.rm = TRUE)
    )
  
  instrumentPlot <- ggplot(instrumentsDf_filtered, aes(x = Correct, fill = Correct)) +
    geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge", stat = "count", show.legend = legend) +
    labs(x = "Responses", y = "Percentage of correct answers", title = paste("Percentage of correct answers for", hearingProfile, "participants,", instrument, "melodies")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, 1), breaks = seq(0, 9, by = 0.1), expand = c(0, 0)) +
    scale_fill_manual(values = c("tomato", "Wheat3"), labels = c("Correct", "Incorrect", "Not Answered")) +
    theme_Publication() +
    scale_fill_Publication() +
    scale_colour_Publication()
}

  
  if (save==TRUE) {
    ggsave(paste(hearingProfile,"for",instrument,".png",sep=""), plot=instrumentPlot, height=700, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')
    print("saved")
  }
  
  return(instrumentPlot)
}

###########################################
### create the main dataframe for March ###
###########################################
marchTable <- loadTable("March")
marchDemographics <-loadDemographics(marchTable, "March")
marchTable[1:44, 2] <- marchDemographics[1:44, 4]
completeDfMarch <- combineData("March", marchTable, marchDemographics)
nrMarchParticipants = as.numeric(nrow(marchDemographics))
nrMarchQuestions = 48


#############################################
### create the main dataframe for October ###
#############################################
octoberTable <- loadTable("October")
octoberDemographics <-loadDemographics(octoberTable, "October")
octoberTable[1:26, 2] <- octoberDemographics[1:26, 4]
colnames(octoberTable)[2] <- "Hearing_Profile"
colnames(octoberTable)[40] <- "Trained"
completeDfOctober <- combineData("October", octoberTable, octoberDemographics)
nrOctoberParticipants = as.numeric(nrow(octoberDemographics))
nrOctoberQuestions = 36


###############################################################################
### Create combined correct answers for each hearing profile and instrument ###
###############################################################################
hearing_profiles <- c("NH","DSHA","Bimodal","SSCI","Bilateral")
instruments <- c("Accordion", "Piano","Accordion & Bass","Piano & Bass")

for (i in 1:length(hearing_profiles)) 
  {
    for (j in 1:length(instruments)) 
      {
          graphTitle <- paste0("Graph_", j)
          assign(graphTitle, generateInstrumentGraph("October", completeDfOctober, hearing_profiles[i], instruments[j], TRUE, FALSE, FALSE))
          #generateInstrumentGraph(hearing_profiles[i], instruments[j], FALSE, TRUE)
    }
    combinedPlotsTotal <- ggarrange(Graph_1, Graph_2, Graph_3, Graph_4, ncol = 2, nrow = 2)
    combinedPlotsTotal
    ggsave(paste("October ",hearing_profiles[i],".png",sep=""), plot=combinedPlotsTotal, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
    print("saved")
}
combinedPlotsTotal


##################################
### Create hearing profile map ###
##################################
HearingProfileSummary <- octoberDemographics %>%
  group_by(`Hearing profile`) %>%
  summarize(Amount = n()) %>%
  arrange(desc(Amount))
colnames(HearingProfileSummary)[1] = "Profile"

HearingProfileSummary <- HearingProfileSummary %>%
  mutate(`Profile` = sapply(`Profile`, function(x) paste(x, collapse = ", ")))%>%
  mutate(Prof_Short = c("Normal hearing", "Cochlear Implant", "Hearing Aid", "Cochlear Implant", "Cochlear Implant", "Unknown"))

distributionPlot <- ggplot(HearingProfileSummary, aes(area = Amount, fill = Profile, subgroup = Prof_Short, label = paste(Profile, Amount, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottom", grow = TRUE,
                             angle = -25,
                             alpha = 0.25, colour = "white",
                             fontface = "italic") +
  theme_Publication() + 
  scale_colour_Publication() + 
  scale_fill_Publication() + 
  theme(legend.position = "none")
distributionPlot

ggsave("distribution October.png", plot=distributionPlot, height=1080, width=1920, scale = 1, units=c("px"), dpi=300, type = 'cairo')


##########################################################################
##### Create demographics bar plot with age and nr of concerts attended.##
##########################################################################
march_df_small <- marchDemographics %>%
  dplyr::select(`Hearing profile`, Age,  `Number of concerts per year`, `Age of onset of deafness`, `Age of implantation`,`Number of year of hearing aid usage before implantation`, `Extent of musical partaking` )

colnames(march_df_small) <- gsub(" ", "_", colnames(march_df_small))
march_df_small[15,6] = NA
march_df_small[10,5] = NA

march_df_means <- march_df_small %>%
  group_by(Hearing_profile) %>%
  filter(Hearing_profile != "Unknown") %>%
  summarise(
    mean_Age = mean(Age, na.rm = TRUE),
    mean_Number_of_concerts_per_year = mean(Number_of_concerts_per_year, na.rm = TRUE),
    mean_Deafness = mean(Age_of_onset_of_deafness, na.rm = TRUE),
    mean_Implantation = mean(Age_of_implantation, na.rm = TRUE),
    mean_HA = mean(Number_of_year_of_hearing_aid_usage_before_implantation, na.rm = TRUE),
    mean_Experince = mean(Extent_of_musical_partaking, na.rm = TRUE),
    
    se_Age = sd(Age, na.rm=TRUE) / sqrt(n()),
    se_Deafness = sd(Age_of_onset_of_deafness, na.rm = TRUE) / sqrt(n()),
    se_Implantation = sd(Age_of_implantation, na.rm = TRUE) / sqrt(n()),
    se_HA = sd(Number_of_year_of_hearing_aid_usage_before_implantation, na.rm = TRUE) / sqrt(n()),
    se_Experience = sd(Extent_of_musical_partaking, na.rm = TRUE) / sqrt(n())
  )


custom_order <- c("Bilateral", "SSCI", "Bimodal", "DSHA", "NH")
custom_order_demographics <- c("Age\n(Years)", "Deaf. Onset\n(Years)", "Implant Age\n(Years)", "HA Use\n(Years)", "Music Exp.\n(Years)", "Concerts / Year"  )

march_df_means <- march_df_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "Average",
    values_to = "Value") %>%
  mutate(Average = ifelse(Average == "mean_Age", "Age\n(Years)", Average)) %>%
  mutate(Average = ifelse(Average == "mean_Number_of_concerts_per_year", "Concerts / Year", Average))%>%
  mutate(Average = ifelse(Average == "mean_Deafness", "Deaf. Onset\n(Years)", Average)) %>%
  mutate(Average = ifelse(Average == "mean_Implantation", "Implant Age\n(Years)", Average)) %>%
  mutate(Average = ifelse(Average == "mean_HA", "HA Use\n(Years)", Average)) %>%
  mutate(Average = ifelse(Average == "mean_Experince", "Music Exp.\n(Years)", Average)) %>%
  mutate(Hearing_profile = factor(Hearing_profile, levels = custom_order)) %>%
  mutate(Average = factor(Average, levels = custom_order_demographics))

demographics_Plot <- ggplot(march_df_means, aes(fill = Hearing_profile, y = Value, x = Average)) +
  geom_bar(position = "dodge", stat = "identity") +
  # geom_errorbar(aes(ymin = ifelse(Average == "Age\n(Years)", Value - se_Age, NA),
  #                   ymax = ifelse(Average == "Age\n(Years)", Value + se_Age, NA)),
  #               position = position_dodge(width = 0.9), width = 0.4, color = "black")+
  # 
  # geom_errorbar(aes(ymin = ifelse(Average == "Deaf. Onset\n(Years)", Value - se_Deafness, NA),
  #                   ymax = ifelse(Average == "Deaf. Onset\n(Years)", Value + se_Deafness, NA)),
  #               position = position_dodge(width = 0.9), width = 0.4, color = "black")+
  # 
  # geom_errorbar(aes(ymin = ifelse(Average == "Implant Age\n(Years)", Value - se_Implantation, NA),
  #                   ymax = ifelse(Average == "Implant Age\n(Years)", Value + se_Implantation, NA)),
  #               position = position_dodge(width = 0.9), width = 0.4, color = "black")+
  # 
  # geom_errorbar(aes(ymin = ifelse(Average == "HA Use\n(Years)", Value - se_HA, NA),
  #                   ymax = ifelse(Average == "HA Use\n(Years)", Value + se_HA, NA)),
  #               position = position_dodge(width = 0.9), width = 0.4, color = "black")+
  geom_text(aes(label = ifelse(Average == "Age\n(Years)", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = ifelse(Average == "Deaf. Onset\n(Years)", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = ifelse(Average == "Implant Age\n(Years)", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = ifelse(Average == "HA Use\n(Years)", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = ifelse(Average == "Music Exp.\n(Years)", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = ifelse(Average == "Concerts / Year", round(Value, digits = 1), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  
  labs(x = "Average demographic characteristic", y = "Value", fill= "Hearing Profile") +
  
  theme_Publication() + 
  scale_colour_Publication() + 
  scale_fill_manual(values = c("#386cb0","#662506","#fdb462","#7fc97f","tomato"))+
  theme(legend.direction = "vertical",
        legend.box = "horizontal",
        legend.position = c(0.85, 0.99),
        legend.justification = c(0, 1))
  
demographics_Plot
ggsave("demographicsPlot_updated.png", plot=demographics_Plot, height=500, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')

######################################
### Experiments or unfinished code ###
######################################
#plots for each question individually
for(i in 1:1)
{
  soloContour <- as.data.frame(Table[1:nrParticipants, i+2])
  #soloContour <- soloContour[1:nrQuestions, ]
  #soloContour <- as.data.frame(soloContour)
  soloContour <- cbind(soloContour, subset(Demographics, select = c(2:12)))
  colnames(soloContour)[1] <- "Contour"
  colnames(soloContour)[4] <- "hearingProfile"
  soloContour$hearingProfile <- sapply(soloContour$hearingProfile, paste, collapse = ", ")
  
  correctAnswer = toString(Table[47, i+2])
  
  # barPlot <- ggplot(data = soloContour, aes(x = soloContour, fill = ifelse(soloContour == correctAnswer, "Highlighted", "Normal"), label = scales::percent(prop.table(stat(count))))) 
  
  barPlot <- ggplot(data = soloContour, aes(x = Contour, fill = ifelse(Contour == correctAnswer, "Highlighted", "Normal"), label = scales::percent(prop.table(stat(count)))))  +
    #geom_bar(stat="count", position = "stack", width =0.9,  alpha = 0.8, show.legend = FALSE) +
    geom_bar_pattern(position="stack", stat = "count", 
                     mapping = aes(pattern = hearingProfile, pattern_angle = hearingProfile),
                     color = "white", width =0.9,  alpha = 0.8, pattern_density = 0.1, pattern_spacing = 0.01,
                     show.legend = TRUE)+ 
    labs(x = "Contours",
         y = "Count",
         title = paste("Distribution of contours for melody nr.", i, "played on: ", Table[48,i+2]),
         fill ="Contours",
         pattern_fill = "Hearing profile") +
    scale_fill_manual(values=c("tomato", "Wheat3"), labels = c("Correct", "Incorrect", "Not Answered"))+
    scale_y_continuous(limits = c(0, 46), breaks = seq(0, 44, by = 2), expand = c(0, 0)) +
    geom_text(stat = 'count', position = position_dodge(.9), vjust = -0.5, size = 5) + 
    theme_solarized(light = TRUE) + 
    theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="black", size=14), axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"),
          panel.background = element_rect(fill="white", colour="black",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.35, linetype = 'dashed',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                          colour = "grey"),
          plot.background = element_rect(fill = "white"),
          legend.key.size = unit(1, "cm"))
  barPlot
  #ggsave(paste(i,".png",sep=""), plot=barPlot, height=700, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')
}

if(TRUE)
{
  combinedPlots <- ggarrange(generateContourPercentageGraph("NH", FALSE, FALSE),
                             generateContourPercentageGraph("DSHA", FALSE, FALSE),
                             generateContourPercentageGraph("Bimodal", FALSE, FALSE),
                             generateContourPercentageGraph("SSCI", FALSE, FALSE),
                             generateContourPercentageGraph("BiImplant", FALSE, FALSE),
                             ncol = 3, nrow = 2)
  ggsave(paste("combinedPlots.png",sep=""), plot=combinedPlots, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  
  combinedPlotsNH <- ggarrange(generateInstrumentGraph("NH", "Accordion", FALSE),
                               generateInstrumentGraph("NH", "Piano", FALSE),
                               generateInstrumentGraph("NH", "Accordion & Bass", FALSE),
                               generateInstrumentGraph("NH", "Piano & Bass", FALSE), 
                               nrow =2, ncol =2)
  ggsave(paste("combinedPlotsNH.png",sep=""), plot=combinedPlotsNH, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  combinedPlotsDSHA <- ggarrange(generateInstrumentGraph("DSHA", "Accordion", FALSE),
                                 generateInstrumentGraph("DSHA", "Piano", FALSE),
                                 generateInstrumentGraph("DSHA", "Accordion & Bass", FALSE),
                                 generateInstrumentGraph("DSHA", "Piano & Bass", FALSE), 
                                 nrow =2, ncol =2)
  ggsave(paste("ccombinedPlotsDSHA.png",sep=""), plot=combinedPlotsDSHA, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  combinedPlotsBimodal <- ggarrange(generateInstrumentGraph("Bimodal", "Accordion", FALSE),
                                    generateInstrumentGraph("Bimodal", "Piano", FALSE),
                                    generateInstrumentGraph("Bimodal", "Accordion & Bass", FALSE),
                                    generateInstrumentGraph("Bimodal", "Piano & Bass", FALSE), 
                                    nrow =2, ncol =2)
  ggsave(paste("ccombinedPlotsBimodal.png",sep=""), plot=combinedPlotsBimodal, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  combinedPlotsSSCI <- ggarrange(generateInstrumentGraph("SSCI", "Accordion", FALSE),
                                 generateInstrumentGraph("SSCI", "Piano", FALSE),
                                 generateInstrumentGraph("SSCI", "Accordion & Bass", FALSE),
                                 generateInstrumentGraph("SSCI", "Piano & Bass", FALSE), 
                                 nrow =2, ncol =2)
  ggsave(paste("ccombinedPlotsSSCI.png",sep=""), plot=combinedPlotsSSCI, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  combinedPlotsBiImplant <- ggarrange(generateInstrumentGraph("BiImplant", "Accordion", FALSE),
                                      generateInstrumentGraph("BiImplant", "Piano", FALSE),
                                      generateInstrumentGraph("BiImplant", "Accordion & Bass", FALSE),
                                      generateInstrumentGraph("BiImplant", "Piano & Bass", FALSE), 
                                      nrow =2, ncol =2)
  ggsave(paste("ccombinedPlotsBiImplant.png",sep=""), plot=combinedPlotsBiImplant, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  
  
  
}


####################################
### Training log data processing ###
####################################
trained_animals <- c("Baever", "Raev", "Egern", "Jaguar", "Ged", "Kanin", "Loeve", "Reje", "Laks")
tracks <- c("Imagine", "Forårsdag", "Papirskip", "What a wonderful world")
training_logs <- read.csv("OnlineTrainingLog.csv")
training_logs <- training_logs %>%
  select(-1) %>%
  mutate(Animal = ifelse(nchar(Animal) == 0 & S_Length > 10, "Loeve", as.character(Animal)),
    Animal = factor(Animal)) %>%
  filter(Animal %in% trained_animals) %>%
  mutate(created_at = as.POSIXct(created_at)) %>%
  mutate(created_at = format(created_at, format = "%Y-%m-%d %H:%M:%S", usetz = FALSE)) %>%
  mutate(S_Length = round(S_Length/60, 0)) %>%
  mutate(Completed = ifelse(is.na(Completed), "FALSE", Completed)) %>%
  mutate(Completed = as.logical(Completed)) %>%
  filter(!is.na(Track)) %>%
  mutate(Track = tracks[Track + 1]) %>%
  mutate(Track = factor(Track, levels = tracks))%>%
  mutate(Animal = factor(Animal, levels = trained_animals))

#Laks an Løve have not completed at least one of the MCI tests, so we cannot compare
train_length <- training_logs %>%
  filter(Animal != "Loeve" & Animal != "Laks") %>%
  group_by(Animal) %>%
  summarise(trained_for = sum(S_Length))%>%
  mutate(Animal = stri_replace_all_regex(Animal, "Raev", "Ræv"))%>%
  mutate(Animal = stri_replace_all_regex(Animal, "Baever", "Bæver"))

###########################
### Training data plots ###
###########################
sum_occurrences <- training_logs %>%
  mutate(Completed = ifelse(is.na(Completed), "FALSE", as.character(Completed)),
         Completed = as.logical(Completed)) %>%
  filter(Completed) %>%
  group_by(Track) %>%
  summarize(total_occurrences = n())

training_sesh_plot <- ggplot(training_logs, aes(x = Track, fill = Track)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Training sessions", x = "Track", y = "Frequency") +
  geom_errorbar(data = sum_occurrences, aes(ymin = total_occurrences, ymax = total_occurrences),
                width = 0.9,
                size = 1.2,
                linetype = "longdash",
                color= "#ffff33") + 
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()
ggsave("Training sessions.png", plot=training_sesh_plot, height=500, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')

training_time_plot <- ggplot(training_logs, aes(x = Track, y = S_Length, fill = Track)) +
  stat_summary(fun = "sum", geom = "bar", position = "dodge", show.legend = FALSE) +
  labs(y = "Minutes") +
  geom_errorbar(stat = "summary", fun.ymin = function(x) sum(x, na.rm = TRUE), 
                fun.ymax = function(x) sum(x, na.rm = TRUE), 
                position = position_dodge(width = 0.9),
                subset = .(Completed)) +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

ggsave("Training time.png", plot=training_time_plot, height=500, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')

training_time_user_plot <- ggplot(training_logs, aes(x = Animal, y = S_Length, fill = Track)) +
  stat_summary(fun = "sum", geom = "bar", position = "dodge", show.legend = FALSE, alpha = 0.8) +
  labs(y = "Minutes", x = "Participant's tag") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

ggsave("Training time per user.png", plot=training_time_user_plot, height=500, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')

training_logs_with_HP <- training_logs %>%
  mutate(Animal = as.character(Animal)) %>%
  mutate(Animal = ifelse(Animal == "Ged", "Ged\n(Bilateral)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Baever", "Bæver\n(2*HA)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Egern", "Egern\n(CI)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Jaguar", "Jaguar\n(Bilateral)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Kanin", "Kanin\n(2*HA)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Raev", "Ræv\n(Bimodal)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Reje", "Reje\n(Bilateral)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Laks", "Laks\n(CI)", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Loeve", "Løve\n(Bilateral)", Animal)) %>%
  mutate(Animal = factor(Animal))  # Convert back to factor if needed

  #reorder the levels for animals
  ordered_animals <- training_logs_with_HP %>%
    group_by(Animal) %>%
    summarise(total_length = sum(S_Length, na.rm = TRUE)) %>%
    arrange(desc(total_length)) %>%
    pull(Animal)
  training_logs_with_HP$Animal <- factor(training_logs_with_HP$Animal, levels = ordered_animals)

training_time_simple_plot <- ggplot(training_logs_with_HP, aes(x = Animal, y = S_Length, fill = Animal)) +
  stat_summary(fun = "sum", geom = "bar", position = "dodge", show.legend = FALSE, alpha = 0.8) +
  labs(y = "Minutes", x = "Participant's tag") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication() +
  scale_y_continuous(limits = c(0, 256), breaks = seq(0, 256, by = 50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Training time per user simple.png", plot=training_time_simple_plot, height=1440, width=2560, scale = 1, units=c("px"), dpi=300, type = 'cairo')



##############################
### Training data analysis ###
##############################
trained_animals_march <- c("Bæver", "Ræv", "Egern", "Jaguar", "Ged", "Kanin", "Løve", "Rejer")
march_correct_perc <- matrix(nrow = length(trained_animals_march), ncol = length(instruments))
rownames(march_correct_perc) <- trained_animals_march
colnames(march_correct_perc) <- instruments

completeDfMarch_no_Arching <- completeDfMarch[completeDfMarch$Correct_Contours != "Arching", ]

for (animal in 1:length(trained_animals_march)){
  for(instrument in 1:length(instruments)){
    total_responses <- sum(completeDfMarch_no_Arching$Animal == trained_animals_march[animal] &
                             completeDfMarch_no_Arching$Instruments == instruments[instrument], na.rm = TRUE)
    correct_responses <- sum(completeDfMarch_no_Arching$Animal == trained_animals_march[animal] &
                               completeDfMarch_no_Arching$Instruments == instruments[instrument] &
                               completeDfMarch_no_Arching$Correct_Contours == completeDfMarch_no_Arching$Response, na.rm = TRUE)
    percentage_correct <- correct_responses / total_responses
    
    #print(c(trained_animals_march[animal], instruments[instrument],  percentage_correct ))
    march_correct_perc[animal, instrument] <- percentage_correct
    }
}

march_correct_perc <- as.data.frame(march_correct_perc)
march_correct_perc$Cumulated <- rowMeans(march_correct_perc, na.rm = TRUE)

trained_animals_october <- c("Bæver", "Rav", "Egern", "Jaguar", "Ged", "Kanin", "Løve", "Reje")
oct_correct_perc <- matrix(nrow = length(trained_animals_october), ncol = length(instruments))
rownames(oct_correct_perc) <- trained_animals_october
colnames(oct_correct_perc) <- instruments

for (animal in 1:length(trained_animals_october)) {
  for (instrument in 1:length(instruments)) {
    total_responses <- sum(completeDfOctober$Animal == trained_animals_october[animal] &
                             completeDfOctober$Instruments == instruments[instrument], na.rm = TRUE)
    correct_responses <- sum(completeDfOctober$Animal == trained_animals_october[animal] &
                               completeDfOctober$Instruments == instruments[instrument] &
                               completeDfOctober$Correct_Contours == completeDfOctober$Response, na.rm = TRUE)
    percentage_correct <- correct_responses / total_responses
    
   #print(c(trained_animals_october[animal], instruments[instrument], percentage_correct))
    oct_correct_perc[animal, instrument] <- percentage_correct
  }
}

oct_correct_perc <- as.data.frame(oct_correct_perc)
oct_correct_perc$Cumulated <- rowMeans(oct_correct_perc, na.rm = TRUE)


oct_correct_perc <- oct_correct_perc %>%
  rownames_to_column(var = "Animal")

march_correct_perc <- march_correct_perc %>%
  rownames_to_column(var = "Animal")

# Add a column to identify the month
oct_correct_perc$Month <- "October"
march_correct_perc$Month <- "March"


# Combine the data frames
combined_df <- rbind(oct_correct_perc, march_correct_perc)

combined_df <- combined_df %>%
  mutate(Animal = ifelse(Animal == "Rav", "Ræv", Animal)) %>%
  mutate(Animal = ifelse(Animal == "Rejer", "Reje", Animal))%>%
  mutate(Animal = ifelse(Animal == "Løve", NA, Animal)) %>%
  filter(!is.na(Animal))
  

# Create the plot
ggplot(combined_df, aes(x = Animal, y = Cumulated, color = Month)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  labs(title = "Percentage of Correct Answers by Animal",
       x = "Animal", y = "Percentage Correct") +
  scale_color_manual(values = c("October" = "blue", "March" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


perc_diff <- oct_correct_perc
for (row in 1:8) {
  for (col in 2:6) {
    # Check if both values are not NaN
    if (!is.nan(oct_correct_perc[row, col]) && !is.nan(march_correct_perc[row, col])) {
      print(oct_correct_perc[row, col]-march_correct_perc[row, col])
      perc_diff[row, col] = oct_correct_perc[row, col]-march_correct_perc[row, col]
    }
  }
}

# Print the resulting dataframe
perc_diff <- perc_diff %>%
  select(-7)  %>%
  filter(row_number() != 7)%>%
  mutate(Animal = ifelse(Animal == "Rav", "Ræv", Animal))%>%
  left_join(select(train_length, Animal, trained_for), by = "Animal")

average_group_change = 48.07- 59.4
perc_diff_adjusted <- perc_diff %>%
  mutate_at(vars(2:6), function(x) x - average_group_change/100)

corr_plot_adjusted <- ggplot(data = perc_diff_adjusted, aes(x = trained_for)) +
               geom_point(aes(y = Accordion, color = "Accordion", alpha = 0.5), size = 2, color ="#386cb0", alpha = 0.8) +
               geom_point(aes(y = Piano, color = "Piano", alpha = 0.5), size = 2, color ="#fdb462", alpha = 0.8) +
               geom_point(aes(y = `Accordion & Bass`, color = "Accordion & Bass", alpha = 0.5), size = 2, color ="#7fc97f", alpha = 0.8) +
               geom_point(aes(y = `Piano & Bass`, color = "Piano & Bass", alpha = 0.5), size = 2, color ="#984ea3", alpha = 0.8) +
               geom_point(aes(y = Cumulated, color = "Cumulated",alpha = 0.5), size = 5, color ="tomato", alpha = 0.8) +
               geom_smooth(aes(y = Accordion, color = "Accordion"), method = "lm", se = FALSE) +
               geom_smooth(aes(y = Piano, color = "Piano"), method = "lm", se = FALSE) +
               geom_smooth(aes(y = `Accordion & Bass`, color = "Accordion & Bass"), method = "lm", se = FALSE) +
               geom_smooth(aes(y = `Piano & Bass`, color = "Piano & Bass"), method = "lm", se = FALSE) +
               geom_smooth(aes(y = Cumulated, color = "Cumulated"), method = "lm", se = FALSE) +
               labs(x = "Minutes trained",
                    y = "MCI improvement",
                    color = "Instrument") +
               theme_Publication() +
               scale_fill_Publication() +
               scale_colour_Publication() +
               scale_color_manual(values = c("Accordion" = "#386cb0", "Piano" = "#fdb462", "Accordion & Bass" ="#7fc97f", "Piano & Bass" = "#984ea3", "Cumulated" = "tomato"))
 
ggsave("Correlation between training and MCI adjusted.png", plot=corr_plot_adjusted, height=1440, width=2560, scale = 1, units=c("px"), dpi=300, type = 'cairo')


ggplot(data = perc_diff, aes(x = Animal, )) +
  geom_bar(aes(y = Cumulated), stat = "identity", fill = "red", position = position_dodge(width = 0.8)) +
  geom_bar(aes(y = Accordion), stat = "identity", fill = "blue", position = position_dodge(width = 0.8)) +
  labs(title = "Cumulated Values for Each Animal",
       x = "Animal",
       y = "Cumulated Value") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication() 


ggplot(data = perc_diff, aes(x = Animal)) +
  geom_bar(aes(y = Piano), stat = "identity", fill = "#fdb462", position = position_nudge(-0.2)) +
  geom_bar(aes(y = Accordion), stat = "identity", fill = "#386cb0", position = position_nudge(0.2)) +
  geom_bar(aes(y = `Accordion & Bass`), stat = "identity", fill = "#7fc97f", position = position_nudge(-0.2)) +
  geom_bar(aes(y = `Piano & Bass`), stat = "identity", fill = "#984ea3", position = position_nudge(0.2)) +
  
  labs(title = "Cumulated Values and Accordion for Each Animal",
       x = "Animal",
       y = "Values") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

perc_diff_pivoted_adjusted <- perc_diff_adjusted %>%
  pivot_longer(cols = -c(1,6,7), names_to = "Instrument", values_to = "MCI_score")%>%
  mutate(Animal = ifelse(Animal == "Ged", "Ged\n(Bilateral)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Bæver", "Bæver\n(2*HA)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Egern", "Egern\n(CI)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Jaguar", "Jaguar\n(Bilateral)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Kanin", "Kanin\n(2*HA)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Ræv", "Ræv\n(Bimodal)", Animal))%>%
  mutate(Animal = ifelse(Animal == "Reje", "Reje\n(Bilateral)", Animal))


MCI_scores_diff_adj <- ggplot(perc_diff_pivoted_adjusted, aes(x = Animal, y = MCI_score, fill = Instrument)) +
  stat_summary(fun = "sum", geom = "bar", position = "dodge", show.legend = TRUE, size = 0.1, color ="black", alpha = 0.8) +
  labs(y = "MCI Score difference", x = "Participant's tag") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication() 

ggsave("Difference between MCI scores adjusted.png", plot=MCI_scores_diff_adj, height=1440, width=2560, scale = 1, units=c("px"), dpi=300, type = 'cairo')


###New experiments


sapply(training_time, class)

total_time_values <- c(round(3732/60) * 9, round(4064/60) * 9, round(3791/60) * 9, round(3793/60) * 9)

training_time <- training_logs %>%
  mutate(Completed = if_else(Completed == "TRUE", TRUE, FALSE))%>%
  group_by(Track) %>%
  summarise(
    total_time_listened = sum(S_Length, na.rm = TRUE),
    total_time_completed = sum(ifelse(Completed, S_Length, 0), na.rm = TRUE))%>%
  mutate(total_time = total_time_values)
 
# Step 1: Add new value as a level if it's not already
new_value <- "What a\nwonderful world"
training_time$Track <- factor(training_time$Track,
                              levels = c(levels(training_time$Track),
                                         new_value))

# Step 2: Change the value of the specific cell
training_time$Track[4] <- new_value

percentage_listened_plot <- ggplot(training_time) +
  geom_bar(aes(x = Track, y = total_time, fill = Track), stat = "identity", 
            alpha = 0.3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  geom_bar(aes(x = Track, y = total_time_listened), stat = "identity", 
           fill = c("#386cb0","#fdb462","#7fc97f","tomato"),
           alpha = 0.8,
           position = position_dodge(width = 0.8), show.legend = FALSE) +
  labs(x = "Track", y = "Minutes") +
  geom_errorbar(aes(x = Track,
                    ymin = total_time_listened,
                    ymax = total_time_listened),
                width = 0.9, size = 1.2, linetype = "solid",
                color = c("#386cb0","#fdb462","#7fc97f","tomato")) +
  geom_text(aes(x = Track, label = paste0(round((total_time_listened / total_time) * 100, 1), "%"), 
                y = total_time_listened + 50), # Adjust the 50 as needed for positioning
            vjust = 0.7, color = "black", size = 5) +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()
ggsave("percentage_listened_plot.png", plot=percentage_listened_plot, height=1440, width=2560, scale = 1, units=c("px"), dpi=300, type = 'cairo')


#Calculate the percentage of correct answers.
percentage_correct <- completeDfMarch_no_Arching %>%
  mutate(Response = replace_na(Response, "No Answer"))%>%
  mutate(Correct_Response = ifelse(Response == Correct_Contours, 1, 0))%>%
  filter(`Hearing profile` != "NH")
  
# Step 2: Calculate the percentage of correct responses
perc_corr_March <- sum(percentage_correct$Correct_Response, na.rm=TRUE) / 36 / 26 * 100

print(paste("Percentage of correct answers:", perc_corr_March, "%"))

percentage_correct_oct <- completeDfOctober %>%
  mutate(Response = replace_na(Response, "No Answer"))%>%
  mutate(Correct_Response = ifelse(Response == Correct_Contours, 1, 0))%>%
  filter(`Hearing profile` != "NH")
  
percentage_correct_october <- sum(percentage_correct_oct$Correct_Response, na.rm=TRUE) / 36 / 26 * 100
print(paste("Percentage of correct answers:", percentage_correct_october, "%"))

#March total, March only Himpaired, Oct total, Oct himpared
correct_perc <- c(62.55, 59.4, 73.82, 48.07) 

############################################################
###Some ANCOVA Trials that cannot work because i'm tired ###
############################################################
trained_animals_oct <- c("Bæver", "Ræv", "Egern", "Jaguar", "Ged", "Kanin", "Løve", "Reje")

renamed_Oct <- completeDfOctober %>%
  mutate(Animal = ifelse(Animal == "Rav", "Ræv", Animal))%>%
  filter(`Hearing profile` != "NH")%>%
  select(Participant_ID, Animal, Question, Response, Correct_Contours)%>%
  mutate(Response = replace_na(Response, "No Answer"))%>%
  mutate(Correct_Response = ifelse(Response == Correct_Contours, 1, 0))
  
percentage_correct_by_animal_oct <- renamed_Oct %>%
  group_by(Animal) %>%
  summarise(perc_Oct = mean(Correct_Response, na.rm = TRUE) * 100)%>%
  mutate(Trained = ifelse(Animal %in% trained_animals_oct, 1, 0))


renamed_march <-completeDfMarch_no_Arching%>%
  filter(`Hearing profile` != "NH")%>%
  select(Participant_ID, Animal, Question, Response, Correct_Contours)%>%
  mutate(Response = replace_na(Response, "No Answer"))%>%
  mutate(Correct_Response = ifelse(Response == Correct_Contours, 1, 0))

percentage_correct_by_animal_march <- renamed_march %>%
  group_by(Animal) %>%
  summarise(perc_March = mean(Correct_Response, na.rm = TRUE) * 100)

combined_perc <- left_join(percentage_correct_by_animal_march, percentage_correct_by_animal_oct[, c("Animal", "perc_Oct")], by = "Animal")



### Some T-Test###

#Extract the untrained animals with hearing impairment
untrained_animals_october <- completeDfOctober %>%
  filter(`Hearing profile` != "NH") %>%
  distinct(Animal)
animals_to_exclude <- c("Hund", "Mariehøne (2)") #These did not fill their hearing profile

untrained_animals_october <- untrained_animals_october$Animal
untrained_animals_october <- setdiff(untrained_animals_october, animals_to_exclude)
untrained_animals_october <- setdiff(untrained_animals_october, trained_animals_october)

oct_correct_perc_untrained <- matrix(nrow = length(untrained_animals_october), ncol = length(instruments))
rownames(oct_correct_perc_untrained) <- untrained_animals_october
colnames(oct_correct_perc_untrained) <- instruments

for (animal in 1:length(untrained_animals_october)) {
  for (instrument in 1:length(instruments)) {
    total_responses <- sum(completeDfOctober$Animal == untrained_animals_october[animal] &
                             completeDfOctober$Instruments == instruments[instrument], na.rm = TRUE)
    correct_responses <- sum(completeDfOctober$Animal == untrained_animals_october[animal] &
                               completeDfOctober$Instruments == instruments[instrument] &
                               completeDfOctober$Correct_Contours == completeDfOctober$Response, na.rm = TRUE)
    percentage_correct <- correct_responses / total_responses
    
    #print(c(trained_animals_october[animal], instruments[instrument], percentage_correct))
    oct_correct_perc_untrained[animal, instrument] <- percentage_correct
  }
}

oct_correct_perc_untrained <- as.data.frame(oct_correct_perc_untrained)
oct_correct_perc_untrained$Cumulated <- rowMeans(oct_correct_perc_untrained, na.rm = TRUE)

oct_correct_perc_untrained <- oct_correct_perc_untrained %>%
  rownames_to_column(var = "Animal")

# Add a column to identify the month
oct_correct_perc_untrained$Trained <- "No"
oct_correct_perc_trained <- oct_correct_perc

# Renaming the column "month" to "Trained"
names(oct_correct_perc_trained)[names(oct_correct_perc_trained) == "Month"] <- "Trained"

# Changing all values in the "Trained" column to "Yes"
oct_correct_perc_trained$Trained <- "Yes"


oct_perc_correct_all <- rbind(oct_correct_perc_untrained, oct_correct_perc_trained)
oct_perc_correct_all <- oct_perc_correct_all[-16, ]


#Shapiro tests for normality
shapiro.test(oct_correct_perc_untrained$Accordion) #W = 0.8414, p-value = 0.05987
shapiro.test(oct_correct_perc_untrained$Piano) #!!!! W = 0.74204, p-value = 0.004383  !!!!!
shapiro.test(oct_correct_perc_untrained$`Piano & Bass`) #!!!! W = 0.81384, p-value = 0.02935 !!!!
shapiro.test(oct_correct_perc_untrained$`Accordion & Bass`) # W = 0.87429, p-value = 0.1366
shapiro.test(oct_correct_perc_untrained$Cumulated) #W = 0.85683, p-value = 0.08855

shapiro.test(oct_correct_perc_trained$Accordion) #W = 0.91336, p-value = 0.4196
shapiro.test(oct_correct_perc_trained$Piano) #W = 0.85575, p-value = 0.1386
shapiro.test(oct_correct_perc_trained$`Piano & Bass`) #W = 0.91511, p-value = 0.4324
shapiro.test(oct_correct_perc_trained$`Accordion & Bass`) #W = 0.96664, p-value = 0.8733
shapiro.test(oct_correct_perc_trained$Cumulated) #W = 0.96664, p-value = 0.8733

#Levene tests for homogeneity of variance
leveneTest(Accordion ~ Trained, data = oct_perc_correct_all) #2.6372 pr = 0.1267
leveneTest(Piano ~ Trained, data = oct_perc_correct_all) #!!! Not applicable
leveneTest(`Piano & Bass` ~ Trained, data = oct_perc_correct_all) #!!! Not applicable
leveneTest(`Accordion & Bass` ~ Trained, data = oct_perc_correct_all)#1.1807, pr = 0.2956
leveneTest(Cumulated ~ Trained, data = oct_perc_correct_all) #3.3161, pr = 0.09004

t.test(Accordion ~ Trained, data = oct_perc_correct_all) # t = -1.3001, df = 11.227, p-value = 0.2196
wilcox.test(Piano ~ Trained, data = oct_perc_correct_all) #W = 24, p-value = 0.4425
wilcox.test(`Piano & Bass` ~ Trained, data = oct_perc_correct_all) #W = 24.5, p-value = 0.4836
t.test(`Accordion & Bass` ~ Trained, data = oct_perc_correct_all) #t = -2.277, df = 12.063, p-value = 0.0418
t.test(Cumulated ~ Trained, data = oct_perc_correct_all) # t = -1.7435, df = 9.8075, p-value = 0.1124


shapiro.test(march_correct_perc$Cumulated) # W = 0.94066, p-value = 0.6176
shapiro.test(oct_correct_perc$Cumulated) # W = 0.96664, p-value = 0.8733

leveneTest(Cumulated ~ Month, combined_df) # 0.216 pr = 0.6504

t.test(Cumulated~Month, combined_df) # t = -1.5286, df = 11.189, p-value = 0.1541





##################################################
### Generate contour and listening group graph ###
##################################################

generateContourListeningGraph(completeDfMarch, "Bilateral", FALSE)

generateContourListeningGraph <- function(completeDf, hearingProfile, save)
{
  nrQuestions = 4

  # 
  # if (month == "March"){
  #   nrQuestions = 4
  # }
  # else {
  #   nrQuestions = 3
  # }
    
  # Exclude rows with "NA" in Correct_Contours
  instrumentsDf_filtered <- completeDf %>% 
    filter(!is.na(Correct_Contours)) 
  
  # Add a collumn for correct/incorrect answers
  instrumentsDf_filtered$IsCorrect <- instrumentsDf_filtered$Response == instrumentsDf_filtered$Correct_Contours
  
  # Group and summarize the data
  summaryDf <- instrumentsDf_filtered %>%
    filter(`Hearing profile` != "Unknown" & (`Hearing profile` == "NH" | `Hearing profile` == hearingProfile))%>%
    group_by(`Hearing profile`, Instruments, Correct_Contours) %>%
    summarise(
      Correct = sum(IsCorrect, na.rm = TRUE),  # Remove NA values in sum
      Total = sum(!is.na(IsCorrect)),          # Count only non-NA responses
      Percentage = (Correct / Total) * 100) %>%
    mutate(Instruments = factor(Instruments, levels = c("Piano", "Piano & Bass", "Accordion", "Accordion & Bass")))
  
  reshaped_df <- summaryDf %>%
    select(`Hearing profile`, Instruments, Correct_Contours, Percentage) %>%
    pivot_wider(names_from = `Hearing profile`, values_from = Percentage, 
                names_sort = TRUE) %>%
    arrange(Instruments, Correct_Contours)%>%
    rename(Contour = Correct_Contours )%>%
    rename(Instrument = Instruments)
  
  plot <- ggplot(reshaped_df) +
    geom_bar(aes(x = Contour, y = NH, fill = Instrument),stat = "identity", 
            alpha = 0.3, position = position_dodge(), show.legend = FALSE) +
    
    geom_bar(aes(x = Contour, y = !!sym(hearingProfile), fill = Instrument),stat = "identity", 
             alpha = 0.8, position = position_dodge(), show.legend = TRUE) +
    
    geom_errorbar(aes(x = Contour,
                      ymin = !!sym(hearingProfile),
                      ymax = !!sym(hearingProfile)),
                  width = 0.9, size = 1.5, linetype = "solid",
                  position = position_dodge2(0.2),
                  color = c("#386cb0","#fdb462","#7fc97f","tomato","#386cb0","#fdb462","#7fc97f","tomato","#386cb0","#fdb462","#7fc97f","tomato","#386cb0","#fdb462","#7fc97f","tomato")) +
    
    geom_errorbar(aes(x = Contour,
                      ymin = NH,
                      ymax = NH),
                  width = 0.9, size = 0.5, linetype = "solid",
                  position = position_dodge2(0.2),
                  color = "darkgray") +
    
    # geom_text(aes(x = Contour, y = NH, label = round(NH, 1), group = Instrument), 
    #           vjust = -0.2,
    #           color = "black",
    #           size = 4,
    #           position = position_dodge(0.9)) +
    
    geom_text(aes(x = Contour, y = !!sym(hearingProfile), label = round(!!sym(hearingProfile), 1), group = Instrument), 
              vjust = -0.3,
              color = "black",
              size = 5,
              position = position_dodge(0.9)) +
    
    labs(x = "Response", y = "Correct (%)", title = paste("Aanswers by", hearingProfile, "compared to normal hearing")) +
    theme_Publication() +
    scale_fill_Publication() +
    scale_y_continuous(limits = c(NA, 100))+
    theme(legend.text = element_text(size = 18))
    scale_colour_Publication()

  if (save==TRUE) {
    ggsave(paste(hearingProfilePercentage,".png",sep=""), plot=plot, height=700, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')
    print("saved")
  }
  
  return(plot)
}

generateContourListeningGraph(completeDfMarch, hearing_profiles[2], FALSE)


for (i in 2:length(hearing_profiles)){
  graphTitle <- paste0("Graph_", i)
  assign(graphTitle,generateContourListeningGraph(completeDfMarch, hearing_profiles[i], FALSE) )
} 
  combinedPlotsTotal <- ggarrange(Graph_2, Graph_3, Graph_4, Graph_5, ncol = 2, nrow = 2)
  combinedPlotsTotal
  ggsave(paste("hearingProfilePercentage ",hearing_profiles[i],".png",sep=""), plot=combinedPlotsTotal, height=1080, width=1920, scale = 3, units=c("px"), dpi=300, type = 'cairo')
  print("saved")


