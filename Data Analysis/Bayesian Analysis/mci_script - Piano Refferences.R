## Packages
# Data wrangling/plotting
library("tidyverse")

# Missing data (multiple imputation)
library("mice")

# Mixed effects models (Frequentist)
library("lme4")
library("broom.mixed")

# Multi-level models (Bayesian)
library("brms") # Can be slow to install

library("bayesplot")

rm(list = ls())

## Working directory
# Set path to folder
#setwd("~/OneDrive/Consultancy/2023/CREATE-Razvan/")
switch(Sys.info()[['sysname']],
       Windows= {setwd("E:/OneDrive - Aalborg Universitet/PhD/Projects/2023 - CI Concert/Data Analysis/Bayesian Analysis/")
         library('Cairo')},
       Linux  = {print("I'm a penguin, i don't know what to do.")},
       Darwin = {setwd("~/OneDrive - Aalborg Universitet/PhD/Projects/2023 - CI Concert/Data Analysis/Bayesian Analysis/")})

load ("MCI env - Piano Refference.RData")

## Data
# Measurement information
mci_data_raw <- read_csv("Data/MCIdata.csv") %>% 
    pivot_longer(cols = Question_1:Question_48, names_to = "Question", values_to = "Answer") %>% 
    mutate(Question = str_remove(Question, "Question_")) %>% 
    rename(ID = Participant_ID, Profile = Hearing_Profile)

# Question information
mci_question_id <- read_csv("Data/MCIquestion.csv") %>% 
    mutate(Question = paste(Question))

# Collected dataframe
mci_data <- mci_data_raw %>% 
    left_join(mci_question_id, by = "Question") %>% # Joining data to a single dataframe
    filter(Profile != "Unknown") %>% # Removed the single unknown contributor
    select(ID, Profile, Instruments, Age, Key, Question, Answer, Contours) %>% 
    mutate( # Creating 0/1 for crr
        Correct = as.numeric(Answer == Contours)
    ) %>%
    mutate( # THIS IS THE REFFERNCE FOR CONSTRAST ENCODING
        Instruments = relevel(factor(Instruments), ref = "Piano"),
        Profile = relevel(factor(Profile), ref = "NH"),
        Contours = relevel(factor(Contours), ref = "Ascending")
    )

# This shows repeated measures => random/mixed effects
mci_data %>% 
    group_by(ID, Instruments, Profile, Contours) %>% 
    summarise(N = n())
## Creating multiple data-set due to missing values (multiple imputation)
# Seed is set to get consistent results, and m = 5 is the norm. 
mci_data_imp <- mice(mci_data, m = 5, seed = 123456, printFlag = FALSE)

mci_data_no_nh <- subset(mci_data, Profile != "NH")
mci_data_no_nh_imp <- mice(mci_data, m = 5, seed = 123456, printFlag = FALSE)
## Logistic regression in GLMER
# Complete (/w interaction on age)
lr_glm_mi <- with(
    mci_data_imp, 
    glmer(
        Correct ~ (Profile + Instruments + Contours) * Age + (1 | ID), 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
        family = binomial(link = "logit")
    )
)

(p_lr_glm_mi <- pool(lr_glm_mi))
summary(p_lr_glm_mi)

# Removing interaction
lr_glm_mi_a <- with(
    mci_data_imp, 
    glmer(
        Correct ~ Profile + Instruments + Contours + Age + (1 | ID), 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
        family = binomial(link = "logit")
    )
)

(p_lr_glm_mi_a <- pool(lr_glm_mi_a))
summary(p_lr_glm_mi_a)

# There is no statistical difference between contour models, i.e. we choose the simpler model. !!!!! - THIS IS GOOD
D1(lr_glm_mi, lr_glm_mi_a)

# Removing main-effects
lr_glm_mi_c <- with(
    mci_data_imp, 
    glmer(
        Correct ~ Profile + Instruments + Age + (1 | ID), 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
        family = binomial(link = "logit")
    )
)

lr_glm_mi_p <- with(
    mci_data_imp, 
    glmer(
        Correct ~ Instruments + Contours + Age + (1 | ID), 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
        family = binomial(link = "logit")
    )
)

lr_glm_mi_i <- with(
    mci_data_imp, 
    glmer(
        Correct ~ Profile + Contours + Age + (1 | ID), 
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
        family = binomial(link = "logit")
    )
)

# There is a statistical difference between models, i.e. we choose 'lr_glm_mi_a'.
D1(lr_glm_mi_a, lr_glm_mi_c)
D1(lr_glm_mi_a, lr_glm_mi_p)
D1(lr_glm_mi_a, lr_glm_mi_i)

# The model is contrast coded, this parameters are thought of as change from the reference.  
summary(p_lr_glm_mi_a)


#This is  the Bayesian analyis

## BRMS (this going to be very slow because it has to be repeated 5 times)
# Complete (/w interactions) -- very very slow
lr_brm_mi <- brm_multiple(
    Correct ~ Profile * Instruments * Contours + Age + (1 | ID), 
    family = bernoulli(link = "logit"), 
    data = mci_data_imp,
    warmup = 1500,
    iter = 3000,
    chains = 4,
    cores = 4
)


(loo_lr_brm_mi <- loo(lr_brm_mi))

# Removing interactions
lr_brm_mi_a <- brm_multiple(
    Correct ~ Profile + Instruments + Contours + Age + (1 | ID), 
    family = bernoulli(link = "logit"), 
    data = mci_data_imp,
    warmup = 1500,
    iter = 3000,
    chains = 4,
    cores = 4
)


lr_brm_mi_noAge <- brm_multiple(
  Correct ~ Profile + Instruments + Contours + (1 | ID), 
  family = bernoulli(link = "logit"), 
  data = mci_data_imp,
  warmup = 1500,
  iter = 3000,
  chains = 4,
  cores = 8
)



# This is the model that will account for training 
# lr_brm_mi_a <- brm_multiple(
#  Correct ~ Profile + Instruments + Contours + Age + Trained + (1 | ID),
#  family = bernoulli(link = "logit"),
#  data = mci_data_imp,
#  warmup = 1500,
#  iter = 3000,
#  chains = 4,
#  cores = 4
# )


lr_brm_mi_c <- brm_multiple(
  Correct ~ Profile + Instruments + Contours + Profile:Instruments + Age + (1 | ID), # this is the same as Profile * Instrument + contour + Age....
  family = bernoulli(link = "logit"), 
  data = mci_data_imp,
  warmup = 1500,
  iter = 3000,
  chains = 4,
  cores = 4
)

(loo_lr_brm_mi_a <- loo(lr_brm_mi_a))
(loo_lr_brm_mi_c <- loo(lr_brm_mi_c))
(loo_lr_brm_mi_noAge <- loo(lr_brm_mi_noAge))

# Choose the model with smallest LOO, i.e. 'lr_brm_mi_a'
loo_compare(loo_lr_brm_mi, loo_lr_brm_mi_a, loo_lr_brm_mi_c)
loo_compare(loo_lr_brm_mi_noAge, loo_lr_brm_mi_c)

#diff = -10 (for c), and -53 (for mi), model "a" has the smallest prediction error aka no significant interaction between profile and instr (not enough data???)

# Model is (again) contrast coded, i.e., the parameters are differences from the reference.
fixef(lr_brm_mi_a)
post_sample <- as_draws_df(lr_brm_mi_a)

# 
plot(density(post_sample$b_ProfileDSHA), col = "#fdb462", main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_ProfileDSHA), col = adjustcolor("#fdb462", alpha.f = 0.25))

lines(density(post_sample$b_ProfileBimodal), col = "#386cb0")
polygon(density(post_sample$b_ProfileBimodal), col = adjustcolor("#386cb0", alpha.f = 0.25))

lines(density(post_sample$b_ProfileSSCI), col = "#7fc97f")
polygon(density(post_sample$b_ProfileSSCI), col = rgb(1, 0, 1, alpha = 0.25))

lines(density(post_sample$b_ProfileBilateral), col = "darkgoldenrod")
polygon(density(post_sample$b_ProfileBilateral), col = rgb(0, 1, 1, alpha = 0.25))


# This will be the plot for "trained" distribution
#plot(density(post_sample$b_Trained), col = "#fdb462", main = "Posterior samples", xlab = "")
#polygon(density(post_sample$b_Trained), col = adjustcolor("#fdb462", alpha.f = 0.25))

some_colors <- c("#386cb0","#fdb462","#7fc97f","tomato","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")

par(mfrow = c(2, 2))

# We can find the differences between levels (even under contrast coding)
plot(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileDSHA), col = "#7fc97f",  main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileDSHA), col = adjustcolor("#7fc97f", alpha.f = 0.45))

lines(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal), col = "#386cb0")
polygon(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal), col = adjustcolor("#386cb0", alpha.f = 0.45))

lines(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileSSCI), col = "tomato")
polygon(density(post_sample$b_ProfileBilateral - post_sample$b_ProfileSSCI), col = adjustcolor("tomato", alpha.f = 0.25))

# Add legend entries for the other differences
legend(
  "topright", # Position of the legend on the plot
  legend = c("Bilateral vs. DSHA", "Bilateral vs. Bimodal", "Bilateral vs SSCI"), # Text labels for the other differences
  fill = c("#7fc97f", "#386cb0", "tomato") # Colors for the other differences
)


plot(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileBilateral), col = "#7fc97f",  main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileBilateral), col = rgb(0, 1, 0, alpha = 0.45))

lines(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileBimodal), col = "#386cb0")
polygon(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileBimodal), col = adjustcolor("#386cb0", alpha.f = 0.45))

lines(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileSSCI), col = "tomato")
polygon(density(post_sample$b_ProfileDSHA - post_sample$b_ProfileSSCI), col = adjustcolor("tomato", alpha.f = 0.25))


legend(
  "topright", # Position of the legend on the plot
  legend = c("DSHA vs. Bilateral", "DSHA vs. Bimodal", "DSHA vs SSCI"), # Text labels for the other differences
  fill = c("#7fc97f", "#386cb0", "tomato") # Colors for the other differences
)


plot(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileBilateral), col = "#7fc97f",  main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileBilateral), col = rgb(0, 1, 0, alpha = 0.45))

lines(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileDSHA), col = "#386cb0")
polygon(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileDSHA), col = adjustcolor("#386cb0", alpha.f = 0.45))

lines(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileSSCI), col = "tomato")
polygon(density(post_sample$b_ProfileBimodal - post_sample$b_ProfileSSCI), col = adjustcolor("tomato", alpha.f = 0.25))

legend(
  "topright", # Position of the legend on the plot
  legend = c("Bimodal vs. Bilateral", "Bimodal vs. DSHA", "Bimodal vs SSCI"), # Text labels for the other differences
  fill = c("#7fc97f", "#386cb0", "tomato") # Colors for the other differences
)


plot(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileBilateral), col = "#7fc97f",  main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileBilateral), col = rgb(0, 1, 0, alpha = 0.45))

lines(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileDSHA), col = "#386cb0")
polygon(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileDSHA), col = adjustcolor("#386cb0", alpha.f = 0.45))

lines(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileBimodal), col = "tomato")
polygon(density(post_sample$b_ProfileSSCI - post_sample$b_ProfileBimodal), col = adjustcolor("tomato", alpha.f = 0.25))


legend(
  "topright", # Position of the legend on the plot
  legend = c("SSCI vs. Bilateral", "SSCI vs. DSHA", "SSCI vs Bimodal"), # Text labels for the other differences
  fill = c("#7fc97f", "#386cb0", "tomato") # Colors for the other differences
)

# Summary of differences between the two levels - 
c(
  Estimate = mean(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal),
  quantile(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal, probs = c(0.025, 0.975)) #credibility intervals, not confidence
)

# Probability of 'Bilateral' being smaller than 'Bimodal' aka the chances of a bilateral to perform worse than bimodal - 0.5 => no difference 
mean((post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal) <= 0)
mean((post_sample$b_ProfileBilateral - post_sample$b_ProfileSSCI) <= 0)
mean((post_sample$b_ProfileBilateral - post_sample$b_ProfileDSHA) <= 0) 

mean((post_sample$b_ProfileBimodal - post_sample$b_ProfileBilateral) <= 0)
mean((post_sample$b_ProfileBimodal - post_sample$b_ProfileSSCI) <= 0) 
mean((post_sample$b_ProfileBimodal - post_sample$b_ProfileDSHA) <= 0) 

mean((post_sample$b_ProfileSSCI - post_sample$b_ProfileBilateral) <= 0) 
mean((post_sample$b_ProfileSSCI - post_sample$b_ProfileBimodal) <= 0)
mean((post_sample$b_ProfileSSCI - post_sample$b_ProfileDSHA) <= 0) 

mean((post_sample$b_ProfileDSHA - post_sample$b_ProfileBimodal) <= 0)
mean((post_sample$b_ProfileDSHA - post_sample$b_ProfileSSCI) <= 0)
mean((post_sample$b_ProfileDSHA - post_sample$b_ProfileBilateral) <= 0)

mean((post_sample$b_ProfileBimodal - post_sample$b_ProfileSSCI) <= 0)
mean((post_sample$b_ProfileBimodal - post_sample$b_ProfileBilateral) <= 0)

mean((post_sample$b_ProfileSSCI - post_sample$b_ProfileBilateral) <= 0) 



# Plot the density for the first difference
plot(
  density(post_sample$b_ProfileBilateral - post_sample$b_ProfileDSHA),
  col = "#7fc97f",
  main = "Posterior samples",
  xlab = ""
)
polygon(
  density(post_sample$b_ProfileBilateral - post_sample$b_ProfileDSHA),
  col = rgb(0, 1, 0, alpha = 0.25)
)

# Add legend for the first difference
legend(
  "topright", # Position of the legend on the plot
  legend = "Bilateral vs. DSHA", # Text label for the first difference
  fill = "#7fc97f" # Color used for the first difference
)

# Add lines and polygons for the second difference
lines(
  density(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal),
  col = "#386cb0"
)
polygon(
  density(post_sample$b_ProfileBilateral - post_sample$b_ProfileBimodal),
  col = adjustcolor("#386cb0", alpha.f = 0.25)
)

# Add legend entries for the other differences
legend(
  "topright", # Position of the legend on the plot
  legend = c("Bilateral vs. DSHA", "Bilateral vs. Bimodal"), # Text labels for the other differences
  fill = c("#7fc97f", "#386cb0") # Colors for the other differences
)

# --------- INSTRUMENTS

#
plot(density(post_sample$b_InstrumentsAccordion), col = "#fdb462", main = "Posterior samples", xlab = "")
polygon(density(post_sample$b_InstrumentsAccordion), col = adjustcolor("#fdb462", alpha.f = 0.25))

lines(density(post_sample$'b_InstrumentsAccordion&Bass'), col = "#386cb0")
polygon(density(post_sample$'b_InstrumentsAccordion&Bass'), col = adjustcolor("#386cb0", alpha.f = 0.25))

lines(density(post_sample$'b_InstrumentsPiano&Bass'), col = "#7fc97f")
polygon(density(post_sample$'b_InstrumentsPiano&Bass'), col = rgb(0, 1, 0, alpha = 0.25))


par(mfrow = c(2, 2))

#
plot(density(post_sample$'b_InstrumentsAccordion&Bass' - post_sample$b_InstrumentsAccordion), col = "#fdb462", main = "Posterior samples", xlab = "")
polygon(density(post_sample$'b_InstrumentsAccordion&Bass' - post_sample$b_InstrumentsAccordion), col = adjustcolor("#fdb462", alpha.f = 0.25))

lines(density(post_sample$'b_InstrumentsAccordion&Bass' - post_sample$'b_InstrumentsPiano&Bass'), col = "#386cb0")
polygon(density(post_sample$'b_InstrumentsAccordion&Bass' - post_sample$'b_InstrumentsPiano&Bass'), col = adjustcolor("#386cb0", alpha.f = 0.25))


plot(density(post_sample$'b_InstrumentsPiano&Bass' - post_sample$'b_InstrumentsAccordion&Bass'), col = "#fdb462", main = "Posterior samples", xlab = "")
polygon(density(post_sample$'b_InstrumentsPiano&Bass' - post_sample$'b_InstrumentsAccordion&Bass'), col = adjustcolor("#fdb462", alpha.f = 0.25))

lines(density(post_sample$'b_InstrumentsPiano&Bass' - post_sample$b_InstrumentsAccordion), col = "#386cb0")
polygon(density(post_sample$'b_InstrumentsPiano&Bass' - post_sample$b_InstrumentsAccordion), col = adjustcolor("#386cb0", alpha.f = 0.25))



#Probability about instruments
mean((post_sample$'b_InstrumentsAccordion&Bass' - post_sample$b_InstrumentsAccordion) <= 0)
mean((post_sample$'b_InstrumentsAccordion&Bass' - post_sample$'b_InstrumentsPiano&Bass') <= 0)

mean((post_sample$b_InstrumentsAccordion - post_sample$'b_InstrumentsAccordion&Bass') <= 0)
mean((post_sample$b_InstrumentsAccordion - post_sample$'b_InstrumentsPiano&Bass') <= 0)

mean((post_sample$'b_InstrumentsPiano&Bass' - post_sample$'b_InstrumentsAccordion&Bass') <= 0)
mean((post_sample$'b_InstrumentsPiano&Bass' - post_sample$b_InstrumentsAccordion) <= 0)


#BLEAH!
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
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
custom_scheme <- c("#ffeac2", "#ffff33",
                   "tomato", "#984ea3",
                   "#386cb0", "#7fc97f")


posterior <- as.matrix(lr_brm_mi_a)

#plot_title <- ggtitle("Posterior distributions for each hearing profile, with medians and 80% intervals")
color_scheme_set(custom_scheme)
hearingProfilesProbs <-mcmc_areas(posterior,
                                 pars = c("b_ProfileDSHA","b_ProfileBimodal", "b_ProfileSSCI","b_ProfileBilateral" ),
                                 prob = 0.8)+
                                  ggplot2::scale_y_discrete(labels = c("b_ProfileDSHA" ="DSHA",
                                                                       "b_ProfileBimodal" = "Bimodal",
                                                                       "b_ProfileSSCI" = "SSCI",
                                                                       "b_ProfileBilateral" = "Bilateral"),
                                                            limits = c("b_ProfileDSHA",
                                                                       "b_ProfileBimodal",
                                                                       "b_ProfileSSCI",
                                                                       "b_ProfileBilateral"))+
                                                            theme_Publication()
hearingProfilesProbs
#ggsave(paste(hearingProfilesProbs,".png",sep=""), plot=hearingProfilesProbs, height=700, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')
print("saved")

color_scheme_set(custom_scheme)
intsruments_probs <-mcmc_areas(posterior,
                                  pars = c("b_InstrumentsAccordion","b_InstrumentsAccordion&Bass", "b_InstrumentsPiano&Bass"),
                                  prob = 0.8)+
  ggplot2::scale_y_discrete(labels = c("b_InstrumentsAccordion" ="Accordion",
                                       "b_InstrumentsAccordion&Bass" = "Accordion
                                                                        and Bass",
                                       "b_InstrumentsPiano&Bass" = "Piano
                                                                    and Bass"),
                            limits = c("b_InstrumentsPiano&Bass",
                                       "b_InstrumentsAccordion",
                                       "b_InstrumentsAccordion&Bass"))+
                            theme_Publication()
intsruments_probs
#ggsave(paste(intsruments_probs,".png",sep=""), plot=intsruments_probs, height=700, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')
#########################
### no-normal hearing ###
#########################


# Collected dataframe
mci_data_no_nh <- mci_data_raw %>% 
  left_join(mci_question_id, by = "Question") %>% # Joining data to a single dataframe
  filter(Profile != "Unknown") %>% # Removed the single unknown contributor
  filter(Profile != "NH") %>% # Removed the single unknown contributor
  select(ID, Profile, Instruments, Age, Key, Question, Answer, Contours) %>% 
  mutate( # Creating 0/1 for crr
    Correct = as.numeric(Answer == Contours)
  ) %>%
  mutate( # THIS IS THE REFFERNCE FOR CONSTRAST ENCODING
    Instruments = relevel(factor(Instruments), ref = "Piano"),
    Profile = relevel(factor(Profile), ref = "DSHA"),
    Contours = relevel(factor(Contours), ref = "Ascending")
  )

# This shows repeated measures => random/mixed effects
mci_data_no_nh %>% 
  group_by(ID, Instruments, Profile, Contours) %>% 
  summarise(N = n())
## Creating multiple data-set due to missing values (multiple imputation)
# Seed is set to get consistent results, and m = 5 is the norm. 
mci_data_imp_noNH <- mice(mci_data_no_nh, m = 5, seed = 123456, printFlag = FALSE)

lr_brm_nonh_mixed <- brm_multiple(
  Correct ~ Profile * Instruments * Contours + Age + (1 | ID), 
  family = bernoulli(link = "logit"), 
  data = mci_data_imp_noNH,
  warmup = 1500,
  iter = 3000,
  chains = 4,
  cores = 4
)

lr_brm_nonh_a <- brm_multiple(
  Correct ~ Profile + Instruments + Contours + Age + (1 | ID), 
  family = bernoulli(link = "logit"), 
  data = mci_data_imp_noNH,
  warmup = 1500,
  iter = 3000,
  chains = 4,
  cores = 4
)

(loo_lr_brm_nonh_mixed <- loo(lr_brm_nonh_mixed))
(loo_lr_brm_nonh_a <- loo(lr_brm_nonh_a))

loo_compare(loo_lr_brm_nonh_mixed, loo_lr_brm_nonh_a)
fixef(lr_brm_nonh_a)
post_nonh = as.matrix(lr_brm_nonh_a)

color_scheme_set(custom_scheme)
intsruments_probs_nonh <-mcmc_areas(post_nonh,
                               pars = c("b_InstrumentsAccordion","b_InstrumentsAccordion&Bass", "b_InstrumentsPiano&Bass"),
                               prob = 0.8)+
  ggplot2::scale_y_discrete(labels = c("b_InstrumentsAccordion" ="Accordion",
                                       "b_InstrumentsAccordion&Bass" = "Accordion 
                                                                        and Bass",
                                       "b_InstrumentsPiano&Bass" = "Piano 
                                                                    and Bass"),
                            limits = c("b_InstrumentsPiano&Bass",
                                       "b_InstrumentsAccordion",
                                       "b_InstrumentsAccordion&Bass"))+theme_Publication()
intsruments_probs_nonh
ggsave("instrument_probs_no_nh.png", plot=intsruments_probs_nonh, height=700, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')

color_scheme_set(custom_scheme)
hearingProfilesProbsNoNh <-mcmc_areas(post_nonh,
                                  pars = c("b_ProfileBimodal", "b_ProfileSSCI","b_ProfileBilateral" ),
                                  prob = 0.8)+
  ggplot2::scale_y_discrete(labels = c("b_ProfileBimodal" = "Bimodal",
                                       "b_ProfileSSCI" = "SSCI",
                                       "b_ProfileBilateral" = "Bilateral"),
                            limits = c("b_ProfileBimodal",
                                       "b_ProfileSSCI",
                                       "b_ProfileBilateral"))+theme_Publication()
hearingProfilesProbsNoNh
ggsave("profile_probs_no_nh.png", plot=hearingProfilesProbsNoNh, height=700, width=1000, scale = 2, units=c("px"), dpi=300, type = 'cairo')
