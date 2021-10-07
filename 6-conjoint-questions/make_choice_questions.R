# Make conjoint surveys using the conjointTools package

# Install packages
# install.packages("fastDummies")
# install.packages("remotes")
# remotes::install_github("jhelvy/conjointTools")

# Load libraries
library(tidyverse)
library(conjointTools)
library(fastDummies)
library(here)

# Define the attributes and levels
levels <- list(
  price       = seq(5000,50000,1000),   # Price ($1,000)
  #fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
  modelyear   = c(1, 2, 3,4,5),      # 0-60 mph acceleration time (s)
  powertrain  = c("Gasoline", "Electric"), 
  brand       = c("Honda","Tesla","Mercedes"),
  Warranty    = c("Yes","No"),
  range       = c(100, 150, 200, 250), # EV driving range
  Mileage     = seq(5000,50000,1000),
  ChargingStation = c("free","discounted","paid")
  )

# Make a full-factorial design of experiment
doe <- makeDoe(levels)
head(doe) # preview

# Recode the design of experiment
doe <- recodeDesign(doe, levels)
head(doe) # preview



# Make a basic survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 2,    # Number of alternatives per question
    nQPerResp = 4     # Number of questions per respondent
)
head(survey) # preview

# Make a labeld survey with "powertrain" as the label
survey_labeled <- makeSurvey(
    doe       = doe,
    nResp     = 1000,
    nAltsPerQ = 2,
    nQPerResp = 4,
    group     = "powertrain"
)
head(survey_labeled) # preview

survey_labeled <- dummy_cols(survey_labeled,"powertrain")
survey_labeled <- survey_labeled %>%
  mutate(ChargingStation=ifelse(powertrain_Electric==1,ChargingStation,NA))
head(survey_labeled)

# Make a survey with outside good
survey_og <- makeSurvey(
    doe       = doe,
    nResp     = 1000,
    nAltsPerQ = 3,
    nQPerResp = 8,
    outsideGood = TRUE
)
head(survey_og) # preview

# Save design
write_csv(survey, here('choice_questions.csv'))
