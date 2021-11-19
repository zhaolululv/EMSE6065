# Make conjoint surveys using the conjointTools package

# Install packages
# install.packages("fastDummies")
# install.packages("remotes")
#remotes::install_github("jhelvy/conjointTools")

# Load libraries
library(logitr)
library(tidyverse)
library(conjointTools)
library(fastDummies)
library(here)
library(cowplot)
`%notin%` <- Negate(`%in%`)

# Define the attributes and levels
levels <- list(
  price       = seq(10000,50000,5000),   # Price ($5,000)
  fuelEconomy = seq(15,35,5),   # Fuel economy (mpg)
  modelyear   = seq(2014,2019,1),
  powertrain  = c("Gas", "Electric"), 
  Warranty    = c("Yes","No"),
  range       = c( 200, 250,300,350,400), # EV driving range
  Mileage     = seq(10000,70000,1000),
  ChargingStation = c("Free","Discounted","No Promotion")
  )

# Make a full-factorial design of experiment
doe <- makeDoe(levels)
dim(doe)
head(doe) # preview

# Recode the design of experiment
doe <- recodeDoe(doe, levels)
head(doe) # preview

# Make a labeld survey with "powertrain" as the label
survey_labeled <- makeSurvey(
    doe       = doe,
    nResp     = 100000,
    nAltsPerQ = 3,
    nQPerResp = 8,
    #group     = "powertrain"
)
head(survey_labeled) # preview

survey_labeled <- dummy_cols(survey_labeled,"powertrain")

bad_doe <- survey_labeled %>% 
  mutate(year=2021-modelyear,
         flag=ifelse(Mileage>=year*10000+20000,1,0)) %>%
  filter(flag==1) %>% 
  distinct(respID)
  
survey_labeled <- survey_labeled %>% 
  filter(respID%notin%bad_doe$respID) 
for (r in 1:nrow(survey_labeled)) {
    survey_labeled[r,"respID"] <-  (r-1)%/%24+1 
}  

survey_labeled <- survey_labeled %>%
  mutate(ChargingStation=ifelse(powertrain_Electric==1,ChargingStation,"No Promotion"),
         range=ifelse(powertrain_Electric==1,range,0),
         fuelEconomy=ifelse(powertrain_Electric==0,fuelEconomy,0))


head(survey_labeled)

# Save design
write_csv(survey_labeled, here('choice_questions.csv'))


# power analysis

survey_labeled <- survey_labeled %>%
  mutate(range=range-100,
         fuelEconomy=fuelEconomy-15,
         modelyear=2021-modelyear,
         price=price/1000,
         Mileage=Mileage/1000)

data <- simulateChoices(
  survey = survey_labeled,
  obsID  = "obsID"
)
head(data)

models <- estimateModels(
  nbreaks = 10,
  data    = data,
  pars    = c("price","fuelEconomy","modelyear","powertrain","Warranty","range","Mileage","ChargingStation"),
  outcome = "choice",
  obsID   = "obsID"
)

results <- getModelResults(models)
head(results)

ggplot(results) +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(aes(x = sampleSize, y = se, color = coef)) +
  expand_limits(y = 0) +
  theme_bw() + 
  labs(
    x = "Sample size", 
    y = "Standard error", 
    color = "Coefficient"
  )

# Model Estimation
source(here::here("data_cleaning.R"))

data <- read_csv(here("data", "choiceData.csv"))


data <- data %>%
  mutate(range=range-100,
         fuelEconomy=fuelEconomy-15,
         modelyear=2021-modelyear,
         price=price/1000,
         Mileage=Mileage/1000)

model <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    "price",
    "fuelEconomy",
    "modelyear",
    "powertrain",
    "Warranty",
    "range",
    "Mileage",
    "ChargingStation")
)
summary(model)

model_result <- data.frame(coef(model))
std <- data.frame(sqrt(diag(vcov(model))))
model_result <- cbind(vars = rownames(model_result), model_result,std)
rownames(model_result) <- NULL
names(model_result) <- c("vars","coef","std")

model_result %>% 
  rename(
    estimates=coef,
    stand_error=std
  ) %>% 
  mutate(
    vars = fct_reorder(vars,estimates),
    vars = fct_recode(vars,
                      "Price"="price",
                      "Fuel Economy"="fuelEconomy",
                      "Model Year"="modelyear",
                      "Is Gas Car"="powertrainGas",
                      "Have Warranty"="WarrantyYes",
                      "Range"="range",
                      "Free Charging Station"="ChargingStationFree",
                      "No Charging Station Promotion"="ChargingStationNo Promotion")
  ) %>% 
  ggplot()+
  geom_point(aes(x=estimates, y=vars),position = position_dodge(width = 0.3))+
  geom_errorbar(
    aes(xmax = estimates + 2*stand_error, 
        xmin = estimates - 2*stand_error,
        y=vars,
        width=0.3),position=position_dodge(width = 0.3))+
  geom_vline(aes(xintercept = 0), color='red', linetype='dashed') +
  theme_minimal_vgrid()+
  scale_x_continuous(
    #limits = c(-10,7),
    expand = expansion(mult = c(0,0.05)),
    #labels = scales::dollar
  ) +
  theme(strip.background = element_rect("grey"),
        legend.position = "none") +
  labs(y="",
       x="Estimation result")
ggsave("estimation_result.png", height = 6,width =7.5 )
