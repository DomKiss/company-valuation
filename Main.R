library(rstudioapi)
library(dplyr)
library(ggplot2)

#import data
comp_data<-tibble(readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "comp_data.xlsx", sep=""))) #read excel

#change column names
comp_data$PE <- comp_data$`P/E`
comp_data$EVEBITDA <- comp_data$`EV/EBITDA`
comp_data$Main_company <- comp_data$`Main company`
comp_data$Growth <- comp_data$`Growth (last year)`
comp_data$Revenue <- comp_data$`Revenue (millions)`


#chipotle regression and summary
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE")
summary(lm_chip)

#jolibee regression and summary
lm_jol <- linear_regression_model(comp_data,"Jolibee", "PE")
summary(lm_jol)


# !! Final model for chipotle !!
#chipotle regression without revenues (because revenues seem to be unsignificant)
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE", Revenue=0)
summary(lm_chip)


#jolibee regression without revenues (because revenues seem to be unsignificant)
lm_chip <- linear_regression_model(comp_data,"Jolibee", "PE", Revenue=0)
summary(lm_chip)


#chipotle regression without revenues and growth
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE", Revenue=0, Growth=0)
summary(lm_chip)

#chipotle regression without revenues and growth
lm_chip <- linear_regression_model(comp_data,"Jolibee", "PE", Revenue=0, Growth=0)
summary(lm_chip)





