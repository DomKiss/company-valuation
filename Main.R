library(rstudioapi)
library(dplyr)
library(ggplot2)

#import data
comp_data<-tibble(readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/comp_data.xlsx", sep=""))) #read excel


#changing column names
comp_data$PE <- comp_data$`P/E`
comp_data$EVEBITDA <- comp_data$`EV/EBITDA`
comp_data$Main_company <- comp_data$`Main company`
comp_data$Growth <- comp_data$`Growth (last year)`
comp_data$Revenue <- comp_data$`Revenue (millions)`


#function for the regressions
linear_regression_model <- function(comp_data, company, ratio, Revenue=1, Beta=1){
  
  comp_data2 <- comp_data %>% filter(Main_company == company)
  
  if (ratio == "PE") {
    ratio2 = comp_data2$PE
  } else{
    ratio2 = comp_data2$EVEBITDA
  }
  
  
  if (Revenue == 1) {
    model <- lm(ratio2 ~ Growth + Revenue + Beta, data = comp_data2)
  } else if(Beta==1){
    model <- lm(ratio2 ~ Growth +Beta, data = comp_data2)
  }else{
    model <- lm(ratio2 ~ Growth, data = comp_data2)
  }
  
  return(model)
}


#P/E

#chipotle regression and summary with all 3 variables: growth, revenue and beta
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE")
summary(lm_chip)

#jolibee regression and summary with all 3 variables: growth, revenue and beta
lm_jol <- linear_regression_model(comp_data,"Jolibee", "PE")
summary(lm_jol)


#!! final model !! chipotle regression without revenues (because revenues seem to be unsignificant)
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE", Revenue=0)
summary(lm_chip)


#jolibee regression without revenues (because revenues seem to be unsignificant)
lm_jol <- linear_regression_model(comp_data,"Jolibee", "PE", Revenue=0)
summary(lm_jol)


#chipotle regression growth only
lm_chip <- linear_regression_model(comp_data,"Chipotle", "PE", Revenue=0, Beta=0)
summary(lm_chip)

#jollibee regression without revenues and growth
lm_jol <- linear_regression_model(comp_data,"Jolibee", "PE", Revenue=0, Beta=0)
summary(lm_jol)


meanPE <- comp_data %>% filter(Main_company=="Chipotle")
meanPE_jol <- comp_data %>% filter(Main_company=="Jolibee")
jolibee_meanPE <- mean(meanPE_jol$PE)
chipotle_meanPE <- mean(meanPE$PE)

jolibee_mean_ev <- mean(meanPE_jol$EVEBITDA)
chipotle_mean_ev <- mean(meanPE$EVEBITDA)


#EV/EBITDA


#chipotle regression and summary
lm_chip <- linear_regression_model(comp_data,"Chipotle", "EVEBITDA")
summary(lm_chip)

#jolibee regression and summary
lm_jol <- linear_regression_model(comp_data,"Jolibee", "EVEBITDA")
summary(lm_jol)


#chipotle regression without revenues (because revenues seem to be unsignificant)
lm_chip <- linear_regression_model(comp_data,"Chipotle", "EVEBITDA", Revenue=0)
summary(lm_chip)


#jolibee regression using growth and beta only
lm_jol <- linear_regression_model(comp_data, company="Jolibee", "EVEBITDA",Revenue=0)
summary(lm_jol)

#jolibee regression Revenue and growth only
joldata <- comp_data %>% filter(Main_company == company)
lm_jol <- lm(EVEBITDA~Revenue+Growth, data=joldata)
summary(lm_jol)

#chipotle regression without revenues and growth
lm_chip <- linear_regression_model(comp_data,"Chipotle", "EVEBITDA", Revenue=0, Beta=0)
summary(lm_chip)

#jollibee regression without revenues and growth
lm_jol <- linear_regression_model(comp_data,"Jolibee", "EVEBITDA", Revenue=0, Beta=0)
summary(lm_jol)







