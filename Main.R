library(rstudioapi)
library(dplyr)
library(ggplot2)

#import data
comp_data<-tibble(readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/comp_data_alteo.xlsx", sep=""))) #read excel


#changing column names
comp_data$PE <- comp_data$`P/E`
comp_data$EVEBITDA <- comp_data$`EV/EBITDA`
comp_data$Main_company <- comp_data$`Main company`
comp_data$Growth <- comp_data$`    RevYoY Growth %`
comp_data$Revenue <- comp_data$Revenue


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

#alteo regression and summary with all 3 variables: growth, revenue and beta
lm_chip <- lm(PE ~ Growth + Revenue + Beta, data = comp_data)
summary(lm_chip)





