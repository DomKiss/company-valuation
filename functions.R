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


company="Chipotle"
chip_data <- comp_data %>% filter(Main_company == company)
chip_data
