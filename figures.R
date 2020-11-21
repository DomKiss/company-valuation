library(ggplot2)
#import data
comp_data<-tibble(readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/comp_data.xlsx", sep=""))) #read excel

#change column names
comp_data$PE <- comp_data$`P/E`
comp_data$EVEBITDA <- comp_data$`EV/EBITDA`
comp_data$Main_company <- comp_data$`Main company`
comp_data$Growth <- comp_data$`Growth (last year)`
comp_data$Revenue <- comp_data$`Revenue (millions)`
comp_data$Market_cap <- comp_data$`Market cap (m) (millions)`



#PE histogram
PE_histogram <- ggplot(comp_data, aes(x=PE))+geom_histogram()+facet_wrap(~Main_company)
show(PE_histogram)


#EV/EBITDA
EVEBITDA_histogram <- ggplot(comp_data, aes(x=EVEBITDA))+geom_histogram()+facet_wrap(~Main_company)
show(EVEBITDA_histogram)

#Growth
PE_histogram <- ggplot(comp_data, aes(x=Growth))+geom_histogram()+facet_wrap(~Main_company)
show(PE_histogram)


#Revenue
Rev_histogram <- ggplot(comp_data, aes(x=Revenue))+geom_histogram()+facet_wrap(~Main_company)
show(Rev_histogram)

#Beta
PE_histogram <- ggplot(comp_data, aes(x=Beta))+geom_histogram()+facet_wrap(~Main_company)
show(Beta_histogram)


