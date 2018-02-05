library(shinydashboard)
library(shiny)
library(BH)
library(dplyr)
library(ggplot2)
library(plotly)

#calls<-read.csv("./Data/AllContacts.csv")
calls<-read.csv("./Data/OfficialSMBCallList.csv" , stringsAsFactors = F)

calls$Call.Status[calls$Call.Status==""] ="Open"
new= nrow(calls %>% 
            select(Contact.Type) %>%
            filter(Contact.Type=="New"))

initiated = calls %>% 
  select(Call.Status) %>%
  filter(Call.Status!="Open") %>% 
  dplyr::group_by(Call.Status) %>% 
  summarise(Count = n())


perc = sum(initiated$Count)*100/nrow(calls)
progress= data.frame(y = "Call Status" , Initiated =  round(perc, 2) , Not_Initiated = 100 - round(perc,2))

Companies_called = nrow(calls %>% 
                          filter(Call.Status!="Open") %>% 
                          select(Company.Name) %>%
                          dplyr::group_by(Company.Name) %>% 
                          count())
companies_total =nrow(calls %>% 
                        select(Company.Name) %>%
                        dplyr::group_by(Company.Name) %>% 
                        count())
perc_comp = Companies_called*100/companies_total
progress_companies =data.frame(y = "Companies Status" , Initiated =  round(perc_comp, 2) , Not_Initiated = 100 - round(perc_comp,2))

leads = calls %>% 
  filter(Call.Status=="Lead") %>% 
  select(Lead...Status) %>%
  dplyr::group_by(Lead...Status) %>% 
  summarise(Count = n())


follow_up = calls %>% 
  filter(Call.Status=="Follow-up") %>% 
  select(Follow...Up) %>%
  dplyr::group_by(Follow...Up) %>% 
  summarise(Count = n())

calls$Phone.Type[calls$Phone.Type == " "] = "Not Sure"
phone_type = calls %>% 
  filter(Call.Status!="Open") %>% 
  select(Phone.Type) %>%
  dplyr::group_by(Phone.Type) %>% 
  summarise(Count = n())


calls$Point.of.Contact.Status[calls$Point.of.Contact.Status == ""] = "Not Sure"
important = calls %>% 
  filter(Call.Status!="Open") %>%
  select(Point.of.Contact.Status) %>%
  dplyr::group_by(Point.of.Contact.Status) %>% 
  summarise(Count = n())

calls = calls %>% 
  mutate(rightPOC = ifelse(Point.of.Contact.Status == "Important" , 1 , 0))
important_companies = calls %>% 
  filter(Call.Status!="Open") %>%
  dplyr::group_by(Company.Name) %>% 
  summarise(imp = max(rightPOC)) %>% 
  select(imp) %>% 
  dplyr::group_by(imp) %>% 
  summarise(Count = n())
important_companies$imp[important_companies$imp=="0"] ="Right POC not Identified"
important_companies$imp[important_companies$imp=="1"] ="Right(Important) POC Identified"
  
  
