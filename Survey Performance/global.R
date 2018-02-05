library(shinydashboard)
library(shiny)
library(BH)
library(dplyr)
library(ggplot2)
library(plotly)

#calls<-read.csv("./Data/All_Spec_CallTracker-v1.0.xlsx - Call_Tracker.csv")
weekly<-read.csv("./Data/All_Spec_CallTracker-v1.0.xlsx - WeeklyProgress.csv")
survey<-read.csv("./Data/All_Spec_Questionnaire.csv" , stringsAsFactors = FALSE); survey = survey[3:nrow(survey),]

#product_search_1: comments_product_sea : How easy is it for you to search and find the product
#checkout_process_1 : comments_checkout_pr: How easy is it for you to navigate the checkout process?
#shipping_options_1 : comments_shipping_op : How easy is it to choose your shipping options?
#NES_overall_1 : comments_overall : Overall how easy is it to find exactly what you need on the All-Spec website?
#overall_experience_1 : comments_overall_exp :What is your overall customer experience with All-Spec?
#ship_dates_1 :comments_ship_dates : How satisfied are you with the expected ship dates?
#freight_cost_1 : comments_frieght_cos : How satisfied are you with the freight cost?
#product_price_1 : comments_prics : How satisfied are you with the prices of the products?
#product_availablity_1 : comments_product_ava : When you go to place an order is the product available and in the quantities you need?
#If you ever needed help using the Live Chat function or customer service, how helpful was it?
# overall_NPS_NPS_GROUP : 
# overall_NPS : comments_overall_NPS : On a scale from 0-10, how likely are you to recommend All-Spec to a friend or colleague?

survey = survey %>% 
  select(name, email_ID , position, overall_NPS_NPS_GROUP,product_search_1 , checkout_process_1 , shipping_options_1, NES_overall_1,overall_experience_1, ship_dates_1 , freight_cost_1,product_price_1,product_availablity_1,overall_NPS,
         comments_product_sea , comments_checkout_pr , comments_shipping_op)

initiated = sum(weekly[nrow(weekly), c("Completed", "Does.Not.remember","Follow.up" , "No.response" , "Partially.Completed" , "Wrong.Person")])
perc =initiated*100/weekly$Grand.Total[nrow(weekly)]
progress= data.frame(y = "Survey Status" , Initiated =  round(perc, 2) , Not_Initiated = 100 - round(perc,2))

initiated_df = data.frame( type = c("Completed", "Does.Not.remember","Follow.up" , "No.response", "Partially.Completed", "Wrong.Person") , count = t(weekly[nrow(weekly), c("Completed","Does.Not.remember","Follow.up" , "No.response", "Partially.Completed", "Wrong.Person")])[,1])

weekly$Week = as.numeric(weekly$Week)
weekly_tracker = weekly[1,]
for(i in 2 : nrow(weekly)){

  a = weekly[i, c("Completed" ,"Does.Not.remember" ,"Follow.up","No.response" , "Open" , "Partially.Completed", "Wrong.Person", "Grand.Total"  , "Total...of.Calls" )] - weekly[i-1, c("Completed","Does.Not.remember"  ,"Follow.up","No.response" , "Open" , "Partially.Completed", "Wrong.Person", "Grand.Total"  , "Total...of.Calls" )]
  weekly_tracker[i,] = c(as.numeric(i) , a)
}
weekly_tracker$Grand.Total=NULL
weekly_tracker$Open=NULL
weekly_tracker[,2:8][weekly_tracker[,2:8]<0] = 0
weekly_tracker$Initiated.Calls = rowSums(weekly_tracker[,2:6])

weekly_tracker = weekly_tracker %>% 
  mutate( Success.rate = round(Completed*100/Initiated.Calls,2))

nrows <- 25
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(weekly[nrow(weekly), 2:8] * ((nrows*nrows)/weekly$Grand.Total[nrow(weekly)]))
#df$category <- factor(rep(names(categ_table), categ_table))  
# NOTE: if sum(categ_table) is not 100 (i.e. nrows^2), it wi


diamonds = survey %>% 
  select(product_search_1 , checkout_process_1 , shipping_options_1, NES_overall_1,overall_experience_1, ship_dates_1 , freight_cost_1,product_price_1,product_availablity_1,overall_NPS) %>% 
  tidyr::gather("Questions", "n", c(product_search_1 , checkout_process_1 , shipping_options_1, NES_overall_1,overall_experience_1, ship_dates_1 , freight_cost_1,product_price_1,product_availablity_1,overall_NPS)) %>%
  dplyr::mutate( n = as.numeric(n) , cut = ifelse(Questions %in% c("product_search_1" ,"checkout_process_1" , "shipping_options_1", "NES_overall_1"), "NES" , "NPS"))

diamonds$Questions[ diamonds$Questions == "product_search_1"] = "How easy is it for you to search and find the product?"
diamonds$Questions[ diamonds$Questions == "checkout_process_1"] = "How easy is it for you to navigate the checkout process?"
diamonds$Questions[ diamonds$Questions == "shipping_options_1"] = "How easy is it to choose your shipping options?"
diamonds$Questions[ diamonds$Questions == "NES_overall_1"] = "Overall how easy is it to find exactly what you need on the All-Spec website?"
diamonds$Questions[ diamonds$Questions == "overall_experience_1"] ="What is your overall customer experience with All-Spec?"
diamonds$Questions[ diamonds$Questions == "ship_dates_1"] = "How satisfied are you with the expected ship dates?"
diamonds$Questions[ diamonds$Questions == "freight_cost_1"] = "How satisfied are you with the freight cost?"
diamonds$Questions[ diamonds$Questions == "product_price_1"] = "How satisfied are you with the prices of the products?"
diamonds$Questions[ diamonds$Questions == "product_availablity_1"] = "When you go to place an order is the product available and in the quantities you need?"
diamonds$Questions[ diamonds$Questions == "overall_NPS"] = "How likely are you to recommend All-Spec to a friend or colleague?"


NPS_groups = survey %>% 
  select(overall_NPS_NPS_GROUP) %>% 
  dplyr::group_by(overall_NPS_NPS_GROUP) %>% 
  summarise(Count = n())

NES_groups = survey %>% 
  select(NES_overall_1) %>% 
  mutate( NES_overall_1 = as.numeric(NES_overall_1)) %>% 
  transmute(nes_Group = ifelse(NES_overall_1 < 4 , "Difficult" , ifelse(NES_overall_1 == 4, "Neutral" , "Easy"))) %>% 
  dplyr::group_by(nes_Group) %>% 
  summarise(Count = n())

