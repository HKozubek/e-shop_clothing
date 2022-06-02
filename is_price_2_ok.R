library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
library(tidyr)
library(lubridate)
library(padr)
library(zoo)
library(janitor)
library(readr)

rm(list=ls())
ls()

df<- read_csv2("e_shop_clothing_2008.csv")


colnames(df)<-c('year','month','day','order','country','id','page1','page2','colour','location','model','price','price2','page')


ggplot(filter(df,page1==1), aes(x=page2, y=location))+
  geom_point(alpha = 0.2)+ theme(legend.position="none")+theme_dark()

??read_csv2

filter(df,page1==3|page1==3)

#View(df)
wynik<-df %>% 
  select('order','country','session ID','page 1 (main category)','page 2 (clothing model)','price','price 2')
wynik<-df %>% 
  group_by(id) %>% 
  mutate(count_=mean(day)) %>% 
  ungroup() %>% 
#  mutate(price2_T = case_when(price <= mean_price  ~ 2,
#                              price > mean_price  ~ 1)) %>% 
  mutate(ok=case_when(mean_day == day  ~ "Tak",
                      mean_day != day  ~ "Nie"))



tabyl(wynik,price,page2)
