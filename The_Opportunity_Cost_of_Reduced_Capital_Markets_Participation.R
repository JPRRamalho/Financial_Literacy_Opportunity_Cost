## 

## #main .nav-pills > li.active > a,

## #main .nav-pills > li.active > a:hover,

## #main .nav-pills > li.active > a:focus {

##    background-color: #22983B;

## }

## 

## #main .nav-pills > li > a:hover {

##   background-color: #008B8B;

## }

## 

## h1,h2,h3,h4,h5,h6,legend{

##     color: #008B8B;

## }

## 

## #nav-top span.glyphicon {

##   color: #008B8B;

## }

## 

## #table-of-contents header{

##     color:#008B8B;

##     background-color: #008B8B;

## }

## 

## #table-of-contents h2{

##     background-color:#22983B;

## }

## 

## a:hover{

##     color:#008B8B

## }

## 

## a:visited{

##     color:#008B8B

## }

## 

## #sidebar {

##     color: #D9AB16;

##     background: #113458;

## }

## 

## #sidebar h2 {

##     color: #FFFFFF;

##     background-color: #4096B8;

## }

## 

## #sidebar a {

##     color: #FFFFFF;

## }

## 

## #sidebar a:hover {

##     background-color: #4096B8;

##     color: #FFFFFF;

## }


## #main a:link {

##   color: #5A7B9C;

## }

## 

## /* visited link */

## #main a:visited {

##   color: #095484;

## }

## 

## /* mouse over link */

## #main a:hover {

##   color: #8ebf42;

## }

## 

## /* selected link */

## #main a:active {

##   color: #716464;

## }

## 


## h1 {

##   text-align: center;

## }

## 

##     #content{

##         max-width:100%;

##     }

##     /* Adjust html width */


## ---- include=FALSE----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, error=TRUE)


## ---- include=FALSE----------------------------------------------------
# Working Directory Setting
setwd("./")


## ---- include=FALSE----------------------------------------------------
## Datasets Import 
library(readr)
library(janitor)

Household_wealth_Bank_of_Portugal_Long_Series_EUR <- read_delim("./Household_wealth_Bank_of_Portugal_Long_Series_EUR.csv", 
    delim = ";", escape_double = FALSE, col_types = cols("# Série" = col_number(), 
        `Período de referência` = col_date(format = "%d/%m/%Y"), 
        Valor = col_number()), trim_ws = TRUE)
Household_wealth_Bank_of_Portugal_Long_Series_EUR <- clean_names(Household_wealth_Bank_of_Portugal_Long_Series_EUR)

MSCI_World_Net_Total_Return_Index_EUR <- read_delim("./MSCI_World_Net_Total_Return_Index_EUR.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Open = col_number(), 
        High = col_number(), Low = col_number(), 
        Close = col_number()), trim_ws = TRUE)
MSCI_World_Net_Total_Return_Index_EUR <- clean_names(MSCI_World_Net_Total_Return_Index_EUR)

Bloomberg_Global_Aggregate_Total_Return_Index_EUR <- read_delim("./Bloomberg_Global_Aggregate_Total_Return_Index_EUR.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Date = col_character(), 
        Open = col_number(), High = col_number(), 
        Low = col_number(), Close = col_number()), 
    trim_ws = TRUE)
Bloomberg_Global_Aggregate_Total_Return_Index_EUR <- clean_names(Bloomberg_Global_Aggregate_Total_Return_Index_EUR)


## Datasets Wrangling & Calculation
library(tidyr)
library(dplyr)

Household_wealth_Bank_of_Portugal_Long_Series_EUR <- Household_wealth_Bank_of_Portugal_Long_Series_EUR %>% 
  select(-c(estado, metrica, unidade_de_medida, number_serie)) %>% 
  pivot_wider(names_from = "designacao_da_serie_pt",
              values_from= "valor") %>% 
  clean_names() %>% 
  mutate(twenty_percent_total_finantial_assets = 0.2* patrimonio_dos_particulares_ativos_financeiros_total) %>% 
  mutate(percentage_cash_in_portfolio = (patrimonio_dos_particulares_ativos_financeiros_numerario_e_depositos / patrimonio_dos_particulares_ativos_financeiros_total)*100) %>% 
  mutate(excess_cash = patrimonio_dos_particulares_ativos_financeiros_numerario_e_depositos - twenty_percent_total_finantial_assets) %>% 
  mutate(excess_cash_percentage = (excess_cash / patrimonio_dos_particulares_ativos_financeiros_total)*100)

library(parsedate)

MSCI_World_Net_Total_Return_Index_EUR <- MSCI_World_Net_Total_Return_Index_EUR %>% 
  mutate(date = parsedate:::parse_date(date)) %>% 
  select(-c(open, high, low)) %>% 
  filter(as.Date(date) < "2022-01-01") %>% 
  filter(as.Date(date) > "2001-01-01") %>% 
  arrange(date) %>% 
  mutate(year_return = (close - lag(close, n=1))/lag(close, n=1)) %>% 
  filter(as.Date(date) > "2002-01-01") %>% 
  mutate(number = row_number())

Bloomberg_Global_Aggregate_Total_Return_Index_EUR <- Bloomberg_Global_Aggregate_Total_Return_Index_EUR %>% 
  mutate(date = parsedate:::parse_date(date)) %>% 
  select(-c(open, high, low)) %>% 
  filter(as.Date(date) < "2022-01-01") %>% 
  filter(as.Date(date) > "2001-01-01") %>% 
  arrange(date) %>% 
  mutate(year_return = (close - lag(close, n=1))/lag(close, n=1)) %>% 
  filter(as.Date(date) > "2002-01-01") %>% 
  mutate(number = row_number())
  
Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- Household_wealth_Bank_of_Portugal_Long_Series_EUR %>% 
  filter(as.Date(periodo_de_referencia) < "2021-01-01") %>% 
  filter(as.Date(periodo_de_referencia) > "2001-01-01") %>% 
  arrange(periodo_de_referencia) %>% 
  mutate(periodo_de_referencia = as.Date(periodo_de_referencia)+1) %>% 
  mutate(number = row_number())
  
Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- full_join(Household_wealth_Bank_of_Portugal_Long_Series_EUR_2, MSCI_World_Net_Total_Return_Index_EUR, by = "number", suffix =c("","MSCI"))

Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- full_join(Household_wealth_Bank_of_Portugal_Long_Series_EUR_2, Bloomberg_Global_Aggregate_Total_Return_Index_EUR, by = "number", suffix =c("MSCI","Bloom"))


Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 %>% 
  mutate(year_total_return = (0.6 * year_returnMSCI + 0.4 * year_returnBloom))

library(purrr)

Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 %>% 
  mutate(year_return_compounded_cummulative = 
           accumulate2(Household_wealth_Bank_of_Portugal_Long_Series_EUR_2$excess_cash, Household_wealth_Bank_of_Portugal_Long_Series_EUR_2$year_total_return, 
             .init = 0,
              ~ ..2*..3 + (..1*0.8*..3) + ..1)[-1])

## Data Analysis & Visualization
library(PerformanceAnalytics)

mean(Household_wealth_Bank_of_Portugal_Long_Series_EUR$percentage_cash_in_portfolio)

Return.annualized(Household_wealth_Bank_of_Portugal_Long_Series_EUR_2$year_total_return, 1)


Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 <- Household_wealth_Bank_of_Portugal_Long_Series_EUR_2 %>% 
  mutate(year_returnMSCI = year_returnMSCI*100) %>% 
  mutate(year_returnBloom = year_returnBloom*100) %>% 
  mutate(year_total_return = year_total_return*100) %>% 
  mutate(periodo_de_referencia = format(as.Date(periodo_de_referencia, format="%Y/%m/%d"),"%Y"))


library(ggplot2)
library(tidyverse)
library(tidyquant)
library(scales)

dollar <- scales::label_dollar(prefix = "€", suffix = " MM")

plot1 <- ggplot(data=Household_wealth_Bank_of_Portugal_Long_Series_EUR_2, aes(x=periodo_de_referencia,y=year_return_compounded_cummulative))+
  geom_col() +
  geom_text(aes(label = dollar(year_return_compounded_cummulative)), vjust = -1.5) +
  labs(x="Year") +
  ggtitle("C  Potential Missed Cumulative Investment Returns")+
  theme_classic() +
    theme(text = element_text(size = 14),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(size=16, face="bold", hjust = 0.04))


plot2 <- ggplot(data=Household_wealth_Bank_of_Portugal_Long_Series_EUR_2, aes(x=periodo_de_referencia))+
  geom_point(aes(y=year_returnMSCI,  colour = "MSCI World Yearly Return"), size = 4) +
  geom_point(aes(y=year_returnBloom, colour = "Bloomberg Global Aggregate Yearly Return"), size = 4) +
  geom_point(aes(y=year_total_return, colour = "60/40 Portfolio Yearly Return"), size = 4) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_y_continuous(name = "%", breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30), labels = waiver())+
  labs(x="Year")+
  ggtitle("B  Historical Portfolio Returns")+
  theme_bw()+
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title.y = element_text(angle = 180, vjust = 0.5),
          legend.title = element_blank(),
          legend.position="bottom",
          plot.title = element_text(size=16, face="bold"))

plot3 <- ggplot(data=Household_wealth_Bank_of_Portugal_Long_Series_EUR_2, aes(x=periodo_de_referencia))+
   geom_point(aes(y=percentage_cash_in_portfolio, colour = "Annual Cash Proportion of Total Finantial Assets"), size = 4)+
   geom_point(aes(y=excess_cash_percentage, colour = "Annual Excess Cash Proportion of Total Finantial Assets"), size = 4)+
   scale_y_continuous(name = "%", breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels = waiver())+
  labs(x="Year")+
  ggtitle("A  Portfolio Cash Position")+
  theme_bw()+
    theme(text = element_text(size = 16),
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title.y = element_text(angle = 180, vjust = 0.5),
          legend.title = element_blank(),
          legend.position="bottom",
          plot.title = element_text(size=16, face="bold"))

library(ggpubr)

figure_1 <- ggarrange(ggarrange(plot3, plot2, ncol = 2), 
                      plot1,
                      nrow = 2)

figure_1 <- annotate_figure(figure_1,
                bottom = text_grob("Data sources: Bank of Portugal, YCharts.",
                                  hjust = 6.7, x = 1, face = "italic", size = 11))


## ---- echo = FALSE, fig.width=19, fig.height=16------------------------
figure_1


## ---- echo=FALSE-------------------------------------------------------
library(report)
cite_packages(
  output = "paragraph",
  out.format = "html")

