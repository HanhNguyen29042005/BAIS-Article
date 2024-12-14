library(car)
library(tidyverse)
library(tibble)
library(dplyr)
library(httr)      
library(rvest)      
library(lubridate)  
library(magrittr) 
library(chromote)
library(writexl)
set_config(user_agent("<nguyentrang@xavier.edu>; +https://www.xavier.edu/business-analytics-program"))

lyon_fri13 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTEzagwIAhIIL20vMDFzbm1yDAgDEggvbS8wZHByZ0ABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoBCIDEgEw&hl=en-US&gl=US"
germany_fri13 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTEzagwIAhIIL20vMDFzbm1yDAgDEggvbS8wMTU2cUABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
spain_fri13 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTEzagwIAhIIL20vMDFzbm1yDAgDEggvbS8wMWY2MkABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
southKor_fri13 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhojEgoyMDI0LTEyLTEzagwIAhIIL20vMDFzbm1yBwgBEgNJQ05AAUgBcAGCAQsI____________AZgBAg&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
vietNam_fri13 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhojEgoyMDI0LTEyLTEzagwIAhIIL20vMDFzbm1yBwgBEgNTR05AAUgBcAGCAQsI____________AZgBAg&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"

vietNam_fri20 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhojEgoyMDI0LTEyLTIwagwIAhIIL20vMDFzbm1yBwgBEgNTR05AAUgBcAGCAQsI____________AZgBAg&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
southKor_fri20 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhojEgoyMDI0LTEyLTIwagwIAhIIL20vMDFzbm1yBwgBEgNJQ05AAUgBcAGCAQsI____________AZgBAg&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
spain_fri20 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTIwagwIAhIIL20vMDFzbm1yDAgDEggvbS8wMWY2MkABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
lyon_fri20 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTIwagwIAhIIL20vMDFzbm1yDAgDEggvbS8wZHByZ0ABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoBSIDEgEw&hl=en-US&gl=US"
germany_fri20 <- "https://www.google.com/travel/flights/search?tfs=CBwQAhooEgoyMDI0LTEyLTIwagwIAhIIL20vMDFzbm1yDAgDEggvbS8wMTU2cUABSAFwAYIBCwj___________8BmAEC&tfu=EggIABABIAIoCCIDEgEw&hl=en-US&gl=US"

data <- list(
  lyon_fri13 = lyon_fri13,
  germany_fri13 = germany_fri13,
  spain_fri13 = spain_fri13,
  southKor_fri13 = southKor_fri13,
  vietNam_fri13 = vietNam_fri13,
  vietNam_fri20 = vietNam_fri20,
  southKor_fri20 = southKor_fri20,
  spain_fri20 = spain_fri20,
  lyon_fri20 = lyon_fri20,
  germany_fri20 = germany_fri20
)

#### Scraping/Generating data Function ####
scrape_flight <- function(url)
{
  flights <- 
    read_html_live(url)
  
  Sys.sleep(2)
  
  depart_time <- flights %>% 
    html_elements("div.wtdjmc.YMlIz.ogfYpf.tPgKwe") %>% 
    html_text2() 
  cleaned_depart_time <- gsub("\u202F", " ", depart_time) #There is a non-breakable space in the string-> need to clean 
  
  Sys.sleep(1)
  
  price <- flights %>% 
    html_elements("div.BVAVmf.tPgKwe") %>% 
    html_elements('span') %>% 
    html_text2()
  
  Sys.sleep(2)
  
  stops <- flights %>% 
    html_elements("div.BbR8Ec") %>% 
    html_elements("div.EfT7Ae.AdWm1c.tPgKwe") %>% 
    html_elements("span.ogfYpf") %>% 
    html_attr("aria-label") %>% 
    substr(1,1)
  
  Sys.sleep(3)
  
  airline <- flights %>%
    html_elements("div.OgQvJf.nKlB3b") %>%  
    html_elements("div.sSHqwe.tPgKwe.ogfYpf") %>%  
    html_text2() %>%
    gsub("Operated by.*", "", .) %>%  
    trimws() 
  airline <- airline[seq(1, length(airline), by = 2)]
  
  Sys.sleep(1)
  
  duration <- flights %>% 
    html_elements("div.Ak5kof") %>% 
    html_elements("div.gvkrdb.AdWm1c.tPgKwe.ogfYpf") %>% 
    html_text2() %>% 
    trimws()
  
  flight_info <- tibble(cleaned_depart_time,price,airline,duration,stops)
  
  return(flight_info)
}

flight_data_list <- list()




flights %>% 
  filter(airline_US=="British AIrways") %>% 
  summarise(price_range = range(price))
# For loop to scrape flight data for each URL
for (var_name in names(data)) {
  flight_data_list[[var_name]] <- scrape_flight(data[[var_name]])
  print("1 done")
  Sys.sleep(4)
}


#### Data Cleaning ####
for (x in 1:length(flight_data_list)) {
  flight_data_list[[x]] <- flight_data_list[[x]] %>% 
    mutate(
      airline_US = sub(",.*", "", airline),  # Extract airline name for US
      airline_outside = sub(".*, ", "", airline),  # Extract airline name for non-US
      price = as.numeric(gsub("[\\$,]", "", price)),  # Remove dollar sign and commas
      time_depart_standard = format(as.POSIXct(cleaned_depart_time, format='%I:%M %p'),"%H:%M"),  # Convert to 24-hour time
      rough_estimated_duration = as.numeric(substr(duration, 1, 2)),  
      flight_hour = as.integer(substr(time_depart_standard, 1, 2))  
    ) %>% 
    select(time_depart_standard, flight_hour, airline_US, airline_outside, price, rough_estimated_duration, stops)
}

#### Separate into 10 different data set ####

lyon_fri13 <- flight_data_list$lyon_fri13 %>% 
  mutate(destination="Lyon", date="Friday 13")
germany_fri13 <- flight_data_list$germany_fri13 %>% 
  mutate(destination="Berlin", date="Friday 13")
spain_fri13 <- flight_data_list$spain_fri13 %>% 
  mutate(destination="Barcelona", date="Friday 13")
southKor_fri13 <- flight_data_list$southKor_fri13 %>% 
  mutate(destination="Seoul", date="Friday 13")
vietNam_fri13 <- flight_data_list$vietNam_fri13 %>% 
  mutate(destination="Ho Chi Minh", date="Friday 13")
lyon_fri20 <- flight_data_list$lyon_fri20 %>% 
  mutate(destination="Lyon", date="Friday 20")
germany_fri20 <- flight_data_list$germany_fri20 %>% 
  mutate(destination="Berlin", date="Friday 20")
southKor_fri20 <- flight_data_list$southKor_fri20 %>% 
  mutate(destination="Seoul", date="Friday 20")
vietNam_fri20 <- flight_data_list$vietNam_fri20 %>% 
  mutate(destination="Ho Chi Minh", date="Friday 20")
spain_fri20 <- flight_data_list$spain_fri20 %>% 
  mutate(destination="Barcelona", date="Friday 20")


#### combind rows into 1 dataset####
flight_data <- rbind(lyon_fri13,germany_fri13,spain_fri13,southKor_fri13,vietNam_fri13,
                     vietNam_fri20,southKor_fri20,spain_fri20,lyon_fri20,germany_fri20)
write.csv(flight_data, "flight_data.csv")



#### Set up ####
library(tidyverse)
library(readr)

flights <- read.csv("flight_data.csv") 

flights <- flights %>% 
  mutate(destination = str_replace(destination, "Ho Chi Minh city", "Ho Chi Minh"),
         destination= factor(destination),
         price= as.numeric(price),
         airline_US = if_else(airline_US == "Air CanadaLufthansa", "Air Canada", 
                   if_else(airline_US == "British AirwaysIberia", "British Airways", 
                           if_else(airline_US == "British AirwaysAmerican", "British Airways",
                                   if_else(airline_outside == "UnitedLufthansa", "United",
                                           ifelse(airline_US == "DeltaVirgin Atlantic", "Delta",
                                                  ifelse(airline_US=="AmericanBritish Airways","American",
                                                         ifelse(airline_US =="DeltaKLM", "Delta",
                                                                ifelse(airline_US=="Air CanadaUnited", "Air Canada", airline_US)))))))),
         airline_outside= sapply(airline_outside, split_airline_names),
         time_frame = case_when(flight_hour < 9 ~ "Early morning", 
           flight_hour >= 9 & flight_hour < 18 ~ "Normal",  
           flight_hour >= 18 & flight_hour < 22 ~ "Night flight", 
           TRUE ~ "Late night" ))





#### (Cleaning Data) Function to split combined airline names####
split_airline_names <- function(name) {
  # Initialize a vector to store the parts of the name
  known_airlines <- c("Iberia", "Lufthansa", "Air Canada", "British Airways", "Air Dolomiti", 
                      "Austrian", "Brussels Airlines", "Air France", "Tap Air Portugal", "KLM", 
                      "Finnair", "Aer Lingus", "American", "United", "SWISS", "Delta", "Virgin Atlantic", 
                      "ITA", "Scandinavian Airlines", "Jeju Air", "Asiana", "Korean Air", "China Airlines", 
                      "Turkish Airlines", "EVA Air", "Emirates", "Qatar Airways", "Etihad", "Cathay Pacific", 
                      "China Southern", "ANA", "STARLUX Airlines", "Vietnam Airlines", "JAL", "China Eastern", 
                      "THAI", "Malaysia Airlines", "Singapore Airlines")
  
  parts <- c()
  
  # Loop over each known airline name
  for (airline in known_airlines) {
    if (grepl(airline, name)) {
      parts <- c(parts, airline)
      return(airline)  # Remove matched airline name from the string
    }
  }
  return(NA)
}


####Analysis - Visualization ####


ggplot(flights, aes(destination,price))+
  geom_boxplot()
table(flights$destination)


ggplot(flights, aes(destination,rough_estimated_duration))+
  geom_boxplot()

model1 <- aov(price~destination , data=flights)
summary(model1)
model2 <- aov(price~rough_estimated_duration , data=flights)
summary(model2)
model3 <- aov(price~destination*rough_estimated_duration, data=flights)
summary(model3)



















