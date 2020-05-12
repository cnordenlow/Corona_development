
#install.packages("scales")
#install.packages("tidyverse") 


require(tidyverse)
#library(shiny)
library(readxl)
library(csv)
library(deSolve)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plotly)
#library(quantmod)
library (readr)
library(deSolve)
require(tibble)
library(leaflet)
library(lubridate)
library(shiny)
library(leaflet)
library(RColorBrewer)

######################Genereal Data

#Data Corona
url_confirmed="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
data_confirmed<-read_csv(url(url_confirmed))
data_death <-read_csv(url(url_deaths))

names(data_confirmed)[names(data_confirmed) == "Country/Region"] <- "Country"
names(data_death)[names(data_death) == "Country/Region"] <- "Country"

#Change name on south korea
data_confirmed <- data_confirmed %>%
  mutate(Country = ifelse(Country == 'Korea, South', "South Korea", Country))     
data_death <- data_death %>%
  mutate(Country = ifelse(Country == 'Korea, South', "South Korea", Country))  


#'population'
url_population <- "https://raw.githubusercontent.com/datasets/population/master/data/population.csv"
population <- read_csv(url(url_population))
population <- population%>%
  filter(Year == "2016")

names(population)[names(population) == "Country Name"] <- "country"
names(population)[names(population) == "Value"] <- "pop"

population <- arrange(population, desc(pop))%>%
  mutate(pop = round(pop / 1000000),0)%>%
  select(country, pop)

#Change names
population <- population %>%
  mutate(
    country = ifelse(country == 'United States', "US", country),
    country = ifelse(country == 'Iran, Islamic Rep.', "Iran", country),
    country = ifelse(country == '	Korea, Rep.', "South Korea", country)
  )


#Get data
adjusted_confirmed <- data_confirmed%>%
  select(-c(Lat,Long))

adjusted_confirmed <- adjusted_confirmed%>%
  tidyr::gather(date, value, -Country, -`Province/State`)%>%
  group_by(Country, date) %>%
  tally(value)

#adjust date
adjusted_confirmed$date <- as.Date(adjusted_confirmed$date,
                                   format = "%m/%d/%y")
#convert to numbers
#as.numeric(data_conf_ajd$Sweden)
adjusted_confirmed <- mutate_if(adjusted_confirmed, is.factor, ~ as.numeric(as.character(.x)))


adjusted_confirmed <- merge(adjusted_confirmed, population, by.x = "Country", by.y = "country", all.x=TRUE)

adjusted_confirmed <- adjusted_confirmed%>%
  mutate(confirmed_per_million = if_else(pop == 0, 0, n / pop))



####


###########
#deaths
##########

#Get data
adjusted_death <- data_death%>%
  select(-c(Lat,Long))

adjusted_death <- adjusted_death%>%
  tidyr::gather(date, value, -Country, -`Province/State`)%>%
  group_by(Country, date) %>%
  tally(value)

#adjust date
adjusted_death$date <- as.Date(adjusted_death$date,
                               format = "%m/%d/%y")
#convert to numbers
#as.numeric(data_conf_ajd$Sweden)
adjusted_death <- mutate_if(adjusted_death, is.factor, ~ as.numeric(as.character(.x)))


adjusted_death <- merge(adjusted_death, population, by.x = "Country", by.y = "country", all.x=TRUE)

adjusted_death <- adjusted_death%>%
  mutate(death_per_million = if_else(pop == 0, 0, n / pop))


#put them together
adjusted_death_v2 <- adjusted_death%>%
  mutate(n_death = n)%>%
  select(n_death, death_per_million)

data_corona <- cbind(adjusted_confirmed, adjusted_death_v2)



adjusted_confirmed <- arrange(adjusted_confirmed, Country, date)%>%
  mutate(chg_confirmed = if_else(Country == lag(Country), (n - lag(n))/pop, 0))

adjusted_death <- arrange(adjusted_death, Country, date)%>%
  mutate(chg_death = if_else(Country == lag(Country), (n - lag(n)), 0))%>%
  mutate(chg_death_million = if_else(Country == lag(Country), (n - lag(n))/pop, 0))




#put them together
adjusted_death_v2 <- adjusted_death%>%
  mutate(n_death = n)%>%
  select(n_death, death_per_million,chg_death,chg_death_million)

data_corona <- cbind(adjusted_confirmed, adjusted_death_v2)


countries_to_show = c("US", "Italy", "Spain", "France", "Sweden", "Norway", "Denmark", "Canada", "United Kingdom", "Greece", "Finland", "Germany","Japan", "Portugal")
data_corona <- data_corona %>%
  filter(date > "2020-03-01")%>%
  filter(Country %in% countries_to_show)






##lägg ut nth starting day. also need to num first day and so on


test <- data_corona %>%
  mutate(nth = case_when(
    pop < 20 ~ 2,
    pop < 100 ~ 5,
    pop < 500 ~ 20,
    pop > 500 ~ 30))%>%
  filter(n_death > nth)

#first country

data_cororna_nth_death <- test %>%
  filter(Country == "US")
data_cororna_nth_death <- arrange(data_cororna_nth_death, date)
data_cororna_nth_death <- tibble::rowid_to_column(data_cororna_nth_death, "Day")


#Loop and add countries with specific first day
countries_to_show <-  c("Italy", "Spain", "France", "Sweden", "Norway", "Denmark", "Canada", "United Kingdom", "Greece", "Finland", "Germany", "Japan", "Portugal")

for (country_2 in countries_to_show){
  test_2 <- test %>%
    filter(Country == country_2)
  test_2 <- arrange(test_2, date)
  test_2 <- tibble::rowid_to_column(test_2, "Day")
  
  data_cororna_nth_death <- rbind(data_cororna_nth_death, test_2)
  
}

#Countries
countries_to_choose_from <- distinct(data_corona, Country)



##############nytt under


url_confirmed_us="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
url_deaths_us = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
url_latitude_us = "https://raw.githubusercontent.com/jasperdebie/VisInfo/master/us-state-capitals.csv"
data_confirmed_us<-read_csv(url(url_confirmed_us))
data_death_us <-read_csv(url(url_deaths_us))
data_latitude_us <- read_csv(url_latitude_us)




#Change coordinates for NY
new_lat_NY <- data_latitude_us %>%
  filter(name == "New York")%>%
  mutate(latitude = 40.730610)%>%
  mutate(longitude =-73.935242)

data_latitude_us <- data_latitude_us%>%
  filter(name != "New York")

data_latitude_us <- rbind(data_latitude_us, new_lat_NY)


#Get data
adjusted_d <- data_death_us %>%
  select(-Population)%>%
  select(-c(UID, Admin2,iso2, iso3, code3, FIPS, Country_Region))

adjusted_d <- adjusted_d%>%
  tidyr::gather(date, value, -Province_State, -Lat, -Long_, -Combined_Key)%>%
  mutate(date = as.Date(parse_date_time(date, orders = "mdy")))%>%
  filter(date > "2020-03-01")%>%
  group_by(Province_State, date, Lat, Long_) %>%
  tally(value)

adjusted_d <- adjusted_d %>%
  # filter(n >0)%>%
  group_by(Province_State, date) %>% 
  summarise(
    # Lat = mean(Lat),
    #Long_ = mean(Long_),
    n_deaths = sum(n)
  )

adjusted_d <- arrange(adjusted_d, date)




#confirmed
adjusted_c <- data_confirmed_us %>%
  # select(-Population)%>%
  select(-c(UID, Admin2,iso2, iso3, code3, FIPS, Country_Region))


adjusted_c <- adjusted_c%>%
  tidyr::gather(date, value, -Province_State, -Lat, -Long_, -Combined_Key)%>%
  mutate(date = as.Date(parse_date_time(date, orders = "mdy")))%>%
  filter(date > "2020-03-01")%>%
  group_by(Province_State, date, Lat, Long_) %>%
  tally(value)

adjusted_c <- adjusted_c %>%
  # filter(n >0)%>%
  group_by(Province_State, date) %>% 
  summarise(
    # Lat = mean(Lat),
    #Long_ = mean(Long_),
    n_confirmed = sum(n)
  )

adjusted_c <- arrange(adjusted_c, date)


adjusted_d <- adjusted_d %>%
  select(-date)

#merge
data_us <- cbind(adjusted_c, adjusted_d)%>%
  select(Province_State, date, n_confirmed, n_deaths)


#adjusted_x <- adjusted_d%>%
#  filter(Province_State != 'Alaska')%>%
#  filter(Province_State != 'Hawaii')
#adjusted_x <- na.omit(adjusted_x)




data_us <- merge(data_us, data_latitude_us, by.x = "Province_State", by.y = "name",all.x = TRUE)
data_us <- data_us%>%
  mutate(Lat= latitude)%>%
  mutate(Long_ = longitude)%>%
  filter(Province_State != 'Alaska')%>%
  filter(Province_State != 'Hawaii')%>%
  select(-c("latitude", "longitude"))

data_us <- na.omit(data_us)



##########The following data extracts and summarizes US. All Latitudes/longitudes that have a number of cases above x.
adjusted_deaths_us_v2 <- data_death_us %>%
  select(-Population)%>%
  select(-c(UID, Admin2,iso2, iso3, code3, FIPS, Country_Region))

adjusted_deaths_us_v2 <- adjusted_deaths_us_v2%>%
  tidyr::gather(date, value, -Province_State, -Lat, -Long_, -Combined_Key)%>%
  mutate(date = as.Date(parse_date_time(date, orders = "mdy")))%>%
  filter(date > "2020-03-01")%>%
  group_by(Province_State, date, Lat, Long_) %>%
  tally(value)

adjusted_deaths_us_v2 <- adjusted_deaths_us_v2 %>%
    filter(n >5)

adjusted_deaths_us_v2 <- adjusted_deaths_us_v2%>%
  filter(Province_State != 'Alaska')%>%
  filter(Province_State != 'Hawaii')%>%
  filter(Lat != 0)%>%
  filter(between(Lat,26,48))
adjusted_deaths_us_v2 <- na.omit(adjusted_deaths_us_v2)


adjusted_conf_us_v2 <- data_confirmed_us %>%
  #select(-Population)%>%
  select(-c(UID, Admin2,iso2, iso3, code3, FIPS, Country_Region))

adjusted_conf_us_v2 <- adjusted_conf_us_v2%>%
  tidyr::gather(date, value, -Province_State, -Lat, -Long_, -Combined_Key)%>%
  mutate(date = as.Date(parse_date_time(date, orders = "mdy")))%>%
  filter(date > "2020-03-01")%>%
  group_by(Province_State, date, Lat, Long_) %>%
  tally(value)

adjusted_conf_us_v2 <- adjusted_conf_us_v2 %>%
    filter(n >50)

adjusted_conf_us_v2 <- adjusted_conf_us_v2%>%
  filter(Province_State != 'Alaska')%>%
  filter(Province_State != 'Hawaii')%>%
  filter(Lat != 0)%>%
  filter(between(Lat,26,48))
adjusted_conf_us_v2 <- na.omit(adjusted_conf_us_v2)



adjusted_deaths_us_v2 <- adjusted_deaths_us_v2 %>%
  mutate(n_relative = case_when(
    between(n,2,10) ~ 10,
    between(n,10.1, 100) ~ 100,
    between(n,100.1,1000) ~ 1000,
    between(n,1000.1,10000) ~ 10000,
    between(n,10000.1,100000) ~ 100000
  )
  )

adjusted_conf_us_v2 <- adjusted_conf_us_v2 %>%
  mutate(n_relative = case_when(
    between(n,20,100) ~ 100,
    between(n,100.1, 1000) ~ 1000,
    between(n,1000.1,10000) ~ 10000,
    between(n,10000.1,100000) ~ 100000,
    between(n,100000.1,1000000) ~ 1000000))



adjusted_deaths_us_v2$n_range =  cut(adjusted_deaths_us_v2$n,
                          breaks=c(0,100,1000,10000,100000,1000000), right=FALSE,
                          labels= c("0-100", "100-1000", "1000-10000", "10000-100000", "100000-1000000"))


adjusted_conf_us_v2$n_range =  cut(adjusted_conf_us_v2$n,
                             breaks=c(0,100,1000,10000,100000,1000000), right=FALSE,
                             labels= c("0-100", "100-1000", "1000-10000", "10000-100000", "100000-1000000"))




data_us_v2 <- adjusted_conf_us_v2
 
##test slut













###################Shiny

#ui <- fluidPage(
ui <- bootstrapPage(
  headerPanel(paste("Corona Development", Sys.Date())),
  tabsetPanel(
    id = "tab_being_displayed", # will set input$tab_being_displayed
    tabPanel('Date',
             sidebarLayout(    
               sidebarPanel(
                 
                 #  selectInput(inputId = "dataset",
                 #             label = "Select countries:",
                 #            choices = data_corona$Country, multiple = TRUE,
                 #           selected = "Sweden"),
                 
                 #checkGroup
                 checkboxGroupInput("dataset", 
                                    label = h3("Select countries"), 
                                    choices = countries_to_choose_from$Country,
                                    selected = "Sweden"),
                 
                 #   checkboxGroupInput("variable", "Variables to show:",
                 #        countries_to_show),
                 
                 
                 sliderInput(inputId = "Order",
                             label = "Choose Timeperiod",  
                             min = as.Date(min(data_corona$date)), max = as.Date(max(data_corona$date)), 
                             value = c(as.Date(min(data_corona$date)), as.Date(max(data_corona$date)))),
                 
                 
                 radioButtons("type", "Actual numbers or relative to population:",
                              c("Relative to population" = "rel",
                                "Actual" = "actual")),
                 
                 radioButtons("dist", "Distribution type:",
                              c("Normal" = "norm",
                                "Log" = "log"))
                 
                 
               ),
               mainPanel(
                 plotOutput("ts_plot1"),
                 plotOutput("ts_plot2"),
                 plotOutput("ts_plot3")
                 #                 verbatimTextOutput("summary")
               )
             )  
    ),
    
    tabPanel('Start date nth death',
             sidebarLayout(    
               sidebarPanel(
                 
                 checkboxGroupInput("dataset_tab2", 
                                    label = h3("Select countries"), 
                                    choices = countries_to_choose_from$Country,
                                    selected = "Sweden"),
                 
                 
                 
                 sliderInput(inputId = "Order_tab2",
                             label = "Choose Timeperiod",  
                             min = (min(data_cororna_nth_death$Day)), max = (max(data_cororna_nth_death$Day)), 
                             value = c((min(data_cororna_nth_death$Day)), (max(data_cororna_nth_death$Day)))),
                 
                 radioButtons("type2", "Actual numbers or relative to population:",
                              c("Relative to population" = "rel",
                                "Actual" = "actual")),
                 
                 radioButtons("dist2", "Distribution type:",
                              c("Normal" = "norm",
                                "Log" = "log"))
                 
                 
                 
               ),
               mainPanel(
                 h4("Number of deaths / million, start day after nth death:"),
                 h5("Countries with population < 20m: 2 deaths"),
                 h5("Countries with population < 100m: 5 deaths"),
                 h5("Countries with population < 500m: 20 deaths"),
                 h5("Countries with population > 500m: 30 deaths"),
                 
                 plotOutput("ts_plot4"),
                 plotOutput("ts_plot5"),
                 plotOutput("ts_plot6")
                 
               )
             )
    ),
    
    
    
    tabPanel("US Map State by State",
             #bootstrapPage(
             tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
             leafletOutput("map", width = "100%", height = 600),
             #, height = "100%" sätter man height i % så renderas inte mappen. konstigt.https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering

             
             absolutePanel(bottom = 20, left = 20,
                           sliderInput("range_us", "Select Mapping Date", 
                                       min = as.Date(min(adjusted_conf_us_v2$date)), 
                                       max = as.Date(max(adjusted_conf_us_v2$date)), 
                                       value = as.Date(min(adjusted_conf_us_v2$date))
                           ),
                           
                           radioButtons("type_us", "Confimed Cases or Reported Deaths:",
                                        c("Confirmed Cases" = "confirmed",
                                          "Reported Deaths" = "deaths"))
                          
             )
    ),
    
    
    
    
    
    
    
    
    tabPanel('Expected Development, SIR-model',
             sidebarLayout(    
               sidebarPanel(
                 
                 
                 radioButtons(inputId = "dataset_tab3",
                              label = "Select country:",
                              choices = countries_to_choose_from$Country,
                              selected = "Sweden"),
                 

                 
               ),
               mainPanel(
                 plotOutput("ts_plot7")
                 
                 
               )
             )
    )
    
  )
)





#setwd("c:/Users/chris/Documents/R_Studio/")



server <- shinyServer(
  
  
  
  
  
  
  function(input,output, session){
    
    datasetInput <- reactive({
      #data_corona %>% filter(Country == input$dataset)
      ### Filter by date
      
      
      
      
      data_corona <- data_corona[as.Date(data_corona$date) >= input$Order[1] & as.Date(data_corona$date) <= input$Order[2] ,] 
      data_corona %>% filter(Country %in% input$dataset)
      
      ###Borde försöka få  ihop adjusted death och adjusted confirmed from start day to same file?
      
      
    })
    
    #second tab
    datasetInput2 <- reactive({
      #data_corona %>% filter(Country == input$dataset)
      ### Filter by date
      
      
      ###Borde försöka få  ihop adjusted death och adjusted confirmed from start day to same file?
      data_cororna_nth_death <- data_cororna_nth_death[(data_cororna_nth_death$Day) >= input$Order_tab2[1] & (data_cororna_nth_death$Day) <= input$Order_tab2[2] ,] 
      data_cororna_nth_death %>% filter(Country %in% input$dataset_tab2)
      
      
    })
    
    
    #third tab
    datasetInput3 <- reactive({
      #data_corona %>% filter(Country == input$dataset)
      ### Filter by date
      
      
      ###Borde försöka få  ihop adjusted death och adjusted confirmed from start day to same file?
      #   data_cororna_nth_death <- data_corona[(data_cororna_nth_death$Day) >= input$Order_tab2[1] & (data_cororna_nth_death$Day) <= input$Order_tab2[2] ,] 
      data_corona_sir <- data_corona %>%
        filter(Country %in% input$dataset_tab3)%>%
        filter(n > 100)
      data_corona_sir <- arrange(data_corona_sir, date)
      
      
      
      
    })
    
    

    
    # plot time series
    output$ts_plot1 <- renderPlot({
      
      dataset <- datasetInput()
      
      #actual or relative
      if (input$type == "actual") {
        dataset$confirmed <-dataset$n
        title_ = paste("Number of confirmed")
        xlab =  "Date"
        ylab = "Number of confirmed"
      }
      
      else if (input$type == "rel"){
        dataset$confirmed <- (dataset$confirmed_per_million)
        title_ = paste("Number of confirmed per million")
        xlab =  "Date"
        ylab = "Number of confirmed per million"
      }
      
      
      
      if (input$dist == "norm") {
        dataset$y_value <-dataset$confirmed
      }
      
      else if (input$dist == "log"){
        dataset$y_value <- log(dataset$confirmed)
      }
      
      
      
      p1 <- ggplot(dataset, aes(x = date, y=y_value, group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
      
      #if (input$dist == "log"){
      #  p1 <- p1 + geom_abline(intercept=0, slope=max(dataset$y_value)/max(dataset$date)) 
      #p1 <- p1+ geom_abline(intercept=0, slope=max(dataset$y_value)/max(dataset$date)) +
      #  xlim(0,NA) +
      # ylim(0,NA)
      
      # p1 <- p1 + abline(coef = c(0,1))
      #   p1 <- p1 + geom_smooth(method = "lm", se = FALSE)
      # p1 <- p1 + geom_abline(data=horizontal_line_dataset)
      #  } 
      
      
      ####Hmmz
      #  .min_date <- input$Order[1]
      #    .max_date <- input$Order[2]
      #    dagar <- as.numeric(.max_date - .min_date)
      
      #     df <- tibble(
      #       date = seq.Date(.min_date, .max_date, by="day"),
      #      value = 0:as.numeric(dagar)) %>%
      #      mutate(doubling_5day = log(2*1.1486979587^value))
      
      #     if (input$dist == "log"){
      #        p1 <- p1 + geom_line(aes(x = date, y = df$doubling_5day))
      
      #}
      
      
      
      
      
      p1
      
      
    })
    
    
    
    output$ts_plot2 <- renderPlot({
      
      dataset <- datasetInput()
      
      
      if (input$type == "actual") {
        dataset$death_type <-dataset$n_death
        title_ = paste("Number of deaths")
        xlab =  "Date"
        ylab = "Number of deaths"
      }
      
      else if (input$type == "rel"){
        dataset$death_type <- (dataset$death_per_million)
        title_ = paste("Number of deaths per million")
        xlab =  "Date"
        ylab = "Number of deaths per million"
      }
      
      
      if (input$dist == "norm") {
        dataset$y_value_death <-dataset$death_type
      }
      
      else if (input$dist == "log"){
        dataset$y_value_death <- log(dataset$death_type)
      }
      
      
      p2 <- ggplot(dataset, aes(x = date, y=y_value_death,group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      p2
      
      
    })
    
    
    
    output$ts_plot3 <- renderPlot({
      
      dataset <- datasetInput()
      
      
      
      if (input$type == "actual") {
        dataset$death_chg_type <-dataset$chg_death
        title_ = paste("Number of deaths each day")
        xlab =  "Date"
        ylab = "Number of deaths each day"
      }
      
      else if (input$type == "rel"){
        dataset$death_chg_type <- (dataset$chg_death_million)
        title_ = paste("Number of deaths per million each day")
        xlab =  "Date"
        ylab = "Number of deaths per million each day"
      }
      
      
      
      if (input$dist == "norm") {
        dataset$y_value_chg_death <-dataset$death_chg_type
      }
      
      else if (input$dist == "log"){
        dataset$y_value_chg_death <- (dataset$death_chg_type)
      }
      
      p3 <- ggplot(dataset, aes(x = date, y=y_value_chg_death,group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      p3
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
    })
    
    ###next tab
    
    output$ts_plot4 <- renderPlot({
      
      dataset <- datasetInput2()
      
      
      if (input$type2 == "actual") {
        dataset$confirmed <-dataset$n
        title_ = paste("Number of confirmed, start day after nth death")
        xlab =  "Days"
        ylab = "Number of confirmed"
      }
      
      else if (input$type2 == "rel"){
        dataset$confirmed <- (dataset$confirmed_per_million)
        title_ = paste("Number of confirmed per million, start day after nth death")
        xlab =  "Days"
        ylab = "Number of confirmed per million"
      }
      
      
      
      if (input$dist2 == "norm") {
        dataset$y_value <-dataset$confirmed
      }
      
      else if (input$dist2 == "log"){
        dataset$y_value <- log(dataset$confirmed)
      }
      
      
      p4 <- ggplot(dataset, aes(x = Day, y=y_value, group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      p4
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
      #grid.arrange(p1,p2, ncol=2)
      
    })
    
    
    output$ts_plot5 <- renderPlot({
      
      dataset <- datasetInput2()
      
      
      if (input$type2 == "actual") {
        dataset$death_type <-dataset$n_death
        title_ = paste("Number of deaths, start day after nth death")
        xlab =  "Days"
        ylab = "Number of deaths"
      }
      
      else if (input$type2 == "rel"){
        dataset$death_type <- (dataset$death_per_million)
        title_ = paste("Number of deaths per million, start day after nth death")
        xlab =  "Days"
        ylab = "Number of deaths per million"
      }
      
      
      if (input$dist2 == "norm") {
        dataset$y_value_death <-dataset$death_type
      }
      
      else if (input$dist2 == "log"){
        dataset$y_value_death <- log(dataset$death_type)
      }
      
      
      
      p5 <- ggplot(dataset, aes(x = Day, y=y_value_death, group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      p5
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
      #grid.arrange(p1,p2, ncol=2)
      
    })
    
    
    
    
    output$ts_plot6 <- renderPlot({
      
      dataset <- datasetInput2()
      
      
      if (input$type2 == "actual") {
        dataset$death_chg_type <-dataset$chg_death
        title_ = paste("Number of deaths each day, start day after nth death")
        xlab =  "Days"
        ylab = "Number of deaths each day"
      }
      
      else if (input$type2 == "rel"){
        dataset$death_chg_type <- (dataset$chg_death_million)
        title_ = paste("Number of deaths per million each day, start day after nth death")
        xlab =  "Days"
        ylab = "Number of deaths per million each day"
      }
      
      
      
      if (input$dist2 == "norm") {
        dataset$y_value_chg_death <-dataset$death_chg_type
      }
      
      else if (input$dist2 == "log"){
        dataset$y_value_chg_death <- (dataset$death_chg_type)
      }
      
      p6 <- ggplot(dataset, aes(x = Day, y=y_value_chg_death,group=Country, colour=Country)) + 
        geom_line()+
        geom_point(size=3)+
        theme_classic()+
        labs(title = title_,
             x = xlab,
             y = ylab
        )+
        theme(
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_line(color = "grey80"),
          panel.grid.major.y = element_line(color = "grey80"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face ="bold")
        )
      p6
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
    })
    
    
    
    
    output$ts_plot7 <- renderPlot({
      
      dataset <- datasetInput3()
      
      Infected <- dataset$n
      
      
      
      #Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711, 9692, 11791, 14380, 17205, 20440)
      Day <- 1:(length(Infected))
      
      #     population_country <- population%>%
      #      filter(Country %in% input$dataset_tab3)
      #    population_country <- population_country[1,2]
      #  N <- 100000000 # population of mainland china
      N <- dataset[1,4]
      N <- N * 1000000
      
      
      SIR <- function(time, state, parameters) {
        par <- as.list(c(state, parameters))
        with(par, {
          dS <- -beta/N * I * S
          dI <- beta/N * I * S - gamma * I
          dR <- gamma * I
          list(c(dS, dI, dR))
        })
      }
      
      #install.packages("deSolve", repos="https://cloud.r-project.org") 
      #install.packages("deSolve")
      #  library(deSolve)
      init <- c(S = N-Infected[1], I = Infected[1], R = 0)
      RSS <- function(parameters) {
        names(parameters) <- c("beta", "gamma")
        out <- ode(y = init, times = Day, func = SIR, parms = parameters)
        fit <- out[ , 3]
        sum((Infected - fit)^2)
      }
      
      Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
      #Opt$message
      
      Opt_par <- setNames(Opt$par, c("beta", "gamma"))
      #Opt_par
      
      
      
      
      
      
      
      
      t <- 1:100 # time in days
      fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
      col <- 1:3 # colour
      
      #matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
      matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
      ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
      ## omitted from logarithmic plot
      
      points(Day, Infected)
      legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
      title(paste("Expected development, SIR model"), outer = TRUE, line = -2)
      
      
      
      #p6 <- ggplot(dataset, aes(x = Day, y=y_value_chg_death,group=Country, colour=Country)) + 
      # geom_line()+
      #  geom_point(size=3)+
      #  theme_classic()+
      #  labs(title =paste("Number of deaths per million each day"),
      #      x = "Days",
      #      y = "Number of deaths per million each day"
      #  )+
      # theme(
      #  panel.grid.major.x = element_line(color = "grey80"),
      #  panel.grid.minor.x = element_line(color = "grey80"),
      #  panel.grid.major.y = element_line(color = "grey80"),
      #  axis.text = element_text(size = 16), 
      #  axis.title = element_text(size = 16),
      #  plot.title = element_text(size = 16, face ="bold")
      #)
      #p6
      #p2 <- ggplot(dataset, aes(x = date, y=confirmed_per_million)) + geom_line()
      
    })
    
    
    
    
    ##################US Data
    
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
      
      # data_us %>% filter(date == req(input$range))
      # filter(n_confirmed>1)
      
      if (input$type_us == "deaths") {
        data_us_v2 <- adjusted_deaths_us_v2
        data_us_v2$n <-adjusted_deaths_us_v2$n
        data_us_v2 %>% filter(date == req(input$range_us))%>%
          filter(n>5)
        
      }
      
      else if (input$type_us == "confirmed"){
        data_us_v2 <- adjusted_conf_us_v2
        data_us_v2$n <- (adjusted_conf_us_v2$n)
        data_us_v2 %>% filter(date == req(input$range_us))%>%
          filter(n>50)
      }
      

      
    })
    
    
    
    #  colorpal <- reactive({
    #  colorNumeric(input$colors, data_us$n_confirmed)
    # })
    
    
    pal = colorFactor(palette = c("red", "black"), domain=data_us_v2$n_range)
    
    output$map <- renderLeaflet({
      
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(data_us_v2) %>% addTiles() %>%
        fitBounds(~min(Long_), ~min(Lat), ~max(Long_), ~max(Lat))
    })  
    
    
    
    observe({
      
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls()%>%
        addCircles(
          lat = ~ Lat, lng = ~ Long_,
          color = ~ pal(n_range), #use the pallete
          
          radius = ~ if (input$type_us == "deaths"){ifelse(sqrt(n)*2000 > 300000, 300000, sqrt(n)*2000)} else if (input$type_us == "confirmed") {ifelse(sqrt(n)*1000 > 300000, 300000, sqrt(n)*1000)},
          
          weight = 1, 
          fillOpacity = 0.5, popup = ~paste(n)
        )%>%
        addLegend(
          position = "bottomleft", # position where the legend should appear
          pal = pal, # pallete object where the color is defined
          values = ~n_range, # column variable or values that were used to derive the color pallete object
          title = if (input$type_us == "deaths"){"Number of reported Deaths"} else if (input$type_us == "confirmed"){"Number of Confirmed Cases"} , # title of the legend
          
          opacity = 1
        )
      
      
      
    })
    
    
  })







#http://www.moneyandscience.com/2016/05/05/shiny-r-code-for-multiple-plots-using/

shiny::shinyApp(ui = ui, server = server)


