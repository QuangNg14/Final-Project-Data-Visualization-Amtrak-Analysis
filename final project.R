dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)

library(ggplot2)
library(interactions)
library(dagitty)
library(ggdag)
library(ggraph)
library(tidyverse)
library(margins)
library(tidyr)
library(zoo)
library(readxl)
library(lubridate)
library(scales)
library(sp)
library(rgdal)
library(cowplot)
library(ggpubr)
library(maps)
library(sf)
library(GISTools)
library(gganimate)
library(dplyr)
library(reshape)
library("readxl")
library(ggrepel)
library(sf)
library(GISTools)
library(plotly)
#Data Cleaning

abbr2state <- function(abbr){
  ab    <- tolower(c("AL",
                     "AK", "AZ", "KS", "UT", "CO", "CT",
                     "DE", "FL", "GA", "HI", "ID", "IL",
                     "IN", "IA", "AR", "KY", "LA", "ME",
                     "MD", "MA", "MI", "MN", "MS", "MO",
                     "MT", "NE", "NV", "NH", "NJ", "NM",
                     "NY", "NC", "ND", "OH", "OK", "OR",
                     "PA", "RI", "SC", "SD", "TN", "TX",
                     "CA", "VT", "VA", "WA", "WV", "WI",
                     "WY", "DC"))
  st    <- c("Alabama",
             "Alaska", "Arizona", "Kansas",
             "Utah", "Colorado", "Connecticut",
             "Delaware", "Florida", "Georgia",
             "Hawaii", "Idaho", "Illinois",
             "Indiana", "Iowa", "Arkansas",
             "Kentucky", "Louisiana", "Maine",
             "Maryland", "Massachusetts", "Michigan",
             "Minnesota", "Mississippi", "Missouri",
             "Montana", "Nebraska", "Nevada",
             "New Hampshire", "New Jersey", "New Mexico",
             "New York", "North Carolina", "North Dakota",
             "Ohio", "Oklahoma", "Oregon",
             "Pennsylvania", "Rhode Island", "South Carolina",
             "South Dakota", "Tennessee", "Texas",
             "California", "Vermont", "Virginia",
             "Washington", "West Virginia", "Wisconsin",
             "Wyoming", "District of Columbia")
  st[match(tolower(abbr), ab)]
}

amtrak = read_excel("AMTRAK-Stations-Database_2012.xls", sheet = "AdrReport1")
amtrak = amtrak[, c('Code', 'StationName', 'Staffed', "StationType", "City", "State",
                    "Zip", "Country", "Division", "TicketOffice", "CheckedBaggage", "CoordinatedThruway",
                    "TimeZone", "CheckInWService", "Latitude/Longitude", "Elevation", "Population",
                    "FirstLvlSupervisorTitle")]
amtrak <- amtrak %>% mutate_all(na_if,"")

amtrak$State = abbr2state(amtrak$State)
amtrak$lat = NA
amtrak$lon = NA

for(i in 1:nrow(amtrak)) {       
  amtrak$lat[i] <- parse_number(strsplit(amtrak[i, ]$`Latitude/Longitude`, split = ",")[[1]][1])
  amtrak$lon[i] <- parse_number(strsplit(amtrak[i, ]$`Latitude/Longitude`, split = ",")[[1]][2])
}

amtrak = amtrak[which(amtrak$StationType != "Pseudo Station"),]

#Making a map
state_count = amtrak %>% count(State, sort = TRUE)
USA.map<-st_read("US_GADM/gadm36_USA_1.shp")
USA.map$State<-USA.map$NAME_1
amtrak.map<-merge(USA.map, state_count, by='State')

amtrak$Staffed = as.factor(amtrak$Staffed)
amtrak$StationType = as.factor(amtrak$StationType)
amtrak.map$`Number_of_Stations` = amtrak.map$n

ggplot(amtrak.map)+
  geom_sf(aes(fill=`Number of Stations`), color="white")+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  geom_point(data=amtrak, aes(x=lon, y=lat, color=StationType), size=1)+
  #zoom into the US continent
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)+
  scale_color_manual(values=c("#d1ff00", "#30ff00", "#000000","#ff000c"))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Amtrak Locations across the US by Station Type")+
  theme(plot.title = element_text(hjust = 0.5, size = 20))

#Making a bar chart by state
state_count = na.omit(state_count)
ggplot(data=state_count, aes(x=reorder(State, n), y=n))+
  geom_col(fill="#F8766D")+
  labs(title="Number of Amtrak Stations per State", y="Number of Amtrak Stations", x="States")+
  geom_text(aes(label=n, hjust=-0.5), size=3)+
  coord_flip()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  theme(axis.title.y=element_text(angle=0))+
  scale_y_continuous(expand=c(0,0))

#Making a bar chart by division
amtrak$Division = as.factor(amtrak$Division)
station_division_count = amtrak %>% group_by(StationType, Division) %>% count(Division, sort = TRUE)
station_division_count = na.omit(station_division_count)

ggplot(data=station_division_count, aes(x=reorder(Division,n), y=n))+
  geom_col(aes(fill=StationType))+
  labs(title="Stacked bar chart on the station types of each train division",
       y="Number of stations", x="Divisions", fill="Station Types")
#scale_fill_discrete(labels=c("Passing Yards", "Rushing Yards"))

#Pie chart on the Station type
stationType_count = amtrak %>% count(StationType, sort = TRUE)
stationType_count$StationType = as.factor(stationType_count$StationType)
stationType_count$labels = round(100 * stationType_count$n/sum(stationType_count$n), 0)
stationType_count$labels = as.character(stationType_count$labels)
stationType_count$labels[-length(stationType_count$labels)-1] <- paste0(stationType_count$labels[-length(stationType_count$labels)-1], '%')
stationType_count$pos = cumsum(stationType_count$n)- stationType_count$n/3

ggplot(data=stationType_count, aes(x="", y=n, fill=StationType))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Percentage of Station Types", fill="Station Types")+
  #scale_fill_discrete(labels=c("Passing TD", "Rushing TD"))+
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), panel.border = element_blank(), panel.grid=element_blank(),axis.ticks = element_blank())

#Histogram for the station types 
amtrak$StationType = as.factor(amtrak$StationType)
table(amtrak$PopType)
amtrak$PopType[amtrak$Population <= 100000] = 0
amtrak$PopType[amtrak$Population > 100000 & amtrak$Population <= 250000] = 1
amtrak$PopType[amtrak$Population > 250000] = 2
amtrak$PopType = as.factor(amtrak$PopType)

amtrakPop_0 = amtrak[which(amtrak$PopType == 0),]
amtrakPop_1 = amtrak[which(amtrak$PopType == 1),]
amtrakPop_2 = amtrak[which(amtrak$PopType == 2),]


#Population Type 0 
ggplot(amtrakPop_0, aes(x=Population, fill=StationType)) +
  geom_histogram(position="dodge")+
  labs(title="Histogram of population of each type of station (Population Type 0)", y="Frequency", x="Population")+
  theme(legend.position="right")

#Population Type 1 
ggplot(amtrakPop_1, aes(x=Population, fill=StationType)) +
  geom_histogram(position="dodge")+
  labs(title="Histogram of population of each type of station (Population Type 1)", y="Frequency", x="Population")+
  theme(legend.position="right")

#Population Type 2 
ggplot(amtrakPop_2, aes(x=Population, fill=StationType)) +
  geom_histogram(position="dodge")+
  labs(title="Histogram of population of each type of station (Population Type 2)", y="Frequency", x="Population")+
  theme(legend.position="right")

#Population Type 3 
ggplot(amtrakPop_3, aes(x=Population, fill=StationType)) +
  geom_histogram(position="dodge")+
  labs(title="Histogram of population of each type of station (Population Type 3)", y="Frequency", x="Population")+
  theme(legend.position="right")

#Population Type 4 
ggplot(amtrakPop_4, aes(x=Population, fill=StationType)) +
  geom_histogram(position="dodge")+
  labs(title="Histogram of population of each type of station (Population Type 4)", y="Frequency", x="Population")+
  theme(legend.position="right")

#making a interactive plot 
p1 = ggplot(amtrak.map)+
  geom_sf(aes(fill=Number_of_Stations, text0=State, text11=Number_of_Stations), color="white")+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  geom_point(data=amtrak, aes(x=lon, y=lat, color=StationType,
                              text1=StationName,
                              text2=Code,
                              text3=Staffed,
                              text4=StationType,
                              text5=City,
                              text6=State,
                              text7=Zip,
                              text8=Division,
                              text9=Elevation,
                              text10=Population
  ), size=1)+
  #zoom into the US continent
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)+
  scale_color_manual(values=c("#d1ff00", "#30ff00", "#000000","#ff000c"))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Amtrak Locations across the US by Station Type")+
  theme(plot.title = element_text(hjust = 0.5, size = 20))

p2 = ggplotly(p1, tooltip=c("text11", "text0", "text1", "text2", "text3", "text4", "text5", "text6", "text7", "text8", "text9", "text10"))
p2









