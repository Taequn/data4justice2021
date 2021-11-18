library(leaflet)
library(tidyverse)
library(rgdal)
library(sf)

csv.1<-read_csv("countycodes.csv")
csv.2<-read_csv("grids.csv")
csv.3<-read_csv("allmn.csv")



############################################################
# Working with data
############################################################
csv.3.updated <- csv.3 %>%
  mutate(fullname = paste(jlname, jfname))%>%
  mutate(rateSev = case_when(severity <= 12 ~ "Standard",
                             (severity >= 13 & severity <= 20) ~ "Sexual assault",
                             (severity >= 51 & severity <= 59) ~ "Drugs",
                             TRUE ~ "Something's wrong"))%>%
  #Establish the postive/negative time difference
  
  
  
  #########
# PLLEEEAAASE
# TO DO:
# double check the it works fine because I'm only 70% certain in this part
#########
mutate(timeDif = case_when(
  confine > Maxtime ~ (confine-Maxtime),
  confine < Mintime ~ -1*(Mintime-confine),
  TRUE ~ 0
))%>%
  #creating a column for neg/positive bias
  mutate(bias = case_when(
    timeDif < 0 ~ "NEG",
    timeDif > 0 ~ "POS",
    timeDif == 0 ~ "NO"
  ))%>%
  mutate(raceChar = case_when(
    race==1 ~ "White",
    race==2 ~ "Black",
    race==3 ~ "Native",
    race==4 ~ "Hispanic",
    race==5 ~ "Asian",
    race==6 ~ "Other",
    race==7 ~ "Unknown"
  ))

#
# Test run on the number of judges with bias
#
csv.3.updated %>%
  group_by(fullname, bias)%>%
  summarise(
    biased = n()
  )%>%
  filter(bias=="POS")%>%               #looking for positive bias
  arrange(-biased)

#
#Check a single judge
#
csv.3.updated %>%
  filter(fullname=="Quam Jay M.",          #Choose the name here
         bias=="POS")%>%
  group_by(raceChar)%>%
  summarise(
    time = mean(timeDif)
  )

#
#Checking mean bias over sample
#
csv.3.updated %>%
  filter(raceChar!="Unknown",
         raceChar!="Other",
         !(is.na(raceChar)),
         inctype!=2)%>%
  group_by(raceChar)%>%
  summarise(
    NumOfCases = n(),
    MeanTimeDif = mean(timeDif),
  )


################################################################################
############ADD COUNTY NAMES TO csv.3.updated
################################################################################

csv.3.updated$countyChar <- csv.1[match(csv.3.updated$county, csv.1$Code),2]


# mn_sf <- st_read("shp_counties_mn/mn_county_boundaries.shp")
mn_sf <- readOGR(dsn="shp_counties_mn",layer="mn_county_boundaries")
mn_sf <- spTransform(mn_sf, CRS("+init=epsg:4326"))


county.csv <- read_csv("countyData.csv")
#county.csv$countyChar <- csv.1[match(county.csv$county, csv.1$Code),2]

mn_sf <- merge(mn_sf, county.csv, by = "CTY_NAME", all=FALSE)



leaflet() %>%
  addTiles() %>%
  addPolygons(data=mn_sf, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", effect)(effect),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), label = ~effect)

