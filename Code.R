
#install.packages("leaflet")
#install.packages("maps")
#install.packages("mapproj")
#install.packages("RColorBrewer")
#install.packages("classInt")
#install.packages("googleVis")
#install.packages("stringi")
#install.packages("lubridate")
#install.packages("readxl")



library(leaflet)
library(maps)
library(mapproj)
library(RColorBrewer)
library(classInt)
library(googleVis)
library(stringi) #capitalize letters
library(lubridate)#extract year from date
library(networkD3)




setwd("C:/WorkStation/EDAV/Final_Project/")

#import data from year 2010 to 2016

data_311 <- read.csv("C:/WorkStation/EDAV/Final_Project/data/final_project_cleaned_data.csv", 
                     header= TRUE, sep=",",
                     quote = "\"", dec =".")

data_311$City <- stri_trans_totitle(data_311$City )

data_311$Borough <- stri_trans_totitle(data_311$Borough )



######################################################################
#                                                                    #
#     Calendar: Daily Illegal Parking Count in New York City         #
#                                                                    #
######################################################################

illegal_parking_calendar <- subset(data_311, Complaint.Type=="Illegal Parking")
illegal_parking_calendar$Created.Date <- as.numeric(as.Date(illegal_parking_calendar$Created.Date,format='%m/%d/%Y'))
illegal_parking_calendar$Created.Date <- as.Date(illegal_parking_calendar$Created.Date,origin="1970-01-01")
for_calendar_illegal_parking <- as.data.frame(table(illegal_parking_calendar$Created.Date))
colnames(for_calendar_illegal_parking) <- c("Date","Illegal_Parking_Count")

for_calendar_illegal_parking$Date <- as.Date(for_calendar_illegal_parking$Date)
for_calendar_illegal_parking$Date <- as.numeric(for_calendar_illegal_parking$Date)
for_calendar_illegal_parking$Date <- as.Date(for_calendar_illegal_parking$Date,origin="1970-01-01")


c2 <- gvisCalendar(for_calendar_illegal_parking, datevar="Date", numvar="Illegal_Parking_Count",
                   options=list(
                       title="Daily Illegal Parking Count in New York City",
                       height=700,
                       calendar="{yearLabel: { fontName: 'Times-Roman',
                       fontSize: 32, color: '#1A8763', bold: true},
                       cellSize: 9,
                       cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                       focusedCellColor: {stroke:'red'}}")
                   )
#plot(c2)

unique(illegal_parking_calendar$Descriptor)


######################################################################
#                                                                    #
#     Calendar: Daily Blocked Driveway Count in New York City        #
#                                                                    #
######################################################################

Blocked_Driveway_calendar <- subset(data_311, Complaint.Type=="Blocked Driveway")
unique(Blocked_Driveway_calendar$Descriptor)
Blocked_Driveway_calendar$Created.Date <- as.numeric(as.Date(Blocked_Driveway_calendar$Created.Date,format='%m/%d/%Y'))
Blocked_Driveway_calendar$Created.Date <- as.Date(Blocked_Driveway_calendar$Created.Date,origin="1970-01-01")
for_calendar_Blocked_Driveway<- as.data.frame(table(Blocked_Driveway_calendar$Created.Date))
colnames(for_calendar_Blocked_Driveway) <- c("Date","Blocked_Driveway_Count")

for_calendar_Blocked_Driveway$Date <- as.Date(for_calendar_Blocked_Driveway$Date)
for_calendar_Blocked_Driveway$Date <- as.numeric(for_calendar_Blocked_Driveway$Date)
for_calendar_Blocked_Driveway$Date <- as.Date(for_calendar_Blocked_Driveway$Date,origin="1970-01-01")


c3 <- gvisCalendar(for_calendar_Blocked_Driveway, datevar="Date", numvar="Blocked_Driveway_Count",
                   options=list(
                       title="Daily Blocked Driveway Count in New York City",
                       height=700,
                       calendar="{yearLabel: { fontName: 'Times-Roman',
                       fontSize: 32, color: '#1A8763', bold: true},
                       cellSize: 9,
                       cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                       focusedCellColor: {stroke:'red'}}")
)
#plot(c3)


c23 <- gvisMerge(c2,c3, horizontal=TRUE) 
plot(c23)





#############################
#                           #
#   Google Motion Chart     #
#                           #
#############################

#Start to plot google motion chart

#import data 
motion_chart_rawExcel <- read.csv("C:/WorkStation/EDAV/Final_Project/data/google_motion_chart.csv", 
                                  header= TRUE, sep=",",
                                  quote = "\"", dec =".")


motion_chart_rawExcel <- motion_chart_rawExcel[-c(55,56,57,58,59,60,61,62,63,64),]
motion_chart_rawExcel <- motion_chart_rawExcel[,-c(33)]

read_motion_chart <- motion_chart_rawExcel[c(1,2,4,17,19,25,3)]
head(read_motion_chart)

test<- motion_chart_rawExcel[c(1,2,4,5,6,7,8,9,10,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,3)]
colnames(test) <- c("Descriptor","Year","Complaint Type","Major borough","Bronx count",
                    "Brooklyn count", "Manhattan count", "Queens count","Staten Island count",
                    "Status Assigned","Status Closed", "Status Open", "Descriptor Count",
                    "No Resolution", "Issue A Summon","Make An Arrest","Condition were gone",
                    "Report was prepared", "Police Action Not Necessary",
                    "Take Action to Fix Condition", "No Evidence of Violation",
                    "Unable to Gain Entry to Premise","Reviewed and provided additional information",
                    "Does not fall under jurisdiction","Non emergency response note for further service",
                    "Will ticket","Additional Information will be available","Insufficient contact information",
                    "Date")


M2 <- gvisMotionChart(test, idvar="Descriptor", timevar="Year",
                      options=list(height=600, width=900))
plot(M2)











#############################
#                           #
#           Map             #
#                           #
#############################

#Illegal parking and blocked driveway
#create data set
#2015_2016 1:253886
#2016 1:60883
#test:1:100

data311_for_map <- data_311[c(2,3,6,7,17,20,25,51,52)]
data311_Year15_16 <- data311_for_map[c(1:253886),]
data311_Year16 <- data311_for_map[c(1:60883),]




data311_Year16 <- data311_Year16[-which(is.na(data311_Year16$Latitude)), ]




map1_illegal_parking <- subset(data311_Year16, Complaint.Type=="Illegal Parking")
map1_blocked_drive_way <- subset(data311_Year16, Complaint.Type=="Blocked Driveway")


head(map1_illegal_parking)



#Popup content for map1

content_map1_illegal_parking <-paste0("<strong>Complaint Type: </strong>", 
                                      map1_illegal_parking$Complaint.Type, 
                                      "<br><strong>Descriptor: </strong>", 
                                      map1_illegal_parking$Descriptor,
                                      "<br><strong>Borough: </strong>",
                                      map1_illegal_parking$Borough,
                                      "<br><strong>City: </strong>",
                                      map1_illegal_parking$City,
                                      "<br><strong>Start Date: </strong>",
                                      map1_illegal_parking$Created.Date,
                                      "<br><strong>End Date: </strong>",
                                      map1_illegal_parking$Closed.Date,
                                      "<br><strong>Status: </strong>",
                                      map1_illegal_parking$Status)
content_map1_blocked_drive_way <-paste0("<strong>Complaint Type: </strong>", 
                                        map1_blocked_drive_way$Complaint.Type, 
                                        "<br><strong>Descriptor: </strong>", 
                                        map1_blocked_drive_way$Descriptor,
                                        "<br><strong>Borough: </strong>",
                                        map1_blocked_drive_way$Borough,
                                        "<br><strong>City: </strong>",
                                        map1_blocked_drive_way$City,
                                        "<br><strong>Start Date: </strong>",
                                        map1_blocked_drive_way$Created.Date,
                                        "<br><strong>End Date: </strong>",
                                        map1_blocked_drive_way$Closed.Date,
                                        "<br><strong>Status: </strong>",
                                        map1_blocked_drive_way$Status)





#map it!
map1 <- leaflet() %>% 
    
    setView(lng = -74.006605, lat = 40.714623, zoom = 11) %>%
    
    # Add tiles as baseGroup  
    addProviderTiles("CartoDB.DarkMatter",group = "Dark Map") %>%
    addProviderTiles("OpenStreetMap", group = "Basic Map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%
    
    addCircles( data = map1_illegal_parking , color = "#23D2D8", weight = 3, radius=60,  
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map1_illegal_parking), group = "Illegal Parking")%>%
    addCircles( data = map1_blocked_drive_way, color = "#EA7956",weight = 3, radius=60, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map1_blocked_drive_way), group = "Blocked Drive Way")%>%
    
    # Layers control
    addLayersControl(
        baseGroups = c("Dark Map","Basic Map","Satellite Map"),
        overlayGroups = c("Illegal Parking","Blocked Drive Way"),
        
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    addLegend("topright", colors = c("#23D2D8", "#EA7956"), 
              labels = c("Illegal Parking Complaint", "Blocked Drive Way Complaint"),    
              opacity = 2,  title = "Complaint Type" ) 

map1



#===================================================================================================


#map2    1/26/2016 Illegal Parking and Blocked Drive Way!
#prepare data set   1/26/16 45383:46367
# 1/25/16 46368:47384

data311_01_26_16 <- data311_for_map[c(45383:47384),]
data311_01_26_16 <- data311_01_26_16[-which(is.na(data311_01_26_16$Latitude)), ]

map2_illegal_parking <- subset(data311_01_26_16, Complaint.Type=="Illegal Parking")
map2_blocked_drive_way <- subset(data311_01_26_16, Complaint.Type=="Blocked Driveway")



#Popup content for map2

content_map2_illegal_parking <-paste0("<strong>Complaint Type: </strong>", 
                                      map2_illegal_parking$Complaint.Type, 
                                      "<br><strong>Descriptor: </strong>", 
                                      map2_illegal_parking$Descriptor,
                                      "<br><strong>Borough: </strong>",
                                      map2_illegal_parking$Borough,
                                      "<br><strong>City: </strong>",
                                      map2_illegal_parking$City,
                                      "<br><strong>Start Date: </strong>",
                                      map2_illegal_parking$Created.Date,
                                      "<br><strong>End Date: </strong>",
                                      map2_illegal_parking$Closed.Date,
                                      "<br><strong>Status: </strong>",
                                      map2_illegal_parking$Status)
content_map2_blocked_drive_way <-paste0("<strong>Complaint Type: </strong>", 
                                        map2_blocked_drive_way$Complaint.Type, 
                                        "<br><strong>Descriptor: </strong>", 
                                        map2_blocked_drive_way$Descriptor,
                                        "<br><strong>Borough: </strong>",
                                        map2_blocked_drive_way$Borough,
                                        "<br><strong>City: </strong>",
                                        map2_blocked_drive_way$City,
                                        "<br><strong>Start Date: </strong>",
                                        map2_blocked_drive_way$Created.Date,
                                        "<br><strong>End Date: </strong>",
                                        map2_blocked_drive_way$Closed.Date,
                                        "<br><strong>Status: </strong>",
                                        map2_blocked_drive_way$Status)





#map it!
map2 <- leaflet() %>% 
    
    setView(lng = -74.006605, lat = 40.714623, zoom = 11) %>%
    
    # Add tiles as baseGroup  
    addProviderTiles("CartoDB.DarkMatter",group = "Dark Map") %>%
    addProviderTiles("OpenStreetMap", group = "Basic Map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%
    
    addCircles( data = map2_illegal_parking , color = "#23D2D8", weight = 3, radius=60,  
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map2_illegal_parking), group = "Illegal Parking")%>%
    addCircles( data = map2_blocked_drive_way, color = "#EA7956",weight = 3, radius=60, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map2_blocked_drive_way), group = "Blocked Drive Way")%>%
    
    # Layers control
    addLayersControl(
        baseGroups = c("Dark Map","Basic Map","Satellite Map"),
        overlayGroups = c("Illegal Parking","Blocked Drive Way"),
        
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    addLegend("topright", colors = c("#23D2D8", "#EA7956"), 
              labels = c("Illegal Parking Complaint", "Blocked Drive Way Complaint"),    
              opacity = 2,  title = "Complaint Type" ) 

map2




#======================================================================================================
#map3  2016 descriptor




map3_BH <- subset(data311_Year16 , Descriptor == "Blocked Hydrant")
map3_BS <- subset(data311_Year16, Descriptor == "Blocked Sidewalk")
map3_COP <- subset(data311_Year16, Descriptor == "Commercial Overnight Parking")
map3_DT <- subset(data311_Year16, Descriptor == "Detached Trailer")
map3_DPV <- subset(data311_Year16, Descriptor == "Double Parked Vehicle")
map3_NAPA <- subset(data311_Year16, Descriptor == "No Access / Partial Access")
map3_OCS <- subset(data311_Year16, Descriptor == "Overnight Commercial Storage")
map3_PPSV <- subset(data311_Year16, Descriptor == "Posted Parking Sign Violation")
map3_UBL <- subset(data311_Year16, Descriptor == "Unauthorized Bus Layover")





#Popup content for map1


content_map3_BH <-paste0("<strong>Complaint Type: </strong>", 
                         map3_BH$Complaint.Type, 
                         "<br><strong>Descriptor: </strong>", 
                         map3_BH$Descriptor,
                         "<br><strong>Borough: </strong>",
                         map3_BH$Borough,
                         "<br><strong>City: </strong>",
                         map3_BH$City,
                         "<br><strong>Start Date: </strong>",
                         map3_BH$Created.Date,
                         "<br><strong>End Date: </strong>",
                         map3_BH$Closed.Date,
                         "<br><strong>Status: </strong>",
                         map3_BH$Status)


content_map3_BS <-paste0("<strong>Complaint Type: </strong>", 
                         map3_BS$Complaint.Type, 
                         "<br><strong>Descriptor: </strong>", 
                         map3_BS$Descriptor,
                         "<br><strong>Borough: </strong>",
                         map3_BS$Borough,
                         "<br><strong>City: </strong>",
                         map3_BS$City,
                         "<br><strong>Start Date: </strong>",
                         map3_BS$Created.Date,
                         "<br><strong>End Date: </strong>",
                         map3_BS$Closed.Date,
                         "<br><strong>Status: </strong>",
                         map3_BS$Status)


content_map3_COP <- paste0("<strong>Complaint Type: </strong>", 
                           map3_COP$Complaint.Type, 
                           "<br><strong>Descriptor: </strong>", 
                           map3_COP$Descriptor,
                           "<br><strong>Borough: </strong>",
                           map3_COP$Borough,
                           "<br><strong>City: </strong>",
                           map3_COP$City,
                           "<br><strong>Start Date: </strong>",
                           map3_COP$Created.Date,
                           "<br><strong>End Date: </strong>",
                           map3_COP$Closed.Date,
                           "<br><strong>Status: </strong>",
                           map3_COP$Status)


content_map3_DT <-paste0("<strong>Complaint Type: </strong>", 
                         map3_DT$Complaint.Type, 
                         "<br><strong>Descriptor: </strong>", 
                         map3_DT$Descriptor,
                         "<br><strong>Borough: </strong>",
                         map3_DT$Borough,
                         "<br><strong>City: </strong>",
                         map3_DT$City,
                         "<br><strong>Start Date: </strong>",
                         map3_DT$Created.Date,
                         "<br><strong>End Date: </strong>",
                         map3_DT$Closed.Date,
                         "<br><strong>Status: </strong>",
                         map3_DT$Status)


content_map3_DPV <-paste0("<strong>Complaint Type: </strong>", 
                          map3_DPV$Complaint.Type, 
                          "<br><strong>Descriptor: </strong>", 
                          map3_DPV$Descriptor,
                          "<br><strong>Borough: </strong>",
                          map3_DPV$Borough,
                          "<br><strong>City: </strong>",
                          map3_DPV$City,
                          "<br><strong>Start Date: </strong>",
                          map3_DPV$Created.Date,
                          "<br><strong>End Date: </strong>",
                          map3_DPV$Closed.Date,
                          "<br><strong>Status: </strong>",
                          map3_DPV$Status)

content_map3_NAPA <-paste0("<strong>Complaint Type: </strong>", 
                           map3_NAPA$Complaint.Type, 
                           "<br><strong>Descriptor: </strong>", 
                           map3_NAPA$Descriptor,
                           "<br><strong>Borough: </strong>",
                           map3_NAPA$Borough,
                           "<br><strong>City: </strong>",
                           map3_NAPA$City,
                           "<br><strong>Start Date: </strong>",
                           map3_NAPA$Created.Date,
                           "<br><strong>End Date: </strong>",
                           map3_NAPA$Closed.Date,
                           "<br><strong>Status: </strong>",
                           map3_NAPA$Status)


content_map3_OCS <-paste0("<strong>Complaint Type: </strong>", 
                          map3_OCS$Complaint.Type, 
                          "<br><strong>Descriptor: </strong>", 
                          map3_OCS$Descriptor,
                          "<br><strong>Borough: </strong>",
                          map3_OCS$Borough,
                          "<br><strong>City: </strong>",
                          map3_OCS$City,
                          "<br><strong>Start Date: </strong>",
                          map3_OCS$Created.Date,
                          "<br><strong>End Date: </strong>",
                          map3_OCS$Closed.Date,
                          "<br><strong>Status: </strong>",
                          map3_OCS$Status)

content_map3_PPSV <-paste0("<strong>Complaint Type: </strong>", 
                           map3_PPSV$Complaint.Type, 
                           "<br><strong>Descriptor: </strong>", 
                           map3_PPSV$Descriptor,
                           "<br><strong>Borough: </strong>",
                           map3_PPSV$Borough,
                           "<br><strong>City: </strong>",
                           map3_PPSV$City,
                           "<br><strong>Start Date: </strong>",
                           map3_PPSV$Created.Date,
                           "<br><strong>End Date: </strong>",
                           map3_PPSV$Closed.Date,
                           "<br><strong>Status: </strong>",
                           map3_PPSV$Status)

content_map3_UBL <-paste0("<strong>Complaint Type: </strong>", 
                          map3_UBL$Complaint.Type, 
                          "<br><strong>Descriptor: </strong>", 
                          map3_UBL$Descriptor,
                          "<br><strong>Borough: </strong>",
                          map3_UBL$Borough,
                          "<br><strong>City: </strong>",
                          map3_UBL$City,
                          "<br><strong>Start Date: </strong>",
                          map3_UBL$Created.Date,
                          "<br><strong>End Date: </strong>",
                          map3_UBL$Closed.Date,
                          "<br><strong>Status: </strong>",
                          map3_UBL$Status)











#map it!
map3 <- leaflet() %>% 
    
    setView(lng = -74.006605, lat = 40.714623, zoom = 11) %>%
    
    # Add tiles as baseGroup  
    addProviderTiles("CartoDB.DarkMatter",group = "Dark Map") %>%
    addProviderTiles("OpenStreetMap", group = "Basic Map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%
    
    addCircles( data = map3_BH, color = "#FA5858",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_BH), group = "Blocked Hydrant")%>%
    addCircles( data = map3_BS, color = "#B18904",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_BS), group = "Blocked Sidewalk")%>%
    
    addCircles( data = map3_COP, color = "#86B404",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_COP), group = "Commercial Overnight Parking")%>%
    
    addCircles( data = map3_DT, color = "#01DF01",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_DT), group = "Detached Trailer")%>%
    
    addCircles( data = map3_DPV, color = "#04B486",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_DPV), group = "Double Parked Vehicle")%>%
    
    addCircles( data = map3_NAPA, color = "#00BFFF",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_NAPA), group = "No Access / Partial Access")%>%
    
    addCircles( data = map3_OCS, color = "#58ACFA",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_OCS), group = "Overnight Commercial Storage")%>%
    
    addCircles( data = map3_PPSV, color = "#D358F7",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_PPSV), group = "Posted Parking Sign Violation")%>%
    
    addCircles( data = map3_UBL, color = "#FE2EF7",weight = 3, radius=20, 
                stroke = TRUE, fillOpacity = 0.8, lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_map3_UBL), group = "Unauthorized Bus Layover")%>%
    
    
    # Layers control
    addLayersControl(
        baseGroups = c("Dark Map","Basic Map","Satellite Map"),
        overlayGroups = c("No Access / Partial Access",
                          "Posted Parking Sign Violation","Blocked Hydrant","Commercial Overnight Parking",
                          "Blocked Sidewalk","Double Parked Vehicle","Overnight Commercial Storage",
                          "Unauthorized Bus Layover","Detached Trailer" ),
        
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    addLegend("bottomright", colors = c("#00BFFF", "#D358F7","#FA5858","#86B404","#B18904","#04B486","#58ACFA",
                                        "#FE2EF7","#01DF01"), 
              labels = c("No Access / Partial Access",
                         "Posted Parking Sign Violation","Blocked Hydrant","Commercial Overnight Parking",
                         "Blocked Sidewalk","Double Parked Vehicle","Overnight Commercial Storage",
                         "Unauthorized Bus Layover","Detached Trailer"),    
              opacity = 2,  title = "Descriptor" ) 

map3

#=======================================================================================
#NYC zip code polygon map
#data is ready, start to map
#prepare for the merged shapefile https://www.mapbox.com/tilemill/docs/guides/joining-data/

library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(leafletR)
library(sp)


setwd("C:/WorkStation/EDAV/Final_Project/")

subdat <- readOGR("data/ZIP_CODE_040114/zip_code_merged_one.shp",
                  layer = "zip_code_merged_one", encoding = "UTF-8")


merged_data_for_zip_map<- read.csv("C:/WorkStation/EDAV/Final_Project/data/merged_data_for_zip_map.csv", 
                                   header= TRUE, sep=",",
                                   quote = "\"", dec =".")

typeof(subdat@data$"No Access / Partial Access Count")




#subdat@data[is.na(subdat@data)] <- 0

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat<-spTransform(subdat, CRS("+init=epsg:4326"))


# ----- change name of field we will map
# ----- change name of field we will map
names(subdat)[names(subdat) == "PO_NAME"]<-"CITY"
names(subdat)[names(subdat) == "ZIPCODE"]<-"Zip Code"
names(subdat)[names(subdat) == "POPULATION"]<-"Population"

names(subdat)[names(subdat) == "merged_d_1"]<-"Complaints Count"
names(subdat)[names(subdat) == "merged_d_2"]<-"Illegal Parking Complaints Count"
names(subdat)[names(subdat) == "merged_d_3"]<-"Blcoked Driveway Complaints Count"
names(subdat)[names(subdat) == "merged_d_4"]<-"No Access / Partial Access Count"

head(merged_data_for_zip_map)

names(subdat)[names(subdat) == "merged_d_5"]<-"Commercial Overnight Parking Count"
names(subdat)[names(subdat) == "merged_d_6"]<-"Blocked Hydrant Count"
names(subdat)[names(subdat) == "merged_d_7"]<-"Posted Parking Sign Violation Count"
names(subdat)[names(subdat) == "merged_d_8"]<-"Double Parked Vehicle Count"

names(subdat)[names(subdat) == "merged_d_9"]<-"Blocked Sidewalk Count"
names(subdat)[names(subdat) == "merged_d10"]<-"Unauthorized Bus Layover Count"
names(subdat)[names(subdat) == "merged_d11"]<-"Detached Trailer Count"
names(subdat)[names(subdat) == "merged_d12"]<-"Overnight Commercial Storage Count"

head(subdat@data)
test <- data.frame(subdat@data)

# ----- save the data slot


subdat_data<-subdat@data[,c("Zip Code", "CITY","Population","Complaints Count",
                            "Illegal Parking Complaints Count",
                            "Blcoked Driveway Complaints Count",
                            "No Access / Partial Access Count",
                            "Commercial Overnight Parking Count",
                            "Blocked Hydrant Count","Posted Parking Sign Violation Count",
                            "Double Parked Vehicle Count","Blocked Sidewalk Count",
                            "Unauthorized Bus Layover Count","Detached Trailer Count",
                            "Overnight Commercial Storage Count")]



# ----- simplification yields a SpatialPolygons class
subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)



downloaddir <- "C:/WorkStation/EDAV/Final_Project/data/ZIP_CODE_040114"


#ready leaflet
leafdat<-paste(downloaddir, "/", "County_2010Census_DP1", ".geojson", sep="") 


writeOGR(subdat, leafdat, layer="", driver="GeoJSON")


# ----- Create the cuts
cuts<-round(quantile(subdat$Population, probs = seq(0, 1, 0.2), na.rm = FALSE), 0)
cuts[1]<- 0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup<-c("CITY","Zip Code", "Population","Complaints Count")


# ----- Gradulated style based on an attribute
sty<-styleGrad(prop="Population", breaks=cuts, right=FALSE, style.par="col",
               style.val=rev(heat.colors(5)), leg="Population(2010)", lwd=1)




#base.map: osm, tl, 
# ----- Create the map and load into browser
map4 <-leaflet(data=leafdat, dest=downloaddir, style=sty,
               title="index", base.map=list("osm", "mqsat", "tls"),
               incl.data=TRUE,  popup=popup)

# ----- to look at the map you can use this code
map4




#-------------------------------------------------------------------------------
#COMPLAINT COUNTS
library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(leafletR)
library(sp)


setwd("C:/WorkStation/EDAV/Final_Project/")

subdat1 <- readOGR("data/ZIP_CODE_040114/zip_code_merged_one.shp",
                   layer = "zip_code_merged_one", encoding = "UTF-8")






# ----- Transform to EPSG 4326 - WGS84 (required)
subdat1<-spTransform(subdat1, CRS("+init=epsg:4326"))


# ----- change name of field we will map
# ----- change name of field we will map
names(subdat1)[names(subdat1) == "PO_NAME"]<-"CITY"
names(subdat1)[names(subdat1) == "ZIPCODE"]<-"Zip Code"
names(subdat1)[names(subdat1) == "POPULATION"]<-"Population"

names(subdat1)[names(subdat1) == "merged_d_1"]<-"Complaints Count"
names(subdat1)[names(subdat1) == "merged_d_2"]<-"Illegal Parking Complaints Count"
names(subdat1)[names(subdat1) == "merged_d_3"]<-"Blcoked Driveway Complaints Count"
names(subdat1)[names(subdat1) == "merged_d_4"]<-"No Access / Partial Access Count"

head(merged_data_for_zip_map)

names(subdat1)[names(subdat1) == "merged_d_5"]<-"Commercial Overnight Parking Count"
names(subdat1)[names(subdat1) == "merged_d_6"]<-"Blocked Hydrant Count"
names(subdat1)[names(subdat1) == "merged_d_7"]<-"Posted Parking Sign Violation Count"
names(subdat1)[names(subdat1) == "merged_d_8"]<-"Double Parked Vehicle Count"

names(subdat1)[names(subdat1) == "merged_d_9"]<-"Blocked Sidewalk Count"
names(subdat1)[names(subdat1) == "merged_d10"]<-"Unauthorized Bus Layover Count"
names(subdat1)[names(subdat1) == "merged_d11"]<-"Detached Trailer Count"
names(subdat1)[names(subdat1) == "merged_d12"]<-"Overnight Commercial Storage Count"



# ----- save the data slot


subdat_data1<-subdat1@data[,c("Zip Code", "CITY","Population","Complaints Count",
                              "Illegal Parking Complaints Count",
                              "Blcoked Driveway Complaints Count",
                              "No Access / Partial Access Count",
                              "Commercial Overnight Parking Count",
                              "Blocked Hydrant Count","Posted Parking Sign Violation Count",
                              "Double Parked Vehicle Count","Blocked Sidewalk Count",
                              "Unauthorized Bus Layover Count","Detached Trailer Count",
                              "Overnight Commercial Storage Count")]



# ----- simplification yields a SpatialPolygons class
subdat1<-gSimplify(subdat1,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat1<-SpatialPolygonsDataFrame(subdat1, data=subdat_data1)



downloaddir <- "C:/WorkStation/EDAV/Final_Project/data/ZIP_CODE_040114"


#ready leaflet
leafdat1<-paste(downloaddir, "/", "County_2010Census_Dp11", ".geojson", sep="") 


writeOGR(subdat1, leafdat1, layer="", driver="GeoJSON")


# ----- Create the cuts
cuts1<-round(quantile(subdat1$"Complaints Count", probs = seq(0, 1, 0.2), na.rm = TRUE), 0)
cuts1[1]<- 0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup1<-c("CITY","Zip Code", "Population","Complaints Count")



# ----- Gradulated style based on an attribute
sty1<-styleGrad(prop="Complaints Count", breaks=cuts1, right=FALSE, style.par="col",
                style.val=rev(heat.colors(5)), leg="Complaints Count", lwd=1)




#base.map: osm, tl, 
# ----- Create the map and load into browser
map5 <-leaflet(data=leafdat1, dest=downloaddir, style=sty1,
               title="Complaints Count", base.map=list("osm", "mqsat", "tls"),
               incl.data=TRUE,  popup=popup1)

# ----- to look at the map you can use this code
browseURL(map5)




#------------------------------------------------------------------------------------
#Illegal parking
library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(leafletR)
library(sp)


setwd("C:/WorkStation/EDAV/Final_Project/")

subdat2 <- readOGR("data/ZIP_CODE_040114/zip_code_merged_one.shp",
                   layer = "zip_code_merged_one", encoding = "UTF-8")






# ----- Transform to EPSG 4326 - WGS84 (required)
subdat2<-spTransform(subdat2, CRS("+init=epsg:4326"))


# ----- change name of field we will map
# ----- change name of field we will map
names(subdat2)[names(subdat2) == "PO_NAME"]<-"CITY"
names(subdat2)[names(subdat2) == "ZIPCODE"]<-"Zip Code"
names(subdat2)[names(subdat2) == "POPULATION"]<-"Population"

names(subdat2)[names(subdat2) == "merged_d_1"]<-"Complaints Count"
names(subdat2)[names(subdat2) == "merged_d_2"]<-"Illegal Parking Complaints Count"
names(subdat2)[names(subdat2) == "merged_d_3"]<-"Blcoked Driveway Complaints Count"
names(subdat2)[names(subdat2) == "merged_d_4"]<-"No Access / Partial Access Count"


names(subdat2)[names(subdat2) == "merged_d_5"]<-"Commercial Overnight Parking Count"
names(subdat2)[names(subdat2) == "merged_d_6"]<-"Blocked Hydrant Count"
names(subdat2)[names(subdat2) == "merged_d_7"]<-"Posted Parking Sign Violation Count"
names(subdat2)[names(subdat2) == "merged_d_8"]<-"Double Parked Vehicle Count"

names(subdat2)[names(subdat2) == "merged_d_9"]<-"Blocked Sidewalk Count"
names(subdat2)[names(subdat2) == "merged_d10"]<-"Unauthorized Bus Layover Count"
names(subdat2)[names(subdat2) == "merged_d11"]<-"Detached Trailer Count"
names(subdat2)[names(subdat2) == "merged_d12"]<-"Overnight Commercial Storage Count"



# ----- save the data slot


subdat_data2<-subdat2@data[,c("Zip Code", "CITY","Population","Complaints Count",
                              "Illegal Parking Complaints Count",
                              "Blcoked Driveway Complaints Count",
                              "No Access / Partial Access Count",
                              "Commercial Overnight Parking Count",
                              "Blocked Hydrant Count","Posted Parking Sign Violation Count",
                              "Double Parked Vehicle Count","Blocked Sidewalk Count",
                              "Unauthorized Bus Layover Count","Detached Trailer Count",
                              "Overnight Commercial Storage Count")]



# ----- simplification yields a SpatialPolygons class
subdat2<-gSimplify(subdat2,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat2<-SpatialPolygonsDataFrame(subdat2, data=subdat_data2)



downloaddir <- "C:/WorkStation/EDAV/Final_Project/data/ZIP_CODE_040114"


#ready leaflet
leafdat2<-paste(downloaddir, "/", "County_2010Census_Dp12", ".geojson", sep="") 


writeOGR(subdat2, leafdat2, layer="", driver="GeoJSON")


# ----- Create the cuts
cuts2<-round(quantile(subdat2$"Illegal Parking Complaints Count", probs = seq(0, 1, 0.2), na.rm = TRUE), 0)
cuts2[1]<- 0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup2<-c("CITY","Zip Code", "Population","Complaints Count","Illegal Parking Complaints Count",
          "Blcoked Driveway Complaints Count")



# ----- Gradulated style based on an attribute
sty2<-styleGrad(prop="Illegal Parking Complaints Count", breaks=cuts2, right=FALSE, style.par="col",
                style.val=rev(heat.colors(5)), leg="Illegal Parking Complaints Count", lwd=1)




#base.map: osm, tl, 
# ----- Create the map and load into browser
map6 <-leaflet(data=leafdat2, dest=downloaddir, style=sty2,
               title="Illegal Parking Complaints Count", base.map=list("osm", "mqsat", "tls"),
               incl.data=TRUE,  popup=popup2)

# ----- to look at the map you can use this code
browseURL(map6)





#------------------------------------------------------------------------------------
#Blocked Driveway

library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(leafletR)
library(sp)


setwd("C:/WorkStation/EDAV/Final_Project/")

subdat3 <- readOGR("data/ZIP_CODE_040114/zip_code_merged_one.shp",
                   layer = "zip_code_merged_one", encoding = "UTF-8")






# ----- Transform to EPSG 4326 - WGS84 (required)
subdat3<-spTransform(subdat3, CRS("+init=epsg:4326"))


# ----- change name of field we will map
# ----- change name of field we will map
names(subdat3)[names(subdat3) == "PO_NAME"]<-"CITY"
names(subdat3)[names(subdat3) == "ZIPCODE"]<-"Zip Code"
names(subdat3)[names(subdat3) == "POPULATION"]<-"Population"

names(subdat3)[names(subdat3) == "merged_d_1"]<-"Complaints Count"
names(subdat3)[names(subdat3) == "merged_d_2"]<-"Illegal Parking Complaints Count"
names(subdat3)[names(subdat3) == "merged_d_3"]<-"Blcoked Driveway Complaints Count"
names(subdat3)[names(subdat3) == "merged_d_4"]<-"No Access / Partial Access Count"

names(subdat3)[names(subdat3) == "merged_d_5"]<-"Commercial Overnight Parking Count"
names(subdat3)[names(subdat3) == "merged_d_6"]<-"Blocked Hydrant Count"
names(subdat3)[names(subdat3) == "merged_d_7"]<-"Posted Parking Sign Violation Count"
names(subdat3)[names(subdat3) == "merged_d_8"]<-"Double Parked Vehicle Count"

names(subdat3)[names(subdat3) == "merged_d_9"]<-"Blocked Sidewalk Count"
names(subdat3)[names(subdat3) == "merged_d10"]<-"Unauthorized Bus Layover Count"
names(subdat3)[names(subdat3) == "merged_d11"]<-"Detached Trailer Count"
names(subdat3)[names(subdat3) == "merged_d12"]<-"Overnight Commercial Storage Count"



# ----- save the data slot


subdat_data3<-subdat3@data[,c("Zip Code", "CITY","Population","Complaints Count",
                              "Illegal Parking Complaints Count",
                              "Blcoked Driveway Complaints Count",
                              "No Access / Partial Access Count",
                              "Commercial Overnight Parking Count",
                              "Blocked Hydrant Count","Posted Parking Sign Violation Count",
                              "Double Parked Vehicle Count","Blocked Sidewalk Count",
                              "Unauthorized Bus Layover Count","Detached Trailer Count",
                              "Overnight Commercial Storage Count")]



# ----- simplification yields a SpatialPolygons class
subdat3<-gSimplify(subdat3,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat3<-SpatialPolygonsDataFrame(subdat3, data=subdat_data3)



downloaddir <- "C:/WorkStation/EDAV/Final_Project/data/ZIP_CODE_040114"


#ready leaflet
leafdat3<-paste(downloaddir, "/", "County_2010Census_Dp13", ".geojson", sep="") 


writeOGR(subdat3, leafdat3, layer="", driver="GeoJSON")


# ----- Create the cuts
cuts3<-round(quantile(subdat3$"Blcoked Driveway Complaints Count",
                      probs = seq(0, 1, 0.2), na.rm = TRUE), 0)
cuts3[1]<- 0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup3<-c("CITY","Zip Code", "Population","Complaints Count","Illegal Parking Complaints Count",
          "Blcoked Driveway Complaints Count")



# ----- Gradulated style based on an attribute
sty3<-styleGrad(prop="Blcoked Driveway Complaints Count", breaks=cuts3, right=FALSE, style.par="col",
                style.val=rev(heat.colors(5)), leg= "Blcoked Driveway Complaints Count", lwd=1)




#base.map: osm, tl, 
# ----- Create the map and load into browser
map7 <-leaflet(data=leafdat3, dest=downloaddir, style=sty3,
               title="Blcoked Driveway Complaints Count", base.map=list("osm", "mqsat", "tls"),
               incl.data=TRUE,  popup=popup3)

# ----- to look at the map you can use this code
browseURL(map7)




#======================================================================================================
#Hottest location map


check_hottest_point_all_map <- read.csv("C:/WorkStation/EDAV/Final_Project/data/check_hottest_point_all.csv", 
                                        header= TRUE, sep=",",
                                        quote = "\"", dec =".")


check_hotpoint_map_illegal_parking_map <- read.csv("C:/WorkStation/EDAV/Final_Project/data/check_hotpoint_map_illegal_parking.csv", 
                                                   header= TRUE, sep=",",
                                                   quote = "\"", dec =".")



check_hotpoint_map_blocked_driveway_map <- read.csv("C:/WorkStation/EDAV/Final_Project/data/check_hotpoint_map_blocked_driveway.csv", 
                                                    header= TRUE, sep=",",
                                                    quote = "\"", dec =".")





top_ten_ticket <- read.csv("C:/WorkStation/EDAV/Final_Project/data/top_ten_ticketing.csv", 
                           header= TRUE, sep=",",
                           quote = "\"", dec =".")



#Start to map__________________


content_hottest_point_all <- paste0( "<strong>Address: </strong>", 
                                     check_hottest_point_all_map$Address,
                                     "<br><strong>Total Complaint Count: </strong>", 
                                     check_hottest_point_all_map$ComplaintCount)

content_hotpoint_map_illegal_parking_map <- paste0("<strong>Address: </strong>", 
                                                   check_hotpoint_map_illegal_parking_map$Address,
                                                   "<br><strong>Illegal Parking Complaint Count: </strong>", 
                                                   check_hotpoint_map_illegal_parking_map$IllegalComplaintCount)

content_hotpoint_map_blocked_driveway_map <- paste0("<strong>Address: </strong>",
                                                    check_hotpoint_map_blocked_driveway_map$Address,
                                                    "<br><strong>Blocked Driveway Complaint Count: </strong>", 
                                                    check_hotpoint_map_blocked_driveway_map$BlockedComplaintCount)

content_top_ten_ticket <-  paste0("<strong>Address: </strong>",
                                  top_ten_ticket$Address,
                                  "<br><strong>Ticket Count: </strong>", 
                                  top_ten_ticket$Ticket_Account)

myIcons <- icons(
    iconUrl = "http://www.buyautoinsurance.com/wp-content/featured-content/seatbelt/images/traffic-ticket.png",
    
    iconWidth = 50, iconHeight = 50,
    iconAnchorX = 12, iconAnchorY = 54
)


#map it!
map8 <- leaflet() %>% 
    
    setView(lng = -74.006605, lat = 40.714623, zoom = 11) %>%
    
    # Add tiles as baseGroup  
    addProviderTiles("CartoDB.DarkMatter",group = "Dark Map") %>%
    addProviderTiles("OpenStreetMap", group = "Basic Map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%
    
    addMarkers(data=top_ten_ticket,lat = ~Latitude, lng = ~Longitude,
               icon = myIcons,popup = ~(content_top_ten_ticket),group = "Top Ten Ticketing Location")%>%
    
    addCircles( data = check_hottest_point_all_map , color = "#F7FE2E",    
                lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_hottest_point_all), radius = ~(ComplaintCount)*3, group = "All complaints")%>%
    addCircles( data = check_hotpoint_map_illegal_parking_map , color = "#58FAF4",   
                lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_hotpoint_map_illegal_parking_map), radius = ~(IllegalComplaintCount)*3, group = "Illegal Parking complaints")%>%
    addCircles( data = check_hotpoint_map_blocked_driveway_map, color = "#F781F3", 
                lat = ~Latitude, lng = ~Longitude,
                popup = ~(content_hotpoint_map_blocked_driveway_map), radius = ~(BlockedComplaintCount)*3, group = "Blocked Driveway complaints")%>%
    
    
    # Layers control
    addLayersControl(
        baseGroups = c("Dark Map","Basic Map","Satellite Map"),
        overlayGroups = c("All complaints","Illegal Parking complaints","Blocked Driveway complaints","Top Ten Ticketing Location"),
        
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    hideGroup("All complaints") %>% 
    
    
    
    addLegend("topright", colors = c("#F7FE2E", "#58FAF4","#F781F3"), 
              labels = c("All complaints", "Illegal Parking complaints","Blocked Driveway complaints"),    
              opacity = 2,  title = "Complaint Type" ) 

map8






#############################
#                           #
#       NetWork  D3         #
#                           #
#############################

#================================================================================================
library()
#import data 
newwork_data <- read.csv("C:/WorkStation/EDAV/Final_Project/data/google_motion_chart.csv", 
                         header= TRUE, sep=",",
                         quote = "\"", dec =".")
newwork_data <- newwork_data[-c(64),]


#descripter-borough
network1 <- newwork_data[c(1,5)]
colnames(network1) <- c("src","target")
simpleNetwork(network1,  
              fontSize = 12, 
              fontFamily = "serif", linkColour = "#666",
              nodeColour = "#3182bd", nodeClickColour = "#FE2E64", 
              textColour = "#0B2161", opacity = 0.6, zoom = T)


network2 <- newwork_data[c(1,4)]
colnames(network2) <- c("src","target")

simpleNetwork(network2, Source = NULL, Target = NULL, height = NULL, 
              width = NULL, 
              fontSize = 12, fontFamily = "serif", linkColour = "#666",
              nodeColour = "#9AFE2E", nodeClickColour = "#E34A33", 
              textColour = "#084B8A", opacity = 0.6, zoom = T)

network3 <- newwork_data[c(4,5)]
colnames(network3) <- c("src","target")
simpleNetwork(network3, Source = NULL, Target = NULL, height = NULL, 
              width = NULL, 
              fontSize = 12, fontFamily = "serif", linkColour = "#666",
              nodeColour = "#F781F3", nodeClickColour = "#FE2E64", 
              textColour = "#0B2161", opacity = 0.6, zoom = T)


network4 <- newwork_data[c(2,4)]
colnames(network4) <- c("src","target")
simpleNetwork(network4, Source = NULL, Target = NULL, height = NULL, 
              width = NULL, 
              fontSize = 12, fontFamily = "serif", linkColour = "#666",
              nodeColour = "#FF8000", nodeClickColour = "#FE2E64", 
              textColour = "#084B8A", opacity = 0.6, zoom = T)



#=================================================================================
#############################
#                           #
#         Plotly            #
#                           #
#############################

#install.packages("plotly")
library(plotly)

#import data 
plotly_data <- data_311

head(plotly_data)
p1 <- ggplot(data = plotly_data, aes(x = Borough, fill = Descriptor)) +
    geom_bar(position = "dodge")
ggplotly(p1)


#=================================================================================
#############################
#                           #
#    highcharter            #
#                           #
#############################

#install.packages("magrittr")
#install.packages("highcharter")
library(magrittr)
library(highcharter)



highcharter_data <- read.csv("C:/WorkStation/EDAV/Final_Project/data/google_motion_chart.csv", 
                             header= TRUE, sep=",",
                             quote = "\"", dec =".")
highcharter_data <- highcharter_data[-c(64),]
highchart() %>% 
    hc_title(text = "Police Action not Necessary vs. No Evidence of violation") %>% 
    hc_add_serie_scatter(highcharter_data$police_action_not_necessary, highcharter_data$no_evidence_of_violation,
                         highcharter_data$Descriptor_Count, highcharter_data$no_evidence_of_violation )








#===============================================================================

#############################
#                           #
#        d3heatmap          #
#                           #
#############################
#install.packages("d3heatmap")
library(d3heatmap)

#year2015
d3heatmap_data <- read.csv("C:/WorkStation/EDAV/Final_Project/data/google_motion_chart.csv", 
                           header= TRUE, sep=",",
                           quote = "\"", dec =".")
d3heatmap_data <- d3heatmap_data[c(46:54),]

d3heatmap_data2 <- d3heatmap_data[,-1]
rownames(d3heatmap_data2) <- d3heatmap_data[,1]


d3heatmap_data2 <- d3heatmap_data2[c(18:31)]
d3heatmap_data2 <- d3heatmap_data2[,-c(13)]

d3heatmap(d3heatmap_data2, scale="column" ,colors="Greens",xaxis_font_size = 10, yaxis_font_size = 10,
          show_grid = TRUE )








