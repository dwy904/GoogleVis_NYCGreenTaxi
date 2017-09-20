# Loading required packages
library(ggplot2)
library(readr)
library(rgeos)
library(sp)
library(rgdal)
library(lubridate)
library(utils)
library(googleVis)
library(chron)

# Disable scientific notation
options(scipen=9999)

# Set current directory 
setwd('Input the Path to Folder, Data_File') 

# Load data from current directory (you do not have to run this line if you got the data already)
Green_Taxi = read_csv("green_tripdata_2015-09.csv")


## Procedure I -  Data Cleaning

# Remove the variable 'Ehail_fee' (All NAs)
Green_Taxi = Green_Taxi[,-17]
# Remove the total amount that is less than 0
Green_Taxi = subset(Green_Taxi, Total_amount >=0)
# Remove the observations that are negative in variable 'Extra'
Green_Taxi = subset(Green_Taxi, Extra >=0)
# Remove the observations that have 0 passenger counts
Green_Taxi = subset(Green_Taxi, Passenger_count > 0)
# Remove the observations that have 0 trip distance
Green_Taxi = subset(Green_Taxi, Trip_distance > 0)
# Remove the observations that has no 'Trip_type' recorded
Green_Taxi = Green_Taxi[is.na(Green_Taxi$Trip_type) == F,]
# Only visualize the cash / credit card payment observations
Green_Taxi = subset(Green_Taxi, Payment_type == 1 | Payment_type == 2)
Green_Taxi$Payment_type[Green_Taxi$Payment_type == 1] = 'Credit Card Payment'
Green_Taxi$Payment_type[Green_Taxi$Payment_type == 2] = 'Cash Payment'
# Rename the information in the variable, Trip_type
Green_Taxi$Trip_type[Green_Taxi$Trip_type == 1] = 'Street-Hail' 
Green_Taxi$Trip_type[Green_Taxi$Trip_type == 2] = 'Dispatch'

# Converting data format
Green_Taxi$RateCodeID = as.factor(Green_Taxi$RateCodeID)
Green_Taxi$Trip_type = as.factor(Green_Taxi$Trip_type)
Green_Taxi$Store_and_fwd_flag = as.factor(Green_Taxi$Store_and_fwd_flag)
Green_Taxi$Payment_type = as.factor(Green_Taxi$Payment_type)
Green_Taxi$VendorID = as.factor(Green_Taxi$VendorID)




# District Identification (Pickup and Dropoff location)
# Load required package for geo-data processing
Map_NYC <- readOGR("ZillowNeighborhoods-NY.shp", layer="ZillowNeighborhoods-NY")
Map_Filter = Map_NYC[Map_NYC$City == 'New York', ]
Location_Data_Pickup <- data.frame(Longitude = Green_Taxi$Pickup_longitude, 
                                   Latitude = Green_Taxi$Pickup_latitude)
Location_Data_Dropoff <- data.frame(Longitude = Green_Taxi$Dropoff_longitude, 
                                    Latitude = Green_Taxi$Dropoff_latitude)
coordinates(Location_Data_Pickup) <- ~ Longitude + Latitude
proj4string(Location_Data_Pickup) <- proj4string(Map_Filter)
District_Pickup = as.character(over(Location_Data_Pickup, Map_Filter)$County)
coordinates(Location_Data_Dropoff) <- ~ Longitude + Latitude
proj4string(Location_Data_Dropoff) <- proj4string(Map_Filter)
District_Dropoff = as.character(over(Location_Data_Dropoff, Map_Filter)$County)

# Rename the information in the variable, Trip_type
District_Dropoff[District_Dropoff == 'Richmond'] = 'Staten Island'
District_Pickup[District_Pickup == 'Richmond'] = 'Staten Island'
District_Dropoff[District_Dropoff == 'New York'] = 'Manhattan'
District_Pickup[District_Pickup == 'New York'] = 'Manhattan'
District_Dropoff[District_Dropoff == 'Kings'] = 'Brooklyn'
District_Pickup[District_Pickup == 'Kings'] = 'Brooklyn'

Green_Taxi$District_Pickup = as.factor(District_Pickup)
Green_Taxi$District_Dropoff = as.factor(District_Dropoff)

# Create a factor variable for pickup and drop off hour  (00:00 ~ 23:00)
Green_Taxi$Pickup_Hour = hour(Green_Taxi$lpep_pickup_datetime)
Green_Taxi$Dropoff_Hour = hour(Green_Taxi$Lpep_dropoff_datetime)

# Remove the NA observations
Green_Taxi = na.omit(Green_Taxi)

# Create a observation count variable
Green_Taxi$Traffic_Count = 1

# Traffic Type
Green_Taxi$Traffic_Type = 'Inter-Borough'
Green_Taxi$Traffic_Type[Green_Taxi$District_Pickup == Green_Taxi$District_Dropoff] = 'Intra-Borough'


## Visualizing the data 

# Traffice Type Distribution
Traffic_Type = data.frame(table(Green_Taxi$Traffic_Type))
colnames(Traffic_Type) = c('Traffic Type', 'Count')
Traffice_Type_Pie <- gvisPieChart(Traffic_Type, 
                    options=list(width = 300, height = 290, fill = 10,
                                 colors = "['pink','lightblue']",
                                 legend="{position: 'bottom'}",
                                 text = "[{color:'white'}]",
                                 title = 'NYC Green Taxi Traffic Types'))
Traffice_Type_Pie$html$footer <- 'NYC Green Taxi Traffic Types'


# District Traffic Distribution
Inter_Borough_District = 
  aggregate(data = Green_Taxi[Green_Taxi$Traffic_Type == 'Inter-Borough',], 
            Traffic_Count ~ District_Pickup, FUN = sum) 
Intra_Borough_District = 
  aggregate(data = Green_Taxi[Green_Taxi$Traffic_Type == 'Intra-Borough',], 
            Traffic_Count ~ District_Pickup, FUN = sum) 
Borough_District = merge(Inter_Borough_District, Intra_Borough_District, by = 'District_Pickup')
colnames(Borough_District)[2:3] = c('Inter_Borough', 'Intra_Borough')
Bar_District <- gvisBarChart(Borough_District, "District_Pickup", c("Inter_Borough","Intra_Borough"),
                    options=list(isStacked= T,
                                 legend = "bottom",
                                 colors = "['pink','lightblue']",
                                 vAxis = "{gridlines:{count:3}}",
                                 width = 400, height = 280,
                                 vAxis = "{title:'District'}",
                                 title = 'Traffic Types By Districts'))
Bar_District$html$footer <- 'Traffic Types By Districts'; 


# Traffic Path
Traffic_Path_1 = aggregate(data = Green_Taxi[Green_Taxi$Traffic_Type == 'Inter-Borough',], FUN = sum,
                      Traffic_Count ~ Trip_type + District_Pickup)
Traffic_Path_1$District_Pickup = paste('', Traffic_Path_1$District_Pickup, sep = '')
colnames(Traffic_Path_1)[1:2] = c('From', 'To')
Traffic_Path_2 = aggregate(data = Green_Taxi[Green_Taxi$Traffic_Type == 'Inter-Borough',], FUN = sum,
                           Traffic_Count ~ District_Pickup + District_Dropoff)
#Traffic_Path_2$District_Pickup = paste('.', Traffic_Path_2$District_Pickup, sep = '')
Traffic_Path_2$District_Dropoff = paste('.', Traffic_Path_2$District_Dropoff, sep = '')
colnames(Traffic_Path_2)[1:2] = c('From', 'To')
Traffic_Path_3 = aggregate(data = Green_Taxi[Green_Taxi$Traffic_Type == 'Inter-Borough',], FUN = sum,
                           Traffic_Count ~ District_Dropoff + Payment_type)
Traffic_Path_3$District_Dropoff = paste('.', Traffic_Path_3$District_Dropoff, sep = '')
colnames(Traffic_Path_3)[1:2] = c('From', 'To')
Traffic_Path_Complete = rbind(Traffic_Path_1, Traffic_Path_2, Traffic_Path_3)
Path_Sankey <- 
  gvisSankey(Traffic_Path_Complete, from = "From", to = "To", 
             weight = "Traffic_Count", 
             options=list(sankey="{link: {color: { fill: 'pink' } },
                            node: { color: { fill: 'pink' }}}",
                          width = 800, height = 270))
Path_Sankey$html$footer <- 'Complete Trip Path Visualization'


# Temporal Traffic Distribution
Green_Taxi$Pickup_Date = as.Date(Green_Taxi$lpep_pickup_datetime)
Borough_Traffic_Anno = aggregate(data = Green_Taxi, FUN = sum,
                                 Traffic_Count ~ Pickup_Date + Traffic_Type)
colnames(Borough_Traffic_Anno) = c('Date', 'Traffic_Type', 'Traffic_Count')
Temporal_Traffic <- gvisAnnotatedTimeLine(Borough_Traffic_Anno, 
                            datevar="Date",
                            numvar="Traffic_Count",
                            idvar="Traffic_Type",
                            options = list(fill=80,
                                           displayExactValues=TRUE,
                                           colors="['pink','lightblue']",
                                           width = 800, height = 300,
                                           title = 'Termporal Traffic Distribution by Types'))


# Create a dashboard
Merge_Dashboard_1 <- gvisMerge(Traffice_Type_Pie, Temporal_Traffic, horizontal = T)
Merge_Dashboard_2 <- gvisMerge(Path_Sankey, Bar_District, horizontal = T)
Merge_Dashboard_3 <- gvisMerge(Merge_Dashboard_1, Merge_Dashboard_2, horizontal = F)
Merge_Dashboard_3$html$footer <- 'Complete Inter - Borough Traffic Path Visualization'
Merge_Dashboard_3$html$header = 'NYC Green Taxi Intra- and Inter - Borough Traffic Visualizations'
Merge_Dashboard_3$html$caption <- NULL
cat(Merge_Dashboard_3$html$header, Merge_Dashboard_3$html$chart,
    Merge_Dashboard_3$html$footer, file="Q5_Interactive_Dashboard.html")

