library(dplyr)


###
#Data source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zi 

# run_analysis.R  does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#download the zip file
if(!file.exists("data")){dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./data/exploratory_data_project.zip",method="auto")
dateDownloaded <- date()

#extract the files
unzip("./data/exploratory_data_project.zip", exdir='./data')

### Load source data to data frames
NEI<- readRDS('./data/summarySCC_PM25.rds')
SCC<- readRDS ('./data/Source_Classification_Code.rds')

######## 
#### (1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#   Using the base plotting system, make a plot showing the total PM2.5 emission from
#   all sources for each of the years 1999, 2002, 2005, and 2008.


# group data by year and sum the pm25 per year. 
pm25_by_year <- group_by(NEI,year) %>% summarize(sum=sum(Emissions)) %>% select (year, sum)

# create plot, overlay line, set axis labels to correspond to the years. 
with(pm25_by_year,  
  plot(year, sum, main = "PM2.5 total emissions from all sources", 
       xlab="Year", ylab = "PM2.5 emissions" , xaxt='n', pch=19, lwd=1)
      ) 

lines(pm25_by_year$year, pm25_by_year$sum)

axis(1, at = seq(1999, 2008, by = 3), las=2)

# Write to PNG 

dev.copy(png,file="pm25_trend.png")

dev.off()


#######
### (2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#   (fips == "24510") from 1999 to 2008? 
#    Use the base plotting system to make a plot answering this question.

# group data by year, filter by baltimore fips='24510', and sum the pm25 per year. 
pm25_baltimore_by_year <- group_by(NEI,year) %>% filter(fips=='24510') %>% summarize(sum=sum(Emissions)) %>% select (year, sum)

# create plot, overlay line, set axis labels to correspond to the years. 
with(pm25_baltimore_by_year,  
     plot(year, sum, main = "PM2.5 total emissions in Baltimore", 
          xlab="Year", ylab = "PM2.5 emissions" , xaxt='n', pch=19, lwd=1)
) 

lines(pm25_baltimore_by_year$year, pm25_baltimore_by_year$sum)

axis(1, at = seq(1999, 2008, by = 3), las=2)

# Write to PNG 

dev.copy(png,file="pm25_trend_Baltimore.png")

dev.off()


######
### (3) Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#   Which have seen increases in emissions from 1999-2008? 
#   Use the ggplot2 plotting system to make a plot answer this question.

# group data by year and type, filter by baltimore fips='24510', and sum the pm25 per year and type 
pm25_baltimore_by_source_year <- group_by(NEI,year, type) %>% filter(fips=='24510') %>% select (year, type, Emissions) %>% summarize_each(funs(sum)) 


# Use ggplot2 to plot the data across type facets. 
library(ggplot2)

g<- ggplot(data=pm25_baltimore_by_source_year,aes(year,Emissions))

p<- g+geom_point()+geom_line() +
  scale_x_continuous(breaks = c(1999,2002,2005,2008)) +
   facet_grid(. ~ type) +labs(title="PM2.5 Emissions in Baltimore, grouped by emission type") + 
   labs(x=expression("Year"),y="PM2.5 Emissions Levels") 

print(p)


# Write to PNG 

dev.copy(png,file="pm25_trend_Baltimore_pertype.png")
dev.off()


###### Across the United States, how have emissions from coal combustion-related sources 
### (4) changed from 1999-2008?

# I interpret this to be where SCC EI.Sector has the word Coal. 

# Create pattern for grep, get vector for all rows with "Coal" for SCC dataset. 
ptn_coal = 'coal'
obs_vector_for_coal<- grep(ptn_coal, SCC$EI.Sector, perl=TRUE, ignore.case=TRUE)

# Merge only rows from SCC where the SCC EI.Sector has "Coal" in text. 
# Merge this dataset with NEI data to get only NEI data with SCC ID's matching those with Coal in EI.Sector. 

coal_emissions_data <- merge(SCC[obs_vector_for_coal,c(1,4)],NEI)

#unique(coal_emissions_data$EI.Sector) used to confirm these are all Combustion of coal related. 

# group data by year and sum the pm25 per year. 
coal_emissions_data_sum <- group_by(coal_emissions_data, year) %>% summarize(sum=sum(Emissions)) %>% select (year, sum)

# create plot, overlay line, set axis labels to correspond to the years. 
with(coal_emissions_data_sum,  
     plot(year, sum, main = "Coal combustion related PM2.5 emissions", 
          xlab="Year", ylab = "PM2.5 emissions" , xaxt='n', pch=19, lwd=1)
) 

lines(coal_emissions_data_sum$year, coal_emissions_data_sum$sum)

axis(1, at = seq(1999, 2008, by = 3), las=2)

# Write to PNG 

dev.copy(png,file="pm25_from_coal_combustion.png")

dev.off()


#######
### (5) How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

### I interpret this to be where EI.Sector has the word "Vehicles"

# Create pattern for grep, get vector for all rows with "Coal" for SCC dataset. 
ptn_vehicle = 'Vehicles'
obs_vector_for_vehicle<- grep(ptn_vehicle, SCC$EI.Sector, perl=TRUE, ignore.case=TRUE)

# Merge only rows from SCC where the SCC EI.Sector has "Vehicles" in text. 
# Merge this dataset with NEI data to get only NEI data with SCC ID's matching those with Vehicles in EI.Sector. 

vehicle_emissions_data <- merge(SCC[obs_vector_for_vehicle,c(1,4)],NEI)

unique(vehicle_emissions_data$EI.Sector) #used to confirm these are all motor vehicle related. 

# group data by year, filter Baltimore City, then summarize PM25 values. 
vehicle_emissions_data_sum <- group_by(vehicle_emissions_data, year) %>% filter(fips=='24510') %>% summarize(sum=sum(Emissions)) %>% select (year, sum)

# create plot, overlay line, set axis labels to correspond to the years. 
with(vehicle_emissions_data_sum,  
     plot(year, sum, main = "Motor vehicle related PM2.5 emissions in Baltimore", 
          xlab="Year", ylab = "PM2.5 emissions" , xaxt='n', pch=19, lwd=1)
) 

lines(vehicle_emissions_data_sum$year, vehicle_emissions_data_sum$sum)

axis(1, at = seq(1999, 2008, by = 3), las=2)

# Write to PNG 

dev.copy(png,file="pm25_from_Baltimore_city_motor_vehicles.png")

dev.off()




######
### (6) Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
#sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# Get vehicle emissions data, filter by both Baltimore City and Los Angeles County, group by both year and fips, then select only the year, fips, and Emissions column, and get sum of Emissions. 
vehicle_emissions_data_BALvsLAC <- filter(vehicle_emissions_data, fips=='24510' | fips=='06037') %>% group_by(year, fips) %>% select (year, fips, Emissions) %>% summarize_each(funs(sum)) 

g<- ggplot(data=vehicle_emissions_data_BALvsLAC,aes(year,Emissions))

p<- g+geom_point()+geom_line() +
  scale_x_continuous(breaks = c(1999,2002,2005,2008)) +
  facet_grid(. ~ fips) +labs(title="Motor Vehicle Emissions in 06037(Los Angeles County) and 24510 (Baltimore)") + 
  labs(x=expression("Year"),y="PM2.5 Emissions Levels") 

# Side-by-side plotting of 2.5 emissions for both LA Couty and Baltimore
# plot shows LA County having a consistent incline in emissions from 1999 to 2005, then a sharp decrease in 2008.
# Baltimore showed a significant decrease from 1999 to 2002, then further decline in 2005 and 2008. 

print(p)

# Write to PNG file 

dev.copy(png,file="motor_vehicle_pm25_Baltimore_vs_LACounty.png")

dev.off()

