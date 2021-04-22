###############################
## Class Test 2 - London_crime
###############################

# Q1
# import the london_crime data frame
london_crime <- read.csv("london-crime-data.csv", na="")

# show structure
str(london_crime)

# amalgamate month and year into new variable called Date
converted_date <- paste(london_crime$month, london_crime$year, sep ="/")
converted_date

# add the day element into the variable
converted_date <- paste("01", london_crime$month, london_crime$year, sep ="/")
converted_date
london_crime$Date <- as.Date(converted_date, "%d/%m/%Y")
str(london_crime)

# Q2
# Format variable names
# show all current col names
colnames(london_crime)
# rename borough to Borough
names(london_crime)[2] <- "Borough"
# rename Major_category to MajorCategory
names(london_crime)[3] <- "MajorCategory"
# rename Minor_category to SubCategory
names(london_crime)[4] <- "SubCategory"
# rename value to Value
names(london_crime)[5] <- "Value"
# rename Date to Date
names(london_crime)[8] <- "CrimeDate"

# drop col's
london_crime$ï..lsoa_code <- NULL
london_crime$year <- NULL
london_crime$month <- NULL

# show all current col names
colnames(london_crime)

# Q3
# Convert CrimeDate so that it is a variable of type Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, "%d/%m/%Y")
# show structure
str(london_crime)
# show content
london_crime$CrimeDate

# Q4
# plot a chart to show summary of Borough info to view where most crimes occur
london_crime$Borough <- factor(london_crime$Borough)
str(london_crime)
plot(london_crime$Borough, main ="Crimes per Borough in London", 
     xlab="Borough", 
     ylab="# of Crimes")
summary(london_crime$Borough)

# which borough has the highest level of crime - Croydon at 5226
# which borough has the lowest level of crime - Kingston upon Thames  at 2201

# Q5
# Display the MajorCategory variable data in a pie chart
london_crime$MajorCategory <- factor(london_crime$MajorCategory)
summary(london_crime$MajorCategory)
pie(summary(london_crime$MajorCategory))
# which major category has the highest level of crime - Theft and Handling  at 33,759 
# which major category has the lowest level of crime - Sexual Offences at 917

# Q6
# assign region to Boroughs
london_crime$Region[london_crime$Borough == "Barking and Dagenham"] <- "East"
london_crime$Region[london_crime$Borough == "Barnet"] <- "North"
london_crime$Region[london_crime$Borough == "Bexley"] <- "East"
london_crime$Region[london_crime$Borough == "Brent"] <- "West"
london_crime$Region[london_crime$Borough == "Bromley"] <- "South"
london_crime$Region[london_crime$Borough == "Camden"] <- "North"
london_crime$Region[london_crime$Borough == "Croydon"] <- "South"
london_crime$Region[london_crime$Borough == "Ealing"] <- "West"
london_crime$Region[london_crime$Borough == "Enfield"] <- "North"
london_crime$Region[london_crime$Borough == "Greenwich"] <- "East"
london_crime$Region[london_crime$Borough == "Hackney"] <- "North"
london_crime$Region[london_crime$Borough == "Hammersmith and Fulham"] <- "West"
london_crime$Region[london_crime$Borough == "Haringey"] <- "North"
london_crime$Region[london_crime$Borough == "Harrow"] <- "West"
london_crime$Region[london_crime$Borough == "Havering"] <- "East"
london_crime$Region[london_crime$Borough == "Hillingdon"] <- "West"
london_crime$Region[london_crime$Borough == "Hounslow"] <- "West"
london_crime$Region[london_crime$Borough == "Islington"] <- "Central"
london_crime$Region[london_crime$Borough == "Kensington and Chelsea"] <- "Central"
london_crime$Region[london_crime$Borough == "Kingston upon Thames"] <- "East"
london_crime$Region[london_crime$Borough == "Lambeth"] <- "Central"
london_crime$Region[london_crime$Borough == "Lewisham"] <- "Central"
london_crime$Region[london_crime$Borough == "Merton"] <- "South"
london_crime$Region[london_crime$Borough == "Newham"] <- "East"
london_crime$Region[london_crime$Borough == "Redbridge"] <- "East"
london_crime$Region[london_crime$Borough == "Richmond upon Thames"] <- "West"
london_crime$Region[london_crime$Borough == "Southwark"] <- "Central"
london_crime$Region[london_crime$Borough == "Sutton"] <- "South"
london_crime$Region[london_crime$Borough == "Tower Hamlets"] <- "Central"
london_crime$Region[london_crime$Borough == "Waltham Forest"] <- "Central"

# show df
london_crime

# reassign N/A's - regions found from https://www.londonhut.com/p/london-boroughs
london_crime$Region[london_crime$Borough == "Wandsworth"] <- "Central"
london_crime$Region[london_crime$Borough == "Westminster"] <- "Central"    
london_crime$Region[london_crime$Borough == "City of London"] <- "Central" 

# Q7
# Display which region has the highest crime rate
london_crime$Region <- factor(london_crime$Region)
#str(london_crime)
plot(london_crime$Region, main ="Crimes per Region in London", 
     xlab="Region", 
     ylab="# of Crimes")
summary(london_crime$Region)

# Central Region had highest crimes at 32,918
# South Region had lowest crimes at 15,487

# Q8
# subset highest and lowest crime regions
highestCrimeRegion <- subset(london_crime, london_crime$Region == "Central")
lowestCrimeRegion <- subset(london_crime, london_crime$Region == "South")

# critique highest crime regions crime category
highestCrimeRegion$MajorCategory <- factor(highestCrimeRegion$MajorCategory)
plot(highestCrimeRegion$MajorCategory, main ="Crime Categories in Highest Crime Region (Central)", 
     xlab="MajorCategory", 
     ylab="# of Crimes")
summary(highestCrimeRegion$MajorCategory)

# top 3 main crime categories:
# theft and handling - 9,548
# Violence against the person - 7,693
# criminal damage - 4,886

# critique highest crime regions crime category
lowestCrimeRegion$MajorCategory <- factor(highestCrimeRegion$MajorCategory)
plot(lowestCrimeRegion$MajorCategory, main ="Crime Categories in Lowest Crime Region (South)", 
     xlab="MajorCategory", 
     ylab="# of Crimes")
summary(lowestCrimeRegion$MajorCategory)

# top 3 main crime categories:
# Violence against the person - 4,609
# theft and handling - 3,708
# criminal damage - 2,396
                         
# Q9
# plot charts side-by-side
par(mfrow=c(1,2))

highestCrimeRegion$MajorCategory <- factor(highestCrimeRegion$MajorCategory)
plot(highestCrimeRegion$MajorCategory, main ="Crime Categories in Highest Crime Region (Central)", 
     xlab="MajorCategory", 
     ylab="# of Crimes")
summary(highestCrimeRegion$MajorCategory)

lowestCrimeRegion$MajorCategory <- factor(highestCrimeRegion$MajorCategory)
plot(lowestCrimeRegion$MajorCategory, main ="Crime Categories in Lowest Crime Region (South)", 
     xlab="MajorCategory", 
     ylab="# of Crimes")
summary(lowestCrimeRegion$MajorCategory)

# Q10
# save the modified data frame
write.csv(london_crime, file = "london-crime-modified.csv")