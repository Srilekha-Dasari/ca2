#SECTION 1 - using NIPostcodes dataset
#file. path() is used to create reproducible code that can be run on other os.
path <- file.path('NIPostcodes.csv')
#To import data in csv format
NIPostcodes_data <- read.csv(path, header=FALSE, stringsAsFactors = FALSE, na.strings="" )
#a)Show the total number of rows, the structure of the data frame, and first 10
#rows of the data frame containing all of the NIPostcode data.
#The total number of rows
nrow(NIPostcodes_data)
#Structure of the data frame
str(NIPostcodes_data)
#First 10 rows of the data frame 
head(NIPostcodes_data,10)
#b) Add a suitable title for each attribute of the data.
column_names <- c("organisation","sub-building","building","block-number",
                  "primary-path","alt-path","secondary-path","locality",
                  "townland","town","country","postcode","x-val","y-val","identifier")
colnames(NIPostcodes_data) <- column_names
#To view column names
colnames(NIPostcodes_data)
#c)Replace and recode all missing entries with a suitable identifier. Decide
#whether it is best to remove none, some or all of the missing data. Discuss the
#missing data using suitable graphical output to explain your answer and justify
#your decision in detail.
str(NIPostcodes_data)
# total number of missing values
sum(is.na(NIPostcodes_data))
#graphically display missing data
install.packages("mice")
library(mice)
md.pattern(NIPostcodes_data)
#using vim packages to display the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(NIPostcodes_data, prop = FALSE, numbers = TRUE)
# missing values summary
summary(missing_values)
#d) Show the total number of missing values for each column in the postcode data
#frame both before and after your decision to deal with the missing data
#variables
#total number of missing values for each column
colSums(is.na(NIPostcodes_data))
#e) Move the primary key identifier to the start of the dataset.
#Move the primary key identifier to the start of the dataset.
NIPostcodes_data <- subset(NIPostcodes_data, select=c(15,1:14))
str(NIPostcodes_data)
#f) Create a new dataset called Limavady_data. Store within it only information
#where locality, townland and town contain the name Limavady. Count and
#display the number of rows. Store this information in a csv file called Limavady.
#creating a new dataset
Limavady_data <- subset(NIPostcodes_data, tolower(locality) == "limavady" | tolower(townland) == "limavady" | tolower(town) == "limavady")
#count and display the number of rows
nrow(Limavady_data)
#store limavady information in a csv file
write.csv(Limavady_data, file="Limavady.csv", row.names = FALSE)
#g) Save the modified NIPostcode dataset in a csv file called
#CleanNIPostcodeData.
#store the cleaned NIPostcodes in a csv file
write.csv(NIPostcodes_data, file="CleanNIPostcodeData.csv", row.names = FALSE)
#SECTION-2 using NICrimeData
#a) amalgamate all of the crime data from each csv file into one dataset.
library(plyr)
library(readr)
#file_path <- file.path('C:/Users/shril/Documents/ca2/NI Crime Data')
file_path <- file.path('./NI Crime Data')
myfiles <- list.files(path=file_path, pattern="*.csv", full.names=TRUE)
myfiles
df1 <- ldply(myfiles, read_csv)
df1
head(df1,5)
#Save this dataset into a csv file called AllNICrimeData
write.csv(df1, file="AllNICrimeData.csv", row.names = FALSE)
# Count and show the number of rows in the AllNICrimeData dataset
nrow(df1)
names(df1)
#b)Modify the structure of the newly created AllNICrimeData csv file and remove
#the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
#last outcome and context. Save this new structure and show the structure of the
#modified file.
include_list <- names(df1)%in%c("Month","Longitude","Latitude","Location","Crime type")
include_list
df1_new <- df1[(include_list)]
df1_new
names(df1_new)
str(df1_new)
head(df1_new,5)
#editing column name Crime type as it contains space
names(df1_new)[names(df1_new)=="Crime type"] <- "Crimetype"
names(df1_new)
df1_new$Crimetype <- as.character(Crimetype)
head(df1_new,5)
df1_new$Crimetype[df1_new$Crimetype == "Anti-social behaviour"] <- "ASBO"
df1_new$Crimetype[df1_new$Crimetype == "Bicycle theft"] <- "BITH"
df1_new$Crimetype[df1_new$Crimetype == "Burglary"] <- "BURG"
df1_new$Crimetype[df1_new$Crimetype == "Criminal damage and arson"] <- "CDAR"
df1_new$Crimetype[df1_new$Crimetype == "DRUGS"] <- "DRUG"
df1_new$Crimetype[df1_new$Crimetype == "Other Theft"] <- "OTTH"
df1_new$Crimetype[df1_new$Crimetype == "Public order"] <- "PUBO"
df1_new$Crimetype[df1_new$Crimetype == "Robbery"] <- "ROBY"
df1_new$Crimetype[df1_new$Crimetype == "Shoplifting"] <- "SHOP"
df1_new$Crimetype[df1_new$Crimetype == "Theft from the person"] <- "THPR"
df1_new$Crimetype[df1_new$Crimetype == "Vehicle crime"] <- "VECR"
df1_new$Crimetype[df1_new$Crimetype == "Violence and sexual offences"] <- "VISO"
df1_new$Crimetype[df1_new$Crimetype == "Other crime"] <- "OTCR"
head(df1_new,10)
write.csv(df1_new, file="AllNICrimeDatanew.csv", row.names = FALSE)
#d) Using the plot() function, show a plot of each crime frequency from the
#crime.type field. Specify relevant options such as title, axes labels, bar colours.
#Provide a detailed discussion about what you found from this chart.
library(plyr)
?count
freq <- count(df1_new,c("Crimetype"))
freq
#plot the graph
install.packages("ggplot2")
install.packages("viridis")
library(ggplot2)
plot <- ggplot(freq,aes(x=Crimetype, y=freq))+
  geom_bar(stat="identity", colour="black", fill="Green") + 
  xlab("") + ylab("") +
  ggtitle("Crime Frequency")
plot
#e) Modify the AllNICrimeData dataset so that the Location attribute contains only a
#street name. For example, the attribute value “On or near Westrock Square” should
#be modified to only contain “Westrock Square”. Modify the resultant empty location
#attributes with a suitable identifier. Show a sample of your data.
library(stringr)
df1_new$Location <- trimws(str_remove(df1_new$Location, "On or near"))
df1_new$Location
#df3 <- ifelse(is.na(df1_new$Location),
                     # ave(df1_new$Location, FUN = function(x) mean(x, na.rm = TRUE)),
                    #  df1_new$Location)
#head(df3,10)
#df1_new$Location <- df3
str(df1_new)
#find the total number of empty values in Location attribute
sum(df1_new$Location == "")
#replace empty fields with NA
df1_new$Location[df1_new$Location == ""] <- NA
df1_new$Location
#view NA records
na_records <- subset(df1_new,is.na(Location))
na_records
nrow(na_records)
#f) Choose 5000 random samples of crime data from the AllNICrimeData dataset
#where the location attribute contains location information. This means that the
#location information should NOT contain an NA identifier. Set the seed value to 100.
#Store this data in a data frame called random_crime_sample. Then create a
#function called find_a_town that uses the CleanNIPostcodeData data frame to find
#correct town/city information for each location variable within the
#random_crime_sample dataset. Save each matched town into the
#random_crime_sample data frame under the column name Town.
#data that does not contain NA value
cleaned_location <- df1_new[complete.cases(df1_new$Location),]
#set the seed value to 100
set.seed(100)
#create random samples
library(dplyr)
random_crime_sample<- sample_n(df1_new[complete.cases(df1_new$Location),],5000)
random_crime_sample
#Read CleanNIPostcodeData.csv file data
Cleaned_NIPostcodeData <- read.csv("CleanNIPostcodeData.csv")
str(Cleaned_NIPostcodeData)
Cleaned_NIPostcodeData <- Cleaned_NIPostcodeData[complete.cases(Cleaned_NIPostcodeData$primary.path),]
#creating function find_a_town

find_a_town <- function(location) {
  Cleaned_NIPostcodeData$town[match(tolower(location),tolower(Cleaned_NIPostcodeData$locality))]
}
print(random_crime_sample$Location)

random_crime_sample$Town <- find_a_town(random_crime_sample$Location)
random_crime_sample$Town
str(random_crime_sample)
write.csv(random_crime_sample, file="test.csv", row.names = FALSE)
#g) Create a function called add_town_data that examines the information from
#each crime record in random_crime_sample and matches each record with
#relevant data in the VillageList.csv file. Add the population attribute to the
#random_crime_sample data frame
# Add the population attribute to the random_crime_sample data frame.
village_file <- file.path('VillageList.csv')
village_data <- read.csv(village_file, header = TRUE)
colnames(village_data)
add_town_data <- function(name) {
  village_data$POPULATION[match(tolower(name), tolower(village_data$ï..CITY.TOWN.VILLAGE))]
}
random_crime_sample$POPULATION <- add_town_data(random_crime_sample$Town)
#h) Update the random sample AllNICrimeData dataset so that it contains only the
#following items
#• Month • Longitude• Latitude• Location• Crime type• City-Town-Village• Population
#Save this data into a csv file called random_crime_sample. 
write.csv(random_crime_sample, file="randomcrimesample.csv", row.names=FALSE)
#i)Now we want to display crime for both cities in Northern Ireland which are Belfast
#and Derry. From the random_crime_sample data frame, sort the chart data by crime
#type for each city and then show both charts side-by-side by setting relevant
#graphical parameters using the par() command. Show a suitable main title for the
#bar chart, and suitable x and y-axis labels and scaling.
#Make sure all labels on the xaxis can be read. 
#Show the bar plot in your CA document.

#select the crime from the places Belfast and Londonderry of NI from random_crime_sample
crime_belfast <- filter(random_crime_sample, Town == 'BELFAST')

crime_londonderry <- filter(random_crime_sample, Town == 'LONDONDERRY')

belfast_count <- count(crime_belfast,Crimetype)
sort(belfast_count$n)

londonderry_count <- count(crime_londonderry,Crimetype)
sort(londonderry_count$n)
# To display graph side by side
par(mfrow = c(2,2))
#plot graph
plot(factor(belfast_count$Crimetype), belfast_count$n, xlab = "Crime Types", 
     ylab = "Crime Count", main = "Belfast Crime Types and Count")
plot(factor(londonderry_count$Crimetype), londonderry_count$n, xlab = "Crime Types", 
     ylab = "Crime Count", main = "Londonderry Crime Types and Count")
