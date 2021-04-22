############################# Solution 1 #################################

# Created a new project called London in GitHub.
# File "london-crime-data.csv" downloaded from blackboard and inserted into data frame called as "london-crime".
# The data set holds  variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.

getwd() # Get the correct working directory

london_crime <- read.csv("london-crime-data.csv", na = "") # Reading london-crime-data.csv file

london_crime [london_crime == ""] <- NA #Assigning blank spaces with NA
london_crime

london_crime$Date <- paste("01", london_crime$month, london_crime$year, sep='/')
str(london_crime) # will display the Date structure as character.

london_crime$Date <- as.Date(london_crime$Date, "%d/%m/%Y") #Converted to date type
str(london_crime$Date) 

############################# Solution 2 ####################################

# Make the relevant changes to the variable names of
# London_crime so that your variables are correctly named


# Updating the name of the variables to make it readable
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "MinorCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"

str(london_crime)
colnames(london_crime)

############################# Solution 3 ####################################

#Q3
#Convert the CrimeDate variable so that it is a variable of type Date. Confirm that the
#variable has been changed to the required variable type by showing the structure and
#content of the date variable.

# Date already converted in the previous solution
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, "%d/%m/%Y")
str(london_crime$CrimeDate)
head(london_crime$CrimeDate,10) # Display 10 records


############################# Solution 4 #################################

#Plot a chart to show the summary of the borough information so that we can view
#where most crimes occur. 
#Using the summary() function, display this data in a chart
#with suitable chart title and x and y axis subtitles. Then answer the following:
#  • Add a comment in your code to show which borough has the highest level of
#crime.
#• And add a comment in your code to indicate which area has the lowest level of
#crime.



# Required Library Installed
install.packages("mice")
library(mice)
md.pattern(london_crime)


# Installed VIM package and displayed the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(london_crime, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)
plot(london_crime)


london_crime$Borough <- factor(london_crime$Borough)
summary(london_crime$Borough)
str(london_crime$Borough)

# Add titles to the chart that are relevant
attach(london_crime)
display_settings <- par(no.readonly = TRUE)
plot(Borough, main = "Number of crime in Borough", xlab = "Borugh", ylab = "Number of crime", col = 'red')


#Borough has the highest level of crimes in Croydon: 5226
#Borough has the lowest level of crimes in City of London: 86

############################# Solution 5 #################################

#Display the MajorCategory variable data in a pie chart. Using data output from the
#summary() function, determine the highest and lowest major categories of crime in
#London. Then display this information in a pie chart using the pie() function.
#• Add a comment in your code to indicate which major category had the highest
#level of crimes.
#• Add a comment in your code to indicate which category had the lowest level of
#crimes.


london_crime$MajorCategory <- factor(london_crime$MajorCategory)
str(london_crime$MajorCategory)
summary(london_crime$MajorCategory)

pie(MajorCategory, main = "percentage of crime by MajorCategory",
    col = c("red","orange","blue","green","violet","pink","cyan","purple","yellow"))

#Category having the highest level of crimes : Theft and Handling: 33759
#Category having the lowest level of crimes : Sexual Offences: 917

############################# Solution 6 #################################

#Categorise each borough in the London_crime dataset into the general area where it
#lies within London. Using the table below, create a new variable called Region and store
#within it the correct region for each borough.
#Once finished, check that all Boroughs have been assigned to a region. You can do this
#by checking whether any boroughs are assigned with an NA value. If you find any
#regions that contain NA, replace them with a suitable Region. Clearly indicate in your
#code how you checked this and any decisions you needed to make.


#Reading the Region data 
str(london_crime)
Region <- read.csv("Region.csv", na = "") # Reading london-crime-data.csv file
str(Region)

london_crime$Borough <- factor(london_crime$Borough)
Region$Borough <- factor(Region$Borough)
Region$Region <- factor(Region$Region)

#Total_london_crime <- rbind()

#Assigning Region to each Borough record

london_crime$Region[Borough == 'Barking and Dagenham' | 
                      Borough == 'Bexley' |
                      Borough == 'Greenwich' | 
                      Borough == 'Havering' |
                      Borough == 'Kingston upon Thames' |
                      Borough == 'Redbridge' |
                      Borough == 'Wandsworth' ] <- 'East'

london_crime$Region[Borough == 'Barnet' | 
                      Borough == 'Camden' |
                      Borough == 'Enfield' | 
                      Borough == 'Hackney' |
                      Borough == 'Haringey' ] <- 'North'

london_crime$Region[Borough == 'Bromley' | 
                      Borough == 'Croydon' |
                      Borough == 'Merton' | 
                      Borough == 'Sutton'  ] <- 'East'


london_crime$Region[Borough == 'Brent' | 
                      Borough == 'Ealing' |
                      Borough == 'Hammersmith and Fulham' | 
                      Borough == 'Harrow' |
                      Borough == 'Hillingdon' |
                      Borough == 'Hounslow' |
                      Borough == 'Richmond upon Thames' ] <- 'West'

london_crime$Region[Borough == 'Islington' | 
                      Borough == 'Kensington and Chelsea' |
                      Borough == 'Lambeth' | 
                      Borough == 'Lewisham' |
                      Borough == 'Southwark' |
                      Borough == 'Waltham Forest' |
                      Borough == 'Westminster' ] <- 'Central'

london_crime$Region[Borough == 'City of London' ] <- 'Central'

london_crime$Region[Borough == '' ] <- NA

london_crime$Region

london_crime$Region <- factor(london_crime$Region)
str(london_crime$Region)

############################# Solution 7 #################################

#Display which region in London has the highest recorded crime rate. Using the plot()
#function, show the number of reported crimes by region. Suitably label the chart and its
#axes.
#• Add a comment in your code to indicate which region had the highest number of
#crimes. How many crimes were committed?
#  • Add a comment in your code to indicate which region had the lowest number of
#crimes. How many crimes were committed?


# Add titles to the chart that are relevant

summary(london_crime$Region)

attach(london_crime)
display_settings <- par(no.readonly = TRUE)
plot(london_crime$Region,
        main = "Number of crime regionwise", 
        xlab = "Region", 
        ylab = "Number of crime", col = 'yellow')

 
# 1. Region having the lowest number of crimes : North
#counts : 19775

# 2. Region having the highest number of crimes : East
#counts : 38755

############################# Solution 8 #################################

#Referring to your answer in Q7, extract out the subset of data that had the highest
#number of crimes. And then extract out a subset of data that had the lowest level of
#crimes.
#• Critique and discuss the major crime category of both regions.

summary(london_crime$Region)

highest_no_of_crimes <- subset(london_crime, london_crime$Region == "East")
str(highest_no_of_crimes)
dim(highest_no_of_crimes)


plot(highest_no_of_crimes$MajorCategory,
     main = "Number of crime in East region by Category ", 
     xlab = "Region", 
     ylab = "Number of crime", col = 'Red')

lowest_no_of_crimes <- subset(london_crime, london_crime$Region == "North")
str(lowest_no_of_crimes)
dim(lowest_no_of_crimes)

plot(lowest_no_of_crimes$MajorCategory,
     main = "Number of crime in North region by Category ", 
     xlab = "Region", 
     ylab = "Number of crime", col = 'Blue')


# Here in the plotting we can see the East region has highest number of  
#crime in the Violence category

# Here in the plotting we can see the North region has lowest number of  
#crime, however major category in the sexual Offences category



############################# Solution 9 #################################

#Q9
#Using information from the summary() function, plot the content of both of your data
#frames side by side. Make sure that the y axis on both charts are shown in the same
#scale and that both charts contain suitably labelled titles, x and y axis. And text on the xaxis
#should be presented vertically.


opar <- par(no.readonly = TRUE)
par = opar

par(mfrow = c(1,2))
plot(highest_no_of_crimes$MajorCategory,
     main = "Number of crime in East region by Category ", 
     xlab = "Region", 
     ylab = "Number of crime", col = 'Red')

plot(lowest_no_of_crimes$MajorCategory,
     main = "Number of crime in North region by Category ", 
     xlab = "Region", 
     ylab = "Number of crime", col = 'Blue')

# Setting a graph range

graph_range <- range(0,10000)
graph_range

# Both graphs will be dispalying on the same window 



par(mfrow = c(1,2))

plot(highest_no_of_crimes$MajorCategory,
     main = "Number of crime in East region by Category ", 
     xlab = "Number or crime", 
     ylab = "category of crime", col = 'Red'
     , las = 3, ylim = graph_range )

plot(lowest_no_of_crimes$MajorCategory,
     main = "Number of crime in North region by Category ", 
     xlab = "Number or crime", 
     ylab = "category of crime", col = 'Blue'
     , las = 3, ylim = graph_range)


############################# Solution 10 #################################

#Q10
#Using the write.csv() command, save the modified london_crime data frame as
#london-crime-modified.csv.
#Finally, sync your script file as well as the London-crime-modified.csv file to the
#remote GitHub repo.

# Link shared through Black board. 



# newly modified london_crime data frame written to a csv file as london_crime_modified.csv 
write.csv(london_crime, file= "london_crime_modified.csv")


##################################### The End #################################


