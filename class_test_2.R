############################# Solution 1 #################################

# Created a new project called London in GitHub.
# File "london-crime-data.csv" downloaded from blackboard and inserted into data frame called as "london-crime".
# The UFO data set holds lots of variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.

getwd()

london_crime <- read.csv("london-crime-data.csv", na = "") # Reading london-crime-data.csv file

london_crime [london_crime == ""] <- NA #Assigning blank spaces with NA
london_crime

london_crime$Date <- paste("01", london_crime$month, london_crime$year, sep='/')
str(london_crime)

london_crime$Date <- as.Date(london_crime$Date, "%d/%m/%Y")
str(london_crime$Date)


#We want to retain only the variables shown in this table, and we wish to convert the
#variable names to that shown in the table. Make the relevant changes to the content of
#London_crime so that your variables are correctly named, and that unrequired variables
#are discarded.


# Updating the name of the variables to make it readable
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "MinorCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"

str(london_crime)
colnames(london_crime)

#Q3
#Convert the CrimeDate variable so that it is a variable of type Date. Confirm that the
#variable has been changed to the required variable type by showing the structure and
#content of the date variable.


#london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, "%d/%m/%Y")
str(london_crime$CrimeDate)
head(london_crime$CrimeDate,10)


#Q4
#Plot a chart to show the summary of the borough information so that we can view
#where most crimes occur. Using the summary() function, display this data in a chart
#with suitable chart title and x and y axis subtitles. Then answer the following:
#  • Add a comment in your code to show which borough has the highest level of
#crime.
#• And add a comment in your code to indicate which area has the lowest level of
#crime.


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


#Q5
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



#Q6
#Categorise each borough in the London_crime dataset into the general area where it
#lies within London. Using the table below, create a new variable called Region and store
#within it the correct region for each borough.
#Once finished, check that all Boroughs have been assigned to a region. You can do this
#by checking whether any boroughs are assigned with an NA value. If you find any
#regions that contain NA, replace them with a suitable Region. Clearly indicate in your
#code how you checked this and any decisions you needed to make.

str(london_crime)
Region <- read.csv("Region.csv", na = "") # Reading london-crime-data.csv file
str(Region)

london_crime$Borough <- factor(london_crime$Borough)
Region$Borough <- factor(Region$Borough)
Region$Region <- factor(Region$Region)

#Total_london_crime <- rbind()

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



#Q8
#Referring to your answer in Q7, extract out the subset of data that had the highest
#number of crimes. And then extract out a subset of data that had the lowest level of
#crimes.
#• Critique and discuss the major crime category of both regions.

summary(london_crime$Region)

highest_no_of_crimes <- (london_crime$Region == 'East')
str(highest_no_of_crimes)

lowest_no_of_crimes <- (london_crime$Region == 'East')
str(lowest_no_of_crimes)


#Q9
#Using information from the summary() function, plot the content of both of your data
#frames side by side. Make sure that the y axis on both charts are shown in the same
#scale and that both charts contain suitably labelled titles, x and y axis. And text on the xaxis
#should be presented vertically.



#Q10
#Using the write.csv() command, save the modified london_crime data frame as
#london-crime-modified.csv.
#Finally, sync your script file as well as the London-crime-modified.csv file to the
#remote GitHub repo.

# Finally, synced the script file as well as modified_ufo.csv, ufo_gb.csv,
# sorted_ufo_data.csv and missingvars.png file to the remote GitHub repo.
# Link shared through Black board. 

##################################### The End #################################


