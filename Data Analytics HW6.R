#install.packages("reshape2") 
#install.packages("Cairo")
library("Cairo")
library("reshape2")
library("plyr")
library("curl")
library("RJSONIO")
library("jsonlite")
library("sqldf")
#################
directory <- "C:\\Users\\terra\\Desktop\\Syracuse\\IST 687\\hw6"
setwd(directory)
getwd()
################
# find column by name function
gn <- function(data,column)
{
  return(data[,which(colnames(data)==column)])
}
# Clean Data, replace na with mean
airQt <- function(x)
{
  # if column in data is n/a
  if (any(is.na(x))==TRUE){
    colnames <- colnames(x)
    for (cName in colnames){
      idx <- which(colnames==cName)
      # replace n/a with mean of specified column
      x[,idx][is.na(x[,idx])] <- mean(na.omit(x[,idx]))
    }
    return(data.frame(x))}
  else{ return(data.frame(x)) }
}
# Create & save histogram for each var in dataset
hisFunc <- function(x)
{
    colnames <- colnames(x)
    for (cName in colnames){
      idx <- which(colnames==cName)
      jpeg(paste("Histogram of ",cName," .jpeg"))
      # Place histogram of specified column
      hist(x[,idx], main = paste(cName," Histogram"), xlab = paste(cName," Quality"))
      # save a plot as a jpg file
      dev.off()
    }
    return(cat("completed"))}
######################

# --- Step 1 --- #
air <- airquality
airQlty <- airQt(air)

# --- Step 2 --- #
hisFunc(airQlty)
# Ozone boxplot
jpeg(paste("Ozone Boxplot.jpeg"))
ozoneboxPlot <- boxplot(airQlty$Ozone, main = "Ozone Boxplot",xlab = "OZONE")
dev.off()

# Wind boxplot, rounded
jpeg(paste("Wind rounded Boxplot.jpeg"))
boxplot(round(airQlty$Wind),main = "Wind Boxplot",xlab = "OZONE")
dev.off()

# --- Step 4 --- #
# create date column with specified structure
airQlty$Date <- paste("1973", airQlty$Month, airQlty$Day, sep="-")

# make column date with data type double
airQlty$Date <- as.Date(airQlty$Date,"%Y-%m-%d")

# View data and check data structure
View(airQlty$Date)
str(airQlty$Date)

# remove month & day columns
DairQlty <- airQlty[,-5:-6]

# --- Step 3 --- #
ozone.line <- ggplot(airQlty,aes(x=Date,y=Ozone,color=Ozone))+
                    theme_classic(base_size = 10)+
                    labs(title="Air Quality Ozone")+geom_line()
temp.line <- ggplot(airQlty,aes(x=Date,y=Temp,color=Temp))+
                    theme_classic(base_size = 10)+
                    labs(title="Air Quality Temperature")+geom_line()
wind.line <- ggplot(airQlty,aes(x=Date,y=Wind,color=Date))+
                    theme_classic(base_size = 10)+
                    labs(title="Air Quality by Date")+geom_line()
solar.line <- ggplot(airQlty,aes(x=Date,y=Solar.R,color=Solar.R))+
                    theme_classic(base_size = 10)+
                    labs(title="Air Quality Solar Radiation")+geom_line()
# all 4 variables
airQLines <- ggplot(airQlty, aes(x=Date)) + 
                geom_line(aes(y=Ozone, color="Ozone")) + 
                geom_line(aes(y=Temp, color="Temp")) +  
                geom_line(aes(y=Wind, color="Wind")) +
                geom_line(aes(y=Solar.R, color="Solar.R")) +  
                theme(plot.title=element_text(hjust=.5)) + 
                theme_classic(base_size = 10)+
                labs(title="Air Quality in 1973") + 
                scale_color_manual(values=c("green4", "orange", "blue", "red"))

# --- Step 4 --- #
airMelt <- melt(airQlty, id = "Date", measure = c("Wind","Temp","Ozone","Solar.R"))
airHeatMap <- ggplot(airMelt, aes(Date, value, colour = variable,size=value))+
                    geom_tile()+
                    labs(x="Date",y=NULL,title = "Air Quality Heatmap")+
                    theme_classic(base_size = 10)+
                    theme(plot.title=element_text(hjust=.5))

# --- Step 5 --- #
airScatter <- ggplot(airQlty)+
                  geom_point(aes(x=Wind, y=Temp,size=Ozone,color=Solar.R))+
                  labs(x="Wind",y="Temp",title ="Air Quality Scatterplot")+
                  theme_classic(base_size = 10)+
                  theme(plot.title=element_text(hjust=.5))


# Save jpeg of all visualizations
jpeg(paste("TempLine.jpeg"))  
temp.line
dev.off()
jpeg(paste("SolarLine.jpeg"))
solar.line
dev.off()
jpeg(paste("OzoneLine.jpeg"))
ozone.line
dev.off()
jpeg(paste("WindLine.jpeg"))
wind.line
dev.off()
jpeg(paste("airQLine.jpeg"))
airQLine
dev.off()
jpeg(paste("airHeatMap.jpeg"))
airHeatMap
dev.off()
jpeg(paste("airScatter.jpeg"))
airScatter
dev.off()

# --- Step 6 --- #




