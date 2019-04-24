
#install.packages(c("maps","mapdata","ggplot2","openintro",
#                    "ggmap","readxl","gdata","zipcode"))


#install.packages("ggmap")


library(gdata) 
library(ggplot2)
library(openintro) 
library(ggmap)
library(readxl)
library(sqldf)
library(zipcode)
library(maps)
library(mapdata)

# remove the axes function
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# --- Step 1 --- #
#1 read the data
setwd("C:\\Users\\terra\\Desktop\\Syracuse\\IST 687")
Mzip <- read_xlsx("MedianZIP_2_2.xlsx")
head(Mzip)
str(Mzip)
View(Mzip)

#2. Clean up the dataframe
#2a. Remove unneeded information
#2b. Update column names (zip, median, mean, population)
Mzip <- Mzip[-1,]
colnames(Mzip) <- c("zip","median","mean","population")

View(Mzip)
#remove commas and make numeric
Mzip$median <- as.numeric(gsub(",", "", Mzip$median ))
Mzip$mean <- as.numeric(gsub(",", "", Mzip$mean))
Mzip$population <- as.numeric(gsub(",", "", Mzip$population))

#3. Load the 'zipcode' package
data(zipcode)
#reformat the zip codes
Mzip$zip <- clean.zipcodes(Mzip$zip) 
#4. Merge the zip code information from two data frames
MzipNew <- merge(Mzip, zipcode, by ="zip")
head(MzipNew)

#5. Remove Hawaii and Alaska
MzipNew <- MzipNew[MzipNew$state != "HI", ]
MzipNew <- MzipNew[MzipNew$state != "AK", ]
MzipNew <- MzipNew[MzipNew$state != "DC", ]

# --- Step 2 --- #
# 1
# get mean from median of state
income <- tapply(MzipNew$median, MzipNew$state, mean)
state <- rownames(income)
medianIncome <- data.frame(state, income)
# get sum of population of state
pop <- tapply(MzipNew$population, MzipNew$state, sum)
state <- rownames(pop)
statePop <- data.frame(state,pop)
# merge the two dataframes by state variable
dfSimple <- merge(medianIncome, statePop, by="state")
head(dfSimple)

# previous steps can be done using sql instead and scaling the income at the state level
#dfSimple<- sqldf("select state, avg(median) as income, sum(population) as pop from MzipNew group by state")
#dfSimple<- sqldf("select state, (income/pop) as income, pop from dfSimple")
#head(dfSimple)

# 2 
#Add state abbreviations and state names (lower case)
dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]
#convert to lower case
dfSimple$stateName <- tolower(dfSimple$stateName)

# 3
#Show us map, representing color with average median income
us <- map_data("state")

MapIncome <- ggplot(dfSimple, aes(map_id=stateName)) + 
                    geom_map(map=us, aes(fill=dfSimple$income)) + 
                    expand_limits(x=us$long, y=us$lat) + 
                    ggtitle("average median income by state") + 
                    theme(plot.title = element_text(hjust=0.5)) + 
                    guides(fill=guide_legend(title="Income")) + 
                    ditch_the_axes
jpeg("Avg Medium Income.jpeg")
MapIncome
dev.off()
#MapIncome <- MapIncome + coord_map() # causing an error



# Draw each zip code on map where color of dot is based on median income
MzipNew$stateName <- state.name[match(MzipNew$state,state.abb)]
MzipNew$stateName <- tolower(MzipNew$stateName)
head(MzipNew)

MapZip <- ggplot(MzipNew, aes(map_id=stateName)) + 
                geom_map(map=us, fill="black", color="white") + 
                expand_limits(x=us$long, y=us$lat) + 
                geom_point(data=MzipNew, aes(x=MzipNew$longitude, 
                                             y=MzipNew$latitude, 
                                             color=MzipNew$median))+
                ggtitle("income per zip code") + 
                theme(plot.title=element_text(hjust=0.5))+
                ditch_the_axes
jpeg("Income per zip code.jpeg")
MapZip
dev.off()
#MapZip <- MapZip + coord_map() # causing errors


# --- Step 3 --- #
# Generate a map showing density of zip codes
MapDensity <- ggplot(MzipNew, aes(map_id=stateName)) + 
                    geom_map(map=us, fill="black", color="white") + 
                    expand_limits(x=us$long, y=us$lat) + 
                    stat_density_2d(data=MzipNew, #function used: Stat_density_2d
                                    aes(x=MzipNew$longitude, 
                                        y=MzipNew$latitude))+
                    ggtitle("zip code density") + 
                    theme(plot.title=element_text(hjust=0.5))
#MapDensity <- MapDensity + coord_map()# causing errors

jpeg("Density of zip codes.jpeg")
MapDensity
dev.off()


# --- Step 5 --- #
#**zoomGeo <- geocode("New York City, NY", source = "dsk") Not Working**#
#Error in geocode("New York City, NY", source = "dsk") : 
#  datasciencetoolkit.org terminated its map service, sorry!
##########################

# get Google API Key
key <- "key"
# must get key and register it with function
register_google(key)
# Create data frame with desired location
df <- data.frame(address = c("New York City, NY"),
                 stringsAsFactors = FALSE)
# use googles mutate geocode in order to get lat long
zoomGeo <- mutate_geocode(df, address)
zoomAmount <- 2
# get XY as center
centerx <- zoomGeo$lon
centery <- zoomGeo$lat
# create xy extents/scale from center
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

MapZipZoom <- MapZip + xlim(xlimit) + ylim(ylimit) + #  coord_map(): there is no package called 'mapproj'
              geom_point(aes(x=centerx, y=centery), 
                             color="darkred", size=3)+
              ggtitle("Income by zip around NYC") + 
              theme(plot.title=element_text(hjust=0.5))

jpeg("Income by zip around NYC.jpeg")
MapZipZoom
dev.off()









