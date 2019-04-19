# Install packages
#install.packages("plyr")
#install.packages("curl")
#install.packages("RJSONIO")
#install.packages("jsonlite")
#install.packages("sqldf")
library("plyr")
library("curl")
library("RJSONIO")
library("jsonlite")
library("sqldf")

# --- Step 1 --- #
# load data via url JSON function.
mdurl <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
md<-fromJSON(mdurl)
# Store the data table instead of the metat data(tags)
mdjson<-md$data
# var stores new column names
cNames <-
  c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK",
    "ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION",
    "CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST",
    "INJURY","COLLISION_WITH_1","COLLISION_WITH_2")


# --- Step 2 --- #
# Rename and remove the first eight columns
qClean <- function(Jdata,clmNames)
{
  # remove first 8 columns
  Jdata <- Jdata[,-1:-8]
  # replace with new column names
  colnames(Jdata) <- clmNames
  # create data frame for cleaned data
  return(data.frame(Jdata))
}

# Capture index of column name
gn <- function(data,column)
{
  return(which(colnames(data)==column))
}
####################

mdClean <- qClean(mdjson,cNames)
View(mdClean)
str(mdClean)
###################

# find index for DAY OF WEEK column
idxday <- gn(mdClean,"DAY_OF_WEEK")
# remove spaces from the rows
mdClean[,idxday] <- gsub(" ","",mdClean[,idxday])
###################

# find index for INJURY column
idx <- gn(mdClean,"INJURY")

# check if na in column
cat("any n/a in Injury column:",any(is.na(mdClean[,idx])))

# OMIT ALL N/A's
mdClean[,idx][is.na(mdClean[,idx])] <- mdClean[,idx][na.omit(mdClean[,idx])]

# check is na again should equal FALSE
cat("any n/a in Injury column: ",any(is.na(mdClean[,idx])))
##################

 
# --- Step 3 --- # SQL
# How many accidents happen on SUNDAY
# How many accidents had injuries (might need to remove NAs from the data) 
#      List the injuries by day

# count of sunday accidents
sundaySql <- sqldf("SELECT count(*)
                      FROM mdClean
                      WHERE DAY_OF_WEEK = 'SUNDAY'")
sundaySql
View(sundaySql)
# sorted by day the injury's
injurList <- sqldf("SELECT *
                    FROM mdClean
                    WHERE INJURY = 'YES'
                    ORDER BY DAY_OF_WEEK ASC")
View(head(injurList,10))
##########################

# --- Step 4 --- # tapply
# How many accidents happen on Sunday
# How many accidents had injuries (might need to remove NAs from the data)
#      List the injuries by day

# attach table so tapply can use column name only
attach(mdClean)
# count of sunday accidents
sundayTapply <- tapply(DAY_OF_WEEK,DAY_OF_WEEK == "SUNDAY",count) #can also use length
sundayTapply
# sorted by day the injury's
injurTapply <- tapply(INJURY,DAY_OF_WEEK,sort)
View(injurTapply)
