# --- Step 1:
# Create a function (named readStates) to read a CSV file into R
dataURL <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"

readStates <- function(URL)
{
  StatesTable <- read.csv(url(URL))
  return(StatesTable)
}
StatesData <- readStates(dataURL)
#View(StatesData)
#str(StatesData)
#head(StatesData, 1)

# --- Step 2: Clean the dataframe

# --- 4. Note the issues that need to be fixed (removing columns, removing rows, changing column names)
#           Within your function make sure there are only 5 columns with the columns having the 
#           following names (stateName, base2010, base2011, jul2010, jul2011)
# --- 5. Make sure the last four columns are numbers (i.e not strings)
# cleaning/munging data function, function is very specific to dataset

CleanData <- function(inData)
{
  # remove rows and keep first five columns
  inData <- inData[9:59,1:5]
  
  # make rownames null in order to give it a new name, and remove period in states column
  rownames(inData) <- NULL
  colnames(inData) <- c("stateName", "base2010", "base2011", "Jul2010", "Jul2011")
  inData$stateName <- gsub("\\.","",inData$stateName)
  
  # for loop through numeric colums and remove comma, for cName in table
  for (cName in colnames(inData[2:5])){
    idx <- which(colnames(inData)==cName)
    inData[,idx] <- gsub("\\,","",inData[,idx])
    inData[,idx] <- as.numeric(inData[,idx])
  }
  return(inData)
}
CleanData(StatesData)

# --- Step 3: Store and Explore the dataset ---
# --- 6. Store the dataset into a dataframe, called dfStates.
dfStates <- data.frame(CleanData(StatesData))
View(dfStates)
# --- 7. Test your dataframe by calculating the mean for the july 2011 data,
#           by doing: mean(dfStates$jul2011)-> answer: 6,109,645
mean(dfStates$Jul2011)
cat("answer: ",mean(dfStates$Jul2011))


# --- Step 4: Find the state with the Highest Population ---
# --- 8. Based on the July 2011 data, what is the population of the state with the highest
#         population? what is the name of that state?
Hpop <- dfStates[which.max(dfStates$Jul2011),]
cat("State with the highest population is: ",Hpop[,1])

# --- 9. Sort the data, in increasing order, based on the July2011 data.
dfStates <- dfStates[order(dfStates$Jul2011),]
dfStates
#View(dfStates$Jul2011)

# --- 10. Write a functions that takes two parameters: the first a vector and the second a number.

# --- 11. The function will return the percentage of the elements within the vector that is
#           less than the same (i.e. the cumulative distribution below the value provided)

# --- 12. For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number
#           passed into the function, the function would return 0.2 (since 20% of the 
#           numbers were below 2).

# --- 13. Test the function with the vector 'dfStates$Jul2011Num', and the mean of dfStates$Jul2011Num'.

dfVector <- dfStates$Jul2011
numMean <- mean(dfStates$Jul2011)

cumulativeDis <- function(vec,num)
{
  #lenght of factors/numbers that are less than mean of vector 
  lenNum <- length(which(vec<num))
  lenVec <- length(vec)
  return(lenNum/lenVec)
}
cumulativeDis(dfVector,numMean)

