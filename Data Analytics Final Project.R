#######################################################################
# ---                      Functions Used                         --- #
#######################################################################
###########################
# find index of column according to name
colIdx <- function(data,column)
{
  column <- as.character(column)
  return(which(colnames(data)==column))
}

###########################
# function to check if package is installed.
EnsurePackage <- function(data)
{
  for (i in data){
    lbry <- as.character(i)
    if (!require(lbry,character.only = TRUE))
    {
      install.packages(pkgs=lbry, repos = "http://cran.r-project.org")
      require(lbry,character.only = TRUE)
      library(as.character(lbry))
    }
    else {as.character(lbry)}
  }
  return(cat("Library: ","\n",data,"\n"," have all been loaded"))
}

printVecInfo <- function(x)
{
  library(moments)
  return(cat("\n",
             "mean: ",mean(x),"\n",
             "median: ",median(x),"\n",
             "min: ",min(x)," max: ",max(x),"\n",
             "sd: ",sd(x),"\n",
             "quantile at 0.05: ",quantile(x,0.05),"\n",
             "quantile at 0.95: ",quantile(x,0.95),"\n",
             "skewness: ",skewness(x),"\n"))
}

###########################
# function to print barplot of each column
plotData <- function(x,dest.folder)
{
  setwd(dest.folder)
  # filter through each column
  colnames <- colnames(x)
  for (cName in colnames){
    # get index of column
    idx <- which(colnames==cName)
    # open jpeg format doc
    jpeg(paste(cName,".jpeg"))
    # input barplot based on column catagory
    barplot(table(data[,idx]),
            ylab = "Frequency",
            main = paste(cName," Distribution"))
    dev.off()}
  cat(" current dir",getwd())
}
###########################
# Vector informaion for var
printVecInfo <- function(x)
{
  library(moments)
  return(cat("\n",
             "mean: ",mean(data,x),"\n",
             "median: ",median(data,x),"\n",
             "min: ",min(data,x)," max: ",max(data,x),"\n",
             "sd: ",sd(data,x),"\n",
             "quantile at 0.05: ",quantile(data,x,0.05),"\n",
             "quantile at 0.05: ",quantile(data,x,0.25),"\n",
             "quantile at 0.05: ",quantile(data,x,0.75),"\n",
             "quantile at 0.95: ",quantile(data,x,0.95),"\n",
             "skewness: ",skewness(data,x),"\n"))
}
#######################################################################
# ---                    Cleaning Data                            --- #
#######################################################################
# check if packages are installed & load libraries
EnsurePackage( c("plyr","curl","RJSONIO","jsonlite",
                 "sqldf","ggplot2","reshape2","dplyr") )

# set working directory
directory <- "C:\\Users\\terra\\Desktop\\Syracuse\\IST 687"
setwd(directory)
getwd() 

# read in Hotel Data
Hdata <- read.csv("Hotel Data org.csv")

# hotel csv as data.frame
Hdata <-data.frame(Hdata)

# if var's / columns are numeric & have NA's. 
#         fill NA's with mean of var
Hdata[sapply(Hdata, is.numeric)] <- 
  lapply(Hdata[sapply(Hdata, is.numeric)], 
         function(x) ifelse(is.na(x), 
                            mean(x, na.rm = TRUE), x))

# omit anyother NA's from non-numeric var's
na.omit(Hdata)

# select specific var's 
Hdata <- select(Hdata,"GUEST_COUNTRY_R","QUOTED_RATE_C","MARKET_GROUP_C",
                "Overall_Sat_H","Guest_Room_H","Staff_Cared_H",
                "Condition_Hotel_H","Customer_SVC_H",
                "Likelihood_Recommend_H","Gender_H","Age_Range_H",
                "LENGTH_OF_STAY_C","Internet_Sat_H")

# rename var's in data frame
newName <- c("GuestCountry","QuotedRate","DiscountGroup",
             "OverallSat","GuestRoom","StaffSat",
             "HotelCondition", "CustomerService",
             "LiketoRecommend", "Gender", "AgeRange",
             "LenOfStay","InternetSat")

colnames(Hdata) <- newName
colnames(Hdata)


#######################################################################
# --- Summerize data accoring to mean & sd of var's observations  --- #
#######################################################################

HGender <- Hdata %>%
  # group dataset by gender
  group_by(Gender) %>%
  # summerise data according to parameters below
  summarise(
    StaffSat = mean(StaffSat),
    ConditionMean  =  mean(HotelCondition),
    InternetMean   =  mean(InternetSat),
    # count of var's frequency, used in summarise
    n=n(),
    # mean of likelihood to recommend
    NPSMean=mean(LiketoRecommend),
    # standard deviation of likelihood to recommend
    sd=sd(LiketoRecommend)
  ) %>%
  # Standerd deviation error
  mutate( se=sd/sqrt(n))  %>%
  # sd error * quantile 
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

HAge <- Hdata %>%
  group_by(AgeRange) %>%
  summarise(
    StaffSat = mean(StaffSat),
    ConditionMean =   mean(HotelCondition),
    InternetMean   =  mean(InternetSat),
    n=n(),
    NPSMean=mean(LiketoRecommend),
    sd=sd(LiketoRecommend)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

Hdiscount <- Hdata %>%
  group_by(DiscountGroup) %>%
  summarise(
    StaffSat = mean(StaffSat),
    ConditionMean =   mean(HotelCondition),
    InternetMean   =  mean(InternetSat),
    n=n(),
    NPSMean=mean(LiketoRecommend),
    sd=sd(LiketoRecommend)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# 95% of guest stay 5 days or less.
HStay <- filter(Hdata,LenOfStay<=5)
# filtered according to 95% of guest 
HLenStay <- HStay %>%
  group_by(LenOfStay) %>%
  summarise(
    StaffSat = mean(StaffSat),
    ConditionMean =   mean(HotelCondition),
    InternetMean   =  mean(InternetSat),
    n=n(),
    NPSMean=mean(LiketoRecommend),
    sd=sd(LiketoRecommend)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# remove empty rows from 
#       Age      &    Gender
HAge <- HAge[-1,]; HGender <- HGender[-1,]

#######################################################################
# ---                      Visualizations                         --- #
#######################################################################
#install.packages("ggthemes") # Install 
library(ggthemes)
library(reshape2)
library(plotly)
#######################
# Barplot Discounts   #
#######################
# PLOT
ggplot(Hdiscount, aes(x = Hdiscount$DiscountGroup,y = Hdiscount$n))+
  geom_bar(aes(fill = ConditionMean),
           stat ="identity",
           position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(legend.position = "right")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Hotel Conditions Rating vs. Discount Groups ")+
  ylab("Frequency / Count")+
  xlab("Discount Groups")+ 
  labs(fill = "Conditions")

########################
# Scatterplot Age Range#
########################
# Sample 100 the number of rows in Age dataset 
ageSample <- HAge[sample(nrow(HAge), 1000,replace = TRUE), ]
# PLOT
plot_ly(ageSample, 
        x = ~ConditionMean,
        y = ~NPSMean, 
        type="scatter",
        mode = "markers", 
        color = ~AgeRange, 
        size = ~n) %>%
  layout(title = "NPS vs. Hotel Condition Rating by Age",
         xaxis = list(title = "Mean Hotel Condition Rating"), 
         yaxis = list(title = "Mean NPS"))

########################
# Scatterplot LenOfStay#
########################
# melt data according to entier lenOfStay 
StayPop <- melt(cbind(HLenStay[,1:4], HLenStay[,6]), id=c("LenOfStay"))
# PLOT
ggplot(data=StayPop, 
       aes(x=variable, 
           y=value, 
           colour=variable)) +
  geom_point(shape=23, fill = "cornsilk1" ,size=3)+
  theme_calc()+ scale_colour_calc()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")+
  ylab("1-5 Day Stay Guest Ratings")+
  xlab("95% Guests")


#######################
# Scatterplot Gender  #
#######################
# melt data according to gender & freqency of gender
GenderVal <- melt(HGender[,1:6], id=c("Gender","n"))

ggplot(GenderVal, aes(x=variable, y=value)) + 
  geom_point(aes(col=Gender, size=n))+
  theme_wsj(base_size = 10)+ scale_colour_wsj("colors6")


##############################################################
# ---               BoxPlot Filtering                    --- #
##############################################################
# filter according to Gender
HdataGender <- filter(Hdata,Gender == 'Male' | Gender == 'Female')
# filter according to Length of Stay
HdataLenStay <- filter(Hdata,LenOfStay<=10)
# filter according to Age Range
HdataAge <- filter(Hdata,AgeRange != '76+')
# filter according to 
HdataDis <- filter(Hdata,DiscountGroup != 'OTHER' & 
                     DiscountGroup!= 'SPECIAL' & 
                     DiscountGroup!= 'WHOLESALE')

#############################################################
# ---                Boxplots Produced                  --- #
#############################################################
#                        Y vs. X        data source
Gender.Staff <- boxplot(StaffSat ~ Gender, data = HdataGender,
                        main = "Hotel Satisfaction based on Gender",
                        xlab = "Gender of Rater", 
                        ylab = "Satisfaction Rating")
summary(Gender.Staff$stats)
###########################
Gender.Condition <- boxplot(HotelCondition ~ Gender, data = HdataGender,
                            main = "Hotel Condition based on Gender",
                            xlab = "Gender of Rater", 
                            ylab = "Condition Rating")
summary(Gender.Condition$stats)
###########################
Age.Staff <- boxplot(StaffSat ~ AgeRange, data = HdataAge,
                     main = "Hotel Satisfaction based on Age",
                     xlab = "Age of Rater", 
                     ylab = "Satisfaction Rating")
summary(Age.Staff$stats)
###########################
Age.Conditon <- boxplot(HotelCondition ~ AgeRange, data = HdataAge,
                        main = "Hotel Condition based on Age",
                        xlab = "Age of Rater", 
                        ylab = "Condition Rating")
summary(Age.Conditon$stats)
###########################
Stay.Staff <- boxplot(StaffSat ~ LenOfStay, data = HdataLenStay,
                      main = "Hotel Satisfaction based on Length of Stay",
                      xlab = "Days Stayed at Hotel", 
                      ylab = "Satisfaction Rating")
summary(Stay.Staff$stats)
###########################
Stay.Conditon <- boxplot(HotelCondition ~ LenOfStay, data = HdataLenStay,
                         main = "Hotel Condition based on Length of Stay",
                         xlab = "Days Stayed at Hotel", 
                         ylab = "Condition Rating")
summary(Stay.Conditon$stats)
###########################
Discount.Staff <- boxplot(StaffSat ~ DiscountGroup, data = HdataDis,
                          main = "Hotel Satisfaction based on Discount type",
                          xlab = "", 
                          ylab = "Satisfaction Rating")
summary(Discount.Staff$stats)
###########################
Discount.Condition <- boxplot(HotelCondition ~ DiscountGroup, data = HdataDis,
                              main = "Hotel Condition based on Discount type",
                              xlab = "", 
                              ylab = "Condition Rating")
summary(Discount.Condition$stats)
#####################################
# summary of variables deemed unique#
#####################################
summary(Hdata[,colIdx(Hdata,"AgeRange")])
summary(Hdata[,colIdx(Hdata,"Gender")])
summary(Hdata[,colIdx(Hdata,"LenOfStay")])
summary(Hdata[,colIdx(Hdata,"DiscountGroup")])
