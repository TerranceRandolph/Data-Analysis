# --- IST 707 HW2 --- #

library(dplyr)
######
CTendency <- function(data){
  cnames <- colnames(data)
  for (i in cnames){
    # get index of columns
    idx <- which(cnames==i)
    # central Tendency: numeric
    # check if column is numeric
    if (is.numeric(data[,idx])==TRUE){
      # do numeric
      cat("mean: ", mean(data[,idx]),"\n",
          "median: ", median(data[,idx]),"\n",
          "freqency: ", table(data[,idx]),"\n",
          "variance: ", var(data[,idx]),"\n",
          "StandDev: ", sd(data[,idx]),"\n",
          "range: ", range(data[,idx]),"\n",
          "IQR: ", IQR(data[,idx]),"\n",
          "minimum: ", min(data[,idx]),"\n",
          "maximum: ", max(data[,idx]),"\n")
    }
    else {cat(i," not numeric","\n")}

  }
}

setwd("C:/Users/terra/Desktop/IST 707")
SchoolData <- read.csv("data-storyteller.csv", na.string=c(""))
### 
str(SchoolData)
# -- Data cleaning
# change section to factor
SchoolData$Section <- factor(SchoolData$Section)
#
sapply(SchoolData[,3:8], as.numeric)

# summery
summary(SchoolData)
#
CTendency(SchoolData)
# number of all students
std.count <- c(SchoolData$Very.Ahead..5,
               SchoolData$Middling..0,
               SchoolData$Behind..1.5,
               SchoolData$More.Behind..6.10,
               SchoolData$Very.Behind..11,
               SchoolData$Completed)

cat("total number of sudents is: ",sum(std.count))
# attach data
attach(SchoolData)
# total number of students per school
std.bySchool <- SchoolData %>%
                    # group dataset by gender
                    group_by(School) %>%
                    # summerise data according to parameters below
                    summarise(
                      sum(Very.Ahead..5,
                          Middling..0,
                          Behind..1.5,
                          More.Behind..6.10,
                          Very.Behind..11,
                          Completed))

# total number of students per section
std.bySection <- SchoolData %>%
                    # group dataset by gender
                    group_by(Section) %>%
                    # summerise data according to parameters below
                    summarise(
                      sum(Very.Ahead..5,
                          Middling..0,
                          Behind..1.5,
                          More.Behind..6.10,
                          Very.Behind..11,
                          Completed))

# sum students per status by Section
bySection <- aggregate(cbind(SchoolData$Very.Ahead..5,
                             VAhead=SchoolData$Very.Ahead..5,
                             Middle=SchoolData$Middling..0,
                             bh_1.5=SchoolData$Behind..1.5,
                             bh_6.10=SchoolData$More.Behind..6.10,
                             bh_11=SchoolData$Very.Behind..11,
                             Comp=SchoolData$Completed),
          by=list(Group.sections=SchoolData$Section),FUN=sum)

# sum students per status by School
bySchool <- aggregate(cbind(VAhead=SchoolData$Very.Ahead..5,
                            Middle=SchoolData$Middling..0,
                            bh_1.5=SchoolData$Behind..1.5,
                            bh_6.10=SchoolData$More.Behind..6.10,
                            bh_11=SchoolData$Very.Behind..11,
                            Comp=SchoolData$Completed),
                      by=list(Group.school=SchoolData$Section),
                      FUN=sum)
# --- Visuals
# plot factors
plot(Section ~ School, SchoolData,
     main="Section density by School", 
     xlab="Schools",
     ylab="Section")


# boxplots: dependent y: completed vs. x: independent 
boxplot(Middling..0 ~ Completed,data=SchoolData,
        main="Course Standings", 
        xlab="Students in the Middle",
        ylab="Students Completed")

boxplot(More.Behind..6.10 ~ Completed,data=SchoolData, 
        main="Course Standings", 
        xlab="Students more behind",
        ylab="Students Completed")

boxplot(Behind..1.5 ~ Completed,data=SchoolData, 
        main="Course Standings", 
        xlab="Students Behind",
        ylab="Students Completed")



