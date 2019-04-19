library(ggplot2)

oilChange <- c(1:10)
repairs <- c(15, 12, 8, 5, 4, 4, 5, 3, 2, 2)

df <- data.frame(oilChange, repairs)
plot(df$oilChange, df$repairs)

m <- lm(repairs ~ oilChange, data = df)

summary(m)
abline(m)

g <- ggplot(df, aes(x = oilChange, y = repairs)) + geom_point()
g
g + stat_smooth(method = "lm", col = "red")

predict(m)

#use cars data set to predict the mpg using other variables
mpg.ml = lm(formula = mpg ~ wt, data = mtcars)
summary(mpg.ml)

plot(mtcars$wt, mtcars$mpg)
abline(mpg.ml)

mpg.ml = lm(formula = mpg ~ hp, data = mtcars)
summary(mpg.ml)

mpg.ml = lm(formula = mpg ~ wt + hp, data = mtcars)
summary(mpg.ml)


#so many variables may neg impat the accuracy of the model
mpg.ml = lm(formula = mpg ~ ., data = mtcars)
summary(mpg.ml)

mpg.ml = lm(formula = mpg ~ wt + cyl, data = mtcars)
summary(mpg.ml)


newdata <- data.frame(wt = 4)
predict(mpg.ml, newdata, type = "response")

sum.model <- summary(mpg.ml)
sum.model$adj.r.squared

g <- ggplot(mtcars, aes(x = cyl, y = wt)) + geom_point(aes(size = mpg, color = mpg))
g
g + stat_smooth(method = "lm")


#mpg.lm = lm(formula = mpg ~ ., data=mtcars)
#step(mpg.lm, data=mtcars, direction = "backward")
#AIC (Akaike information criterion) 

# --- HW8 --- #

library(readxl)
setwd("C:/Users/terra/Desktop/Syracuse/IST 687/HW/hw8")
# 1. Read data into R
dataURL <-"C:/Users/terra/Desktop/Syracuse/IST 687/mlr01.xls"
# function: read excel file and input into data frame with col names
readData <- function(dataURL,cNames)
  {
    rawData <- read_excel(dataURL,na="empty")
    df <- data.frame(rawData)
    colnames(df) <- cNames
    return(df)
}
# 2. store data frame
antelopData <- readData(dataURL,
                        c("fawn", "adultPop", "annualPrecip", "badWinter"))
View(antelopData)
# 3. review structure of data frame
str(antelopData)
# review first 6 row of all columns including column names
head(antelopData)

# --- 4. Bivariant Plot --- #
plot.one <- plot(antelopData$adultPop, antelopData$fawn, 
                  main="Plot 1 Fawn & Antelope", 
                  xlab="Antelope Population Independant Variable", 
                  ylab="Fawn Dependent Variable")
plot.two <- plot(antelopData$annualPrecip, antelopData$fawn, 
                 main="Plot 2 Fawn & Precipertation", 
                 xlab=" precipertation Independant Variable", 
                 ylab=" Fawn Dependent Variable")
plot.three <- plot(antelopData$badWinter, antelopData$fawn, 
                 main="Plot 3 Fawn & Winter Effects", 
                 xlab=" Winter Independant Variable", 
                 ylab=" Fawn Dependent Variable")

jpeg("plot1.jpeg")
plot.one
dev.off()

jpeg("plot2.jpeg")
plot.two
dev.off()

jpeg("plot3.jpeg")
plot.three
dev.off()

# --- 5.three regression models --- #
# predict the number of fawns from the severity of the winter
model.one <- lm(formula = fawn ~ badWinter, data = antelopData)
summary(model.one)
predict(model.one)

# the second model, predict the number of fawns from two variables 
#           (one should be the severity of the winter).
model.two <- lm(formula = fawn ~ badWinter + annualPrecip, data = antelopData)
summary(model.two)
predict(model.two)

# the third model predict the number of fawns from the three other variables
model.three <- lm(formula = fawn ~ ., data = antelopData)
summary(model.three)
predict(model.three)


pdf("model 1.pdf")
plot(model.one)
dev.off()

pdf("model 2.pdf")
plot(model.two)
dev.off()

pdf("model 3.pdf")
plot(model.three)
dev.off()



# Which model works best? 
# Which of the predictors are statistically
# significant in each model? 
# If you wanted to create the most parsimonious model 
#             (i.e.,the one that did the best job with the 
#                   fewest predictors), what would it contain?
                                                                                the one that did the best job with the fewest predictors), what would it contain?
  
#Inspect the data to ensure 8 observations of 4 variables


# adj R-squared how well the model is fitting the actual data
#Residual Standard Error: Residual Standard Error is measure of the quality of a linear regression fit
#The coefficient t-value is a measure of how many standard deviations our coefficient estimate is far away from 0
#Residuals are essentially the difference between the actual observed response values
#F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables


