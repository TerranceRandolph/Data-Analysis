
# --- step 1 --- #
# write a function prinVecinfo
# --- Function for printing valuable statistics
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
printVecInfo(c(1,2,3,4,5,6,7,8,9,10,50))


###############
# --- Step 2 functions 
# --- Function sample mean function of object 
samFuc <- function(orginal,samSize,samObj)
{
  sam <- sample(orginal,samSize,replace=TRUE)
  num <- length(sam[sam==samObj])/length(sam) 
  return(mean(num))
}
# --- Sample replicate function of meaned sample
samRep <- function(repNumber,container,samSize,samObj)
{
  sRep <- replicate(repNumber,samFuc(container,samSize,samObj))
  return(sRep)
}


# --- Step 2 Answers --- #
# Q4 create Jar with 50 red, 50 blue marbles
Red <- "Red"
Blue <- "Blue"
R <- replicate(50,Red)
B <- replicate(50,Blue)
JarM <- c(R,B)
############

# Q5 sum of Red & percentage
length(JarM[JarM=="Red"])
cat("Number of Red marbles in Jar are: ",
length(JarM[JarM=="Red"]),"\n",
"total number of marbles are: ",length(JarM))
############

# Q6 sample of 10 & percentage
s.10 <- sample(JarM,10,replace = TRUE)
sr.10 <- length(s.10[s.10=="Red"])
cat(" Ten sampled ",length(s.10[s.10=="Red"])," are Red","\n",
 "Red's are ",length(s.10[s.10=="Red"])/length(s.10)*100," percent of this sample")
###########

# Q7 sample of 10 replicate 20x & stats with histogram
printVecInfo(samRep(20,JarM,10,"Red"))
jpeg("samp10_rep20.jpeg")
hist(samRep(20,JarM,10,"Red"))
dev.off()
###########

# Q8 sample of 100 replicate 20x & stats with histogram
printVecInfo(samRep(20,JarM,100,"Red"))
jpeg("samp100_rep20.jpeg")
hist(samRep(20,JarM,100,"Red"))
dev.off()
##########

# Q9 sample of 10 replicate 20x & stats with histogram
printVecInfo(samRep(100,JarM,100,"Red"))
jpeg("samp100_rep100.jpeg")
hist(samRep(100,JarM,100,"Red"))
dev.off()
##########

# --- Step 3: Explore the airquality dataset --- #
# Q10 store airquality in var
airData <- airquality
##########

# Q11 airquality function: find n/a & replace with 
#     mean of the column itself.
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

View(airQt(airData))
###########

# Q12 print stats and create pdf histogram of each column
colair <- colnames(airQt(airData))
for (i in colair){
  index <- which(colair==i)
  cat("Stat for: ",i,"\n")
  printVecInfo(airQt(airData)[,index])
  cat("\n","Histogram for : ",i)
  pdf(paste(i,".pdf"))
  hist(airQt(airData)[,index], main = i, xlab = paste(i," quality"))
  dev.off()
  cat("\n")
  jpeg(paste(i,".jpeg"))
  hist(airQt(airData)[,index], main = i, xlab = paste(i," quality"))
  dev.off()
  cat("\n")
}


#####################
#install.packages("ggplot2")
library("ggplot2")

a <- airQt(airData)
b <- data.frame(a[,1],a[,2])
ggplot(b, aes( a[,1] = value, color = variable)) + 
  geom_point(aes(y = a[,1], col = "y1")) + 
  geom_point(aes(y = a[,2], col = "y2"))






