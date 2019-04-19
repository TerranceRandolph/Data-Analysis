#R Code - unexecuted
# ---------- HW1: Intro -----------
library(data.table)
myCars <- mtcars

# --- Step	1:	What	is	the	hp	(hp	stands	for	"horse	power")

# 1) What	is	the	highest	hp?
maxHP <- myCars[which.max(myCars$hp),]
maxHP

# 2) Which	car	has	the	highest	hp?
cat("The car with the heightest hp is:  ", row.names(maxHP))



# --- Step	2:	Explore	mpg	(mpg	stands	for	"miles	per	gallon")

# 3) What	is	the	highest	mpg?
maxMPG <- myCars[which.max(myCars$mpg),]
maxMPG

# 4) Which	car	has	the	highest	mpg?
cat("The car with the heightest hp is:  ", row.names(maxMPG))

# 5) Create	a	sorted	dataframe,	based	on	mpg
df <- data.frame(myCars)
aDF <- df[order(df$mpg),] #sorted asending order
aDF
dDF <- df[order(-df$mpg),] #sorted desending order
dDF

# --- Step	3:	Which	car	has	the	"best"	combination	of	mpg	and	hp?

# 6) What	logic	did	you	use?

# Ordered myCars$mpg & myCars$hp from high to low
# Get head() of each column which is the defualt largest six of each var
# but was adjusted because no match found
# Compare var's to see which rows.names() match
# Take those matching names and find best mpg as BestCar

# 7) Which	car?
# --- method one --- #
odrMpg <- myCars[order(-myCars$mpg),]
odrHP <- myCars[order(-myCars$hp),]
nameHP <- row.names(head(odrHP,15))
nameMPG <- row.names(head(odrMpg,15))

cbind(nameHP,nameMPG)

carsDF <- myCars[row.names(myCars) %in% intersect(nameHP,nameMPG),]
cat("best car for fuel and horse power is: ", row.names(carsDF))

# --- method two: meadian & best Hp --- # code works but commented out for no confusion
# hpM <- myCars[myCars$hp >= median(myCars$hp),]
# mpgM <- myCars[myCars$mpg >= median(myCars$mpg),]
# carNames <- intersect(row.names(mpgM),row.names(hpM))
# bcDF <- myCars[row.names(myCars) %in% carNames,]
# bcHP <- which.max(bcDF$hp) # if you like more hp, Best Car
# BestCar <- row.names(bcDF[bcHP,])
# ------------------------------------ #

# --- Step	4:	 Which	car	has "best"	car combination	of	mpg	and	hp,	
#     where	mpg	and	hp	must be	given	equal	weight?

# Create new weighted mpg & hp columns with scale function
myCars$mpgWT <- scale(myCars$mpg)  
myCars$hpWT <- scale(myCars$hp)  
cbind(myCars$mpgWT,myCars$hpWT)

# Store mpg & hp in new data frame if greater than median
# Above advg. in those areas
hpM_WT <- myCars[myCars$hpWT >= median(myCars$hpWT),]
mpgM_WT <- myCars[myCars$mpgWT >= median(myCars$mpgWT),]

# Find the car names that intersect and pull data according to names 
carNames_WT <- intersect(row.names(mpgM_WT),row.names(hpM_WT))
bcDF_WT <- myCars[row.names(myCars) %in% carNames_WT,]
bcDF_WT

# Find max hp because I like power, than extract row name
bcHP_WT <- which.max(bcDF_WT$hpWT) # if you like more hp, Best Car
BestCarWT <- row.names(bcDF_WT[bcHP_WT,])
BestCarWT