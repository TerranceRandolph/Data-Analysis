# IST687 –--- Intro Homework ----

# Define the following vectors, which represent the weight and height of people on a
# particular team (in inches and pounds):

height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)

#Define a variable:
  a <- 150
  
# ---- Step 1: Calculating means ----
  
# 1) Compute, using R, the average height (called mean in R)
mean(height)
paste("The vector 'height' mean is ",collapse = " ",mean(height))  

# 2) Compute, using R, the average weight (called mean in R)
mean(weight)
paste("The vector 'weight' mean is ",collapse = " ",mean(weight))

# 3) Calculate the length of the vector ‘height’ and ‘weight’
paste("The vector 'height' length is ",collapse = " ",length(height))
paste("The vector 'weight' length is ",collapse = " ",length(weight))

# 4) Calculate the sum of the heights
paste("The sum of the vector height is ",collapse = " ",sum(height))

# 5) Compute the average of both height and weight, by dividing 
# the sum (of the height or the width, as appropriate), by the length of the vector. 
# How does this compare to the ‘mean’ function?
##################################################
# ----          My Answer       ----
# ---- The mean function programmaticaly adds all vectors, counts index (add 1, because of index 0)
# ---- and then divides vector sums by the indexed count.
##################################################
both_vector_mean <- (sum(height) + sum(weight)) / (length(height) + length(weight))
both_vector_mean


# ---- Step 2: Using max/min functions ----
  
# 6) Compute the max height, store the result in ‘maxH’
maxH <- max(height)
maxH

# 7) Compute the min weight, store the results in ‘minW’
minW <- min(weight)
minW


# ---- Step 3: Vector Math ----
  
# 8) Create a new vector, which is the weight + 5 (every person gained 5 pounds)
gainW <- weight + 5
gainW

# 9) Compute the weight/height for each person, using the new weight just created
gainW/height  # divides the weight of that perticular index to its corrisponding height index

# ---- Step 4: Using Conditional if statements ----

# Hint: In R, one can do:
#  if ( 100 < 150 ) "100 is less than 150" else "100 is greater than 150"

# 10) Write the R code to test if max height is greater than 60 (output “yes” or “no”)
if (maxH > 60) "yes" else "no"

# 11) Write the R code to if min weight is greater than the variable ‘a’ (output “yes” or “no”)
if (minW > a) "yes" else "no"

  
