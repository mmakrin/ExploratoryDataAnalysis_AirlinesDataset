# Libraries
library(moments)
library(DescTools)
library(ggplot2)
library(dplyr)
library(caret)
library(plotrix)
library(nortest)
library(corrplot)
library(stringr)
library(MASS)
library(fitdistrplus)
library(goftest)
library(gridExtra)


#---------------------------------------------------------------------------------------------#
#-------------------------------------CHAPTER 1-----------------------------------------------#
#-----------------------------------PRE PROCESSING---------------------------------------------#

# 1.Load and inspect data
# Load the dataset Airlines

data <- read.csv("Airlines")
print(data)

#Structure of the data(type of data)
str(data)

# 2. Data quality checks(missing,duplicates)

# Check for missing values in each column(if there are any)
colSums(is.na(data)) #no missing values

# Check for duplicates
any(duplicated(data)) #no duplicates


# 3. Descriptive statistics
#Statistical summary of each column
summary(data)

#check for skewness and kurtosis for the numerical features(duration,price,days_left)

skewness(data$duration) # 0.9035273

skewness(data$price) # 1.77812

skewness(data$days_left) # -0.04258498

kurtosis(data$duration) # 2.949861

kurtosis(data$price) # 7.118112

kurtosis(data$days_left) # 1.856952

# 4.Cleaning and encoding

# Variety of unique categories in categorical variables

sapply(data[, sapply(data, is.character)], unique)

# Check if source city and class have the same value in all rows
all_equal_source <- length(unique(data$source_city)) == 1  
all_equal_class <- length(unique(data$class)) == 1   
all_equal_source #only one value
all_equal_class #only one value

# Since the columns source_city and class contain the same value in all rows, we decided to drop them as they will not provide any useful information for our Exploratory Data Analysis (EDA)
data_v1 = data[,!(names(data) %in% c("source_city","class"))] #drop columns source_city and class
colnames(data_v1) #check if they dropped

# Check if there are only 6 sequence values in variable departure time and arrival time
unique(data_v1$departure_time)
unique(data_v1$arrival_time)

#-------------------------------------------------------------------------------------------------------------------#
#-----------------------------------VARIABLE : FLIGHT----------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------#

data_v1 %>%
  count(flight, sort = TRUE)   #count how many number of flight exists and put them in ascendind order


# Checking only a random flight number to see how the columns changes 

flight_subset <- data_v1 %>%
  filter(flight == "I5-773")
View(flight_subset)


# From here we can see that the only variables that change its the duration,days left and price the other variables stay the same
prefix_reference <- data.frame(
  airline = c("Air_India", "SpiceJet", "Vistara", "GO_FIRST","Indigo","AirAsia"),
  expected_prefix = c("AI", "SG", "UK","G8" ,"6E","I5")
)
flights <- data_v1 %>%
  mutate(actual_prefix = str_sub(flight, 1, 2))
flights_checked <- flights %>%
  left_join(prefix_reference, by = "airline") %>%
  mutate(prefix_match = actual_prefix == expected_prefix)
View(flights_checked)  ######the only incorrect values are 6. instead of 6E in the flight number so we are going to fix these inconsistencies



# In the column flight there are some wrong-typo and replace it with the right value
wrong_numberflight <- grepl("^\\s*6\\.?0+E", data_v1$flight, ignore.case = TRUE)
data_v1[wrong_numberflight, c("airline","flight")]  
table(wrong_numberflight) # 9751 False,249 True

# Fix the mismatch
data_v1$flight <- gsub("^6\\.?0+E", "6E", data_v1$flight, ignore.case = TRUE)

# Check if the fix is okay and we create a variable for checking it
subset_mismatch <- subset(data_v1, airline == "Indigo" & substr(flight, 1, 2) != "6E")
subset_mismatch #0

# Check a specific row to see if it is okay 
data_v1[80, ] # correct



######################################## END OF CHAPTER 1 ####################################

#--------------------------------------------------------------------------------------------#
#-------------------------------------CHAPTER 2----------------------------------------------#
#-------------------------------UNIVARIATE ANALYSIS----------------------------------------------#


# UNIVARIATE ANALYSIS AND VISUALIZATION

# Histogram for all the numeric features 1) price 2)duration 3)days left

# Function to plot histogram with custom h = (2 * IQR) / (n^(1/3))
plot_hist <- function(x, main = "Histogram", xlab = "Value",ylim = NULL, xlim = NULL, col = "skyblue") {
# Remove missing values
x <- x[!is.na(x)]
# Calculating IQR,h to find the perfect width and number of bins for histogram  
# Calculate Q1, Q3, IQR, and n
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR_value <- Q3 - Q1
n <- length(x)
  
# Compute h
h <- (2 * IQR_value) / (n^(1/3))
  
# Define breaks
breaks_seq <- seq(min(x), max(x) + h, by = h)
  
# Print information
cat("Bin width (h):", h, "\nNumber of bins:", length(breaks_seq) - 1, "\n")
  
# Plot histogram
hist(x, main = main, xlab = xlab, ylim = ylim, xlim = xlim, breaks = breaks_seq, border = "white", col = col)
}

# Histograms

# VARIABLE 1: PRICE
plot_hist(data_v1$price,main = "Distribution of Flight Prices",xlab = "Price",ylim = c(0, 2500),xlim = c(0, 35000),col = "skyblue")

#VARIABLE 2: DURATION
plot_hist(data_v1$duration, main="Flight Duration Distribution", xlab="Duration",ylim = c(0,3000),xlim = c(0,35), col="red")

#VARIABLE 3: DAYS LEFT
plot_hist(data_v1$days_left, main="Days Left Distribution", xlab="Days Left",ylim = c(0,500),xlim = c(0,55),col="orange")

# Box Plot for all the numeric features 1) price 2)duration 3)days left

# VARIABLE 1: Price
boxplot(data_v1$price,
        main = "Prices of the flights boxplot",  
        ylab = "Price",                        
        col = "skyblue",                      
        border = "darkblue")

# VARIABLE 2: Duration
boxplot(data_v1$duration,
        main = "Duration of the flights boxplot",  
        ylab = "Hours",                        
        col = "red",                      
        border = "black")

#VARIABLE 3: Days Left
boxplot(data_v1$days_left,
        main = "Days left from booking to trip date boxplot",
        ylab = "Hours",                        
        col = "orange",                      
        border = "red")

# Bar Plot for categorical variables 1)airline 2)destination_city 3)departure_time 4)stops 5)arrival_time 6)flight number(top 10)

# VARIABLE 1: Airline
ggplot(data_v1, aes(x = airline)) +
  geom_bar(fill = "skyblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 4)+
  theme_minimal() +
  labs(title = "Flights by Airline",
       x = "Airlines",
       y = "Count")

# VARIABLE 2: Destination_city
ggplot(data_v1, aes(x = destination_city)) +
  geom_bar(fill = "orange") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 4)+
  theme_minimal() +
  labs(title = "Flights by Destination City",
       x = "Destination City",
       y = "Count")

# VARIABLE 3: Departure_time
ggplot(data_v1, aes(x = departure_time)) +
  geom_bar(fill = "pink") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 4)+
  theme_minimal() +
  labs(title = "Flights by Departure Time",
       x = "Departure Time",
       y = "Count")

# VARIABLE 4: Stops
ggplot(data_v1, aes(x = stops)) +
  geom_bar(fill = "green") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 4)+
  theme_minimal() +
  labs(title = "How many stops per flight",
       x = "stops",
       y = "Count")

# VARIABLE 5: Arrival time
ggplot(data_v1, aes(x = arrival_time)) +
  geom_bar(fill = "purple") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 4)+
  theme_minimal() +
  labs(title = "Flights by Arrival time",
       x = "Arrival Time",
       y = "Count")

# VARIABLE 6: Flight Number(10 most frequent)
data_v1 %>%
  count(flight, sort = TRUE) %>%
  head(10)

data_v1 %>%
  count(flight, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(flight, n), y = n)) +
  geom_bar(stat = "identity", fill = "yellow") +
  geom_text(aes(label = n), hjust = -0.2, size = 2)+
  theme_minimal() +
  labs(title = "Top 10 Most Frequent Flights", 
       x = "Flight", 
       y = "Count")


# Pie chart for the categorical variables 1)airline, 2)destination city 3)departure time 4)arrival time 5)stops 6)flight number(top 10)

# VARIABLE 1: Airline
slices <- table(data_v1$airline)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie3D(slices,
    labels=lbls,
    col=rainbow(length(lbls)),
    labelcex = 0.8,
    main="Pie chart for different airlines")

# VARIABLE 2: Destination_city
slices <- table(data_v1$destination_city)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie3D(slices,
      labels=lbls,
      col=rainbow(length(lbls)),
      labelcex = 0.8,
      main="Pie chart for different destination cities")

# VARIABLE 3: Departure time
slices <- table(data_v1$departure_time)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie3D(slices,
      labels=lbls,
      col=rainbow(length(lbls)),
      labelcex = 0.8,
      main="Pie chart for different departure times")

# VARIABLE 4: Arrival time
slices <- table(data_v1$arrival_time)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie3D(slices,
      labels=lbls,
      col=rainbow(length(lbls)),
      labelcex = 0.8,
      main="Pie chart for different arrival times")

# VARIABLE 5: Stops
slices <- table(data_v1$stops)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie3D(slices,
      labels=lbls,
      col=rainbow(length(lbls)),
      labelcex = 0.8,
      main="Pie chart for the number of stops")

# VARIABLE 6: Flight numbers

top10_flights <- data_v1 %>%
  count(flight, sort = TRUE) %>%
  slice_head(n = 10)
slices <- top10_flights$n
names(slices) <- top10_flights$flight
lbls <- names(slices)
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct, "%")
pie3D(
  slices,
  labels = lbls,
  col = rainbow(length(lbls)),
  labelcex = 0.8,
  explode = 0.1,
  main = "Pie Chart for the Top 10 Most Frequent Flights"
)

# From what we can see from the boxplots we indicate that there are 
# many outliers in the column price so we should find IQR in order to check properly the outliers

summary(data_v1$price)
quantile(data_v1$price, probs = c(0.95, 0.99, 1))
skewness(data_v1$price)
Q1 <- quantile(data_v1$price, 0.25)
Q3 <- quantile(data_v1$price, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1- 1.5*IQR
upper_bound <- Q3+1.5*IQR
price_cleaned <- data$price
sum(data_v1$price  < lower_bound | data_v1$price  > upper_bound) #791

# Checking some samples in the column price of the outliers to see what are they look like

outliers <- data_v1 %>% 
  filter(price< lower_bound | price > upper_bound) %>%
  arrange(desc(price))

head(outliers)


############################# OPTION 1:log transformation for price ##############################################
# creating data_v2 in order to save transformation of the price
data_v2 <- data_v1
data_v2$log_price <- log1p(data_v2$price)
par(mfrow=c(1,1))
boxplot(data_v2$price, main="Original Price", col="lightblue")
boxplot(data_v2$log_price, main="Log-transformed Price", col="lightgreen")
summary(data_v2$log_price)
skewness(data_v2$log_price) #0.3461171
kurtosis(data_v2$log_price) #2.499487
var(data_v2$log_price) #0.3046668
sd(data_v2$log_price) #0.5519663
IQR(data_v2$log_price) #0.7753938

# VARIABLE 1: Price
boxplot(data_v2$log_price,
        main = "Prices of the flights boxplot Log Transformation",  
        ylab = "Price",                        
        col = "skyblue",                      
        border = "darkblue")


#lillie.test(data$log_price) #D = 0.089005, p-value < 2.2e-16

##################### OPTION 2: BOX-COX TRANSFORMATION #####################################
bc <- BoxCoxTrans(data_v2$price)
bc$lambda # -0.3
data_v2$price_boxcox <- predict(bc, data_v2$price)
par(mfrow = c(1, 2))
hist(data_v2$price, main="Original Price", xlab="Price", col="skyblue")
hist(data_v2$price_boxcox, main="Box-Cox Transformed Price", xlab="Transformed", col="lightgreen")

summary(data_v2$price_boxcox)
skewness(data_v2$price_boxcox) #0.02916809
kurtosis(data_v2$price_boxcox) #2.151943
var(data_v2$price_boxcox) #0.00175558
sd(data_v2$price_boxcox) #0.04189964
IQR(data_v2$price_boxcox) #0.06202726

# VARIABLE 1: Price BOX PLOT COMPARABLE
par(mfrow = c(1,3))
boxplot(data_v1$price,
        main = "Raw Prices of the flights ",  
        ylab = "Price",                        
        col = "skyblue",                      
        border = "darkblue")


boxplot(data_v2$log_price,
        main = "Log Transformatio Prices of the flights ",  
        ylab = "Price",                        
        col = "lightgreen",                      
        border = "black")


boxplot(data_v2$price_boxcox,
        main = "BoxCox Prices of the flights",  
        ylab = "Price",                        
        col = "salmon",                      
        border = "black")

par(mfrow=c(1,1))

#lillie.test(data_v2$price_boxcox)#D = 0.10002, p-value < 2.2e-16 we have too many observations, so lillie test cant tell exactly

# VARIABLE 1: Price HISTOGRAM COMPARABLE 

p1 <- ggplot(data_v1, aes(price)) +
  geom_histogram(bins = 50, fill="skyblue", color="black") +
  ggtitle("Raw Price") +
  theme_minimal()

p2 <- ggplot(data_v2, aes(log_price)) +
  geom_histogram(bins = 50, fill="lightgreen", color="black") +
  ggtitle("Log Price") +
  theme_minimal()

p3 <- ggplot(data_v2, aes(price_boxcox)) +
  geom_histogram(bins = 50, fill="salmon", color="black") +
  ggtitle("Box–Cox Price") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)


#DISTRIBUTION OF THE NUMERICAL VARIABLES

# Variable 1: Days_Left
##########################Step 1: CHECK NORMALITY FOR DAYS LEFT ######################################
qqnorm(data$days_left); qqline(data$days_left, col="red")
lillie.test(data$days_left)


# Plotting ECDF of the variables 1) price 2) duration 3) days left

#ECDF PLOT: PRICE#
plot(ecdf(data$price), xlab = "Price", ylab = "Probability",
     main = "Price ECDF")

#ECDF PLOT: DURATION#
plot(ecdf(data$duration), xlab = "Duration", ylab = "Probability",
     main = "Duration ECDF")

#ECDF PLOT: DAYS_LEFT#
plot(ecdf(data$days_left), xlab = "Days left", ylab = "Probability",
     main = "Days Left ECDF")

# Check distribution for other variables
# Variable 2: Duration

#Parameter estimation with moment method

# extract non missing and take only positive price values
x <- na.omit(data_v1$price) 
x <- x[!is.na(x) & x>0]
# extract non missing and take only positive duration values
y <- na.omit(data_v1$duration) 
x <- y[!is.na(x) & x>0]

# Moment estimators for gamma distribution(price)
mean_x <- mean(x)
var_x  <- var(x)

# gamma(shape,rate) moment estimators
shape  <- (mean_x^2) / var_x
rate   <- mean_x / var_x
scale  <- 1 / rate

# Moment estimators for gamma distribution(price)
mean_y <- mean(y)
var_y  <- var(y)
shape_y  <- (mean_y^2) / var_y
rate_y   <- mean_y / var_y
scale_y  <- 1 / rate_y


#######################

# Anderson-Darling test

#######################

ad_result_x <- ad.test(x, null = "pgamma", shape = shape, rate = rate)
print(ad_result_x)

ad_result_y <- ad.test(y, null = "pgamma", shape = shape_y, rate = rate_y)

print(ad_result_x) #An = 189.95, p-value = 6e-08
print(ad_result_y) #An = 187.62, p-value = 6e-08

#######################################################################
##### KOLMOGOROV-SMIRNOV TEST FOR UNIFORM DISTRIBUTION: days left #####
#######################################################################

x <- data_v1$days_left
x <- na.omit(x)

# Firstly normalize to [0,1] for uniformity test
x_norm <- (x - min(x)) / (max(x) - min(x))

# KS test against U(0,1)
ks_result <- ks.test(x_norm, "punif", 0, 1)
print(ks_result)


########################################################################
###################Histogram and Curve Distributions####################

# VARIABLE : PRICE -> GAMMA DISTRIBUTION
price<- data$price

# fit Gamma by method of moments

fit <- fitdist(price, "gamma", method = "mme")
shape <- fit$estimate["shape"]
rate <- fit$estimate["rate"]
hist(price, prob = TRUE, breaks = 30,
     main = "Gamma distribution for Price",
     xlab = "Price", ylab = "Density",
     col = "white", border = "black", ylim= c(0,0.00025), xlim = c(0, 35000))
curve(dgamma(x, shape = shape, rate = rate),
      col = "blue", lwd = 2, add = TRUE)


# VARIABLE 2:DURATION -> GAMMA DISTRIBUTION

duration<- data$duration
fit <- fitdist(duration, "gamma", method = "mme")
shape <- fit$estimate["shape"]
rate <- fit$estimate["rate"]
hist(duration, prob = TRUE, breaks = 30,
     main = "Gamma distribution for Duration",
     xlab = "Duration", ylab = "Density",
     col = "white", border = "black", ylim= c(0,0.3), xlim = c(0, 30))
curve(dgamma(x, shape = shape, rate = rate),
      col = "blue", lwd = 2, add = TRUE)

###### FOCUS ON DURATION #######  

summary(x)
hist(x, breaks = 30, col = "lightblue", main = "Duration distribution")

#AIC HYPOTHESIS TESTING FOR VARIABLE: DURATION TO CHECK WHAT FITS BEST
# create multiple candidate distributions using MLE
f_gamma   <- fitdist(x, "gamma", method = "mle")
f_lnorm   <- fitdist(x, "lnorm", method = "mle")
f_exp <- fitdist(x, "exp", method = "mle")

# compare the AIC values
summary(f_gamma)
summary(f_lnorm)
summary(f_exp)

# another refit for confirmation
f_exp <- fitdist(x, "exp")
f_gamma <- fitdist(x, "gamma")
f_lnorm <- fitdist(x, "lnorm")

#As we see gamma distribution fits better as its the lowest value of AIC

###### QQ-PLOT #####
###### QQ-PLOT FOR VARIABLE: DURATION (GAMMA) #########################################################
x <- na.omit(data_v1$duration)
x <- x[x > 0]  
mean_x <- mean(x)
var_x  <- var(x)

shape_hat <- (mean_x^2) / var_x
rate_hat  <- mean_x / var_x

n <- length(x)
theoretical_q_gamma <- qgamma(ppoints(n), shape = shape_hat, rate = rate_hat)


qqplot(theoretical_q_gamma, sort(x),
       main = "QQ-plot vs theoretical gamma distribution",
       xlab = "Theoretical quantiles (Gamma)",
       ylab = "Observed quantiles (duration)",
       pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)


################### QQ-PLOT - VARIABLE 2: PRICE GAMMA DISTRIBUTION #########################
x <- na.omit(data_v1$price)
x <- x[x > 0]
mean_x <- mean(x)
var_x  <- var(x)

shape_hat <- (mean_x^2) / var_x
rate_hat  <- mean_x / var_x

n <- length(x)
theoretical_q_gamma <- qgamma(ppoints(n), shape = shape_hat, rate = rate_hat)

qqplot(theoretical_q_gamma, sort(x),
       main = "QQ-plot vs theoretical gamma distribution",
       xlab = "Theoretical quantiles (Gamma)",
       ylab = "Observed quantiles (price)",
       pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

#############QQ PLOT FOR VARIABLE DAYS-LEFT-UNIFORM DISTRIBUTION#############
x <- na.omit(data_v1$days_left)
x <- x[x > 0]
min_hat <- min(x)
max_hat <- max(x)
n <- length(x)
theoretical_q_unif <- qunif(ppoints(n), min = min_hat, max = max_hat)

qqplot(theoretical_q_unif, sort(x),
       main = "QQ-plot vs theoretical uniform distribution",
       xlab = "Theoretical quantiles (Uniforme)",
       ylab = "Observed quantiles (duration)",
       pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)



#########################################END OF CHAPTER 2#################################################################

#--------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------CHAPTER 3-------------------------------------------------------------------------------#
#----------------------------------BIVARIATE ANALYSIS----------------------------------------------------------------------#



#BIVARIATE ANALYSIS

# Creating correlation matrix only for the numerical variables


# Creating a variable to save numeric variables
# in the correlation matrix we use method spearman because as we saw variable price has extreme outliers and spearman its better than pearson for this
numeric_variables <- data[sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_variables, method="spearman")


# Plotting correlation matrix
corrplot(correlation_matrix,
         method="color",
         addCoef.col="black",
         tl.col="black",
         tl.srt=45)


par(mfrow=c(1,1))

# Convert price into bins in order to do cramer matrix 

# creating data_V3 in order to save price_category
data_v3 <- data_v2
data_v3$price_category <- cut(
  data_v3$price,
  breaks = quantile(data_v3$price, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High", "Very High")
)

# Correlation between categorical variables

categorical_variables <- names(data_v1)[sapply(data_v1, function(x) is.factor(x) | is.character(x))] # we choose only the variables that are characters or factorised
categorical_variables

# Creating cramer matrix to compare different categorical variables with eachother

cramer_matrix <- matrix(NA, ncol = length(categorical_variables), nrow = length(categorical_variables),
                        dimnames = list(categorical_variables, categorical_variables))

for (i in 1:length(categorical_variables)) {
  for (j in 1:length(categorical_variables)) {
    if (i == j) {
      cramer_matrix[i, j] <- 1
    } else {
      tbl <- table(data[[categorical_variables[i]]], data[[categorical_variables[j]]])
      cramer_matrix[i, j] <- suppressWarnings(CramerV(tbl))
    }
  }
}
# If the value>0.7 high correlated

corrplot(cramer_matrix,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         )
title(main = "Cramer's Matrix (Association Strength)", line = 2)

# we take again the categorical variables but now with the other dataset

cat_variables <- names(data_v3)[sapply(data_v3, function(x) is.factor(x) | is.character(x))]
cat_variables

# Chi-square test of independence and cramer V were computed between price categories and all categorical features to assess association strength
# Now its not only for price category 
# Create an empty data frame to store results of the bivariate analysis
# Each row will contain variable name, its chi-square p-value, and the strength of association measured by Cramer's V.
bivariate_summary <- data.frame(
  variable = character(),
  p_value = numeric(),
  cramer_v = numeric()
)
for (v in cat_variables) {
  tbl <- table(data_v3[[v]], data_v3$price_category)
  test <- suppressWarnings(chisq.test(tbl))
  cv <- suppressWarnings(CramerV(tbl))   
  bivariate_summary <- rbind(bivariate_summary,
                         data.frame(variable = v, p_value = test$p.value, cramer_v = cv))
}

# Sort the results by Cramer's V in descending order
# This helps identify which variables have the strongest association
bivariate_summary <- bivariate_summary[order(-bivariate_summary$cramer_v), ]
print(bivariate_summary)

##################################### BOX-PLOT ###############################################################################

# Box plot using ggplot for creating different correlations with price

# Correlation 1: Price vs Destination City
ggplot(data_v3, aes(x=destination_city, y=price, fill=destination_city))+
  geom_boxplot()+
  labs(title="Ticket Price Distribution by Destination City") +
  theme_minimal()

# correlation 2: Price vs Airline and Log_Price vs Airline
par(mfrow=c(1,2))
ggplot(data_v3, aes(x=airline, y=price, fill=airline))+
  geom_boxplot()+
  labs(title="Ticket Price Distribution by Airline") +
  theme_minimal()

ggplot(data_v2, aes(x=airline, y=log_price,fill=airline)) + 
  geom_boxplot(outlier.alpha = 0.2) + 
  theme_minimal()+
  labs(title="Ticket Log Price Distribution by Airline")

ggplot(data_v2, aes(x=airline, y=price_boxcox,fill=airline)) + 
  geom_boxplot(outlier.alpha = 0.2) + 
  theme_minimal()+
  labs(title="Ticket BoxCox Price Distribution by Airline")


# Correlation 3: Price vs departure time
ggplot(data_v3, aes(x=departure_time, y=price, fill=departure_time))+
  geom_boxplot()+
  labs(title="Ticket Price Distribution by Departure Time") +
  theme_minimal()

# Check for each value of departure time the distribution with price
table(data_v3$price_category, data_v3$departure_time)

# Using proportions
round(prop.table(table(data_v3$price_category, data_v3$departure_time), margin = 2) * 100,2)

# Chi squared test
chisq.test(table(data_v3$price_category, data_v3$departure_time)) #X-squared = 312.47, df = 15, p-value < 2.2e-16 strongly associated

# Stack Bar departure time vs price_category
df_plot <- data_v3 %>%
  count(price_category, departure_time)


ggplot(df_plot, aes(x = departure_time, y = n, fill = price_category)) +
  geom_bar(stat = "identity") +
  ylim(0, 3000) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribution of Departure Time by Price Category",
    x = "Departure Time",
    y = "Count",
    fill = "Price Category"
  )

# Correlation 4: Price vs arrival time
ggplot(data_v3, aes(x=arrival_time, y=price, fill=arrival_time))+
  geom_boxplot()+
  labs(title="Ticket Price Distribution by Arrival Time") +
  theme_minimal()

# Check for each value of departure time the distribution with price
table(data_v3$price_category, data_v3$arrival_time)

round(prop.table(table(data_v3$price_category, data_v3$arrival_time), margin = 2) * 100,2)

chisq.test(table(data_v3$price_category, data_v3$arrival_time)) #X-squared = 723.18, df = 15, p-value < 2.2e-16 storngly assosiated

# Stack Bar arrival time vs price_category
df_plot <- data_v3 %>%
  count(price_category, arrival_time)
ggplot(df_plot, aes(x = arrival_time, y = n, fill = price_category)) +
  geom_bar(stat = "identity") +
  ylim(0, 3000) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribution of Arrival Time by Price Category",
    x = "Arrival Time",
    y = "Count",
    fill = "Price Category"
  )

# Correlation 5: Price vs stops
ggplot(data_v3, aes(x=stops, y=price, fill=stops))+
  geom_boxplot()+
  labs(title="Ticket Price Distribution by Number of stops") +
  theme_minimal()



# heatmap price_category vs stops
data_v3 %>% count(price_category, stops) %>% group_by(stops) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = stops, y = price_category)) +
  geom_tile(mapping = aes(fill = prop))

# Based on our observations from boxplot and heatmap we want to check if our hypothesis of direct flights are cheaper is confirmed
data_v2$stops <- factor(data_v2$stops)

# Linear Regression on log price
m_stops <- lm(log_price ~ stops + airline + days_left + duration, data = data_v2)

# Extract coefficients of the airlines and chooses AirIndia as alphabetically
coef_airlines <- coef(m_stops)[grep("airline", names(coef(m_stops)))]

print(coef_airlines)

# Convert it in the percentage to be more interpretable
percent_effect <- (exp(coef_airlines) - 1) * 100
round(percent_effect, 2)

coef(m_stops)["stopszero"] |> exp() - 1  #direct flights are 43% cheaper than one stop flights
coef(m_stops)["stopstwo_or_more"] |> exp() - 1  #two or more stops are 9% expensive than one stop flights


# Correlation 6: Price vs days_left using also method loess 
ggplot(data_v2, aes(x = days_left, y = log_price)) +
  geom_point(alpha = 0.3, color = "grey30") +
  geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth = 1) +
  labs(title = "Price vs Days Left Before Departure",
       x = "Days Left to Departure",
       y = "Ticket Price") +
  theme_minimal()

cor(data_v2$days_left, data_v3$log_price, method = "spearman") #-0.5051195 inverse relationship

table(data_v3$price_category, data_v3$days_left)

round(prop.table(table(data_v3$price_category, data_v3$days_left), margin = 2) * 100,2)

chisq.test(table(data_v3$price_category, data_v3$days_left)) #X-squared = 4228.5, df = 144, p-value < 2.2e-16

#Comments for correlation 5: As we can see from the scatter plot using the method loess there is an inverse relationship between price and days left, as the days are close to zero the price is high.

# Correlation between price,stops and duration
ggplot(data_v2, aes(x = duration, y = log_price, color = stops)) +
  geom_point(alpha = 0.5)+labs(title = "Price vs Stops vs Duration")


# Correlation 7: Airlines,Average price and Departure time,Arrival time

data %>%
  group_by(airline, departure_time,arrival_time) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = airline, y = avg_price, fill = departure_time)) +
  geom_col(position = "dodge", color = "white", width = 0.8) +
  facet_wrap(~arrival_time, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c(
    "#4E79A7", # blue
    "#F28E2B", # orange
    "#E15759", # red
    "#76B7B2", # teal
    "#59A14F", # green
    "#EDC948"  # yellow
  )) +
  labs(title = "Average Flight Price by Airline,Departure Time and Arrival time",
       x = "Airline", y = "Average Price",fill="Departure time")+
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "gray20"),
    strip.text = element_text(face = "bold", size = 12, color = "#084594"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    panel.spacing = unit(0.8, "lines")
  )












