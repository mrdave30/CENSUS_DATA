require(tidyverse)
require(readxl)

# This function takes three arguments: d_i, n_i, and c_i. 
# d_i is a vector of land size classes in hectares, 
# n_i is a vector of the number of farms in each class, and 
# c_i is a vector of the desired interpolation points.
# The function returns a data frame with three columns: 
#the interpolated land size classes, 
#the interpolated number of farms, and 
#the interval number corresponding to each c_i value.

interpolation <- function(d_i, n_i, c_i) {
  # First, we calculate the mean and standard deviation of the log-transformed d_i values, excluding the first element (which is zero).
  mean_val <- mean(log(d_i[-1], 2.718))
  stdev_val <- sd(log(d_i[-1], 2.718) * sqrt((length(d_i) - 2) / (length(d_i) - 1)))
  
  # Next, we combine the d_i and c_i vectors and sort them in ascending order.
  d_i_c_i <- sort(unique(c(d_i, c_i)))
  
  # We initialize a vector to store the interpolated number of farms for each land size class.
  n_int <- rep(0, length(d_i_c_i) - 1)
  
  # We loop over the original land size classes and interpolate the number of farms using the lognormal distribution function.
  j <- 1
  for (int in 1:(length(d_i) - 1)) {
    lim_inf <- d_i[int]
    lim_sup <- d_i[int + 1]
    # We count how many interpolation points are within each original land size class.
    number_in <- sum(c_i < lim_sup & c_i > lim_inf)
    # We create a vector of the lower and upper limits of each original land size class and the interpolation points within it.
    limits <- c(lim_inf, c_i[which(c_i < lim_sup & c_i > lim_inf)], lim_sup)
    # We loop over the subintervals and calculate the interpolated number of farms using the lognormal probability function.
    for (k in 1:(length(limits) - 1)) {
      n_int[j] <- (plnorm(limits[k + 1], mean_val, stdev_val) - plnorm(limits[k], mean_val, stdev_val)) /
        (plnorm(lim_sup, mean_val, stdev_val) - plnorm(lim_inf, mean_val, stdev_val)) * n_i[int]
      j <- j + 1
    }
  }
  
  # We create a data frame with the interpolated land size classes and the interpolated number of farms.
  total <- data.frame(d_i_c_i[-1], n_int)
  
  # We initialize a vector to store the interval number corresponding to each c_i value.
  interval <- rep(0, length(d_i_c_i) - 1)
  
  # We loop over the interpolation points and assign them to the appropriate interval number based on their position in the d_i_c_i vector.
  for (new_int in 1:length(c_i)) {
    temp <- c_i[new_int]
    for (i in 1:length(interval)) {
      if (interval[i] == 0 & d_i_c_i[i + 1] <= temp) interval[i] = new_int
    }
  }
  
  # We add the interval column to the data frame and return it.
  total=data.frame(d_i_c_i[-1],n_int,interval)
  test <- data.frame(tapply(total$n_int,total$interval,sum))
  
  # If 'c_i' has an extra element, remove it
  if(length(c_i) > nrow(test)) {
    c_i <- c_i[-length(c_i)]
  }
  
  # If 'c_i' is shorter, pad it with NA
  if(length(c_i) < nrow(test)) {
    diff_len <- nrow(test) - length(c_i)
    c_i <- c(c_i, rep(NA, diff_len))
  }
  
  # Bind 'test' dataframe and 'c_i' vector, replacing missing rows with NA
  total2 <- bind_cols(test, c_i = c_i) %>%
    mutate(status=if_else(tapply.total.n_int..total.interval..sum. %in% n_i, "", "Interpolated" ))
   
}

# This is an example of how to use the function with some sample data.

d_i <- c(0,2,5,10,19,30, 50,100,200) 
n_i <- c(7300,15210,18830,25480,15190,16080,9760,2400)
c_i <- c(1,2,3,4,5,10,20,50,100,200,500,1000,2500,5000)

result <- interpolation(d_i,n_i,c_i)

print(result)


# This code is a possible solution to apply the interpolation function to several countries' land size classes in a dataframe
# It uses the tidyverse and readxl packages to manipulate and read data
# It assumes that the data is stored in an Excel file with one sheet per country
# It also assumes that the interpolation function is defined elsewhere and takes three arguments: d_i, n_i, and c_i

# Read the data from the Excel file
data <- read_excel("tes.xlsx", sheet = NULL)


# Initialize an empty dataframe to store the results
all_countries_result <- data.frame()

# Loop through each unique country in the data
for (country in unique(data$sheet)) {
  # Print a message to indicate the progress
  print(paste("Processing country:", country))
  
  # Subset the data for the current country
  country_data <- data %>% filter(sheet == country)
  
  # Print a few rows of the subset data for inspection
  print(head(country_data))
  
  # Extract the columns for d_i and n_i from the subset data
  # These are assumed to be the columns named 'upper' and 'numeric' in the data
  # Adjust as needed if the column names are different
  d_i <- country_data$upper
  n_i <- country_data$numeric
  
  # Apply the interpolation function to the subset data
  # The third argument c_i is assumed to be a constant value for all countries
  # Adjust as needed if c_i varies by country or is not needed
  interpolated_data <- interpolation(d_i, n_i, c_i)
  
  # Print a few rows of the interpolated data for inspection
  print(head(interpolated_data))
  
  # Check if the interpolation function returned a non-empty dataframe
  if (nrow(interpolated_data) > 0) {
    # Add a column for the country name to the interpolated data
    interpolated_data$sheet <- country
    
    # Combine the interpolated data with the overall results
    all_countries_result <- rbind(all_countries_result, interpolated_data)
  } else {
    # Print a message to indicate that the interpolation function failed for the current country
    print(paste("Interpolation returned an empty dataframe for country:", country))
  }
}

# Print the final combined dataframe with all countries' results
print(all_countries_result)

# View the final combined dataframe in a spreadsheet-like format
view(all_countries_result)
