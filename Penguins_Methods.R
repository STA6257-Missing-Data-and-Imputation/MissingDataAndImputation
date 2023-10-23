library(tidyverse)
library(palmerpenguins) #necessary to pull in penguins dataset
data("penguins")


# One of the methodology mentioned before is the removal of any rows that have missing values. Using the code below we can filter down to see the rows 
# within the penguin dataset that show any NA values in any of the columns. 

# Filtering to rows with NA's
penguin_rows_with_na <- penguins[!complete.cases(penguins)]
print(penguin_rows_with_na)


# In this case, the dataset only has 11 rows with NA values.
11/344
# Once those rows are removed, we retain 333 out of 344 rows. We only lose 0.03% of the entire dataset. 
# This may not be an option for all datasts. As discussed previously removing the rows can lead to a significantly smaller dataset and may create bias.

# Penguins dataset filtered out all NA values
penguins_filtered <- na.omit(penguins)
print(penguins_filtered)

# Another way to handle the missing values is to replace the values with the mean. 
# We have created a modified dataset that removes 50% of the random data in column_flipper_length_mm.
penguins_missing <- delete_MCAR(penguins,0.5,"flipper_length_mm")

# Make a copy of the dataset and calculate the mean of the specific column
penguins_Mean_imputated <- penguins_missing


# If we only replaced the missing values for bill_length_mm.
bill_length_mm_mean_value <- mean(penguins$bill_length_mm, na.rm = TRUE)
penguins_Mean_imputated$bill_length_mm[is.na(penguins_Mean_imputated$bill_length_mm)] <- bill_length_mm_mean_value
penguins_Mean_imputated[!complete.cases(penguins)]
penguins_Mean_imputated[!complete.cases(penguins)]

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_Mean_imputated, na.action=na.omit)
print(paste(summary(regg)$adj.r.squared, "is the R-Squared for the linear model where missing values are excluded from the dataset."))

# When just comparing the the actual data to the data that has values missing at random for bill_length_mm the r-square drops from 0.7569 to 0.7414.

#But what happens when you replace all NA value?

bill_length_mm_mean_value <- mean(penguins$bill_length_mm, na.rm = TRUE)
bill_depth_mm_mean_value <- mean(penguins$bill_depth_mm, na.rm = TRUE)
flipper_length_mm_mean_value <- mean(penguins$flipper_length_mm, na.rm = TRUE)
body_mass_g_mean_value <- mean(penguins$body_mass_g, na.rm = TRUE)

# Replace NAs in the specific column with the mean
penguins_Mean_imputated$bill_length_mm[is.na(penguins_Mean_imputated$bill_length_mm)] <- bill_length_mm_mean_value
penguins_Mean_imputated$bill_depth_mm[is.na(penguins_Mean_imputated$bill_depth_mm)] <- bill_depth_mm_mean_value
penguins_Mean_imputated$flipper_length_mm[is.na(penguins_Mean_imputated$flipper_length_mm)] <- flipper_length_mm_mean_value
penguins_Mean_imputated$body_mass_g[is.na(penguins_Mean_imputated$body_mass_g)] <- body_mass_g_mean_value

penguins_Mean_imputated[!complete.cases(penguins)]


# For all columns with numeric values we can simply calculate the mean, median, or mode and replace the NA values. However, the sex column is composed
# of a boolean value which is Female or Male. In this case, we can either replace the NA's values with unknown's and create a third option for this column.
# We could also make all NA values either Female or Male. Lastly, we could randomly assign female or male to the missing values.

sex_values <- c("female", "male")

# Replace NAs in the 'sex' column with random values
penguins_Mean_imputated$sex[is.na(penguins_Mean_imputated$sex)] <- sample(sex_values, size = sum(is.na(penguins_Mean_imputated$sex)), replace = TRUE)

penguins_Mean_imputated[!complete.cases(penguins), ]

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_Mean_imputated, na.action=na.omit)
print(paste(summary(regg)$adj.r.squared, "is the R-Squared for the linear model where missing values are excluded from the dataset."))

#Using the mean to replace all missing values resulted in a 0.3461 R-squared showing the difference resulting in actual data collected compared to data
#that was created. 


