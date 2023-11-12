library(tidyverse)
library(palmerpenguins) #necessary to pull in penguins dataset
data("penguins")

library(readr)
library(missMethods)
library(simputation)
library(ggplot2)


# One of the methodology mentioned before is the removal of any rows that have missing values. Using the code below we can filter down to see the rows 
# within the penguin dataset that show any NA values in any of the columns. 

# Filtering to rows with NA's
penguin_rows_with_na <- penguins[!complete.cases(penguins), ]
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


penguins_missing <- delete_MCAR(penguins,0.5,"flipper_length_mm")

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_missing, na.action=na.omit) 
print(paste(summary(regg)$adj.r.squared, "is the R-Squared for the linear model where missing values are excluded from the dataset."))



# Make a copy of the dataset and calculate the mean of the specific column
penguins_Mean_imputated <- penguins_missing


# If we only replaced the missing values for bill_length_mm.
flipper_length_mm_mean_value <- mean(penguins$flipper_length_mm, na.rm = TRUE)
penguins_Mean_imputated$flipper_length_mm[is.na(penguins_Mean_imputated$flipper_length_mm)] <- flipper_length_mm_mean_value
penguins_Mean_imputated[!complete.cases(penguins), ]

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

penguins_Mean_imputated[!complete.cases(penguins), ]


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

count(penguins$year)
unique(penguins$island)
unique(penguins$year)

#Shows the first six rows of the data set
head(penguins)

#Shows all the variables in the data set 
str(penguins)

# Install the summarytools package (only need to do this once)
install.packages("summarytools")

# Load the summarytools package
library(summarytools)

# Generate a summary of the data
descr(penguins)

# install.packages("mice")
# library(mice)


install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)


# This bar creates the mean for each species from the original penguins data set and omitted two rows for having Na's
ggplot(penguins, aes(x = species, y = flipper_length_mm)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Average Flipper Length by Penguin Type", x = "Penguin Type", y = "Flipper Length (mm)") 


# Count table to see how the breakdown looks for every species. This table does include the NA values for each species
count_table <- penguins %>%
  group_by(species) %>%
  summarize(count = n(), mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))
print(count_table)

library(ggplot2)
library(dplyr)

# Bar chart to show the NA values for each species
penguins %>%
  mutate(species_category = ifelse(is.na(species), "NA", as.character(species)),
         flipper_length_category = ifelse(is.na(flipper_length_mm), "NA", "Not NA")) %>%
  ggplot(aes(x = species_category, fill = flipper_length_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Flipper Length by Penguin Type",
       x = "Penguin Type",
       fill = "Flipper Length Availability") +
  scale_fill_manual(values = c("red", "skyblue"), name = "NA Count")







# Duplicated for dataset with half the values removedThis bar creates the mean for each species from the original penguins data set and omitted two rows for having Na's
ggplot(penguins_Mean_imputated, aes(x = species, y = flipper_length_mm)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Average Flipper Length by Penguin Type", x = "Penguin Type", y = "Flipper Length (mm)") 

count_table <- penguins_Mean_imputated %>%
  group_by(species) %>%
  summarize(count = n(), mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))
print(count_table)

penguins_Mean_imputated %>%
  mutate(species_category = ifelse(is.na(species), "NA", as.character(species)),
         flipper_length_category = ifelse(is.na(flipper_length_mm), "NA", "Not NA")) %>%
  ggplot(aes(x = species_category, fill = flipper_length_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Flipper Length by Penguin Type",
       x = "Penguin Type",
       fill = "Flipper Length Availability") +
  scale_fill_manual(values = c("skyblue"), name = "NA Count")



# As we can see the mean flipper length has changed for every species but the count has not. This is an example of an
# issues that can arise with single imputation. The count looks exactly the same however, the only way to note any difference
# in the mean flipper length would be to have the original values for comparison. In single imputation we do not distinct
# the calculated values that replaced the NA values from the original values in the data set. 



penguins_missing <- delete_MCAR(penguins,0.5,c("flipper_length_mm","bill_length_mm","bill_depth_mm","body_mass_g","species","sex"))
bill_length_mm_mean_value <- mean(penguins$bill_length_mm, na.rm = TRUE)
bill_depth_mm_mean_value <- mean(penguins$bill_depth_mm, na.rm = TRUE)
flipper_length_mm_mean_value <- mean(penguins$flipper_length_mm, na.rm = TRUE)
body_mass_g_mean_value <- mean(penguins$body_mass_g, na.rm = TRUE)
species_mode_value <- mode(penguins$species)
sex_mode_value <- mode(penguins$sex)

# Replace NAs in the specific column with the mean
penguins_Mean_imputated$bill_length_mm[is.na(penguins_Mean_imputated$bill_length_mm)] <- bill_length_mm_mean_value
penguins_Mean_imputated$bill_depth_mm[is.na(penguins_Mean_imputated$bill_depth_mm)] <- bill_depth_mm_mean_value
penguins_Mean_imputated$flipper_length_mm[is.na(penguins_Mean_imputated$flipper_length_mm)] <- flipper_length_mm_mean_value
penguins_Mean_imputated$body_mass_g[is.na(penguins_Mean_imputated$body_mass_g)] <- body_mass_g_mean_value
penguins_Mean_imputated$species[is.na(penguins_Mean_imputated$species)] <- species_mode_value
penguins_Mean_imputated$sex[is.na(penguins_Mean_imputated$sex)] <- sex_mode_value

penguins_Mean_imputated[!complete.cases(penguins_Mean_imputated), ]

# Created a copy of the original dataset and appended the calculated imputated values in order to create comparisons
penguins_data_w_appendedcolumns<- penguins
penguins_data_w_appendedcolumns$imp_bill_length <- bill_length_mm_mean_value
penguins_data_w_appendedcolumns$imp_bill_depth <- bill_depth_mm_mean_value
penguins_data_w_appendedcolumns$imp_flipper_length <- flipper_length_mm_mean_value
penguins_data_w_appendedcolumns$imp_body_mass <- body_mass_g_mean_value


# Renaming columns to 'original_values' and 'calculated_values'
original_values <- penguins_data_w_appendedcolumns$bill_length_mm
calculated_values <- penguins_data_w_appendedcolumns$imp_bill_length

# Demonstrating box plot
box_data <- data.frame(
  Group = rep(c("Original", "Calculated"), each = length(original_values)),
  Value = c(original_values, calculated_values))

ggplot(box_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(color = "black", width = 0.5) +
  labs(title = "Comparison of Original and Calculated Bill Depth",
       x = "Group", y = "Bill Depth") +
  scale_fill_manual(values = c("skyblue", "orange"))


# Renaming columns to 'original_values' and 'calculated_values'
original_values <- penguins_data_w_appendedcolumns$body_mass_g
calculated_values <- penguins_data_w_appendedcolumns$imp_body_mass

# Demonstrating box plot
box_data <- data.frame(
  Group = rep(c("Original", "Calculated"), each = length(original_values)),
  Value = c(original_values, calculated_values))

ggplot(box_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(color = "black", width = 0.5) +
  labs(title = "Comparison of Original and Calculated Body Mass",
       x = "Group", y = "Body Mass (g)") +
  scale_fill_manual(values = c("skyblue", "orange"))

# This kind of graph can be used for any variable, however I chose two variables bill_length & body mass to demonstrate
# the variance we see when comparing the original values and the calculated values that primarily used the mean for 50% of the columns.

