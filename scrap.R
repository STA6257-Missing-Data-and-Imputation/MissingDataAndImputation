library(readr)
#library(lava)
library(missMethods)
library(simputation)
library(ggplot2)
set.seed(1) 


penguins <- readr::read_csv('penguins.csv')

summary(penguins)

penguins_missing <- delete_MCAR(penguins,0.5,"flipper_length_mm")
penguins_missing_all <- delete_MCAR(penguins,0.5,c("flipper_length_mm","bill_length_mm","bill_depth_mm","body_mass_g","species","sex"))
tryCatch(
  {
    penguins_imputed_missing <- impute_lm(penguins_missing_all, flipper_length_mm ~ species + bill_length_mm + bill_depth_mm)
    
    penguins_imputed_missing[!complete.cases(penguins_imputed_missing), ]
    
    regg = lm(body_mass_g~ flipper_length_mm, data = penguins_missing_all,na.action = na.fail())
    print(paste(summary(regg)$adj.r.squared, "is the R-Squared for the linear model where imputation of missing values was performed with another linear model before modeling."))
  },
  error = function(e) 
  {
    print("This caused an error!") # or whatever error handling code you want
  }
)


ggplot(penguins, aes(body_mass_g, flipper_length_mm)) + 
  geom_count() + 
  geom_smooth(method='lm')

regg = lm(body_mass_g~ flipper_length_mm, data = penguins, na.action=na.omit) 
summary(regg)

penguins_imputed <- impute_lm(penguins_missing, flipper_length_mm ~ species + bill_length_mm + bill_depth_mm)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_imputed)
summary(regg)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_missing, na.action=na.omit) 
summary(regg)

