library(palmerpenguins)
library(caret)

df = palmerpenguins::penguins

gentoo_male = subset(df, species =='Gentoo' & sex =='male')

gentoo_selected = gentoo_male[, c("body_mass_g", "flipper_length_mm")]

gentoo_cleaned = na.omit(gentoo_selected)

plot(x = gentoo_cleaned$body_mass_g,
     y = gentoo_cleaned$flipper_length_mm,
     xlab = 'Body Mass g',
     ylab = 'Flipper Length mm',
     col = 'red')

######################### No Intercept Model ###################
model_no_intercept = lm(flipper_length_mm ~ body_mass_g -1,
                        data = gentoo_cleaned)

abline(model_no_intercept)

summary(model_no_intercept)

######################### With Intercept Model ###################
model = lm(flipper_length_mm ~ body_mass_g,
                        data = gentoo_cleaned)

abline(model, col='blue')

summary(model)

###################### More Realistic (split) ##################

df = palmerpenguins::penguins

gentoo = subset(df, species == 'Gentoo'& sex == 'male')

gentoo_selected <- gentoo[, c("body_mass_g", "flipper_length_mm")]

gentoo_cleaned = na.omit(gentoo_selected)

plot(x = gentoo_cleaned$body_mass_g,
     y = gentoo_cleaned$flipper_length_mm,
     xlab = 'Body Mass g',
     ylab = 'Flipper Length mm',
     col = 'red')

set.seed(31415) 
trainIndex <- createDataPartition(gentoo_cleaned$body_mass_g, p = .8, 
                                  list = FALSE, 
                                  times = 1)
gentoo_train <- gentoo_cleaned[trainIndex, ]
gentoo_test <- gentoo_cleaned[-trainIndex, ]

model <- lm(flipper_length_mm ~ body_mass_g, data = gentoo_train)
abline(model, col='blue')

predictions <- predict(model, newdata = gentoo_test)

results <- data.frame(Actual = gentoo_test$flipper_length_mm,
                      Predicted = predictions)

ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = "Predictions vs Actual Values",
       x = "Actual Flipper Length(mm)",
       y = "Predicted Flipper Length(mm)") +
  theme_minimal()
