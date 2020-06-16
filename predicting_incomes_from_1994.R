# Justin Farnsworth
# Predicting 1994 Incomes
# June 8, 2020

# NOTE: It will take about 45 minutes to run all the code.


# LOADING THE DATA
####################################################################################################

# Required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")


# Create a temporary file and load the dataset into it
# NOTE: The CSV is already on this project's GitHub repo.
# Original Source: https://www.kaggle.com/uciml/adult-census-income/data
datafile = tempfile()
download.file("https://raw.github.com/farnswj1/Predicting_Incomes_From_1994/master/adult.csv", datafile)

# Read the data from the file
data <- read.csv(datafile)

# Delete the temporary file
rm(datafile)


# EXPLORING THE DATASET - OVERVIEW
####################################################################################################

# Count the number of rows and columns
dim(data)

# Show the first 10 rows of the dataset
head(data, 10)

# Check if any values in the table are null
any(is.na(data))

# Show column names and their datatypes
data.frame(
  column_names = colnames(data),
  data_type = map_chr(colnames(data), function(colname) {class(data[,colname])})
)


# EXPLORING THE DATASET - AGE
####################################################################################################

# Find the range of age values in the dataset
range(data$age)

# Calculate the number of people and 
# the percentage of people who made >$50k for each age
data_age_groups <- data %>% 
  group_by(age) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) 

# Plot the number of people in the dataset by age.
data_age_groups %>% 
  ggplot(aes(age, total)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Number of Adults by Age") + 
  xlab("Age") + 
  ylab("Total") + 
  scale_x_continuous(labels = seq(20, 90, 10), breaks = seq(20, 90, 10)) + 
  scale_y_continuous(labels = seq(0, 1000, 200), breaks = seq(0, 1000, 200))

# Plot the percentage of people what made over $50k by age
data_age_groups %>%
  ggplot(aes(age, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Age") + 
  xlab("Age") + 
  ylab("Percentage") +
  scale_x_continuous(labels = seq(20, 90, 10), breaks = seq(20, 90, 10))

# Show the number of adults in the dataset that are over 75 by age
data_age_groups %>% 
  group_by(age) %>% 
  filter(age > 75) %>% 
  select(total)


# EXPLORING THE DATASET - WORK CLASS
####################################################################################################

# Show the different types of work classes
unique(data$workclass)

# Show the percentages and total number of people
data_work_classes <- data %>% 
  group_by(workclass) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))
data_work_classes

# Plot the total number of people from each work class
data_work_classes %>% 
  mutate(workclass = reorder(workclass, total)) %>% 
  ggplot(aes(workclass, total)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Number of People by Work Class") + 
  xlab("Work Class") + 
  ylab("Total") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot the percentage of people what made over $50k by work class
data_work_classes %>% 
  mutate(workclass = reorder(workclass, percentage)) %>% 
  ggplot(aes(workclass, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Work Class") +  
  xlab("Work Class") + 
  ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels = seq(0, 60, 10), breaks = seq(0, 60, 10))


# EXPLORING THE DATASET - EDUCATION
####################################################################################################

# Show the different levels of education along with the totals and percentages
data_education <- data %>% 
  select(education, education.num, income) %>% 
  group_by(education.num, education) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(education.num)) %>% 
  ungroup()
data_education

# Plot the number of people for each level of education
data_education %>% 
  mutate(education = reorder(education, education.num)) %>% 
  ggplot(aes(education, total)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Number of People by Level of Education") + 
  xlab("Level of Education") + 
  ylab("Total") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot the percentage of people what made over $50k by level of education
data_education %>% 
  mutate(education = reorder(education, education.num)) %>% 
  ggplot(aes(education, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Work Class") +  
  xlab("Level of Education") + 
  ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels = seq(0, 80, 10), breaks = seq(0, 80, 10))


# EXPLORING THE DATASET - MARITAL & RELATIONSHIP STATUS
####################################################################################################

# Show the total number of people and the percentage of 
# people that made over $50k by marital status
data %>% 
  group_by(marital.status) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))

# Show the total number of people and the percentage of 
# people that made over $50k by relationship status
data %>% 
  group_by(relationship) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))


# EXPLORING THE DATASET - OCCUPTAION
####################################################################################################

# Show the number of people in each type of occupation along with the 
# percentage of people who made over $50k for each occupation type.
data_occupations <- data %>% 
  group_by(occupation) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))
data_occupations

# Plot the percentage of people what made over $50k by level of education
data_occupations %>% 
  mutate(occupation = reorder(occupation, percentage)) %>% 
  ggplot(aes(occupation, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Occupational Type") +  
  xlab("Occupational Type") + 
  ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels = seq(0, 80, 10), breaks = seq(0, 80, 10))


# EXPLORING THE DATASET - RACE & SEX
####################################################################################################

# Show the totals and percentages for each racial group
data_races <- data %>% 
  group_by(race) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))
data_races

# Show the totals and percentages for males and females
data_sexes <- data %>% 
  group_by(sex) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage))
data_sexes

# Show the totals and percentages by race and sex together
data_races_and_sexes <- data %>% 
  group_by(race, sex) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(percentage)) %>% 
  ungroup()
data_races_and_sexes

# Plot the percentages by race and sex
data_races_and_sexes %>% 
  mutate(race = reorder(race, percentage)) %>% 
  ggplot(aes(race, percentage, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Race and Sex") +  
  xlab("Race") + 
  ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# EXPLORING THE DATASET - CAPITAL
####################################################################################################

# Show the totals and percentages by net capital gains (rounded to the nearest 1000)
data_net_capital_gains <- data %>%
  mutate(net_capital_gain = round((capital.gain - capital.loss) / 1000) * 1000) %>% 
  group_by(net_capital_gain) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(net_capital_gain))
data_net_capital_gains  %>% print(n = Inf)

# Plot the percentages by net capital gain
data_net_capital_gains %>% 
  filter(net_capital_gain <= 50000) %>%
  ggplot(aes(net_capital_gain, percentage)) + 
  geom_bar(stat = "identity") + 
  geom_point() + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Net Capital Gains") +  
  xlab("Net Capital Gain") + 
  ylab("Percentage")


# EXPLORING THE DATASET - HOURS PER WEEK
####################################################################################################

# Plot the percentage of people who made over $50k by weekly hours
data %>% 
  group_by(hours.per.week) %>% 
  summarize(percentage = mean(income == ">50K") * 100) %>% 
  ggplot(aes(hours.per.week, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Weekly Hours") +  
  xlab("Hours Per Week") + 
  ylab("Percentage") + 
  scale_x_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10))


# EXPLORING THE DATASET - NATIVE COUNTRY
####################################################################################################

# Show total and percentages by native country
data_native_countries <- data %>% 
  group_by(native.country) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100) %>% 
  arrange(desc(total))
data_native_countries %>% print(n = Inf)

# Plot the percentages by country
data_native_countries %>% 
  mutate(native.country = reorder(native.country, percentage)) %>% 
  ggplot(aes(native.country, percentage)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Native Country") +  
  xlab("Country") + 
  ylab("Percentage") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  gghighlight(native.country == "United-States")

# Show the native countries with the least amount of adults in the dataset
data_native_countries %>% 
  arrange(total) %>% 
  head(10)

# Show the totals and percentages based on whether the adult is born in the US
data_us_born <- data %>% 
  mutate(
    is_US_born = factor(
      ifelse(native.country == "United-States", "Born in the US", "Not Born in the US")
    )
  ) %>% 
  group_by(is_US_born) %>% 
  summarize(total = n(), percentage = mean(income == ">50K") * 100)
data_us_born

# Plot the percentages based on whether the adult is born in the US
data_us_born %>% 
  ggplot(aes(is_US_born, percentage, fill = is_US_born)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Percentage of Adults That Made Over $50,000 by Native Status") +  
  xlab("Country") + 
  ylab("Percentage") + 
  labs(fill = "Native Status")


# EXPLORING THE DATASET - FINAL WEIGHT
####################################################################################################

# Calculate the mean, standard error, and total number of the final weights by income
data_final_weights <- data %>% 
  group_by(income) %>% 
  summarize(total = n(), 
            proportion = n()/nrow(data), 
            avg = mean(fnlwgt), 
            se = sd(fnlwgt)/sqrt(n()), 
            conf_low = avg - 2 * se, 
            conf_high = avg + 2 * se)
data_final_weights

# Plot the mean and confidence intervals of the final weights by income
data_final_weights %>% 
  ggplot(aes(income, avg, ymin = avg - 2 * se, ymax = avg + 2 * se)) + 
  geom_point() +
  geom_errorbar() + 
  ggtitle("Disribution of Final Weights By Income Classification") +  
  xlab("Income") + 
  ylab("Final Weight")


# MODELS - PREPARING THE DATASET
####################################################################################################

# Convert the columns to numerical values instead of factors.
# Then remove the columns that won't be used for the models
data <- data %>% 
  mutate(workclass = as.numeric(workclass), 
         fnlwgt = as.numeric(fnlwgt), 
         education = as.numeric(education), 
         marital.status = as.numeric(marital.status),  
         occupation = as.numeric(occupation), 
         relationship = as.numeric(relationship), 
         race = as.numeric(race), 
         sex = as.numeric(sex), 
         net_capital_gain = as.numeric(capital.gain - capital.loss), 
         hours.per.week = as.numeric(hours.per.week), 
         native.country = as.numeric((native.country))
  ) %>% 
  select(-c(education.num, capital.gain, capital.loss))
  
  
# MODELS - TRAINING AND TEST SETS
####################################################################################################

# Split the data into a training set (80%) and a test set (20%)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(data$income, times = 1, p = 0.2, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]
rm(test_index)

# Show the proportion of incomes less than or equal to $50k in the training set
mean(train_set$income == "<=50K")

# Show the proportion of incomes less than or equal to $50k in the test set
mean(test_set$income == "<=50K")


# MODELS - LOGISTIC REGRESSION
####################################################################################################

# Train the model
set.seed(1, sample.kind = "Rounding")
train_glm <- train(income ~ ., 
                   method = "glm", 
                   data = train_set)

# Make the predictions
y_hat_glm <- predict(train_glm, test_set)

# Determine accuracy of the model
results_glm <- confusionMatrix(data = y_hat_glm, reference = test_set$income)
results_glm


# MODELS - QUADRATIC DISCRIMINANT ANALSIS (QDA)
####################################################################################################

# Train the model
set.seed(1, sample.kind = "Rounding")
train_qda <- train(income ~ ., 
                   method = "qda", 
                   data = train_set)

# Make the predictions
y_hat_qda <- predict(train_qda, test_set)

# Determine accuracy of the model
results_qda <- confusionMatrix(data = y_hat_qda, reference = test_set$income)
results_qda


# MODELS - LOCAL REGRESSION (LOESS)
####################################################################################################

# Train the model
set.seed(1, sample.kind = "Rounding")
train_loess <- train(income ~ ., 
                     method = "gamLoess", 
                     data = train_set)

# Make the predictions
y_hat_loess <- predict(train_loess, test_set)

# Determine accuracy of the model
results_loess <- confusionMatrix(data = y_hat_loess, reference = test_set$income)
results_loess


# MODELS - CLASSIFICATION TREE
####################################################################################################

# Train the model
set.seed(1, sample.kind = "Rounding")
train_ct <- train(income ~ ., 
                  method = "rpart", 
                  data = train_set, 
                  tuneGrid = data.frame(cp = seq(0, 0.01, 0.001)))

# Make the predictions
y_hat_ct <- predict(train_ct, test_set)

# Determine accuracy of the model
results_ct <- confusionMatrix(data = y_hat_ct, reference = test_set$income)
results_ct
 
# Plot the model's accuracy for each complexity parameter
plot(train_ct, main = "Classification Tree Results")

# Show the most optimal paramater value
train_ct$bestTune


# MODELS - RANDOM FOREST
####################################################################################################

# Train the model
# NOTE: This will take roughly 40 minutes to complete
set.seed(1, sample.kind = "Rounding")
train_rf <- train(income ~ ., 
                  method = "rf", 
                  data = train_set, 
                  ntree = 200,
                  tuneGrid = data.frame(mtry = 1:5), 
                  importance = TRUE)

# Make the predictions
y_hat_rf <- predict(train_rf, test_set)

# Determine accuracy of the model
results_rf <- confusionMatrix(data = y_hat_rf, reference = test_set$income)
results_rf

# Show the most important variables in the model
varImp(train_rf)

# Plot the model and the accuracies for each predictor
plot(train_rf, 
     main = "Random Forest Results", 
     xlab = "# of Randomly Selected Predictors"
)

# Show the most optimal paramater value
train_rf$bestTune

 
# MODELS - K-MEANS CLUSTERING
####################################################################################################

# Train the model
set.seed(1, sample.kind = "Rounding")
train_kmeans <- kmeans(select(train_set, -income), centers = 3)

# Prediction function for the k-means clustering model
# Assigns each row to a cluster from k_means
predict_kmeans <- function(predictors, k_means) {
  # Get cluster centers
  centers <- k_means$centers
  
  # Calculate the distance from the cluster centers
  distances <- sapply(1:nrow(predictors), function(i) {
    apply(centers, 1, function(y) dist(rbind(predictors[i,], y)))
  })
  
  # Select the cluster that is closest to the center
  max.col(-t(distances))
}

# Make the predictions
y_hat_kmeans <- factor(ifelse(predict_kmeans(select(test_set, -income), train_kmeans) == 2, ">50K", "<=50K"))

# Determine accuracy of the model
results_kmeans <- confusionMatrix(data = y_hat_kmeans, reference = test_set$income)
results_kmeans


# MODELS - ENSEMBLE
####################################################################################################

# Create the ensemble
ensemble <- data.frame(glm = y_hat_glm,
                       qda = y_hat_qda,
                       loess = y_hat_loess,
                       ct = y_hat_ct,
                       kmeans = y_hat_rf)

# Make the predictions
y_hat_ensemble <- factor(ifelse(rowMeans(ensemble == ">50K") > 0.5, ">50K", "<=50K"))

# Determine accuracy of the model
results_ensemble <- confusionMatrix(data = y_hat_ensemble, reference = test_set$income)
results_ensemble


# RESULTS
####################################################################################################

# Save the model names
models = c(
  "Logistic Regression", 
  "QDA", 
  "Loess", 
  "Classification Tree", 
  "Random Forest", 
  "K-Means Clustering", 
  "Ensemble"
)

# Save the model accuracies
accuracies = c( 
  mean(test_set$income == y_hat_glm), 
  mean(test_set$income == y_hat_qda), 
  mean(test_set$income == y_hat_loess), 
  mean(test_set$income == y_hat_ct), 
  mean(test_set$income == y_hat_rf), 
  mean(test_set$income == y_hat_kmeans), 
  mean(test_set$income == y_hat_ensemble)
)

# Save the model sensitivities
sensitivities = c(
  sensitivity(data = y_hat_glm, reference = test_set$income), 
  sensitivity(data = y_hat_qda, reference = test_set$income), 
  sensitivity(data = y_hat_loess, reference = test_set$income), 
  sensitivity(data = y_hat_ct, reference = test_set$income), 
  sensitivity(data = y_hat_rf, reference = test_set$income), 
  sensitivity(data = y_hat_kmeans, reference = test_set$income), 
  sensitivity(data = y_hat_ensemble, reference = test_set$income)
) 

# Save the model specificities
specificities = c(
  specificity(data = y_hat_glm, reference = test_set$income), 
  specificity(data = y_hat_qda, reference = test_set$income), 
  specificity(data = y_hat_loess, reference = test_set$income), 
  specificity(data = y_hat_ct, reference = test_set$income), 
  specificity(data = y_hat_rf, reference = test_set$income), 
  specificity(data = y_hat_kmeans, reference = test_set$income), 
  specificity(data = y_hat_ensemble, reference = test_set$income)
) 

# Save the model precision
precisions = c(
  precision(data = y_hat_glm, reference = test_set$income), 
  precision(data = y_hat_qda, reference = test_set$income), 
  precision(data = y_hat_loess, reference = test_set$income), 
  precision(data = y_hat_ct, reference = test_set$income), 
  precision(data = y_hat_rf, reference = test_set$income), 
  precision(data = y_hat_kmeans, reference = test_set$income), 
  precision(data = y_hat_ensemble, reference = test_set$income)
) 

# Save the model F1 scores
F1s = c(
  F_meas(data = y_hat_glm, reference = test_set$income), 
  F_meas(data = y_hat_qda, reference = test_set$income), 
  F_meas(data = y_hat_loess, reference = test_set$income), 
  F_meas(data = y_hat_ct, reference = test_set$income), 
  F_meas(data = y_hat_rf, reference = test_set$income), 
  F_meas(data = y_hat_kmeans, reference = test_set$income), 
  F_meas(data = y_hat_ensemble, reference = test_set$income)
) 

# Combine the results into a data frame, then display them
results <- data.frame(
  Model = models, 
  Accuracy = accuracies, 
  Sensitivity = sensitivities, 
  Specificity = specificities, 
  Precision = precisions, 
  F1 = F1s
  )
results

# Plot the accuracies of each model
results %>% 
  mutate(Model = reorder(Model, Accuracy)) %>%
  ggplot(aes(Model, Accuracy)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Model Results vs. Baseline Model (Green Line)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(test_set$income == "<=50K"), color = "green")

# Plot the specificities of each model
results %>% 
  mutate(Model = reorder(Model, Specificity)) %>%
  ggplot(aes(Model, Specificity)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Specificity of the Models") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
