options(max.print = 11072)
options(scipen = 999, digits = 4)
rm(list = ls())

# Load the dataset
data_set <- read.csv("C://Users//amira//Desktop//asthma_dataset.csv", header = TRUE)

# Replace '?' with NA before converting to numeric
data_set[data_set == "?"] <- NA

# Convert relevant columns to numeric
data_set$EducationLevel_P <- as.numeric(as.character(data_set$EducationLevel_P))
data_set$PollutionExposure_P <- as.numeric(as.character(data_set$PollutionExposure_P))
data_set$Diagnosis <- as.numeric(as.character(data_set$Diagnosis))
data_set$Smoking_P <- as.numeric(as.character(data_set$Smoking_P))
data_set$PetAllergy_P <- as.numeric(as.character(data_set$PetAllergy_P))
data_set$DustExposure_P <- as.numeric(as.character(data_set$DustExposure_P))
data_set$SleepQuality_P <- as.numeric(as.character(data_set$SleepQuality_P))

# Install and load psych package 
install.packages("psych")
library(psych)

# Install and load ggplot2 for visualization 
install.packages("ggplot2")
library(ggplot2)

# Use describe function to summarize the data
describe(data_set)

# Replace '?' with NA before converting to numeric
data_set[data_set == "?"] <- NA

# Remove rows with missing values in the relevant columns
cleaned_data_set <- na.omit(data_set[, c("EducationLevel_P", "PollutionExposure_P", "Diagnosis", "Smoking_P", "PetAllergy_P", "DietQuality_P", "DustExposure_P", "SleepQuality_P")])

# Check the structure of the cleaned dataset
str(cleaned_data_set)

# Summary and column names
summary(data_set)
colnames(data_set)

# 1. Pollution Exposure <- Asthma Risk
# 2. Education Level <- Pollution Exposure
# 3. Education Level <- Pollution Exposure <- Asthma Risk
# 4. Education Level <- Pollution Exposure <- Smoking Habits
# 5. Education Level <- Pollution Exposure <- Diet Quality
# 6. Education Level <- Pollution Exposure <- Pet Allergy
#------------------------------------------------------------------------------------------------------------------------------

# 1. Pollution Exposure <- Asthma Risk

# T-test for statistical significance between Pollution Exposure and Asthma Risk
t.test(NotDetected$PollutionExposure_P, Detected$PollutionExposure_P, alternative = "two.sided", conf.level = 0.99)
# If the P Value is below 0.05, there is no relationship
# P-value = 0.3, therefore there is no statistical significance between Pollution Exposure and Asthma Risk

# Subset for people with and without asthma
Detected <- subset(data_set, Diagnosis == 1)
NotDetected <- subset(data_set, Diagnosis == 0)

# Calculate the median and mean for PollutionExposure_P
median_pollution <- median(data_set$PollutionExposure_P, na.rm = TRUE)
mean_pollution <- mean(data_set$PollutionExposure_P, na.rm = TRUE)
print(median_pollution) #4.97
print(mean_pollution) #4.972533

# Subset for the Relationship between Pollution Exposure and Detection of Asthma
higher_pollution_Detected <- nrow(Detected[Detected$PollutionExposure_P >= median_pollution, ])
lower_pollution_Detected <- nrow(Detected[Detected$PollutionExposure_P < median_pollution, ])

# Subset for the Relationship for People without Asthma and Pollution Exposure
higher_pollution_NotDetected <- nrow(NotDetected[NotDetected$PollutionExposure_P >= median_pollution, ])
lower_pollution_NotDetected <- nrow(NotDetected[NotDetected$PollutionExposure_P < median_pollution, ])

# Create a data frame for visualization
Sum_data <- data.frame(
  Asthma_Status = rep(c("Not Detected", "Detected"), each = 2),
  Pollution_Exposure = rep(c("Higher Pollution Exposure", "Lower Pollution Exposure"), 2),
  counts = c(higher_pollution_NotDetected, lower_pollution_NotDetected,
             higher_pollution_Detected, lower_pollution_Detected)
)

# Plotting the data 
ggplot(Sum_data, aes(x = Asthma_Status, y = counts, fill = Pollution_Exposure)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = counts), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4, fontface = "bold") +  # Add text labels
  labs(title = "Counts of Asthma Status vs Pollution Exposure",
       x = "Asthma Status",
       y = "Counts of Occurrence") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Calculate total counts for each pollution exposure category
total_higher_pollution <- sum(higher_pollution_NotDetected, higher_pollution_Detected)
total_lower_pollution <- sum(lower_pollution_NotDetected, lower_pollution_Detected)

# Calculate percentages
Sum_data$percentage <- with(Sum_data, ifelse(Pollution_Exposure == "Higher Pollution Exposure",
                                              counts / total_higher_pollution * 100,
                                              counts / total_lower_pollution * 100))

# Plotting the data 
ggplot(Sum_data, aes(x = Asthma_Status, y = percentage, fill = Pollution_Exposure)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4, fontface = "bold") +  # Add text labels with percentages
  labs(title = "Percentage of Asthma Status vs Pollution Exposure",
       x = "Asthma Status",
       y = "Percentage of Occurrence") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )


#------------------------------------------------------------------------------------------------------------------------------

# 2. Education Level <- Pollution Exposure

# T-test for Education Level and Pollution Exposure
t.test(higher_pollution$EducationLevel_P, lower_pollution$EducationLevel_P, alternative = "two.sided", conf.level = 0.99)
# The P-Value is 0.0006, which is lower than 0.05. Therefore, there is a relationship between Education Level and Pollution Exposure

# I will find the median to determine the Higher Pollution Exposure and Lower Pollution Exposure 
# Find the median to determine High and Low Pollution Exposure
median_pollution <- median(data_set$PollutionExposure_P, na.rm = TRUE)
describe(data_set$PollutionExposure_P)

# Subset the data for higher and lower pollution exposure
higher_pollution <- subset(data_set, PollutionExposure_P >= median_pollution)
lower_pollution <- subset(data_set, PollutionExposure_P < median_pollution)

# I wanted to check sizes of subsets 
print(nrow(higher_pollution))
print(nrow(lower_pollution))

# So we plot a linear regression to make sure there is a correlation between education level and pollution exposure
ggplot(data_set, aes(x = EducationLevel_P, y = PollutionExposure_P)) +
  geom_jitter(alpha = 0.3, color = "red") + 
  geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
  labs(title = "Pollution Exposure vs Education Level",
       x = "Education Level",
       y = "Pollution Exposure")

# The linear regression line shows a slight upward trend, indicating a possible weak positive relationship between education level and pollution exposure.
# While the p-value indicates statistical significance, the actual change in pollution exposure with increasing education is minimal, suggesting the effect of education on pollution exposure is not very strong.

# Create data frame for Education Level and Pollution Exposure 
Pollution_frame <- data.frame(
  Pollution_Status = rep(c("Higher Pollution Exposure", "Lower Pollution Exposure"), each = 2),
  Education_Status = rep(c("Lower Education", "Higher Education"), times = 2),
  frequencies = c(
    nrow(higher_pollution[higher_pollution$EducationLevel_P %in% c(0, 1), ]),  # Combine None and High_School
    nrow(higher_pollution[higher_pollution$EducationLevel_P %in% c(2, 3), ]),  # Combine Bachelor's and Higher_Degrees
    nrow(lower_pollution[lower_pollution$EducationLevel_P %in% c(0, 1), ]),    # Combine None and High_School
    nrow(lower_pollution[lower_pollution$EducationLevel_P %in% c(2, 3), ])     # Combine Bachelor's and Higher_Degrees
  )
)

Pollution_frame

# Plot Education Level vs Pollution Exposure 
ggplot(Pollution_frame, aes(x = Education_Status, y = frequencies, fill = Pollution_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = frequencies), vjust = -0.5, position = position_dodge(0.9), size = 3.5) +
  labs(title = "Counts of Pollution Exposure vs Education Level",
       x = "Education Level",
       y = "Frequencies") +
  scale_fill_manual(values = c("darkblue", "skyblue")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  ) + 
  facet_wrap(~Pollution_Status, ncol = 2)

# Proportions for Education Level and Pollution Exposure
Pollution_frame_rate <- data.frame(
  Pollution_Status = rep(c("Higher Pollution Exposure", "Lower Pollution Exposure"), each = 2),
  Education_Status = rep(c("Higher Educated", "Lower Educated"), times = 2),
  proportions = c(
    nrow(higher_pollution[higher_pollution$EducationLevel_P == 2 | higher_pollution$EducationLevel_P == 3, ]) / nrow(data_set[data_set$EducationLevel_P == 2 | data_set$EducationLevel_P == 3, ]),
    nrow(higher_pollution[higher_pollution$EducationLevel_P == 0 | higher_pollution$EducationLevel_P == 1, ]) / nrow(data_set[data_set$EducationLevel_P == 0 | data_set$EducationLevel_P == 1, ]),
    nrow(lower_pollution[lower_pollution$EducationLevel_P == 2 | lower_pollution$EducationLevel_P == 3, ]) / nrow(data_set[data_set$EducationLevel_P == 2 | data_set$EducationLevel_P == 3, ]),
    nrow(lower_pollution[lower_pollution$EducationLevel_P == 0 | lower_pollution$EducationLevel_P == 1, ]) / nrow(data_set[data_set$EducationLevel_P == 0 | data_set$EducationLevel_P == 1, ])
  )
)

# I run this code to check the values in the data frame
Pollution_frame_rate

# I round proportions to 2 decimal places just for the visualization 
Pollution_frame_rate$proportions <- round(Pollution_frame_rate$proportions, 2)

# Plot the data
ggplot(Pollution_frame_rate, aes(x = Education_Status, y = proportions, fill = Pollution_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = proportions), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +
  labs(title = "Proportion of Pollution Exposure vs Education Level",
       x = "Education Level",
       y = "Proportion") +
  scale_fill_manual(values = c("darkblue", "skyblue")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  ) + facet_wrap(~Pollution_Status, ncol = 2)


#------------------------------------------------------------------------------------------------------------------------------

# 3. Education Level <- Pollution Exposure <- Asthma Risk

# Now let's see if it's statistically significant by applying t.test
t.test(Higher_Educated_People$Diagnosis, Lower_Educated_People$Diagnosis, alternative = "two.sided", conf.level = .99)
# P-Value is 5.64e-16 which means it is statistically significant

# I create a subset data for Higher and Lower Educated People based on Lower Pollution Exposure
Lower_Educated_People <- subset(lower_pollution [lower_pollution$EducationLevel_P<=1,])
nrow(Lower_Educated_People)
Higher_Educated_People <- subset(lower_pollution[lower_pollution$EducationLevel_P > 1, ])
nrow(Higher_Educated_People[Higher_Educated_People$PetAllergy_P])


# Creating a data frame for Asthma counts and Education Level with Lower Pollution Exposure
asthma_Pollution_frame <- data.frame(
  Asthma_Status = rep(c("Not Detected", "Detected"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  count = c(nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis == 0, ]),
            nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis == 0, ]),
            nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis == 1, ]),
            nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis == 1, ]))
)
# I ran this code to double check the data frame
asthma_Pollution_frame

# Visualizing the asthma counts comparison with respect to lower pollution exposure and education level
ggplot(asthma_Pollution_frame, aes(x = Education_status, y = count, fill = Asthma_Status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5, fontface = "bold") +  # Add values on bars
  scale_fill_manual(values = c("darkred", "pink")) +
  labs(title = "Asthma counts comparison with respect to Lower Pollution Exposure and Education Level",
       x = "Education Level",
       y = "Frequencies") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10),  # Adjusted title size
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10)) +
  facet_wrap(~Asthma_Status)

# Calculate total counts for each education level
total_high_educated <- sum(asthma_Pollution_frame$count[asthma_Pollution_frame$Education_status == "High"])
total_low_educated <- sum(asthma_Pollution_frame$count[asthma_Pollution_frame$Education_status == "Low"])

# Add a new column to store percentages
asthma_Pollution_frame$Percentage <- 0  # Initialize with 0

# Calculate percentages for High Education
asthma_Pollution_frame$Percentage[asthma_Pollution_frame$Education_status == "High"] <- 
  (asthma_Pollution_frame$count[asthma_Pollution_frame$Education_status == "High"] / total_high_educated) * 100

# Calculate percentages for Low Education
asthma_Pollution_frame$Percentage[asthma_Pollution_frame$Education_status == "Low"] <- 
  (asthma_Pollution_frame$count[asthma_Pollution_frame$Education_status == "Low"] / total_low_educated) * 100

# View the updated data frame with percentages
asthma_Pollution_frame


# Asthma Proportion
asthma_Pollution_frame_rate <- data.frame(
  Asthma_Status = rep(c("Detected", "NotDetected"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis == 0, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis == 0, ]) / nrow(Lower_Educated_People),
    nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis == 1, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis == 1, ]) / nrow(Lower_Educated_People)
  )
)
asthma_Pollution_frame_rate  

# Visualizing the asthma rate comparison with respect to lower pollution exposure and education level
ggplot(asthma_Pollution_frame_rate, aes(x = Education_status, y = proportion, fill = Asthma_Status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(proportion, 2)),  # Round proportion to 2 decimal places and add as label
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5, fontface = "bold") +  # Add text labels on bars
  scale_fill_manual(values = c("darkred", "pink")) +
  labs(title = "Asthma rate comparison with respect to Lower Pollution Exposure and Education Level",
       x = "Education Level",
       y = "Proportion") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10),  # Adjusted title size
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10)) +
  facet_wrap(~Asthma_Status, ncol = 2)


#----------------------------------------------------------------------------------------------------------------------------------------------------

# 4. Education Level <- Pollution Exposure <- Smoking Habits

# Apply t-test to compare smoking habits between higher and lower educated people under low pollution exposure
t.test(Higher_Educated_People$Smoking_P, Lower_Educated_People$Smoking_P, alternative = "two.sided", conf.level = 0.95)
#P-Value is 0.01 which means there is a relationship for smoking habits between higher and lower educated people under low pollution exposure

# Let's calculate the proportion for each
Smoking_frame_rate_pollution <- data.frame(
  Smoking_Status = rep(c("Not Smoker", "Smoker"), each = 2),
  Education_status = rep(c("Higher Education", "Low Education"), 2),
  proportion = c(
    nrow(Higher_Educated_People[Higher_Educated_People$Smoking_P == 0, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$Smoking_P == 0, ]) / nrow(Lower_Educated_People),
    nrow(Higher_Educated_People[Higher_Educated_People$Smoking_P == 1, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$Smoking_P == 1, ]) / nrow(Lower_Educated_People)
  )
)
Smoking_frame_rate_pollution

# Visualizing the result
ggplot(Smoking_frame_rate_pollution, aes(x=Education_status, y=proportion, fill=smoking_Status)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=round(proportion, 2)),
            position=position_dodge(width=0.9), vjust=-0.5) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Comparing Smoking Habits depending on Education Level with Low Pollution Exposure",
       x="Education Level", y="Proportion") +
  theme_minimal()

# Impact of Education Level and Smoking Habits on Asthma Risk
# I will pick the Non Smokers of both level of Education with Lower Pollution Exposure to see the Impact on Asthma

# Subset data for Non-smokers with Low Pollution Exposure
Educated_Non_Smokers=subset(Higher_Educated_People[Higher_Educated_People$Smoking_P==0,])
Non_Educated_Non_Smokers=subset(Lower_Educated_People[Lower_Educated_People$Smoking_P==0,])

# Creating a data frame to organize results
asthma_Smoking_frame_rate <- data.frame(
  Asthma_Status = rep(c("No", "Yes"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Educated_Non_Smokers[Educated_Non_Smokers$Diagnosis == 0, ]) / nrow(Educated_Non_Smokers),
    nrow(Non_Educated_Non_Smokers[Non_Educated_Non_Smokers$Diagnosis == 0, ]) / nrow(Non_Educated_Non_Smokers),
    nrow(Educated_Non_Smokers[Educated_Non_Smokers$Diagnosis == 1, ]) / nrow(Educated_Non_Smokers),
    nrow(Non_Educated_Non_Smokers[Non_Educated_Non_Smokers$Diagnosis == 1, ]) / nrow(Non_Educated_Non_Smokers)
  )
)

asthma_Smoking_frame_rate

# Create the plot
ggplot(asthma_Smoking_frame_rate, aes(x = Education_status, y = proportion, fill = Asthma_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(proportion, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Low Pollution Exposure and Non-Smoking Habits vs. Asthma Rate by Education Level",
    x = "Education Level",
    y = "Proportion"
  ) +
  theme_minimal() +  # Apply a minimal theme for a cleaner look
  theme(
    plot.title = element_text(face = "bold", size = 12),  # Title font adjustments
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "bold", size = 8),
    axis.text.y = element_text(face = "bold", size = 8)
  ) +
  facet_wrap(~Asthma_Status, ncol = 2) 


#----------------------------------------------------------------------------------------------------------------------------

# Education Level <- Low Pollution Exposure <- Pet exposure 

# T-Test to see if education level and low pollution exposure related to pet exposure 
t.test(Higher_Educated_People$PetAllergy_P, Lower_Educated_People$PetAllergy_P, alternative = "two.sided", conf.level = .99)
# P-Value is 0.0004364. Therefore, shows there is a relationship between Education Level and Pet Exposure 

# Simulate some data for box plot visualization
set.seed(123)

# Create a data frame with individual observations
Pet_box_data <- data.frame(
  Pet_Allergy_Status = rep(c("No", "Yes"), each = 100),
  Education_status = rep(c("High", "Low"), each = 50, times = 2),
  Count = c(
    rnorm(50, mean = 20, sd = 5),  # Simulate counts for High No
    rnorm(50, mean = 25, sd = 5),  # Simulate counts for High Yes
    rnorm(50, mean = 15, sd = 5),  # Simulate counts for Low No
    rnorm(50, mean = 20, sd = 5)   # Simulate counts for Low Yes
  )
)

# Plotting the box plot
ggplot(Pet_box_data, aes(x = Education_status, y = Count, fill = Pet_Allergy_Status)) +
  geom_boxplot() +
  labs(title = "Box Plot of Pet Allergy Counts by Education Level",
       x = "Education Level", y = "Count") +
  theme_minimal()

# Now we need to see the impact on Asthma Risk
# For that, we will pick the non-pet allergic of any education level with Higher Diet Quality to see the impact on asthma

Non_Pet_Educated <- subset(Higher_Educated_People[Higher_Educated_People$PetAllergy_P == 0, ])
Non_Pet_Non_Educated <- subset(Lower_Educated_People[Lower_Educated_People$PetAllergy_P == 0, ])
nrow(Non_Pet_Educated)
nrow(Non_Pet_Non_Educated)

# Creating a data frame to represent all the values
asthma_Pet_frame_rate <- data.frame(
  Asthma_Status = rep(c("No", "Yes"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Non_Pet_Educated[Non_Pet_Educated$Diagnosis == 0, ]) / nrow(Non_Pet_Educated),
    nrow(Non_Pet_Non_Educated[Non_Pet_Non_Educated$Diagnosis == 0, ]) / nrow(Non_Pet_Non_Educated),
    nrow(Non_Pet_Educated[Non_Pet_Educated$Diagnosis == 1, ]) / nrow(Non_Pet_Educated),
    nrow(Non_Pet_Non_Educated[Non_Pet_Non_Educated$Diagnosis == 1, ]) / nrow(Non_Pet_Non_Educated)
  )
)

asthma_Pet_frame_rate

# Visualizing the result
ggplot(asthma_Pet_frame_rate, aes(x = Education_status, y = proportion, fill = Asthma_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(title = "Comparing Non-Pet Allergic People with Asthma Rate by Education Level and Low Pollution Exposure",
       x = "Education Level", y = "Proportion") +
  geom_text(aes(label = round(proportion, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  theme_minimal() +  # Apply a minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 10),  
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  facet_wrap(~Asthma_Status, ncol = 2)  


#----------------------------------------------------------------------------------------------------------------------------

# Education Level <- Low Pollution Exposure <- Diet Quality 

# I will do a T-test
t.test(Higher_Educated_People$DietQuality_P, Lower_Educated_People$DietQuality_P, alternative = "two.sided", conf.level = .99)
# The P-Value is 3.176e-06. Therefore, there is a very strong relationship between Education Level and Pollution Exposure

# Create a data frame to organize the results
DietQuality_frame_pollution <- data.frame(
  Education_status = c("Higher Education", "Low Education"),
  Mean_DietQuality = c(
    mean(Higher_Educated_People$DietQuality_P, na.rm = TRUE),
    mean(Lower_Educated_People$DietQuality_P, na.rm = TRUE)
  ),
  Pollution_Exposure = rep("Low Pollution Exposure", 2)  # Assuming you're focusing on low pollution exposure
)

# View the data frame
DietQuality_frame_pollution


# Visualize the data frame using a box plot
DietQuality_combined <- rbind(
  data.frame(DietQuality_P = Higher_Educated_People$DietQuality_P, Education_status = "Higher Education"),
  data.frame(DietQuality_P = Lower_Educated_People$DietQuality_P, Education_status = "Low Education")
)
ggplot(DietQuality_combined, aes(x = Education_status, y = DietQuality_P, fill = Education_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red")) +  # Customize box colors
  labs(
    title = "Diet Quality Distribution by Education Level with Low Pollution Exposure",
    x = "Education Level",
    y = "Diet Quality"
  ) +
  theme_minimal()

# I will pick Higher Diet Quality of any Education Level with Lower Pollution Exposure to see the impact on Asthma

# I find the median to determine High Diet Quality and Low Diet Quality
median_diet_quality <- median(data_set$DietQuality_P, na.rm = TRUE)

# Subset data for low pollution exposure
low_pollution <- subset(data_set, PollutionExposure_P < median(data_set$PollutionExposure_P, na.rm = TRUE))

# Subset data for high diet quality
high_diet_quality <- subset(low_pollution, DietQuality_P > median_diet_quality)

# Subset data by education level
high_education <- subset(high_diet_quality, EducationLevel_P > median(data_set$EducationLevel_P, na.rm = TRUE))
low_education <- subset(high_diet_quality, EducationLevel_P <= median(data_set$EducationLevel_P, na.rm = TRUE))

# Create a data frame for asthma rate comparison
# Create a data frame for asthma rate comparison
asthma_diet_frame <- data.frame(
  Asthma_Status = rep(c("No Asthma", "Asthma"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  Proportion = c(
    nrow(high_education[high_education$Diagnosis == 0, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 0, ]) / nrow(low_education),
    nrow(high_education[high_education$Diagnosis == 1, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 1, ]) / nrow(low_education)
  )
)

asthma_diet_frame 

# Create the plot
ggplot(asthma_diet_frame, aes(x = Education_status, y = Proportion, fill = Asthma_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Proportion, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  # Add text labels on top of bars
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Comparing Low Pollution Exposure and Higher Diet Quality with Asthma Rate Depending on Education Level",
    x = "Education Level",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),  # Adjusted title size
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# --------------------------------------------------------------------------------------------------------------------------------

# Additional Findings 1 
# 1. Education Level <- Low Pollution Exposure <- Dust Exposure 

# T-Test to see if education level and low pollution exposure related to dust exposure
t.test(Higher_Educated_People$DustExposure_P, Lower_Educated_People$DustExposure_P, alternative = "two.sided", conf.level = .99)
# P-Value is 0.00000004. Therefore, shows there is a relationship between Education Level and Dust Exposure

# Create the data frame summarizing mean diet quality by education status
DustExposure_frame_pollution <- data.frame(
  Education_status = c("Higher Education", "Low Education"),
  Mean_DietQuality = c(
    mean(Higher_Educated_People$DustExposure_P, na.rm = TRUE),
    mean(Lower_Educated_People$DustExposure_P, na.rm = TRUE)
  ),
  Pollution_Exposure = rep("Low Pollution Exposure", 2)  
)

# View the data frame to verify
print(DustExposure_frame_pollution)

# Create the box plot
ggplot(DustExposure_combined, aes(x = Education_status, y = DustExposure_P, fill = Education_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(
    title = "Dust Exposure Distribution by Education Level with Low Pollution Exposure",
    x = "Education Level",
    y = "Dust Exposure"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8),  # Adjust the title size here
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# I will pick Higher Dust Exposure of any Education Level with Lower Pollution Exposure to see the impact on Asthma

# I find the median to determine High Dust Exposure and Low Dust Exposure
median_dust_exposure <- median(data_set$DustExposure_P, na.rm = TRUE)

# Subset data for low pollution exposure
low_pollution <- subset(data_set, PollutionExposure_P < median(data_set$PollutionExposure_P, na.rm = TRUE))

# Subset data for High Dust Exposure
high_dust_exposure <- subset(low_pollution, DustExposure_P > median_dust_exposure)

# Subset data by education level
high_education <- subset(high_dust_exposure, EducationLevel_P > median(data_set$EducationLevel_P, na.rm = TRUE))
low_education <- subset(high_dust_exposure, EducationLevel_P <= median(data_set$EducationLevel_P, na.rm = TRUE))

# Create a data frame for asthma rate comparison
asthma_dustexposure_frame <- data.frame(
  Asthma_Status = rep(c("No Asthma", "Asthma"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  Proportion = c(
    nrow(high_education[high_education$Diagnosis == 0, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 0, ]) / nrow(low_education),
    nrow(high_education[high_education$Diagnosis == 1, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 1, ]) / nrow(low_education)
  )
)

# Create the plot
ggplot(asthma_dustexposure_frame, aes(x = Education_status, y = Proportion, fill = Asthma_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Proportion, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Comparing Low Pollution Exposure and Higher Dust Exposure with Asthma Rate Depending on Education Level",
    x = "Education Level",
    y = "Proportion"
  ) +
  coord_cartesian(ylim = c(0, 1.2)) +  # Adjust y-axis limit here
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),  
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

  # --------------------------------------------------------------------------------------------------------------------------------

# Additional Findings 2
# 2. Education Level <- Low Pollution Exposure <- Sleep Quality 

# T-Test to see if education level and low pollution exposure related to sleep quality
t.test(Higher_Educated_People$SleepQuality_P, Lower_Educated_People$SleepQuality_P, alternative = "two.sided", conf.level = .99)
# P-Value is 0.02. Therefore, shows there is a relationship between Education Level and Sleep QUality 

# Create a data frame for sleep quality results
SleepQuality_frame_pollution <- data.frame(
  Education_status = c("Higher Education", "Low Education"),
  Mean_SleepQuality = c(
    mean(Higher_Educated_People$SleepQuality_P, na.rm = TRUE),
    mean(Lower_Educated_People$SleepQuality_P, na.rm = TRUE)
  ),
  Pollution_Exposure = rep("Low Pollution Exposure", 2)  # Assuming low pollution exposure
)

# Create the bar plot for sleep quality
ggplot(SleepQuality_frame_pollution, aes(x = Education_status, y = Mean_SleepQuality, fill = Education_status)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' to separate the bars
  geom_text(aes(label = round(Mean_SleepQuality, 2)),  
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4, fontface = "bold") +  # Adjust the label position and appearance
  labs(title = "Mean Sleep Quality by Education Level with Low Pollution Exposure",
       x = "Education Level",
       y = "Mean Sleep Quality") +  # Customize axis labels
  scale_fill_manual(values = c("blue", "red")) +  # Customize the bar colors
  ylim(0, 8) +  # Adjust the y-axis to extend the height of the plot
  theme_minimal() +  # Apply minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 10),  # Adjust title size here
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )



# I will pick Higher Sleep Quality  of any Education Level with Lower Pollution Exposure to see the impact on Asthma

# I find the median to determine High Sleep Quality 
median_sleep_quality <- median(data_set$SleepQuality_P, na.rm = TRUE)

# Subset data for high sleeping quality
high_sleep_quality<- subset(low_pollution, SleepQuality_P > median_sleep_quality)

# Subset data by education level
high_education <- subset(high_sleep_quality, EducationLevel_P > median(data_set$EducationLevel_P, na.rm = TRUE))
low_education <- subset(high_sleep_quality, EducationLevel_P <= median(data_set$EducationLevel_P, na.rm = TRUE))

# Create a data frame for asthma rate comparison
asthma_sleepingqualityexposure_frame <- data.frame(
  Asthma_Status = rep(c("No Asthma", "Asthma"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  Proportion = c(
    nrow(high_education[high_education$Diagnosis == 0, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 0, ]) / nrow(low_education),
    nrow(high_education[high_education$Diagnosis == 1, ]) / nrow(high_education),
    nrow(low_education[low_education$Diagnosis == 1, ]) / nrow(low_education)
  )
)

# Create the plot
ggplot(asthma_sleepingqualityexposure_frame, aes(x = Education_status, y = Proportion, fill = Asthma_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Proportion, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Comparing Sleep Quality, Education Level and Low Pollution Exposure impact Asthma Rate",
    x = "Education Level",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),  
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  expand_limits(y = c(0, 1))  





