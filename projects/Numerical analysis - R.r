library(epitools)
library(Hmisc)
library(corrplot)

data <- read_sav("HSE 2011.sav")

# Total sample size
nrow(data)

# Count people who drink 
table(data$dnnow)

# Calculate percentage who drink
prop.table(table(data$dnnow)) * 100

# Remove missing data
drinkers <- subset(data, dnnow > 0)  
prop.table(table(drinkers$dnnow)) * 100

# Count by gender
table(data$Sex)

# Calculate percentage
prop.table(table(data$Sex)) * 100

# Remove missing data
gender_valid <- subset(data, Sex > 0)  
prop.table(table(gender_valid$Sex)) * 100

# Specifically for women (Sex = 2)
sum(data$Sex == 2, na.rm = TRUE) / sum(data$Sex > 0, na.rm = TRUE) * 100

# Distribution of educational levels
table(data$topqual3)

# Frequency of each education level
prop.table(table(data$topqual3)) * 100

# Mode
which.max(table(data$topqual3))

# Distribution of marital statuses
table(data$marstatc)

# Calculate percentage of divorced (5) and separated (4) people
# Remove negative values
marital_valid <- subset(data, marstatc > 0)  

divorced_separated <- sum(marital_valid$marstatc == 4 | marital_valid$marstatc == 5) 
total_valid <- nrow(marital_valid)
divorced_separated_percent <- divorced_separated / total_valid * 100
divorced_separated_percent


# For household size 
summary(data$HHSize)
sd(data$HHSize, na.rm = TRUE)
range(data$HHSize, na.rm = TRUE)

# Mode
names(which.max(table(data$HHSize)))

# For BMI (bmival)
summary(data$bmival)
sd(data$bmival, na.rm = TRUE)
range(data$bmival, na.rm = TRUE)

# Mode
names(which.max(table(data$bmival)))

# For age (Age)
summary(data$Age)
sd(data$Age, na.rm = TRUE)
range(data$Age, na.rm = TRUE)

# Mode
names(which.max(table(data$Age)))


#Inferential Statistics
##Run a significance test to find out which gender drinks more alcohol.

# Create a contingency table of gender and drinking status
gender_drink_table <- table(data$Sex, data$dnnow)

# View the table
gender_drink_table

# Run chi-square test
chisq_test <- chisq.test(gender_drink_table)
print(chisq_test)

# For percentages within each gender
prop.table(gender_drink_table, margin = 1) * 100

# Calculate odds ratio
library(epitools)
oddsratio(gender_drink_table)


#b. Run a significance test to find out which region drinks the most alcohol.
# Create a contingency table of region and drinking status
region_drink_table <- table(data$gor1, data$dnnow)

# View the table
region_drink_table

# Run chi-square test
region_chisq_test <- chisq.test(region_drink_table)
print(region_chisq_test)

# For percentages within each region
region_props <- prop.table(region_drink_table, margin = 1) * 100
print(region_props)

# For a more visual comparison, you can create a barplot
barplot(region_props[,1], main="Percentage of People Who Drink by Region", 
        xlab="Region", ylab="Percentage (%)", col="green")





# summary of height variable
summary(data$htval)

# Check for missing values
sum(is.na(data$htval))

# Split height by gender and check summary statistics
tapply(data$htval, data$Sex, summary, na.rm = TRUE)

# Run normality tests for height by gender

# Shapiro-Wilk test for men 
shapiro.test(data$htval[data$Sex == 1])

# Shapiro-Wilk test for women
shapiro.test(data$htval[data$Sex == 2])

# Visual check for normality
# Histograms
hist(data$htval[data$Sex == 1], main="Height Distribution - Men", xlab="Height")
hist(data$htval[data$Sex == 2], main="Height Distribution - Women", xlab="Height")

#Wilcoxon test since data is not normally distributed
wilcox_test_result <- wilcox.test(htval ~ Sex, data = data)
print(wilcox_test_result)

# Calculate medians for each group for reporting
median_male <- median(data$htval[data$Sex == 1], na.rm=TRUE)
median_female <- median(data$htval[data$Sex == 2], na.rm=TRUE)
print(paste("Median height for males:", median_male, "cm"))
print(paste("Median height for females:", median_female, "cm"))

# Boxplot
boxplot(htval ~ Sex, data = data, names = c("Male", "Female"), 
        main = "Height Comparison by Gender", xlab = "Gender", ylab = "Height (cm)")

#b Valid weight
# Summary of weight variable
summary(data$wtval)

# Check for missing values
sum(is.na(data$wtval))

# Split weight by gender and check summary statistics
tapply(data$wtval, data$Sex, summary, na.rm = TRUE)

# Run normality tests for weight by gender
# Shapiro-Wilk test for men
shapiro.test(data$wtval[data$Sex == 1])

# Shapiro-Wilk test for women
shapiro.test(data$wtval[data$Sex == 2])

# Visual check for normality
# Histograms
hist(data$wtval[data$Sex == 1], main="Weight Distribution - Men", xlab="Weight (kg)")
hist(data$wtval[data$Sex == 2], main="Weight Distribution - Women", xlab="Weight (kg)")


# Use non-parametric Wilcoxon test since data is not normally distributed
wilcox_test_result_weight <- wilcox.test(wtval ~ Sex, data = data)
print(wilcox_test_result_weight)

# Calculate medians for each group for reporting
median_male_wt <- median(data$wtval[data$Sex == 1], na.rm=TRUE)
median_female_wt <- median(data$wtval[data$Sex == 2], na.rm=TRUE)
print(paste("Median weight for males:", median_male_wt, "kg"))
print(paste("Median weight for females:", median_female_wt, "kg"))

# Boxplot
boxplot(wtval ~ Sex, data = data, names = c("Male", "Female"), 
        main = "Weight Comparison by Gender", xlab = "Gender", ylab = "Weight (kg)")


#correlation

# Proper encoding
# Convert dnnow to binary (1 = drinks, 0 = doesn't drink)
data$drinks_binary <- ifelse(data$dnnow == 1, 1, 0)

# Convert gender to binary (1 = male, 0 = female)
data$gender_binary <- ifelse(data$Sex == 1, 1, 0)

# Removing any rows with missing values
corr_data <- na.omit(data.frame(
  drinks = data$drinks_binary,
  income = data$totinc,
  age = data$Age,
  gender = data$gender_binary
))

# Check the structure of our data
str(corr_data)

# Simple correlation matrix using cor()
correlation_matrix <- cor(corr_data, method = "spearman")
print(correlation_matrix)
print(round(correlation_matrix, 3))  # Rounded for readability

# Test for significance of correlations
library(Hmisc)
rcorr_matrix <- rcorr(as.matrix(corr_data), type="spearman")
print(rcorr_matrix)