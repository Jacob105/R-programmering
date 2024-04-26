# Load required libraries
library(readxl)

# Read the Excel file
data <- read_excel("C:/Users/jacob/Documents/R Programmering/SCB_bilar.xlsx", col_names = TRUE)

# Extract the years and total cars
years <- as.numeric(names(data))
total_cars <- as.numeric(data)
par(mfrow = c(2, 1))
# Set larger margins
par(mar = c(4, 4, 4, 4))  # Set bottom margin to 4 for labels

# Plot
barplot(total_cars, 
        names.arg = years,
        main = "Total Number of Cars Each Year", 
        xlab = "Model Year", 
        ylab = "Total Number of Cars", 
        col = "skyblue")

# Add labels for each bar (total number of cars)
text(x = barplot(total_cars, 
                 plot = FALSE),
     y = total_cars + -10,  # Adjust the vertical position of the labels
     labels = total_cars,
     pos = 1,  # Position the label above the bar
     col = "black")

# Plot
plot(years, total_cars, type = "o", pch = 19, col = "skyblue", 
     main = "Total Number of Cars Each Year", 
     xlab = "Model Year", 
     ylab = "Total Number of Cars")



