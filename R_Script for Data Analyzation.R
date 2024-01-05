


#                                               F O R   T H E   P L O T       
######################################################################################################################
library(ggplot2)
# Getting the required data
CEREAL_YIELD <- read.csv(file.choose(), header = TRUE)
FERTILIZER_CONS <- read.csv(file.choose(), header = TRUE)

# Transposing it to convert from horizontal to vertical following the progression of Years
CEREAL_YIELD <- t(CEREAL_YIELD)
FERTILIZER_CONS <- t(FERTILIZER_CONS)
# Creating a "Years" column
YEARS <- c(1961:2021)

COMPLETE_DATA <- data.frame(YEARS,CEREAL_YIELD, FERTILIZER_CONS)






# Creating a data frame for ggplot
ggplot_data <- data.frame(YEARS = COMPLETE_DATA$YEARS,
                          CEREAL_YIELD = COMPLETE_DATA$CEREAL_YIELD,
                          FERTILIZER_CONS = COMPLETE_DATA$FERTILIZER_CONS)

# Scatter plot with a red trendline for reference
# Blue for the points
ggplot(ggplot_data, aes(x = FERTILIZER_CONS, y = CEREAL_YIELD)) +
  geom_point(size = 1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, level = 0.99, color = "red") +
  labs(title = "Cereal Yield and Fertilizer Consumption (1961-2021)",
       x = "Fertilizer Consumption",
       y = "Cereal Yield") +
  theme_bw() +
  geom_text(aes(label = YEARS), vjust = 0.5, hjust = -0.1, color = "darkgray")
####################################################################################################################




#                 F O R    T H E     L I N E A R     R E G R E S S I O N     M O D E L
####################################################################################################################
# Perform linear regression
linear_model <- lm(CEREAL_YIELD ~ FERTILIZER_CONS, data = COMPLETE_DATA)

# Print the summary of the linear model
summary(linear_model)
####################################################################################################################
