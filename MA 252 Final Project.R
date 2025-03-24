
library(tidyverse)
library(readr)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(GGally)
library(esquisse)
library(plotly)
library(canvasXpress)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)

walmart <- Wal.data
min(walmart$Weekly_Sales)
max(walmart$Weekly_Sales)
sd(walmart$Weekly_Sales)
mean(walmart$Weekly_Sales)

min(walmart$Holiday_Flag)
max(walmart$Holiday_Flag)
sd(walmart$Holiday_Flag)
mean(walmart$Holiday_Flag)

min(walmart$Temperature)
max(walmart$Temperature)
sd(walmart$Temperature)
mean(walmart$Temperature)

min(walmart$Fuel_Price)
max(walmart$Fuel_Price)
sd(walmart$Fuel_Price)
mean(walmart$Fuel_Price)

min(walmart$CPI)
max(walmart$CPI)
sd(walmart$CPI)
mean(walmart$CPI)

min(walmart$Unemployment)
max(walmart$Unemployment)
sd(walmart$Unemployment)
mean(walmart$Unemployment)


# Assuming your dataset is named "walmart" and contains the columns "Weekly_Sales" and "Unemployment"

ggplot(walmart, aes(Unemployment, Weekly_Sales)) +
  geom_point() +
  labs(title = "Weekly Sales vs. Unemployment")


ggplot(walmart, aes(CPI, Weekly_Sales)) +
  geom_point() +
  labs(title = "Weekly Sales vs. CPI")
#Create subset
#store_1 = subset(walmart, Store == 1)
#Report for store 1
#options(dplyr.summarise.inform = FALSE)
#skim_without_charts(store_1)
#create_report(store_1, output_file = 'store_1.html')


store_1 <- df[Wal.data$Store == 1, ]

# Create the scatter plot matrix using GGally
scatter_plot_matrix1 <- ggpairs(store_1, 
                                columns = c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment", "Holiday_Flag"), 
                                ggplot2::aes(color = as.factor(Holiday_Flag)))
print(scatter_plot_matrix1)






