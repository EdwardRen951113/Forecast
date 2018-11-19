# Load needed library files
library(forecast)
library(ggplot2)
library(readxl)

# Import population data from Excel 
aaPopR <- read_excel("Data/aaPop.xlsx")

# Preview first few columns of data to verify it was loaded into memory properly
head(aaPopR)

# Plot data to a chart
aaPopPlot <- ggplot(aaPopR, aes(x = YEAR, y = POP)) +
  geom_point() +
  ggtitle("Anne Arundel County Population Growth  1950 - 2017") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Population") +
  theme_bw()
print (aaPopPlot) 
  
# Run a simple regression model plotting population vs year
  aaPopReg <- lm(POP ~ YEAR, aaPopR)
  
# View regression model summary  
  summary(aaPopReg)
  
# Visualize the regression model
  aaPopRegPlot <- ggplot(aaPopR, aes(x = YEAR, y = POP)) +
    geom_point(shape = 1) +
    geom_smooth(method = 'lm', se = FALSE) + 
    ggtitle("Anne Arundel County Population Growth  1950 - 2017") +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Population") +
    annotate("text", x=1985, y=300000, label = "R^2 == 0.983", parse=T) +
    theme_bw()
  print(aaPopRegPlot)
    
# Create a time series object 
aaPopTs <- ts(aaPopR[2], start = c(1950,1), frequency = 1)

# Forecast a 20 year trend using Holt's method
fcPopHolt <- holt(aaPopTs, h = 25)
summary(fcPopHolt)
autoplot(fcPopHolt)
checkresiduals(fcPopHolt)

# Validate the use of Holt's forecast method by comparing results to ETS.
fitaaPop <- ets(aaPopTs)
summary(fitaaPop)
autoplot(forecast(fitaaPop))
checkresiduals(fitaaPop)




