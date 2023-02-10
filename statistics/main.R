library(ggplot2)
# importing big data set from given file (file in same folder as program)
Life.Expectancy.Data <- read.csv("Life Expectancy Data.csv")
# putting data into variable "Data"
Data <- Life.Expectancy.Data
# changing name of a variable to be more reasonable
colnames(Data)[19] <- "thinness.10.19.years"
# attaching data so that we can access variables
attach(Data)
# showing view of our data
View(Data)
# showing summary of our data
summary(Data)
# creating smaller subsets with data that interests us
Data.subset1 <- subset(Data, select = 
                         c("Life.expectancy", "Year", "Schooling", "Alcohol", 
                           "BMI", "thinness.5.9.years", "thinness.10.19.years", 
                          "percentage.expenditure"))
Data.subset2 <- subset(Data, select = 
                         c("Life.expectancy", "Year", "Schooling", "Alcohol", 
                           "BMI", "thinness.5.9.years", "thinness.10.19.years", 
                           "percentage.expenditure", "Status"))
# showing correlation between some of chosen variables
# using use = "complete" so that we take only full data into correlation,
# which means that if there is "NA" value in a row we skip it
cor(Data.subset1, use = "complete")
# showing average life time change over the years
ggplot(data = Data.subset1, 
       aes(x = Year,
           y = Life.expectancy,
           color = Status)) + geom_point() + 
  labs(x = "Year", y = "Expected length of life in years", 
       title = "Average length of life over the years") + 
  geom_smooth(method = "lm", se = FALSE)
# creating two new subsets that consist only of developing/developed countries
developing_countries <- subset(Data.subset2, Status == "Developing")
developed_countries <- subset(Data.subset2, Status == "Developed")
# showing correlation between alcohol consumption and life length in developing
# countries
ggplot(data = developing_countries, aes(x = Alcohol, 
                                y = Life.expectancy)) + geom_point() + 
  labs(x = "Alcohol consumption", y = "Expected length of life in years", 
       title = "Length of life in comparision to alcohol consumption 
in developing countries") + geom_smooth(method = "lm", se = FALSE)
# showing same correlation for developed countries
ggplot(data = developed_countries, aes(x = Alcohol, 
                                        y = Life.expectancy)) + geom_point() + 
  labs(x = "Alcohol consumption", y = "Expected length of life in years", 
       title = "Length of life in comparision to alcohol consumption 
in developing countries") + geom_smooth(method = "lm", se = FALSE)
# showing expected lifetime in correlation with years of schooling for
# developing countries
ggplot(data = developing_countries, 
       aes(x = Schooling, 
           y = Life.expectancy)) + 
  geom_point() + 
  labs(x = "Years of schooling", y = "Expected length of life in years", 
       title = "Length of life in comparision to number of years of schooling 
in developing countries") +
  geom_smooth(method = "lm", se = FALSE)
# same for developed countries
ggplot(data = developed_countries, 
       aes(x = Schooling,
           y = Life.expectancy)) + 
  geom_point() + 
  labs(x = "Years of schooling", y = "Expected length of life in years", 
       title = "Length of life in comparision to number of years of schooling
in developed countries")+
  geom_smooth(method = "lm", se = FALSE)
# comparing length of life with BMI score only for developing countries because
# there is too little variety for developed countries data
ggplot(data = developing_countries, aes(x = BMI, 
                                y = Life.expectancy)) + geom_point() + 
  labs(x = "BMI", y = "Expected length of life in years", 
       title = "Length of life in comparision to BMI score for 
developing countries") +
  geom_smooth(method = "lm", se = FALSE)
# showing amount of money that a country put into health correlate with
# lifetime of people living there, we get weird results so we going to make
# more tests
ggplot(data = Data.subset2, aes(x = percentage.expenditure, 
                                y = Life.expectancy,
                                color = Status)) + geom_point() + 
  labs(x = "percentage of GDP per capita spent on health", 
       y = "Expected length of life in years", 
       title = "Length of life in comparision to money spent on health") +
  geom_smooth(method = "lm", se = FALSE)
# creating two new subsets with higher and lower GDP spent per capita
higher_gdp <- subset(Data.subset2, percentage.expenditure > 1000)
lower_gdp <- subset(Data.subset2, percentage.expenditure < 1000)
# repeating comparison for new data sets, now we get more believable results
ggplot(data = higher_gdp, aes(x = percentage.expenditure, 
                                y = Life.expectancy,
                                color = Status)) + geom_point() + 
  labs(x = "percentage of GDP spent on health", 
       y = "Expected length of life in years", 
       title = "Length of life in comparision to money spent on health 
in countries where more money is spent on it") +
  geom_smooth(method = "lm", se = FALSE)
# repeating comparison for new data sets, now we get more believable results
ggplot(data = lower_gdp, aes(x = percentage.expenditure, 
                              y = Life.expectancy,
                              color = Status)) + geom_point() + 
  labs(x = "percentage of GDP spent on health", 
       y = "Expected length of life in years", 
       title = "Length of life in comparision to money spent on health 
in countries where less money is spent on it") +
  geom_smooth(method = "lm", se = FALSE)
# creating subset to analyze some other attributes that may contribute to
# length of life
Data.subset3 <- subset(Data, select = 
                         c("Life.expectancy", "Year", "Status",
                           "Adult.Mortality","infant.deaths", 
                           "under.five.deaths"))
# checking correlation between attributes, 
# all 3 new attributes have negligible impact on length of life 
# which is pretty unexpected.
cor(Data.subset3[c(1, 2, 4, 5, 6)], use = "complete")
# showing BMI levels over the years both for developing and developed countries
ggplot(data = Data.subset2, 
       aes(x = Year,
           y = BMI,
           color = Status)) + geom_point() + 
  labs(x = "Year", y = "BMI", 
       title = "BMI level over the years") + 
  geom_smooth(method = "lm", se = FALSE)
# showing amount of alcohol drunk over the years
ggplot(data = Data.subset2, 
       aes(x = Year,
           y = Alcohol,
           color = Status)) + geom_point() + 
  labs(x = "Year", y = "alcohol consumption", 
       title = "Alcohol consumption over the years") + 
  geom_smooth(method = "lm", se = FALSE)
# showing money spent for health in developing countries, no big changes here
ggplot(data = developing_countries, 
       aes(x = Year,
           y = percentage.expenditure)) + geom_point() + 
  labs(x = "Year", y = "money spent on health", 
       title = "Average money spent on health over the years 
in developing countries") + 
  geom_smooth(method = "lm", se = FALSE)
# showing number of years of schooling in developing countries over the years
ggplot(data = developing_countries, 
       aes(x = Year,
           y = Schooling)) + geom_point() + 
  labs(x = "Year", y = "years of schooling", 
       title = "Average number of schooling years over the years 
in developing countries") + 
  geom_smooth(method = "lm", se = FALSE)
# same for developed countries
ggplot(data = developed_countries, 
       aes(x = Year,
           y = Schooling)) + geom_point() + 
  labs(x = "Year", y = "years of schooling", 
       title = "Average number of schooling years over the years 
in developed countries") + 
  geom_smooth(method = "lm", se = FALSE)
# creating linear model of length of life and years in developing countries
linear_model_developing = lm(Life.expectancy ~ Year, data = developing_countries)
# creating data frame of years which we want to predict
variable_year <- data.frame(Year = c(2025,2035,2045,2055,2065,2075,2085,2100))
# predicting average length of people's life in given years
predict(linear_model_developing, newdata = variable_year)
# creating linear model of length of life and years in developed countries
linear_model_developed = lm(Life.expectancy ~ Year, data = developed_countries)
# predicting average age of people in given years
predict(linear_model_developed, newdata = variable_year)
# creating linear model of years of schooling over years in developing countries
linear_model2 = lm(Schooling ~ Year, data = developing_countries)
# predicting average age of people in given years
predict(linear_model2, newdata = variable_year)
# creating linear model of length of life and years in developing countries
linear_model3 = lm(Alcohol ~ Year, data = developing_countries)
# predicting average alcohol consumption in given years
predict(linear_model3, newdata = variable_year)
