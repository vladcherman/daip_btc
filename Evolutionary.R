library(quantmod)
library(gramEvol)
library(dplyr)
library(tidyverse)
library(patchwork)
library(zoo)
library(xts)
library(caTools)

df_cleaned <- read.csv("data/evolutionary_data.csv", fileEncoding="UTF-8-BOM")

# create a lag feature
df_cleaned <- df_cleaned %>% 
  mutate(Bitcoin_price_lag=lag(Bitcoin_price,1)) %>%
  mutate(Bitcoin_price_change_lag=lag(Bitcoin_price_change,1)) %>%
  mutate(Bitcoin_price_change_percent_lag=lag(Bitcoin_price_change_percent,1)) %>%
  na.omit()
rownames(df_cleaned_twitter) <- NULL

# make datasets that only contain relevant data to each feature
df_cleaned_twitter <- df_cleaned %>%
  filter(Twitter_volume > 0) 
df_cleaned_ethereum <- df_cleaned %>%
  filter(Ethereum_price > 0) 
df_cleaned_cardano <- df_cleaned %>%
  filter(Cardano_price > 0) 
df_cleaned_ripple <- df_cleaned %>%
  filter(Ripple_price > 0) 

# add a lag variable

df_cleaned %>%
  filter(!is.na(DJI)) %>%
  ggplot() +
  geom_point(aes(DJI, Bitcoin_price))

df_cleaned %>%
  filter(!is.na(NASDAQ)) %>%
#  filter(as.Date(date,format="%d/%m/%Y") >= as.Date("01/01/2016",format="%d/%m/%Y")) %>%
  ggplot() +
  geom_point(aes(NASDAQ, Bitcoin_price))

df_cleaned %>%
  filter(!is.na(S.P500)) %>%
  #  filter(as.Date(date,format="%d/%m/%Y") >= as.Date("01/01/2016",format="%d/%m/%Y")) %>%
  ggplot() +
  geom_point(aes(S.P500, Bitcoin_price))

df_cleaned %>%
  filter(!is.na(Gold)) %>%
#  filter(as.Date(date,format="%d/%m/%Y") >= as.Date("01/01/2016",format="%d/%m/%Y")) %>%
  ggplot() +
  geom_point(aes(Gold, Bitcoin_price))

df_cleaned %>%
  filter(!is.na(Google_trends)) %>%
  ggplot() +
  geom_point(aes(Google_trends, Bitcoin_price))

df_cleaned %>%
  filter(Twitter_volume > 0) %>%
  ggplot() +
  geom_point(aes(Twitter_volume, Bitcoin_price))

# linearly proportional to square root of weekly google trends
df_cleaned %>%
  filter(!is.na(Google_trends)) %>%
  ggplot() +
  geom_point(aes(sqrt(Google_trends), Bitcoin_price))

# plot some graphs to study relationship to stock indexes and gold
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date,format="%Y-%m-%d"), Bitcoin_price)) +
  xlab("Date") -> p1
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date,format="%Y-%m-%d"), Gold)) +
  xlab("Date") -> p2
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date,format="%Y-%m-%d"), NASDAQ)) +
  xlab("Date") -> p3
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date,format="%Y-%m-%d"), DJI)) +
  xlab("Date") -> p4
(p1+p2)/(p3+p4)

ggplot(df_cleaned) +
  geom_point(aes(as.Date(Date), Google_trends)) +
#  geom_line(aes(as.Date(Date), Bitcoin_price)) +
  xlab("Date") -> p1
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date), Bitcoin_price)) +
  xlab("Date") -> p2
p1/p2


# plot some graphs to study relationship to other cryptocurrencies
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date), Bitcoin_price)) +
  xlab("Date") -> p1
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date), Ethereum_price)) +
  xlab("Date") -> p2
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date), Ripple_price)) +
  xlab("Date") -> p3
ggplot(df_cleaned) +
  geom_line(aes(as.Date(Date), Cardano_price)) +
  xlab("Date") -> p4
(p1+p2)/(p3+p4)

# plot correlations
library(corrplot)
correlations <- cor(df_cleaned[,3:16])
corrplot(correlations)

# look at autocorrelation
acf(df_cleaned$Bitcoin_price)

# with other features
ccf(df_cleaned_twitter$Bitcoin_price, df_cleaned_twitter$Twitter_volume, lag.max = 20) # lag of 12 seems most correlated
ccf(df_cleaned_google$Bitcoin_price, df_cleaned_google$Google_trends, lag.max = 20) # lag of 0 seems most correlated, but lag of 1 is close

ccf(df_cleaned_ethereum$Bitcoin_price, df_cleaned_ethereum$Ethereum_price, lag.max = 20) # lag of 0 seems most correlated
ccf(df_cleaned_ripple$Bitcoin_price, df_cleaned_ripple$Ripple_price, lag.max = 20) # lag of 0 seems most correlated, steep fall beyond that
ccf(df_cleaned_cardano$Bitcoin_price, df_cleaned_cardano$Cardano_price, lag.max = 20) # lag of 0 seems most correlated, steep fall beyond that

ccf(df_cleaned$Bitcoin_price, df_cleaned$S.P500, lag.max = 20) # lag of 0 seems most correlated, but lag of 1 is close
ccf(df_cleaned$Bitcoin_price, df_cleaned$DJI, lag.max = 20) # lag of 0 seems most correlated, but lag of 1 is close
ccf(df_cleaned$Bitcoin_price, df_cleaned$NASDAQ, lag.max = 20) # lag of 0 seems most correlated, but lag of 1 is close
ccf(df_cleaned$Bitcoin_price, df_cleaned$Gold, lag.max = 30) # most lags seem equally correlated up to 15 or so
ccf(df_cleaned$Bitcoin_price, df_cleaned$Silver, lag.max = 20) # lag of 0 seems most correlated
ccf(df_cleaned$Bitcoin_price, df_cleaned$Commodities, lag.max = 30) # negative correlation, rises the further you lag
ccf(df_cleaned$Bitcoin_price, df_cleaned$T_Bill, lag.max = 30) # negative correlation, rises slightly the further you lag
ccf(df_cleaned$Bitcoin_price, df_cleaned$Oil, lag.max = 30) # negative correlation, rises the further you lag

###
### GRAMMATICAL EVOLUTION
###

# split data into train and test sets
trainingData <- df_cleaned_ethereum %>% filter(as.Date(Date) <= as.Date("2020-02-26",format="%Y-%m-%d"))
testingData <- df_cleaned_ethereum %>% filter(as.Date(Date) > as.Date("2020-02-26",format="%Y-%m-%d"))

# set mydate variable
mydata <- trainingData

# define grammar rules - give as many options as possible, including squaring and cubing values
rules <- list(expr = grule(op(expr, expr), func(expr), var),
              func = grule(log, sqrt),
              op = grule('+', '-', '*', '/'),
              var = grule(dat, dat^n),
              dat = grule(mydata$Ethereum_price, 
                          #mydata$Cardano_price, 
                          #mydata$Ripple_price,
                          #mydata$Twitter_volume,
                          mydata$Bitcoin_price_change_lag,
                          #mydata$Bitcoin_price_lag,
                          mydata$S.P500, mydata$DJI, mydata$NASDAQ, mydata$Gold, mydata$Google_trends,
                          mydata$Silver, mydata$Commodities, mydata$T_Bill, mydata$Oil),
              n = grule(2,3))
bit_grammar <- CreateGrammar(rules)
bit_grammar

# create a fitness function that uses RMSE as a cost
bit_fit_func <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean((mydata$Bitcoin_price - result)^2)))
}

# create the GE and look at the outcome
ge <- GrammaticalEvolution(bit_grammar, bit_fit_func, iterations = 1000, max.depth = 10)
ge

#Best Expression:  mydata$NASDAQ/(sqrt(mydata$Silver)/log(mydata$Google_trends)) 
#Best Cost:        2207.9687649562 

#Best Expression:  mydata$S.P500 * log(mydata$Google_trends) 
#Best Cost:        1863.76235175956 

#Best Expression:  mydata$Commodities^2 * (mydata$Ethereum_price/mydata$T_Bill^3) 
#Best Cost:        1779.45576778528 

#Best Expression:  log(log(sqrt(mydata$Commodities^3 * mydata$Silver - mydata$Silver))) * (sqrt(mydata$Ethereum_price) * mydata$Silver^2) 
#Best Cost:        2278.19730780886 - best test fit

# check on data
mydata <- trainingData

sqrt(mean((mydata$Bitcoin_price-eval(ge$best$expressions))^2))
plot(trainingData$Bitcoin_price)
points(eval(ge$best$expressions), col = "red", type = "l")

mydata <- testingData

sqrt(mean((mydata$Bitcoin_price-eval(ge$best$expressions))^2))
plot(testingData$Bitcoin_price)
points(eval(ge$best$expressions), col = "red", type = "l")





# check previous best results
sqrt(mean((mydata$Bitcoin_price - eval(mydata$S.P500 * log(mydata$Google_trends) - mydata$Google_trends + mydata$Ethereum_price))^2))
plot(trainingData$Bitcoin_price)
points(mydata$NASDAQ/(sqrt(mydata$Silver)/log(mydata$Google_trends)), col = "red", type = "l")

sqrt(mean((mydata$Bitcoin_price - eval(mydata$S.P500 * log(mydata$Google_trends)))^2))
plot(trainingData$Bitcoin_price)
points(mydata$S.P500 * log(mydata$Google_trends), col = "blue", type = "l")

sqrt(mean((mydata$Bitcoin_price - eval(mydata$Commodities^2 * (mydata$Ethereum_price/mydata$T_Bill^3)))^2))
plot(trainingData$Bitcoin_price)
points(mydata$Commodities^2 * (mydata$Ethereum_price/mydata$T_Bill^3), col = "yellow", type = "l")


mydata <- drop_na(testingData)

sqrt(mean((mydata$Bitcoin_price - eval(mydata$NASDAQ/(sqrt(mydata$Silver)/log(mydata$Google_trends))))^2))
plot(testingData$Bitcoin_price)
points(mydata$NASDAQ/(sqrt(mydata$Silver)/log(mydata$Google_trends)), col = "red", type = "l")

sqrt(mean((mydata$Bitcoin_price - eval(mydata$S.P500 * log(mydata$Google_trends)))^2))
plot(testingData$Bitcoin_price)
points(mydata$S.P500 * log(mydata$Google_trends), col = "blue", type = "l")

sqrt(mean((mydata$Bitcoin_price - eval(mydata$Commodities^2 * (mydata$Ethereum_price/mydata$T_Bill^3)))^2))
plot(testingData$Bitcoin_price, ylim = c(0,200000))
points(mydata$Commodities^2 * (mydata$Ethereum_price/mydata$T_Bill^3), col = "yellow", type = "l")




# look at success of ge's with change rather than absolute value
bit_change_fit_func <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean((mydata$Bitcoin_price_change - result)^2)))
}

bit_change_percent_fit_func <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean((mydata$Bitcoin_price_change_percent - result)^2)))
}

ge <- GrammaticalEvolution(bit_grammar, bit_change_fit_func, iterations = 10000, max.depth = 8)
ge




# focus on dates where twitter data is available, and use best lag of 12
df_cleaned_twitter <- df_cleaned_twitter %>% 
  mutate(Twitter_volume_lag12=lag(Twitter_volume,12)) %>%
  na.omit()
rownames(df_cleaned_twitter) <- NULL

# remake training data
trainingData <- df_cleaned_twitter %>% filter(as.Date(Date) <= as.Date("2018-10-18",format="%Y-%m-%d"))
testingData <- drop_na(df_cleaned_twitter %>% filter(as.Date(Date) > as.Date("2018-10-18",format="%Y-%m-%d")))

# make better rules
rules_2 <- list(expr = grule(op(expr, expr), func(expr), var),
              func = grule(log, sqrt),
              op = grule('+', '-', '*', '/'),
              var = grule(dat, dat^n),
              dat = grule(mydata$Ethereum_price, mydata$Cardano_price, 
                          mydata$S.P500, mydata$DJI, mydata$NASDAQ, mydata$Gold, 
                          mydata$Silver, mydata$Commodities, mydata$T_Bill, 
                          mydata$Twitter_volume_lag12, mydata$Google_trends),
              n = grule(2,3))
bit_grammar_2 <- CreateGrammar(rules_2)
bit_grammar_2

cor(df_cleaned_ethereum$Bitcoin_price, df_cleaned_ethereum$Google_trends)

# reset mydata
mydata <- trainingData

# run ge again
ge <- GrammaticalEvolution(bit_grammar_2, bit_fit_func, iterations = 10000, max.depth = 8)
ge






# leaps and bounds for to select best features
library(leaps)
best_subset <- leaps(x = df_cleaned_ethereum[, c(3,6:13,15,20)], y = df_cleaned_ethereum[, 16],
                     nbest = 3, method = "adjr2",
                     names = colnames(df_cleaned_ethereum[, c(3,6:13,15,20)]))

data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

# plot
plot(best_subset$size, best_subset$adjr2,
     ylab="Adjusted R-squared",
     xlab="Number of variables (including intercept)")

# suggests ethereum price, oil, google trends might give best results
# alt: ethereum, silver, oil, google_trends, twitter_volume_lag12




# create a time series object and decompose it to see trends
ts <- ts(df_cleaned$Bitcoin_price_change_percent, frequency = 365)
ts.components <- decompose(ts, type = "multiplicative")
plot(ts.components) # not very effective - likely no noticeable seasonality

# Holt-Winters prediction
hw_ts <- ts(trainingData$BTC.price, frequency = 365)
hw_forecasts <- HoltWinters(hw_ts, beta = FALSE, gamma = FALSE)
plot(hw_forecasts)
hw_forecasts
sqrt(mean((df_cleaned$BTC.price - hw_forecasts$fitted)^2))
hw_predictions <- predict(hw_forecasts, n.ahead = 365, prediction.interval = T, level = 0.95)
plot(hw_forecasts, hw_predictions)