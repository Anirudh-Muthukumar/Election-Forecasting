library(ggplot2)
library(maps)
library(ggmap)


#Loading States map
statesMap = map_data("state")

str(statesMap)

nrow(table(statesMap$group))

#Plotting States map using ggplot()
ggplot(statesMap, aes(x=long , y=lat, group=group)) + geom_polygon(fill="white", color="black")

#Loading the CSV file containing 2013 election polling details
polling = read.csv("PollingImputed.csv")

str(polling)

#Splitting the data into Train and Test set

Train = subset(polling, polling$Year>=2004 & polling$Year<=2008)
Test = subset(polling, polling$Year==2012)

#Logistic regression model

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction>0.5)

#Creating a data frame of predictions and state for plotting
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

#Number of states with prediction Republican(1) or Democratic(0) for 2012
t = table(predictionDataFrame$TestPredictionBinary)

#Average predicted Probability for 2012 
mean(TestPrediction)


#Converting state names to lower case
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#Merging predicting Data Frame with map data
predictionMap = merge(statesMap, predictionDataFrame, by="region")
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)

#Coloring States by prediction
ggplot(predictionMap, aes(x=long, y=lat, group = group, fill=TestPredictionBinary)) + geom_polygon(color="black")
#LightBlue represents Republican prediction


ggplot(predictionMap, aes(x=long, y=lat, group = group, fill=TestPredictionBinary)) + geom_polygon(color="black")+scale_fill_gradient(low="blue", high="red", guide= "legend", breaks=c(0,1), labels=c("Democrat", "Republican"), name="Predictions for 2012")

str(predictionMap)
head(subset(predictionMap, Test.State=='Florida'))

#We incorrectly predicted Republicans won in Florida instead of Democrats 
head(subset(predictionMap, Test.State=='Florida'))


#Since we have just predicted one state with close contention incorrectly, our model does a good job in predicting the 2012 US presidential election results
