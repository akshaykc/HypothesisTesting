#install.packages("e1071")

#19.11.2    K-Fold Cross-Validated Paired t Test

#Procure data and divide into k sets
library(e1071)
mydata <- read.table("krkopt.csv", header = TRUE, sep=",")

k <- 10
decisonBoundaryProb <- 0.10
degreeOfFreedom <- k-1

kSamples <- sample(rep(1:k, ceiling(nrow(mydata)/k)))[1:nrow(mydata)]
mydata$elem <- kSamples

#Repeat learning and predicton for K sets
for (elem in 1:k){
  trainingData <- mydata[elem != mydata$elem,]
  testingData <- mydata[elem == mydata$elem,]
  
  withoutBlackKingFit <- naiveBayes(optimalDepthOfWin ~ WhiteKingFile+WhiteKingrankRow+WhiteRookfile+WhiteRookrank, data=trainingData)
  withoutWhiteRookFit <- naiveBayes(optimalDepthOfWin ~ WhiteKingFile+WhiteKingrankRow+BlackKingfile+BlackKingrank, data=trainingData)

  mydata[elem==mydata$elem, "outwithoutBlackKing"] = predict(withoutBlackKingFit, newdata=testingData, type="class")
  mydata[elem==mydata$elem, "outWithoutWhiteRook"] = predict(withoutWhiteRookFit, newdata=testingData, type="class")
}

#Get the error values from prediction data
kFoldLevels <- unique(mydata[, "elem"])
  
withoutBlackKingModelErrors <- mydata[, "optimalDepthOfWin"] != mydata[, "outwithoutBlackKing"]
withoutBlackKingModelAggError <- sapply(kFoldLevels, function(elem) sum(withoutBlackKingModelErrors[mydata[, "elem"] == elem])/sum(mydata[, "elem"] == elem))

withoutWhiteRookModelErrors <- mydata[, "optimalDepthOfWin"] != mydata[, "outWithoutWhiteRook"]
withoutWhiteRookModelAggError <- sapply(kFoldLevels, function(elem) sum(withoutWhiteRookModelErrors[mydata[, "elem"] == elem])/sum(mydata[, "elem"] == elem))

#Get t-statistic according to the chapter 19.11.2
m <- mean(withoutBlackKingModelAggError - withoutWhiteRookModelAggError)
S <- sqrt(var(withoutBlackKingModelAggError - withoutWhiteRookModelAggError))
rootK <- sqrt(k)

tTestStat <- (rootK * m)/ S

probVector <- c(decisonBoundaryProb/2, 1-decisonBoundaryProb/2)
tStatDecisonBoundary <- qt(probVector, df = degreeOfFreedom )

#Conclude and print the results
printResults <- function(tValue, tDecisonBoundary)
{
  cat(paste("\nt value = ", tValue))
  cat(paste("\nInterval for given ",decisonBoundaryProb,"significance limit is ",tDecisonBoundary[1],tDecisonBoundary[2]))
  cat("\n\nHypotheses : Both classifiers perform with similar error rates")
  
  if((tValue >= tDecisonBoundary[1])&&(tValue<=tDecisonBoundary[2])){
    cat("We fail to reject the above mentioned hypothese")
  } else {
    cat("\nWe can reject the above mentioned hypotheses")
  }
}

printResults(tTestStat, tStatDecisonBoundary)


