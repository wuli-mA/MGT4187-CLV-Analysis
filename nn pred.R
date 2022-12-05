library(neuralnet)

setwd('E:\\Couse Materials\\MGT 4187\\Final project\\IBM Watson')
NeuralPriceAdsData <-read.csv("E:\\Couse Materials\\MGT 4187\\Final project\\IBM Watson\\Labeled_dummy.csv")
lm1 <- lm(CLV ~ ., data = NeuralPriceAdsData)  
HighLeverage <- cooks.distance(lm1) > (4/nrow(NeuralPriceAdsData))
LargeResiduals <- rstudent(lm1) > 3
NeuralPriceAdsData <- NeuralPriceAdsData[!HighLeverage & !LargeResiduals,]
lm1<-lm(write ~ read +ses + prog+race.f, data = hsb2)  

set.seed(224)
col_types <- as.vector(sapply(NeuralPriceAdsData, class))
NeuralPriceAdsData <- NeuralPriceAdsData[,-which(col_types=='character')]

# Dividing training and testing set following the 80/20 rule.
# This is to get the row index of 267 random rows of data 
train_ind_mlr <- sample(seq_len(nrow(NeuralPriceAdsData)), size = floor(nrow(NeuralPriceAdsData) * 0.8))


# Normalise data
maxs <- apply(NeuralPriceAdsData, 2, max) 
mins <- apply(NeuralPriceAdsData, 2, min)
# Now the scaled version of the data are saved as "scaled".
scaled <- as.data.frame(scale(NeuralPriceAdsData, center = mins, scale = maxs - mins))
# Divide the data into training and testing set following the same 80/20 rule:

train_ind_nn <- sample(seq_len(nrow(NeuralPriceAdsData)), size = floor(nrow(NeuralPriceAdsData) * 0.8))

train_nn <- scaled[train_ind_nn,]
test_nn <- scaled[-train_ind_nn,]
nn <- neuralnet(formula = CLV ~ ., 
                data = train_nn,
                hidden = 2, 
                linear.output=TRUE, 
                err.fct = 'sse')
plot(nn)
summary(nn)
nn$result.matrix['error',]
fitted.train_nn <- nn$net.result[[1]] * (max(NeuralPriceAdsData$CLV)-min(NeuralPriceAdsData$CLV))+min(NeuralPriceAdsData$CLV)

#use the row index to get the original value of sales in train dataset. 
train_nn.r <- NeuralPriceAdsData$CLV[train_ind_nn]

#calculate the Root Mean Squared Error of train dataset
rmse.train_nn <- (sum((train_nn.r - fitted.train_nn )^2)/nrow(fitted.train_nn))^0.5
rmse.train_nn
