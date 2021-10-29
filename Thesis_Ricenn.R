library(neuralnet)
library(readxl)
library(forecast)

#Read in data
Data <- read_excel("Thesis2Data.xlsx", sheet = "overall")


Ricenn <- as.data.frame(Data$Rice)
Acf(Ricenn)

x1 <- Ricenn[6:75,]
x2 <- Ricenn[5:75,]
x3 <- Ricenn[4:75,]
x4 <- Ricenn[3:75,]
x5 <- Ricenn[2:75,]
x6 <- Ricenn[1:75,]
Rice <- Ricenn[7:75,]

length(x1) = 75
length(x2) = 75
length(x3) = 75
length(x4) = 75
length(x5) = 75
length(x6) = 75
length(Rice) = 75
x <- cbind(Rice,x6,x5,x4,x3,x2,x1)
x <- as.data.frame(x)
View(x)

#start cut from the last row without "NA"
x <- x[1:69,]

# Random sampling to split data into 75:35 train:test ratio
set.seed(81)
index <- sample(seq_len(nrow(x)), size = 0.75 * nrow(x))

dataTrain <- x[index,]
dataTest <- x[-index,]


max = apply(x, 2, max)
min = apply(x, 2, min)
scaled <- as.data.frame(scale(x, center = min, scale = max - min))

#Creating training and test sets
trainNN = scaled[index,]
testNN = scaled [-index,]

# fit neural network with 4 input neurons, 3 hidden neurons and one output neuron
set.seed(11)
nn <- neuralnet(Rice~x6+x5+x4+x3+x2+x1, trainNN, hidden= 5, linear.output=T)

#Plot the best replicate of the network
#plot(nn, rep="best")

#Predict values of the testData using the network
predictnn = compute(nn, testNN[,c("x6","x5","x4","x3","x2","x1")])

#Converting prediction from scale to number value
predictnn_scaled <- predictnn$net.result*(max(scaled$Rice)- min(scaled$Rice))+min(scaled$Rice)

predictNN_test = (predictnn$net.result * (max(x$Rice) - min(x$Rice))) + min(x$Rice)


#Calculating RMSE of Neural Network prediction
RMSE.NN = (sum((dataTest$Rice - predictNN_test)^2)/ nrow(dataTest))^0.5

#Calculating MAPE of Neural Network Prediction
MAPE = (sum(abs(dataTest$Rice - predictNN_test)/dataTest$Rice)/nrow(dataTest)) * 100

#Display RMSE and MAPE
print(c (RMSE.NN,MAPE))




