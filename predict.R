pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


if(! 'pml-training.csv' %in% dir()) download.file(url = 
        'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',
        destfile = './')
if(! 'pml-testing.csv' %in% dir()) download.file(url = 
        'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',
        destfile = './')
data <- read.csv(file = 'pml-training.csv', stringsAsFactors = F, header = T, 
                 sep = ',', na.strings = c('','NA'))
target <- read.csv(file = 'pml-testing.csv', stringsAsFactors = F, header = T, 
                  sep = ',', na.strings = c('','NA'))
#delete na's
id <- which(!is.na(target[1,]))
#delete physically nonsense data, such as tester names, window numbers, time tag, etc
data <- data[,id[8:60]]
target <- target[,id[8:60]]
#seeking further methods to decrease df of variables, by PCA
data$classe <- as.factor(data$classe)
library(caret)
inTrain <- createDataPartition(y = data$classe, p = .75, list = F)
training <- data[ inTrain, ]
testing <- data[-inTrain, ]
rfFit <- randomForest(x = data[, 1:(ncol(data)-1)],
                      y = data$classe,
                      #mtry = 3,
                      importance = T,
                      na.action = na.omit
                      )
plsClass <- predict(rfFit, newdata = testing)

confusionMatrix(testing$classe, plsClass)

ans <- as.character(predict(rfFit, newdata = target))

pml_write_files(ans)
