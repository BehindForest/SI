library(class)
#wczytanie danych
data <- read.csv("weatherAUS.csv")
data <- na.omit(data) #usuwa wiersze z nieuzupełnionymi danymi
data <- data[1:1500, c(3:5,7:17,20:23)] # usuwanie niepotrzebnych kolumn nie wnoszących za wiele


data$RainTomorrow[data$RainTomorrow == "No"] <- 0
data$RainTomorrow[data$RainTomorrow == "Yes"] <- 1
data$RainTomorrow <- as.numeric(data$RainTomorrow)
#modyfikacja ostatniej kolumny w celu stworzenia jej numerycznie

data$RainToday[data$RainToday == "No"] <- 0
data$RainToday[data$RainToday== "Yes"] <- 1
data$RainToday <- as.numeric(data$RainToday)

data <- data[, sapply(data, function(x) !is.character(x))]
#usuwanie tekstowych kolumn

#lista z podziałami
#ratios = list(c(0.3,0.7),c(0.5,0.5),c(0.7,0.3))
ratio = c(0.3,0.7)
#lista z różńymi wartościami k
k_list = list(3,5,7)
cf_sd = c()
cf_mean = c()
for(k in k_list)
{
  for(r in ratios)
  {
    cf_errors = c()
    for(i in 1:100)
    {
      r
      idxs <- sample(2,nrow(data),replace = T,prob = r)
      trainData <- data[idxs==1,]
      testData <- data[idxs==2,]
      
      train=trainData[1:14]
      test=testData[1:14] 
      
      cl_test=testData[,15]
      cl_train=trainData[,15]
      cf
      ratio
      model <- knn(train,test,cl=cl_train,k=k) 
      cf = table(testData[,15],model)
      cf_errors = append(cf_errors,sum(diag(cf))/sum(cf))
    }
    cf_sd =append(cf_sd,sd(cf_errors))  #sd to ochylenie standardowe
    cf_mean = append(cf_mean,mean(cf_errors)) # srednia
    
  }
}



result_table <- data.frame(k=c(3,3,3,5,5,5,7,7,7),sd=c(cf_sd),mean = c(cf_mean))

result_table
