library(naivebayes)
#wczytanie danych
data <- read.csv("weatherAUS.csv")
data <- na.omit(data) #usuwa wiersze z nieuzupełnionymi danymi
data <- data[1:1500,c(3:5,7:17,20:23)]


data$RainTomorrow <- as.factor(data$RainTomorrow)
#modyfikacja ostatniej kolumny w celu stworzenia jej jako faktor


data$RainToday <- as.factor(data$RainToday)

data <- data[, sapply(data, function(x) !is.character(x))]



#lista z podziałami
ratios = list(c(0.3,0.7),c(0.5,0.5),c(0.7,0.3))

jakosc_klasyfikatora = c()
odchylenie = c()
for(ratio in ratios)
{
  
  cf_errors = c()
  for(i in 1:100)
  {
    idxs <- sample(2,nrow(data),replace = T,prob = ratio)
    train <-data[idxs==1,]
    test <- data[idxs==2,]
    
    model=naive_bayes(RainTomorrow  ~., data=train, usekernel = T)
    #plot(model)
    p = predict(model, test)
    
    cf=table(p,test$RainTomorrow)
    cf
    pcf=cf/sum(cf)
    ratio
    cf_errors = append(cf_errors, sum(diag(cf))/sum(cf))
    
  }
  odchylenie = append(odchylenie, sd(cf_errors))
  jakosc_klasyfikatora = append(jakosc_klasyfikatora,mean(cf_errors))
  
}
jakosc_klasyfikatora
odchylenie
plot(model)

