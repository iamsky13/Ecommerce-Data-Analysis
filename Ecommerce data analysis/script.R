#load train data
train <- read.csv("mydata.csv", header = TRUE)

str(train)

train$Sell = as.factor(train$Sell)

table(train$Sell)

table(train$city)

ecity <- function(name) {
    name <- as.character(name)
    if (length(grep("Hels", name)) > 0) {
        return("Ktm")
    }
    if (length(grep("Por", name)) > 0) {
        return("Bkt")
    }
    if (length(grep("Tamp", name)) > 0) {
        return("Lalitpur")
    }
    else {
        return("Other")
    }
}

city <- NULL
for (i in 1:nrow(train)) {
    city <- c(city, ecity(train[i, "city"]))
}

train$city <- as.factor(city)