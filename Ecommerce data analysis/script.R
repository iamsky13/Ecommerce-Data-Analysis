#load train data
train <- read.csv("mydata.csv", header = TRUE)

#structure
str(train)

#changing datatype
train$Sell = as.factor(train$Sell)

#check
table(train$Sell)

table(train$city)

#Converting city names just to make it easy 
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

table(train$city)

library(ggplot2)

ggplot(train, aes(x = city, fill = factor(Sell))) +
geom_bar(width = 0.5) +
xlab("City") +
ylab("Number") +
labs(fill = "Sells")


ggplot(train, aes(x = Sex, fill = factor(Sell))) +
geom_bar(width = 0.5) +
facet_wrap(~city) +
xlab("Gender") +
ylab("Number") +
labs(fill = "Sells")

ggplot(train, aes(x = Sib_Sp, fill = factor(Sell))) +
geom_bar(width = 0.5) +
facet_wrap(~city) +
xlab("sibling spouse") +
ylab("Number") +
labs(fill = "Sells")

ggplot(train, aes(x = Par_ch, fill = factor(Sell))) +
geom_bar(width = 0.5) +
facet_wrap(~city) +
xlab("parent children") +
ylab("Number") +
labs(fill = "Sells")

