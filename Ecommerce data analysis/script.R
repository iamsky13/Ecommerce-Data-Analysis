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

#city vs sell
ggplot(train, aes(x = city, fill = factor(Sell))) +
geom_bar(width = 0.5) +
xlab("City") +
ylab("Number") +
labs(fill = "Sells")

#sex vs sell
ggplot(train, aes(x = Sex, fill = factor(Sell))) +
geom_bar(width = 0.5) +
xlab("Gender") +
ylab("Number") +
labs(fill = "Sells")

#both vs sell
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

summary(train$Cost)

ggplot(train, aes(x = Payment, fill = factor(Sell))) +
geom_bar(width = 0.5) +
facet_wrap(~city) +
xlab("parent children") +
ylab("Number") +
labs(fill = "Sells")

#chk data repeat
length(unique(as.character(train$Name)))

library(stringr)


extractTitle <- function(name) {
    name <- as.character(name)

    if (length(grep("Miss.", name)) > 0) {
        return("Miss.")
    } else if (length(grep("Master.", name)) > 0) {
        return("Master.")
    } else if (length(grep("Mrs.", name)) > 0) {
        return("Mrs.")
    } else if (length(grep("Mr.", name)) > 0) {
        return("Mr.")
    } else {
        return("Other")
    }
}

titles <- NULL
for (i in 1:nrow(train)) {
    titles <- c(titles, extractTitle(train[i, "Name"]))
}
train$title <- as.factor(titles)

ggplot(train, aes(x = title, fill = Sell)) +
geom_bar(width = 0.5) +
xlab("Titles") +
ylab("number") +
labs(fill = "Sell")

ggplot(train, aes(x = title, fill = Sell)) +
geom_bar(width = 0.5) +
facet_wrap(~city) +
xlab("Titles") +
ylab("number") +
labs(fill = "Sell")


ggplot(train, aes(x = Age, fill = Sell)) +
  facet_wrap(~Sex + city) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

#boy

boys <- train[which(train$title == "Master."),]
summary(boys$Age)

#miss
miss <- train[which(train$title == "Miss."),]
summary(miss$Age)

#familysize
temp.sibsp <- c(train$Sib_Sp)
temp.parch <- c(train$Par_ch)
train$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(train, aes(x = family.size, fill = Sell)) +
  stat_count(width = 1) +
  facet_wrap(~city + title) +
  ggtitle("city, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0, 150) +
  labs(fill = "Sell")



library(randomForest)

rf.train.1 <- train[, c("city", "title")]
rf.label <- (train$Sell)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train a Random Forest using pclass, title, & sibsp
rf.train.2 <- train[, c("city", "title", "Sib_Sp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a Random Forest using pclass, title, & parch
rf.train.3 <- train[, c("city", "title", "Par_ch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


# Train a Random Forest using pclass, title, & family.size
rf.train.4 <- train[1:891, c("city", "title", "family.size")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)