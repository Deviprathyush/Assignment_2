### ASSIGNMENT 2

## Question 1

data("iris")

data_set <- iris
data_set1 <- subset(data_set,data_set$Species=='setosa')
boxplot(data_set1[1:4],beside=T)

data_set2 <- subset(data_set,data_set$Species=='versicolor')
boxplot(data_set2[1:4],beside=T)

data_set3 <- subset(data_set,data_set$Species=='virginica')
boxplot(data_set3[1:4],beside=T)

plot(data_set$Sepal.Length,data_set$Petal.Length,col=data_set$Species)

## we can observe in scatter plot by coloring based on species it forms three clusters, so we 
## can conclude that petal lenght and sepal length will depends on species

## Question 2
library(imager)


flip <- function(img){
  col.mat <- as.array(img[, ,1, ])
  
  dims <- dim(col.mat)
  print(dims)
  rot <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      rot[i, j, ] <- col.mat[dims[1]-i+1,j, ]
    }
  }
  
  return(rot)
}

## Question 3

library(MASS)

data <- ships

dataA <- subset(data,data$type == 'A')
dataB <- subset(data,data$type == 'B')
dataC <- subset(data,data$type == 'C')
dataD <- subset(data,data$type == 'D')
dataE <- subset(data,data$type == 'E')

plot(dataA$service,dataA$incidents,type = "o",col = "red", xlab = "service", ylab = "incidents", main = "ships data",xlim = c(0,45000),ylim = c(0,60))
lines(dataB$service,dataB$incidents,type = "o",col = "yellow")
lines(dataC$service,dataC$incidents,type = "o",col = "blue")
lines(dataD$service,dataD$incidents,type = "o",col = "green")
lines(dataE$service,dataE$incidents,type = "o",col = "pink")
co = 0
co[1] = cor(dataA$service,dataA$incidents)
co[2] = cor(dataB$service,dataB$incidents)
co[3] = cor(dataC$service,dataC$incidents)
co[4] = cor(dataD$service,dataD$incidents)
co[5] = cor(dataE$service,dataE$incidents)

### here by observing this graph we can't say b is least trustworthy ship as it has done so many months of service 
## hence given statement is disproved

## Question 4

library(rvest)
library(tidyverse)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")

questions <- html_elements(html,".s-post-summary--content-title a") %>% html_text()
values <- html_elements(html,".s-post-summary--stats-item-number") %>% html_text()
views = 0
answers = 0
votes = 0
for(i in 0:14){
  votes[i+1] = values[3*i+1]
}
for(i in 0:14){
  answers[i+1] = values[3*i+2]
}
for(i in 0:14){
  views[i+1] = values[3*i+3]
}
data_frame <- data.frame(questions,views,answers,votes)

## Question 5

t = numeric(1000)
t[1:1000] = 0


for(i in 1:1000){
  k = 1
  p = 1
  while(k == 1){
    k = sample(c(0,1),size = 1,prob = c(1-p,p))
    t[i] = t[i] + 1
    p = p - (1/100)
  }
}

ans = mean(t)
