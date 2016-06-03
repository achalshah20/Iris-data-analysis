# Author: Achal Shah
# Date: 02/25/2016
# Title: Iris data analysis
# Description: Basic statistics analysis and visulization on iris dataset.

library(dplyr)
library(ggplot2)

#Directory
#setwd("")

#Read Iris Data and extract features
iris.data = read.csv("data/iris.data.txt",col.names = c("Sepal.length","Sepal.Width","Petal.length","Petal.width","Species"),header = F)
iris.data$Species = factor(iris.data$Species,labels = c("Sentosa","Versicolor","Virginica"))
iris.data.features = iris.data[,1:4]


#a) Find mean and Sd
features.mean = iris.data.features %>% summarise_each(funs(mean))
features.sd = iris.data.features %>%  summarise_each(funs(sd))

print(features.mean)
print(features.sd)

#b)Find mean and SD for each owner
mean.by.species = iris.data %>% group_by(Species) %>% select(1:4) %>% summarise_each(funs(mean))
sd.by.species = iris.data %>% group_by(Species) %>% select(1:4) %>% summarise_each(funs(sd))

print(mean.by.species)
print(sd.by.species)

#c)Draw four box plots, one for each feature

#Including outliers
Sepal.length.boxplot = ggplot(iris.data,aes(y = Sepal.length,x = factor(Species),fill = Species)) + 
        stat_boxplot(geom ='errorbar') +
        geom_boxplot() +
        xlab("Species") +
        ylab("Sepal length") +
        ggtitle("Sepal length of different species")


Sepal.Width.boxplot = ggplot(iris.data,aes(y = Sepal.Width,x = factor(Species),fill = Species)) + 
        stat_boxplot(geom ='errorbar') +
        geom_boxplot() +
        xlab("Species") +
        ylab("Sepal width") +
        ggtitle("Sepal width of different species")



Petal.length.boxplot = ggplot(iris.data,aes(y = Petal.length,x = factor(Species),fill = Species)) + 
        stat_boxplot(geom ='errorbar') +
        geom_boxplot() +
        xlab("Species") +
        ylab("Petal length") +
        ggtitle("Petal length of different species")


Petal.width.boxplot = ggplot(iris.data,aes(y = Petal.width,x = factor(Species),fill = Species)) + 
        stat_boxplot(geom ='errorbar') +
        geom_boxplot() +
        xlab("Species") +
        ylab("Petal width") +
        ggtitle("Petal width of different species")

#Saving plots
ggsave("../../Images/Que1/Original/Sepal_length_boxplot.jpg",Sepal.length.boxplot)
ggsave("../../Images/Que1/Original/Sepal_Width_boxplot.jpg",Sepal.Width.boxplot)
ggsave("../../Images/Que1/Original/Petal_length_boxplot.jpg",Petal.length.boxplot)
ggsave("../../Images/Que1/Original/Petal_Width_boxplot.jpg",Petal.width.boxplot)


#with jitter
Sepal.length.boxplot.jitter = Sepal.length.boxplot + geom_jitter()
Sepal.Width.boxplot.jitter = Sepal.Width.boxplot +  geom_jitter()
Petal.length.boxplot.jitter = Petal.length.boxplot + geom_jitter()
Petal.width.boxplot.jitter = Petal.width.boxplot +  geom_jitter()

#Saving plots with jitter
ggsave("../../Images/Que1/Jitter/Sepal_length.boxplot_jitter.jpg",Sepal.length.boxplot.jitter)
ggsave("../../Images/Que1/Jitter/Sepal_Width.boxplot_jitter.jpg",Sepal.Width.boxplot.jitter)
ggsave("../../Images/Que1/Jitter/Petal_length.boxplot_jitter.jpg",Petal.length.boxplot.jitter)
ggsave("../../Images/Que1/Jitter/Petal_width.boxplot_jitter.jpg",Petal.width.boxplot.jitter)

