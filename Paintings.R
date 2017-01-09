#load libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Hmisc)

#read file
paint <- read.csv("c:/users/alex/desktop/R/exercise data/Paintings/paintings.csv", header = T, sep = ";")

#structure of data
str(paint)
describe(paint)
head(paint)
tail(paint)
names(paint)

#restructuring data
#Creating Summary Columns of Data.

paint$mean <- apply(paint[,c(1:48)], 1, mean)
paint$median <- apply(paint[,c(1:48)], 1, median)
paint$min <- apply(paint[,c(1:48)], 1, min)
paint$max <- apply(paint[,c(1:48)], 1, max)

#Removing Uneeded columns
paint <- paint %>%
  select( -S1,-S2,-S3,-S4,-S5,-S6,-S7,-S8,-S9,-S10,-S11,-S12,-S13,-S14,-S15,-S16,-S17,-S18,-S19,-S20,
          -S21,-S22,-S23,-S24,-S25,-S26,-S27,-S28,-S29,-S30,-S31,-S32,-S33,-S34,-S35,-S36,-S37,-S38,
          -S39,-S40,-S41,-S42,-S43,-S44,-S45,-S46,-S47,-S48)

#Plotting the data
#Distribution of Mean
hist(paint$mean , main = "Histogram of Mean Score", xlab = "Mean" , ylab = "Count")

#Mean by artist
ggplot(paint , aes(paint$artist, fill = paint$artist ,paint$mean)) + geom_bar(stat = "identity") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + labs(x= "Artist",y= "Mean" , title = "Artist Mean Score")

#Mean score of painting by art.movement
barplot(paint$mean[which(paint$art.movement=="Renaissance")] , main = "Barplot of Mean Score per Painting of Renaissance Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("The Birth of Venus", "Lady With an Ermine","Three Graces") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Baroque")] , main = "Barplot of Mean Score per Painting of Baroque Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Entombment", "Rokeby Venus","The Night Watch") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Neoclassicism")] , main = "Barplot of Mean Score per Painting of Neoclassicism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Napoleon Crossing the Alps", "Mademoiselle Caroline Riviere","The Nude Maja") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Romanticism")] , main = "Barplot of Mean Score per Painting of Romanticism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Liberty Leading the People", "The Raft of the Medusa","The Cross in the Mountains") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Impressionism")] , main = "Barplot of Mean Score per Painting of Impressionism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("The Luncheon on the Grass", "Impression, Sunrise","Ballet Rehearsal") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Post-Impressionism")] , main = "Barplot of Mean Score per Painting of Post-Impressionism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Tahitian Women on the Beach", "Sunflowers","The Card Players") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Symbolism")] , main = "Barplot of Mean Score per Painting of Symbolism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("The Cyclops", "The Scream","Jason and Medea") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Art Nouveau")] , main = "Barplot of Mean Score per Painting of Art Nouveau Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Four Seasons", "The Kiss","The Letter") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Cubism")] , main = "Barplot of Mean Score per Painting of Cubism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Guernica", "Violin and Candlestick","Portrait of Picasso") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Abstract art")] , main = "Barplot of Mean Score per Painting of Abstract art Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Moscow. Red Square", "Composition II in Red, Blue, and Yellow","Black Square") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Surrealism")] , main = "Barplot of Mean Score per Painting of Surrealism Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("The Temptation of St. Anthony", "The Hat Makes the Man","The False Mirror") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Op art")] , main = "Barplot of Mean Score per Painting of Op-Art Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Zebras", "Cataract 3","Color Vertical") , ylim = c(0,4))
barplot(paint$mean[which(paint$art.movement=="Pop art")] , main = "Barplot of Mean Score per Painting of Pop-art Art Movement", 
        col = paint$art.movement , ylab = "Mean", xlab = "Paintings" , names.arg = c("Campbell's Soup Cans", "0 through 9","Adriana") , ylim = c(0,4))

#mean score per art.movement
ggplot(paint, aes(paint$art.movement, fill = paint$art.movement, paint$mean/3)) + geom_bar(stat = "identity")+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + labs(x= "Art Movement",y= "Mean Score" , title = "Mean Score per Art Movement")

#Score per painting
ggplot(paint, aes(paint$painting, fill = paint$painting, paint$mean)) + geom_bar(stat = "identity")+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + labs(x= "Painting",y= "Mean Score" , title = "Mean Score of Paintings")+ 
  guides(fill=guide_legend(title="Painting"))

