# Week 2 Critical thinking
#website for the ggplot boxplots
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization 

#Webiste for histograms
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
#These are the necesary libraries used


#libraries used for graphs
install.packages("tidyverse")

library(ggplot2)

install.packages("ggplot2")

install.packages("ggiraph")

install.packages("ggiraphExtra")

install.packages("plyr")

install.packages("ggpubr")
library(ggpubr)

install.packages("plyr")
library(plyr)



#Importing the data set from desktop

cereals.df<-read.csv(file="ADD FILE PATH CSV CAN BE DOWNLOADED FROM DATA SETS REPOSITORY",
               head=TRUE)

#Testing the upload and performing basic summary statistics of the data frame df
head(cereals.df)
summary(cereals.df)

#perfomring procedure outline in section 4.4 for df

#mean, standard dev, min, max, median, length, and missvalues for calories
#variables
mean(cereals.df$calories)
sd(cereals.df$calories)
min(cereals.df$calories)
max(cereals.df$calories)
median(cereals.df$calories)
length(cereals.df$calories)

#find the number of missing values of variable calories
sum(is.na(cereals.df$calories))


#mean, standard dev, min, max, median, length, and missvalues for all variables
#variables 
#issues with including categorical or non-numerical values in the dataframe, so non-numerical values
#are excluded using the num.cereals.df
num.cereals.df <- data.frame(cereals.df$calories, cereals.df$protein, cereals.df$fat, cereals.df$sodium, cereals.df$fiber, 
               cereals.df$carbo, cereals.df$sugars, cereals.df$potass, cereals.df$vitamins, cereals.df$shelf,
               cereals.df$weight, cereals.df$cups)

data.frame(mean=sapply(num.cereals.df, mean),
           sd=sapply(num.cereals.df, sd),
           min=sapply(num.cereals.df, min),
           max=sapply(num.cereals.df, max),
           median=sapply(num.cereals.df, median),
           length=sapply(num.cereals.df, length),
           miss.val=sapply(num.cereals.df, function(x)
             sum(length(which(is.na(x))))))

#correltation table
cor(num.cereals.df)

#aggrigation by a single variable fat
table(num.cereals.df$cereals.df.fat)


#create bins if size 1
num.cereals.df$cereals.df.calories.bin <- .bincode(num.cereals.df$cereals.df.calories, c(1:9))

#Average of protien by fat and calories
#aggregate() us agrument by= to define the list of aggregating variables
#and FUN= as an aggregating function
aggregate(num.cereals.df$cereals.df.protein, by=list(cal=num.cereals.df$cereals.df.calories.bin, pt=num.cereals.df$cereals.df.fat), FUN=mean)
#returned <0 row>

######################################################
#PIVOT TABLES
#creating pvito tables using functions melt() and cast()
#libraries necessary for pivot tables
library(reshape2)
#create bins of size 1 this is a repeated function
cereals.df$calories.bin <- .bincode(cereals.df$calories, c(1:9))
#use melt() to stack a set of columns into a single column of data
mlt <- melt(cereals.df, id=c("calories.bin", "protein"), measure=c("fat")) #
head(mlt, 5)

# use cast() to reshape data and generate pivot table

acast(mlt, calories.bin ~ cereals.df$protein, subset=variable.names("fiber"),
     margines=c("grand_row", "grand_col"), mean)
                                                     
dcast(mlt, calories.bin ~ cereals.df$protein, subset=variable.names("fiber"),
      margines=c("grand_row", "grand_col"), mean)
#conclusion of the 4.4 exersize

#additiona of the graphs
#creating two boxplots to call
a <- ggplot(cereals.df, aes(x=cereals.df$protein, y=cereals.df$protein)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)


b <- ggplot(cereals.df, aes(x=cereals.df$fat, y=cereals.df$fat)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.sixe=4)


#creating two histograms to call
c <- ggplot(cereals.df, aes(x=cereals.df$protein)) + 
  geom_histogram(aes(y=..density..), color="black", fill="darkblue") + 
  geom_density(alpha=.2, fill="#FF6666")


d <- ggplot(cereals.df, aes(x=cereals.df$fat)) + 
  geom_histogram(aes(y=..density..), color="black", fill="lightblue") + 
  geom_density(alpha=.2, fill="#FF6666")


ggarrange(a, b, c, d,
          labels = c("Protein_Boxplot", "Fat_Boxplot", "Protein_Histogram", "Fat_Histogram"),
          ncol = 2, nrow = 2)











