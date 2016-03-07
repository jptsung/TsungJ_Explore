#1. Figure out how to generically extract all numeric columns from any data frame
#the method developed can be used on any data

require("ggplot2") #uploads the ggplot2 package
data(diamonds) #uploads the data file "diamonds"

extract <- diamonds[sapply(diamonds,is.numeric)] #assign vector to 'extract'
#function sapply checks each column in data for numerical data 
#after all columns are checked, columns with data will be extracted
#returns data in a vector, matrix, or list

str(extract) #displays structure of extract to see the data extracted

#2. Create a data frame that contains each pair of variable names in the first column
#in a single string separated by a -, eg. for x & y, should form "x-y"
#use paste function and Pearson correlation coefficient in second column
#using function that calculates correlation coefficients and don't repeat pairs

correl_coeff= numeric (0) #sets initial value of correl_coeff as 0
#makes sure there are no repeating pairs

for(i in 1:ncol(combn((1:ncol(extract)),2))) #gives combinations of every pair of the variables in 'extract'
#for i counts up by 1 from 1 to last pair
#outer "ncol" function return the number of columns (pairs) in the data

correl_coeff=c(correl_coeff,cor(extract[[combn((1:ncol(extract)),2)[1,i]]],
                                  extract[[combn((1:ncol(extract)),2)[2,i]]]))
#calculates the correlation coefficient of each pair
#places corresponding correlation coefficients in second column, "correl_coeff"

pairs=paste(combn(colnames(extract),2)[1,],'-',combn(colnames(extract),2)[2,],sep='')
#defines 'pairs' as first column with variable pairs with corresponding correlation coefficients
#pastes a '-' between variable names

frame=data.frame(pairs,correl_coeff)
# Creates a new data frame that contains each pair of variable names with corresponding correl coeff

frame #outputs the data frame 

#3 Create and label a scatterplot for every pair of numeric variables
#Add a title to the plot that contains the calculated Pearson correlation coefficient of the plotted pairs
#extract all numeric columns so method can be used in any data frame

require(ggplot2) #loads package ggplot2

data(diamonds) #loads data 'diamonds'

for(i in 1:ncol(combn((1:ncol(extract)),2)))
{
scatterplot <- ggplot(extract,aes(x=extract[[combn((1:ncol(extract)),2)[1,i]]], 
                                  y=extract[[combn((1:ncol(extract)),2)[2,i]]])) +
                      labs(x=combn(colnames(extract),2)[1,i],
                           y=combn(colnames(extract),2)[2,i])+
                      geom_smooth(method=lm) +
                      geom_point(shape=16) + 
                      ## Prof G: Looks like the reference to r in the next line should be frame so
                      ## Prof G: I commented out your line and fixed it.
                      ## Prof G: ggtitle(paste('Correlation Coefficient=',r[i,2]))
                      ggtitle(paste('Correlation Coefficient=',frame[i,2]))
#using data 'extract', the x and y aes records the values of the ith pairs
#labs() labels the x and y aes with the pair names
#geom_smooth() adds a regression line as linear lm
#geom_point() specifies the plotted points to be filled in circles
#ggtitle() pastes the title of the scatterplots with the Correl Coefficient

print(scatterplot) #prints all the scatterplot pairs in separate graphs

}
