# Hw2

library(openxlsx)
library(data.table)
library(ggplot2)
setwd('C:/Users/KaposztassyG/Documents/_CEU/R/RHF/')
df <- read.xlsx('mtcars.xlsx') 

#1.Load the content of the https://bit.ly/mtcars-csv CSV file and save as  df  (check the variable names in the manual of  mtcars )
#2.Transform  df  to a  data.table  object
setDT(df)
#3.Count the number of cars with  4  gears
df[,.N]
#4.Count the number of cars with  4  gears and less than 100 horsepower
df[gear == 4 & hp < 100, .N]
#5.What's the overall weight of cars with  4  cylinders?
df[gear == 4, .(.N, mean(wt))]
#6.Which car is the heaviest?
head(setorder(df, -wt),1)
#7.Plot the distribution of weights
hist(df$wt)
ggplot(df, aes(x=wt)) + geom_histogram() + theme_bw() 
#8.Plot the distribution of gears
hist(df$gear)
#9.Plot the distribution of weights per gears
ggplot(df, aes(x=wt)) + geom_histogram() + theme_bw() 
#10.Plot the average weight per gears
hist(df$wt/df$gear)
#11.Which car has the best fuel consumption?

#12.Plot the weight and horsepower of cars
ggplot(df, aes(x=wt, y=hp)) + geom_point()
#13.Add a linear trend line to the above plot
ggplot(df, aes(x=wt, y=hp)) + geom_point() + geom_smooth(method='lm')
#abline(fit, col = 'red')

#14.Add a 3rd degree polynomial model to the above plot
ggplot(df, aes(x=wt, y=hp)) + geom_point() + geom_smooth(method='lm')
?geom_smooth
15.Fit a linear model on  hp  to predict weight
16.Estimate the weight based on the above model for  Lotus Europa 
17.Compute a new variable in the dataset for the ratio of  wt  and  hp 
18.Plot the distribution of this new variable on a boxplot
19.Create an aggregated dataset on  mtcars  including the average  hp  and  wt  grouped by the number of gears
20.Merge the average  hp  and  wt  per gears from the above dataset to the original  df  object based on the number of gears
21.Compute a new variable for fuel consumption using the "liters per 100 kilometers" unit based on  mpg 
22.Which car has the best fuel consumption?
23.Compute  wt2  to store the weight in kilograms based on  wt 
24.Apply k-means clustering on the dataset to split the observations into 3 groups
25.Perform hierarchical clustering on the dataset and plot the dendogram
26.Build a decision tree to tell if a car has automatic or manual transmission
27.Visualize the above decision tree
28.Create a confusion matrix for the above model
29.Use the k-NN algorithm to fit a similar model and decide on the best number of neighbors to use
