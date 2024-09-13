#AnnVenditti_Unit1_BIOL672.R

#packages loaded
library(ggplot2)
library(readr)
library(reshape2)

#data files used and clear environment
rm(list = ls())


#random number samples of 5000 numbers 
#x= represents the number pool to choose from, size = grab certain amount of numbers
#replace = TRUE means each number can be pulled more than once
set.seed(0)
mydata<-sample(x = 1:100, size = 5000, replace = TRUE)

##to make data frame but cant find mean so didnt use
#n= amount of samples, min and max are number pool
#mydata<-as.data.frame(matrix(round(runif(n=5000, min=1, max=100), 0)))
View(mydata)

#script for sample mean and sample standard deviation of data
Mydata_mean<-mean(mydata)
Mydata_sd<-sd(mydata)


#histogram script with density line; not ideal changed to use ggplot2
#setting probability to true 
#hist(mydata, probability = TRUE, xlab = "Sample Numbers", ylab = "Density", col = "papayawhip", )
#add density line to histogram
#lines(density(mydata))



######SECOND QUESTION#######
#using ggplot2 code
##make mydata into data.from
density_plot<-ggplot(data = data.frame(mydata), aes(x = mydata)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "white", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Line",
       x = "Value",
       y = "Density")
  print(density_plot)
#adding normal bellcurve to the model
##make mydata into data.frame
normal_plot<-ggplot(data = data.frame(mydata), aes(x = mydata)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "white", color = "black") +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = Mydata_mean, sd = Mydata_sd), 
                color = "blue", 
                size = 1) +
  labs(title = "Histogram with Density and Normal Curve",
       x = "Value",
       y = "Density") +
print(normal_plot)



####THIRD QUESTION#####
#sinking to make a new file adding mean and sd into file
#what chatgpt says to do:
#####onedrive_path <- Sys.getenv("OneDrive")
#####desc_txt <- file.path(onedrive_path, "desc.txt")
#sink("desc.txt")

sink("desc.txt")
print(Mydata_mean)
print(Mydata_sd)
sink()

#save both plots as histo.pdf instead of Rplots.pdf
pdf("histo.pdf")
print(density_plot)
print(normal_plot)
dev.off()
