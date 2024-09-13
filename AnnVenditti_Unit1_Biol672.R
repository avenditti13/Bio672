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



########FOURTH QUESTION########
#Load in pre-set R data: "PlantGrowth"
data("PlantGrowth")
View(PLantgrowth)

##making data into one that can be made into read.table
##sep = "\t" makes into a usable read.table file
##row.names = FALSE makes so row names are not shown in table
write.table(PlantGrowth, file = "plantgrowth.txt", sep = "\t", row.names = FALSE)
myplantdata <- read.table("plantgrowth.txt", header = TRUE, sep = "\t")
#myplantdata becomes the table for PlantGrowth data

##running a One Way Anova on tablized myplantdata
Plant_anova <- oneway.test(weight ~ group, data = myplantdata)
print(Plant_anova)
##adding error bar chart of the outputs for anova NEED DYPLR package
###first get summ stats from anova plantgrowth
plant_summary_data <- myplantdata %>%
  group_by(group) %>%
  summarise(
    mean_weight = mean(weight),
    sd_weight = sd(weight),
    se_weight = sd(weight) / sqrt(n())
  )
print(plant_summary_data)

##making the error bar chart of the one way anova plantgrowth with colors
ggplot(plant_summary_data, aes(x = group, y = mean_weight, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Plant Weight by Group with Error Bars", 
       x = "Group", y = "Mean Weight") +
  scale_fill_manual(values = c("ctrl" = "lightblue", "trt1" = "forestgreen", "trt2" = "darkblue"))

##adding in paired t-test for categories pairs in myplantdata
plant_pairwise_results <- pairwise.t.test(myplantdata$weight, myplantdata$group, p.adjust.method = "none")
##look at the p values from that
print(plant_pairwise_results$p.value)

#use the Bonferroni p adujstment method on plant_pairwise_results
bonferroni_pvalues <- p.adjust(plant_pairwise_results$p.value, method = "bonferroni")
print("Bonferroni Corrected p-values:")
print(bonferroni_pvalues)
##use the benjamini-hochberg p adjustment method on plant_pairwise_results
benjamini_hochberg_pvalues <- p.adjust(plant_pairwise_results$p.value, method = "BH")
print("Benjamini-Hochberg Corrected p-values:")
print(benjamini_hochberg_pvalues)

WHAT CHATGPT SAYS: NOT IN R YET BECAUSE INCORRECT USE OF CAT AND NOT PRINT
sink("results_summary.txt")

cat("ANOVA Results:\n")
cat(anova_summary, sep = "\n")
cat("\n")

cat("Pairwise t-Test Results (Raw p-values):\n")
print(pairwise_results$p.value)
cat("\n")

cat("Pairwise t-Test Results (Bonferroni Corrected p-values):\n")
print(bonferroni_p_values)
cat("\n")

cat("Pairwise t-Test Results (Benjamini-Hochberg Corrected p-values):\n")
print(benjamini_hochberg_p_values)
cat("\n")

cat("Interpretation:\n")
cat("1. The ANOVA test result provides the F-statistic and p-value to assess whether there are significant differences among the groups.\n")
cat("2. Pairwise t-tests compare each pair of groups to identify which pairs differ significantly.\n")
cat("3. Bonferroni correction adjusts p-values to account for multiple comparisons conservatively.\n")
cat("4. Benjamini-Hochberg correction controls the false discovery rate, which is less conservative than Bonferroni.\n")

sink()

# Confirmation message
cat("Results have been saved to 'results_summary.txt' and 'error_bar_plot.pdf'.\n")

