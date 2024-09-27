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
# Save the error bar plot to a PDF file
ggsave("error_bar_plot.pdf", plot = error_bar_plot, width = 8, height = 6)

# Perform pairwise t-tests
pairwise_results <- pairwise.t.test(plant_data$weight, plant_data$group, p.adjust.method = "none")

# Apply Bonferroni correction
bonferroni_p_values <- p.adjust(pairwise_results$p.value, method = "bonferroni")

# Apply Benjamini-Hochberg correction
benjamini_hochberg_p_values <- p.adjust(pairwise_results$p.value, method = "BH")

####exporting outputs using sink() to txt and pdf files
#include verbal interpretation to the screen and output files

#save error bar plot as .pdf:
ggsave("error_bar_plot_plant.pdf", plot = error_plantgrowth)
#sink() export files with separate lines between each output w/ sep=\n and cat(\n):
sink("plant_growth_summary_results.txt")

cat("Plant Growth ANOVA Results:\n")
cat(plant_anova_summary, sep = "\n")
cat('\n')

cat("Plant Growth Pairwise T Test:\n")
print(plant_pairwise_results)
cat("\n")

cat("Pairwise T Test Bonferroni Corrected Pvalues:\n")
print(bonferroni_pvalues)
cat("\n")

cat("Pairwise T Test Benjamini-Hochberg Corrected Pvalues:\n")
print(benjamini_hochberg_pvalues)
cat("\n")
##giving verbal interpretations of what each printed result means/shows
cat("Result Interpretations:\n")
cat("1. ANOVA test results provides the F-statistic and p-value that assess if there are significant differences among groups.\n")
cat("2. Pairwise t-tests compare each pair of groups to identify which pairs differ significantly.\n")
cat("3. Bonferroni correction adjusts p-values to account for multiple comparisons.\n")
cat("4. Benjamini-Hochberg correction adjusts for the false discovery rate.\n")

sink()

#############FIFTH QUESTION#############
#Kruskal Wallis applied to Plant Growth data used in ANOVA test
View(PlantGrowth)
Plant_kruskal_test <- kruskal.test(weight ~ group, data = myplantdata)
print(Plant_kruskal_test)

##have to subset data into control and test groups to compare weight within 2 groups
##also create data frame with the 2 groups to use in tests for ease
Plant_ctrl <- subset(PlantGrowth, group == "ctrl")$weight
Plant_trt1 <- subset(PlantGrowth, group == "trt1")$weight
Plant_newgrouped_data <- data.frame(Plant_ctrl, Plant_trt1)

##pearson correlation test of regrouped Plant Growth data frame
my_pearson_correlation <- cor.test(Plant_ctrl, Plant_trt1, method = "pearson")
print(my_pearson_correlation)

##spearman correlation test of regrouped Plant Growth data frame
my_spearman_correlation <- cor.test(Plant_ctrl, Plant_trt1, method = "spearman")
print(my_spearman_correlation)

##pearson corr scatter plot:
scatter_pearson_plot <- ggplot(Plant_newgrouped_data, aes(x = Plant_ctrl, y = Plant_trt1)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add a linear regression line for Pearson correlation
  labs(title = "Scatterplot of Ctrl vs Trt1 (Pearson Correlation)",
       x = "Plant Ctrl Weight", 
       y = "Plant Trt1 Weight") +
  theme_minimal()
print(scatter_pearson_plot)

##spearmans corr scatter plot:
scatter_spearman_plot <- ggplot(Plant_newgrouped_data, aes(x = Plant_ctrl, y = Plant_trt1)) +
  geom_point(color = "green") +
  geom_smooth(method = "loess", color = "darkorange", se = FALSE) +  # Add a smooth line for Spearman correlation
  labs(title = "Scatterplot of Ctrl vs Trt1 (Spearman Rank Correlation)",
       x = "Group Ctrl Weight", 
       y = "Group Trt1 Weight") +
  theme_minimal()
print(scatter_spearman_plot)

##one sample KS test of normality on Plant Growth data
#####INPUT CODE HERE###########

##Printing/export results and plots
sink("Normality_Assumption_analyses.txt")

cat("Kruskal-Wallis Test Results:\n") #Kruskal wallis test print
print(Plant_kruskal_test)
cat("\nInterpretation:\n")
cat("The Kruskal-Wallis test checks for differences between the medians of the groups.\n")
cat("If the p-value is less than the significance level we reject the null hypothesis that all groups have the same median.\n")

cat("Pearson Correlation Results:\n") #pearsons test print
print(my_pearson_correlation)
print(scatter_pearson_plot)
cat("\n")

cat("Spearman Correlation Results:\n") #spearmans test print
print(my_spearman_correlation)
print(scatter_spearman_plot)
cat("\n")

cat("Interpretation:\n")
cat("1. Pearson correlation assesses the linear relationship between the two groups. Our p-value is greater than 0.05 meaning there is a most likely not a linear relationships here.\n")
cat("2. Spearman rank correlation assesses the monotonic relationship, which is more robust to non-linearity and outliers. Our p-value is greater than 0.05 meaning the relationship between these vairables is most liekly due to random chance.\n")

cat("One-Sample Kolmogorov-Smirnov Test for Normality Results:\n") #One sample KS Test
print(ks_test_plantgrowth)
cat("\nInterpretation:\n")
cat("The KS test compares the empirical distribution of the sample against the theoretical normal distribution.\n")
cat("Since our p-value is greater than 0.05, we fail to reject the null hypothesis, indicating that the data does follow a normal distribution.\n")

cat("In conclusion, these tests do not apeear consistent as the one sample KS test results suggest that we cannot reject normal distribution in the data and yet both pearson's and spearman's correlation demonstrated no linear or monotonic relationhsip.\n")

sink()


#########SIXTH QUESTION###########
#running simple linear regression to compare #5 results
PlantGrowth$group <- as.factor(PlantGrowth$group) #make group into factor for regression
Plant_linearmodel <- lm(weight ~ group, data = PlantGrowth)
summary(Plant_linearmodel)

##create text file for lm summary
sink("PlantGrowth_linear_regression_summary.txt")
cat("Simple Linear Regression Summary for PlantGrowth Dataset:\n")
print(summary(Plant_linearmodel))
sink()

#plotting lm of plant growth
##weight is dependent while group is categorical independent

##plot regression from visualizing boxplot of PlantGrowth
Plant_groupmeans <- aggregate(weight ~ group, data = PlantGrowth, mean) #group means for boxplot
Plant_groupsd <- aggregate(weight ~ group, data = PlantGrowth, sd) #sd for error bars

lm_Plantgrowth <- ggplot(Plant_groupmeans, aes(x = group, y = weight, fill = group)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = weight - Plant_groupsd$weight, ymax = weight + Plant_groupsd$weight), width = 0.2) +
  labs(title = "Mean Plant Weight by Group with Error Bars",
       x = "Group",
       y = "Mean Weight") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "green", "purple"))
print(lm_Plantgrowth)

##exporting and printing 6th question lm regression results
##create text file for lm summary
sink("PlantGrowth_linear_regression_summary.txt")
cat("Simple Linear Regression Summary for PlantGrowth Dataset:\n")
print(summary(Plant_linearmodel)) ##lm results stats

cat("Linear Regression Plotted as a Box Plot:\n")
print(lm_Plantgrowth)  #lm plot

cat("Interpretation:\n")
cat("GroupTrt1 is not significantly different from the control, GroupTrt2 is significantly different from the control group in weight.\n")
cat("The F-statistic and p-value indicates that treatment group has an influence of plant weight.\n")
cat("This is different from the correlation tests that suggested normal distribution as well as the differences coming from simple random chance.\n")
cat("All in all, the linear regression versus correlation tests do not have similar results.\n")
cat("Regression should be used when you wish to evaluate the relationship between a dependent and one or more independent vairables.\n")
cat("Correlation is used when you want to measure the strength/direction of a linear relationship.\n")

sink()


########SEVENTH QUESTION##########
#using R generated data again called Palmer Penguins
install.packages("palmerpenguins")
library(palmerpenguins)
data(penguins)
