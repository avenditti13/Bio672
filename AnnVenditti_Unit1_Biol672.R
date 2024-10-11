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

##############EIGHTH QUESTION############
#Clean up penguins data set to omit empties
penguins_clean <- na.omit(penguins)

#Dependent Variables (Quantitative Measurements): 
#Bill Length, Bill Depth, Flipper Length, Body Mass
#Independent Variable (Categorical): Species

Peng_dependent_vars <- cbind(penguins_clean$bill_length_mm, 
                        penguins_clean$bill_depth_mm, 
                        penguins_clean$flipper_length_mm, 
                        penguins_clean$body_mass_g) #Combine depen. variables
penguins_manova <- manova(Peng_dependent_vars ~ species, data = penguins_clean)
Manova_summary <- summary(penguins_manova)
Pillai_test <- summary(penguins_manova, test = "Pillai") ##common MANOVA test

#summary.aov() creates break MANOVA into individual ANOVA
Peng_anovaresults <- (summary.aov(penguins_manova))

##printing to screen or output
sink("Penguin_MANOVA_Results.txt")

cat("MANOVA Results:\n")
print(Manova_summary)
print(Pillai_test)

cat("Univariate ANOVA Results:\n")
print(Peng_anovaresults)

cat("Interpretations:\n")
#MANOVA TEST
cat("The MANOVA test indicates whether combined dependent variables (bill length, bill depth, flipper length, and body mass) differ across penguin species.\n")
cat("The MANOVA results suggest that there are significant differences between penguin species across the combined dependent variables (bill length, bill depth, flipper length, and body mass).\n")
#ANVOVA TEST
cat("The univariate ANOVA test indicates show which specific dependent variables (bill length, bill depth, flipper length, and body mass) differ across species.\n")
cat("The univariate ANOVA results show that each individual variable is significantly different across species.\n")
sink()



###########NINTH QUESTION########
#predicting body mass
penguins_lm <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = penguins_clean)
Peng_lm_summary <- summary(penguins_lm)

#finding the best predictor of body mass
best_predictor <- Peng_lm_summary$coefficients[which.max(abs(Peng_lm_summary$coefficients[, "t value"])), ]

###using Adelie Penguins for one category test
adelie_penguins <- subset(penguins_clean, species == "Adelie")

penguins_lm_adelie <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = adelie_penguins)
lm_summary_adelie <- summary(penguins_lm_adelie)

#best predictor of body weight fro adelie penguins
best_predictor_adelie <- lm_summary_adelie$coefficients[which.max(abs(lm_summary_adelie$coefficients[, "t value"])), ]

##sinking to screen with interpretations
sink(file = "Penguins_Linear_Regressions.txt")
cat("Multiple Regression Results (All Species):\n")
print(Peng_lm_summary)
cat("All Species Interpretation:\n")
cat("The multiple regression model assesses how body mass is predicted by bill length, bill depth, and flipper length.\n")
cat("The best predictor for all species is flipper length since it has the largest t-value and teh smallest p-value.\n")

cat("Multiple Regression For Adelie Species:\n")
print(lm_summary_adelie)
cat("Adelie Interpretation:\n")
cat("The best predictor for Adelie penguins is also flipper length for the same reasons.\n")
sink()


###########TENTH QUESTION#########
#composite vairable is ratio of flipper length to body mass
penguins_clean$flipper_body_ratio <- penguins_clean$flipper_length_mm / penguins_clean$body_mass_g

#ANCOVA test on comp. variable
ancova_model <- aov(body_mass_g ~ bill_length_mm + flipper_body_ratio, data = penguins_clean)
summary(ancova_model)

#SInk or export to screen
sink("Penguins_ANCOVA_Test_Results.txt")

cat("ANCOVA Results:\n")
print(summary(ancova_model))

cat("Interpretation:\n")
cat("This ANCOVA model examines how body mass (dependent variable) is predicted by bill length (independent variable) while controlling for the flipper length to body mass ratio (covariate).\n")
cat("Since bill length is significant, it means that bill length significantly influences body mass even with accoutning for my composite variable.\n")
cat("The composite flipper body ratio is also significant meaning that the body mass flipper ratio also affects body mass.\n")
sink()




##########ELEVENTH QUESTION#########
##favorite hypothesis test or statistical method: one-way anova test

#original IRIS data import
originalIRIS_data <- read_csv("C:/Users/Ann/OneDrive/Iris_tab excel.csv")
##corrupted data import
iris_tab_smnoise <- read_csv("Bio Stats RIT/iris_tab_smnoise.txt.csv")
View(iris_tab_smnoise_txt)
iris_tab_randclass <- read_csv("Bio Stats RIT/iris_tab_randclass.csv")
View(iris_tab_randclass)
iris_tab_no_setosa <- read_csv("Bio Stats RIT/iris_tab_no_setosa.csv")
View(iris_tab_no_setosa)

##ORIGINAL ISRIS ANOVA TEST (sepal length across species)
originalIris_anova <- aov(Sepal.Length ~ Species, data = iris)
originalIris_summary <- summary(originalIris_anova)
print(originalIris_summary)

##Corrupted Iris ANOVA tests (sepal length across species)
corrupted_anova1 <- aov(sepal_length ~ species, data = iris_tab_smnoise)
summ_corrupt1 <- summary(corrupted_anova1)

corrupted_anova2 <- aov(sepal_length ~ species, data = iris_tab_randclass)
summ_corrupt2 <- summary(corrupted_anova2)

corrupted_anova3 <- aov(sepal_length ~ species, data = iris_tab_no_setosa)
summ_corrupt3 <- summary(corrupted_anova3)

sink("Corrupted_Iris_data_ANOVA")

cat("Iris noise data\n")
print(corrupted_anova1)
print(summ_corrupt1)
cat("Interpretation:\n")
cat("Despite the added noise, the test still finds a highly significant difference in Sepal Length between species, suggesting the test is robust to small amounts of noise in the data.\n")

cat("Randomized Iris data\n")
print(corrupted_anova2)
print(summ_corrupt2)
cat("Interpretation:\n")
cat("With species labels randomized, the ANOVA fails to find a significant difference in Sepal Length between species, which indicates that the test is highly sensitive to changes in category labels.\n")

cat("Iris data missing Setosa Species\n")
print(corrupted_anova3)
print(summ_corrupt3)
cat("Interpretation:\n")
cat("There is a significant difference in Sepal Length between the remaining species (likely versicolor and virginica).\n")
sink()



########TWELFTH QUESTION#######
#import purchase data from extended iris data folder github
iris_csv_purchase <- read_csv("C:/Users/Ann/Downloads/iris_csv_purchase.csv")
View(iris_csv_purchase)
##fact.anal and prin.comp pca and factor analysis

##PCA
##columns 8 and 9 are likert scale
pca_purchase <- princomp(iris_csv_purchase[, 8:9], cor = TRUE)
pca_summary <- summary(pca_purchase)  # View importance of components
loadings(pca_purchase)  # View component loadings

##Factor Analysis (needs at least 3 variables)
#have to combine to make 3 usable variables in the factor analysis (combine likert and measurements)
combined_data <- iris_csv_purchase[, c(2, 3, 4, 5, 8, 9)]
factor_analysis_purchase <- factanal(combined_data, factors = 2, scores = "regression")
print(factor_analysis_purchase)

Sink("PCA_FactorAnalysis_Purchase_Results.txt")

cat("PCA Results:\n")
print(pca_purchase)
print(pca_summary)
cat("PCA Interpretation:\n")
cat("PCAs provide the proportion of variance explained by each variable. Component 1 suggests buyers that are pickier while component 2 suggests buyers that are more likely to buy as attractiveness goes up.\n")

cat("Factor Analysis results:\n")
print(factor_analysis_purchase)
cat("Factor Analysis Interpretation:\n")
cat("Factor analyses look at latent variables. These results suggest that factor 1 and factor 2 both influence likeliness to buy, but factor 2 (sepal width) is much less influential. There is also evidence suggesting that flower size still does not have a dramatic impact on likeliness to buy, supported by the significant p-value.\n")

sink()

#######THIRTEENTH QUESTION######
#PCA purchase data without categorical data
#columns 1 through 5 are numerical
numerical_data <- iris_csv_purchase[, 1:5]
pca_result_Q13 <- prcomp(numerical_data, scale. = TRUE)
print(pca_result_Q13)
PCA_Q13_summ <- summary(pca_result_Q13)

##scree plot
scree_plot <- ggplot(data = data.frame(PC = 1:5, Variance = pca_result_Q13$sdev^2 / sum(pca_result_Q13$sdev^2)),
                     aes(x = PC, y = Variance)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot", x = "Principal Components", y = "Proportion of Variance")
print(scree_plot)

#loadings of results
Q13_loadings <- pca_result_Q13$rotation

sink("PCA_Data_Reduction.txt")

cat("PCA Results:\n")
print(pca_result_Q13)
print(PCA_Q13_summ)
cat("Scree Plot and Loadings:\n")
print(scree_plot)
print(Q13_loadings)

cat("Interpretations:\n")
cat("This data was successfully reduced, the lot shows that once we include PC3 the vast majorty of the variance is accounted for.\n")
cat("The loadings suggest that PC1 defines most of the overall size variation, with a high negative loadings. This shows the negative relationship with measurements and this component. They also show that PC2 is influenced by purchase variables meaning it is an important compoenent in a purchase analysis.\n")
cat("It seems like PC4 has the most positive loadings which would indicate the greatest description of size variation, but there is are still negative loadings for sepal width and petal measurements.\n")
sink()


###########FOURTEENTH QUESTION##########
#select only numerical columns from purchase dataset
Q14_numeric_data <- iris_csv_purchase[, c("sepal_length", "sepal_width", "petal_length", "petal_width", "attractiveness", "likelytobuy")]
Q14_fa_result <- factanal(Q14_numeric_data, factors = 2, rotation = "varimax")
print(Q14_fa_result)

sink("Factor_Analysis.txt")

cat("Factor Analysis Results:\n")
print(Q14_fa_result)

cat("Interpretation:\n")
cat("Factor 1 is influenced by sepal length, petal length, and petal width showing this as a latent underlying factor to the data based on the high positive loadings. There could also be some form of latentcy in factor 2 from sepal width on attractiveness and buying but its not as inlfuential as factor 1.\n")
cat("To answer about distance between traits, being far apart indicates a negative correlation of different constructs while being close together indicates a positive correlation and shared variance which suggests they measuer similar characteristics.\n")
cat("My results were successful in finding 1 significant latent underlying factor (Factor 1) but I would not say it was successful in finding 2 as factor 2 lacked explanatory impact.\n")

sink()



#######FIFTEENTH QUESTION####
