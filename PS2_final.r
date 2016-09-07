#PS2 Working with R
#August 24, 2016


#QUESTION 1 
getwd()
setwd("/Users/jsmith/Documents/bi623/160824_class8_PS2/PS2/")

#read in the RNAseq data as a table. seperator is a comma. 
#specify row 1 is contains names with row.names = 1
data_csv <- read.table("GacuRNAseq_Subset-1.csv", row.names = 1, header = T, sep = ",")

#look at the first few rows of data
head(data_csv)

#find the dimensions of the dataset
#dim returns the length of the row that was specified in row.names attribute when reading in the table
dim(data_csv)
#shows that there are 20 row and 185 columns

#read in the text file as a table. seperator is a tab. 
data_txt <- read.table("GacuRNAseq_Subset-1.txt", header = T, sep = '\t', row.names = 1)

#confirm the datasets are equivalent using Dim
dim(data_txt)
head(data_txt)
#yes, data_txt and data_csv are identical 

#QUESTION 2
#what type of object is data_txt?
class(data_txt)
#it is a data.frame

#print out the column names 
names(data_txt)

#QUESTION 3
#What are the "explanatory variables"?
#the explanatory variables are population, treatment, and sex 
#the rest of the columns are gene names for stickleback


#factors are explanatory variables that are needed for 
#statistical analyses. you can classify things like treatment or control
#or population as freshwater and marine in order to make comparisons. 


#two ways to look at the factor level for column 1, population
unique(data_txt[,1])
unique(data_txt$Population)
#two facotr levels for population: boot and rabbit slough.

#treatment factor
unique(data_txt[,2])
#conventional and monoAssoc

#sex factor
unique(data_txt[,3])
#male and female

class(data_txt$Population)
#class factor
class(data_txt$Treatment)
#class factor
class(data_txt$Sex)
#class factor

#if these columns were not classified as factors, but instead 
#as character (character = string in python)
#change that using data_txt$Sex <- as.factor(data_txt$Sex)


#QUESTION4
#for this dataset,  the number of rows are the fish samples
#so 20 fishes samples, factors recorded for pop, treat, sex
#response being measured is changes in gene expression 

#response variables are 185 columns - 3 factor columns = 182 
responseVariables <- length(data_txt[,4:185])
print(responseVariables)
#182 response variables 

#for loop to call the class function on all response variables 
for (i in data_txt[,4:185]){print (class(i))}
#response variables are all of class "numeric".


#QUESTION 5
#create a function to calculate the mean expression values
myMean <- function(x){
  expression <- x 
  Sum <- sum(expression)
  N <- length(expression) #number of rows, therefore total # of samples
  (Sum/N)
}

#call the function on the first gene in the dataset.
myMean(data_txt[,4])

#for loop to call the function on two columns of gene expression data
for (i in data_txt[,4:5]){print (myMean(i))}

#lapply is able to call the function on a over a list
#it prints out both the column name and the mean
#so this is more beneficial than the for loop
lapply(data_txt[,4:5], myMean)

#QUESTION 6 
#use lapply to find the mean of the first 100 genes in the dataset
expr_means_100 <- lapply(data_txt[,4:103], mean) 

mean(expr_means_100)
#this does not work, the argument is not numerical or logical

class(expr_means_100)
#class 'list' 

#unlist the variable expr_means_100 to make it a vector
vector_expr_100 <- unlist(expr_means_100)

names(vector_expr_100) #the unlist function retains the gene names for each mean expression value
#Mean function now works on the vector
mean(vector_expr_100)

class(vector_expr_100)
#class 'numeric' now

#create a pdf file to write to 
pdf(file = "hist_mean_exp.pdf")

#create a histogram of the mean expression levels now that they are a vector
hist_mean_exp <- hist(vector_expr_100, col='blue', main = "Mean Expression Level for 100 genes", xlab = "mean expression level")

#turn the device off
dev.off()

#QUESTION 7
#log transform the expression values
#must specify log10 becuase the default is the natural log
log_expr_100 = log10(vector_expr_100)

#create a PDF file to write a histogram to 
pdf(file = "hist_logmean_exp.pdf")
#create a histogram with the log transformed values
hist(log_expr_100, col='purple', main = "Log10 Mean Expression Level for 100 Genes", xlab = "Mean Expresion Level (log10)")
#close the device 
dev.off()

#QUESTION 8
#create a subset of the expression vectors under 500
expr_100_subset <- subset(vector_expr_100, subset = vector_expr_100 < 500)
print(expr_100_subset)
class(expr_100_subset)

#how many genes have expression values under 500 
length(expr_100_subset)
#93 genes have expression levels less than 500. 

#create a pdf file
pdf(file = "hist_subset_exp.pdf")
#make a histogram with the subset of genes with expression < 500 
hist(expr_100_subset, main = "Mean Expression Level for 93 Genes", xlab = "Mean Expression Level", col = "steel blue")
#close the device
dev.off()


#QUESTION 9 
#plot the mean expresion values for the 2nd gene in the table
#idividual plots for the two populations
gene2nd <- data_txt[,5]
print(gene2nd)

#find the mean for the second gene in the data set and 
#separate by population 
tapply(data_txt$ENSGACG00000000004, data_txt$Population, FUN = mean)
#Mean for Boot Lake is 9.848 and mean for Rabbit Slough is 0.181

#find the mean of all individuals for the second gene
mean(data_txt$ENSGACG00000000004)
#mean for all 20 individuals is 5.0144 

#find the variance for the two populations for gene 2
variancepops <- tapply(data_txt$ENSGACG00000000004, data_txt$Population, FUN = var)

#find the standard deviation (square root of variance)
stdDevpops <- sqrt(variancepops)
print(stdDevpops)
# SD for Boot is 5.604 and RabbitSlough is 0.382.


#find the variance for all individuals for gene 2
varianceAll <- var(data_txt$ENSGACG00000000003)
#find the standard deviation for all individuals 
stdDevAll <- sqrt(varianceAll)
print(stdDevAll)
#SD for all individuals is 10.943.

#generate a sample of 1000 values from a normal distribution
#using the mean and standard deviation from the Rabbit Slough individuals

#normal distribution of Boot Population Expression data for second gene
BootNormDist <- rnorm(1000, mean = 9.848, sd = 5.604)

#normal distribution of Rabbit Slough Population Expression data for second gene
rabbitNormDist <- rnorm(1000, mean = 0.1809, sd = 0.382)

#normal distribution for all individuals for second gene
AllNormDist <- rnorm(1000, mean = 5.014, sd = 10.943)

#create a figure with 3 panels and plot the normal distributions
#create a pdf file 
pdf(file = "Gene2_pop_normSamp.pdf")
#Settings to place all histograms on the same figure
op <- par(mfrow = c(3, 1))
#NOTE: mfrow parameter specifies 3 rows and 1 column
#can use pty = "s" to keep the figures square. if not used, then they are rectangular

#plot the distributions as histograms
hist(BootNormDist, xlim = c(-40, 40), ylim = c(0,350), main = "Boot Lake Distribution ",
    xlab = "Mean  Expression", col = "blue")
hist(rabbitNormDist, xlim = c(-40,40), ylim = c(0, 350), main = "Rabbit Lake Distribution",
     xlab = "Mean Expression", col = "purple")
hist(AllNormDist, xlim = c(-40,40), ylim = c(0,350), main = "All Individuals Distribution",
     xlab = "Mean Expression", col = "red")
par(op)
#turn the device off 
dev.off()

#QUESTION 10 
#make a variable to hold the factor columns and expression data from first three genes
threeGenes <- data_txt[,1:6]

#create a data frame with the factor columns and genes
three_genes <- data.frame(threeGenes)

#write the data frame to a csv file
write.csv(three_genes, file = "three_genes.csv")

#QUESTION 11 
#write a function called coefvar to calculate the coefficient of variation (CV)
#then create a for loop to generate a vector of CVs for all genes 
#express CV as the standard deviation as a percentage of the mean

#Coefficient of variation function 
coefvar <- function(mean, sd){
  (sd/mean)*100 #CV is the stddev as a percentage of the mean
}

#initialize a variable to hold the numeric vectors of CV for each gene
CV_list <- NULL

#For loop find the mean and standard deviation for each gene
#then apply the CV function and save output to the list above
for(i in data_txt[,4:185]){
  geneMean <- mean(i) #find the mean of each gene
  Variance <- var(i) #find the variance of each gene
  stdDev <- sqrt(Variance) #stdDev is the squareroot of the variance
  CV <- coefvar(geneMean, stdDev) #call the CV function using the mean and STDdev
  CV_list <- append(CV_list, CV) #append the CV to the empty variable
}

print(CV_list)

#create a pdf 
pdf(file = "CV_hist.pdf")
#plot a histogram of the CVs for all genes
hist(CV_list, main = "Coefficient of Variation (CV) for Gene Expression Levels", 
     col = "aquamarine", xlab = "CV values")
dev.off()

#create pdf
pdf(file = "CV_boxplot.pdf")
boxplot(CV_list, main = "Coefficient of Variation (CV) for Gene Expression Levels",
        col = "navy", ylab = "CV values")
dev.off()

#QUESTION 12 
#Calculate the CV values for all genes, but seperate on populations.
#Make a subset with only the individuals from Boot Lake population.
boot <- subset(data_txt, Population == "Boot")

#Make a subset with the individuals from Rabbit Slough population
rabbit <- subset(data_txt, Population == "RabbitSlough")

#initialize a variable 
bootCV_list <- NULL

#For loop to find the CV for boot lake individuals  
for(i in boot[,4:185]){ 
  geneMean <- mean(i) 
  Variance <- var(i) 
  stdDev <- sqrt(Variance) 
  CV <- coefvar(geneMean, stdDev) 
  bootCV_list <- append(bootCV_list, CV) 
}
print(bootCV_list)
#initialize an empty varaible
RabbitCV_list <- NULL

for(i in rabbit[,4:185]){
  geneMean <- mean(i) 
  Variance <- var(i) 
  stdDev <- sqrt(Variance) 
  CV <- coefvar(geneMean, stdDev) 
  RabbitCV_list <- append(RabbitCV_list, CV)  
}
print(RabbitCV_list)

print(RabbitCV_list)
#create a PDF
pdf(file = "popCVHist.pdf")
#set parameters to have both hisograms in one figure 
op <- par(mfrow = c(2, 1))
#create histograms
hist(bootCV_list, main = "Coefficient of Variation (CV) for Gene Expression Levels\n in Boot Lake Population", 
     xlab = "CV values", col = "royalblue", xlim = c(0,300), ylim = c(0,150) )
hist(RabbitCV_list, main = "Coefficient of Variation (CV) for Gene Expression Levels\n in Rabbit Slough Population", 
     xlim = c(0,300), ylim = c(0,150), col = "royalblue4", xlab = "CV values")
dev.off()

#How do the two populations compare? 

#The coefficient of variation(CV) looks at the standard deivation as a 
#percentage of the mean. Higher CV values indicate that there is a large 
#amount of variablility in the data points used to calculate the mean, while
#low CV indicate that the data  points used to calculate the mean
#are more precise. The boot lake population has less variation in its
#gene expression levels, becuase the spread of CV values is wide, and approximately
#90 genes' CV values are less than 50. Thus about half of the genes in the dataset
#have lower CV values. Overall, the Rabbit Slough population has more variability
#in its expression levels, since a there are genes with CV values of up to 300. 















