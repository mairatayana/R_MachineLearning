##############################################################################
# Some basic stuff for iris data analysis. The famous (Fisher?s or Anderson?s) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
# The scripts include:
# Loading e showing part of the dataset
# Scatter matrix of the dataset
# Some basic information (features types, etc.)
# Some descriptive statistics (univariate - per feature)
# Histograms
# Skeweeness and kurtosis
# Pie chart (class)
# Covariance and correlation (multivariate)
# Starplot
# Heatmap

##############################################################################
# First let's get a sampling of the data
head(iris)
tail(iris)
##############################################################################
# descriptive statistics in iris
dim(iris)
summary(iris)
##############################################################################
# some boxplots
boxplot(iris$Sepal.Length ~ iris$Species,data = iris,  xlab = "Iris Species",  ylab = "Sepal Length",         col = 2:4)
# Exercise: the boxplots of the other input features
boxplot(iris$Petal.Length ~ iris$Species,data = iris,  xlab = "Iris Species",  ylab = "Petal Length",         col = 2:4)
boxplot(iris$Sepal.Width ~ iris$Species,data = iris,  xlab = "Iris Species",  ylab = "Sepal Width",         col = 2:4)
boxplot(iris$Petal.Width ~ iris$Species,data = iris,  xlab = "Iris Species",  ylab = "Petal Width",         col = 2:4)
##############################################################################
# Histograms
library(ggplot2)
library(gridExtra)
library(grid)
HisSl <- ggplot(data=iris, aes(x=iris$Sepal.Length))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Sepal Length (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Sepal Length")+      geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Length)),linetype="dashed",color="grey")
show(HisSl)
# Exercise: the histograms of the other input features (HistSw,  HistPl, HistPw)
HistSw <- ggplot(data=iris, aes(x=iris$Sepal.Width))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Sepal Width (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Sepal Width")+      geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Width)),linetype="dashed",color="grey")
show(HistSw)
HistPl <- ggplot(data=iris, aes(x=iris$Petal.Length))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Petal Length (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Petal Length")+      geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Length)),linetype="dashed",color="grey")
show(HistPl)
HistPw <- ggplot(data=iris, aes(x=iris$Petal.Width))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Petal Width (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Petal Width")+      geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Width)),linetype="dashed",color="grey")
show(HistPw)
# Joining the histograms
# Plot all visualizations
grid.arrange(HisSl + ggtitle(""), HistSw + ggtitle(""), HistPl + ggtitle(""), HistPw  + ggtitle(""),             nrow = 2, top = textGrob("Iris Frequency Histogram", gp=gpar(fontsize=15)))
##############################################################################
# pie chart
# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(iris$Species)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, main="Pie Chart of Species\n (with sample sizes)")
##############################################################################
# Correlation plot
library(corrplot)
library(Hmisc)
# correlation and covariance
cov(iris[,1:4])
cor(iris[,1:4]) #-> para ver correlacao
r = rcorr(as.matrix(iris[,1:4]),type="spearman")
corrplot(r$r, type="upper",addCoef.col="black",tl.cex=1.5,tl.col="black",number.cex=1.2)
##############################################################################
# scatter plot of the iris dataset
library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[2:5], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Flower Characteristics in Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                 col = super.sym$col[1:3]),
                 text = list(c("setosa", "versicolor", "virginica"))))
##############################################################################
# bagplot
library(aplpack)
bagplot(iris$Sepal.Length,iris$Sepal.Width)
# Exercice: do other combinations
bagplot(iris$Sepal.Length,iris$Petal.Length)
bagplot(iris$Sepal.Length,iris$Petal.Width)
bagplot(iris$Sepal.Width,iris$Petal.Length)
bagplot(iris$Sepal.Width,iris$Petal.Width)
bagplot(iris$Petal.Length,iris$Petal.Width)
##############################################################################
# Chernoff faces
library(aplpack)
set.seed(1234)
sample_rows = sample(1:nrow(iris), 25)
tmp = iris[sample_rows,1:4]
faces(tmp, print.info=F)
##############################################################################
# star plot
stars(iris)
##############################################################################
# heatmap
library(gplots)
heatmap.2(t(iris[, 1:4]), trace="none", scale="row", key=TRUE, mar=c(2, 8), cexRow=1, ColSideColors=c("grey", "black", "yellow")[iris$Species])

