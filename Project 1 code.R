## setting working directory. 
setwd("~/Desktop/UCD/Quantitative Data Analysis/assignment 1")

## dataset
SRdata = read.csv ("SusRtlx.csv")

## check out the head and tail. 
head(SRdata)
tail(SRdata)

## Data Cleaning
## Check the maximum and minimum values
which(SRdata$SUS.Score<0)
which(SRdata$SUS.Score>100)
which(SRdata$RTLX.Score<0)
which(SRdata$RTLX.Score>126)
## R tells me that row number 92 and 88 is out of the range

## This problematic data should be an error, an impossible scores rather than an outlier. It is not possible to know the the actual score or use this incorrect score to infer the true score. To avoid inferential statistical bias due to problematic data, these two figures in row 92 and row 88 were simply removed. The dataset has 100 rows of data and can afford the loss of these two rows.

## make a new dataframe that excludes those two rows.
SRdata.clean = SRdata[-c(92,88),]

## use boxplots and histograms to visualize. 

## give histogram a main title, a meaningful x axis label, and define the breaks. The range of SUS is 0 to 100, I'll count by 20. The actural range of RTLX is 20 to 62, I'll count by 10. 
hist (SRdata.clean$SUS.Score, main="Histogram of system usability SUS.Score", xlab='SUS.Score', breaks=c (0,20,40,60,80,100))
hist (SRdata.clean$RTLX.Score, main="Histogram of subjective workload RTLX.Score", xlab='RTLX.Score', breaks=c (20,30,40,50,60,70))

## give boxplots a main title, a meaningful y axis label 
boxplot (SRdata.clean$SUS.Score, main="boxplot of system usability SUS.Score", ylim= c(0,100), ylab = "SUS.Score")
boxplot (SRdata.clean$RTLX.Score, main="boxplot of subjective workload RTLX.Score", ylim= c(20,70), ylab = "RTLX.Score")

## The the normal distribution were visually assessed. Data are normally distributed without skew.
## The boxplots indicated that there was no outlier on both variables. But I'll check if there's an outlier by comparing it to the mean plus/minus three standard deviations.

which (SRdata.clean$SUS.Score > mean(SRdata.clean$SUS.Score)+3*sd(SRdata.clean$SUS.Score))
which (SRdata.clean$SUS.Score < mean(SRdata.clean$SUS.Score)-3*sd(SRdata.clean$SUS.Score))
which (SRdata.clean$RTLX.Score > mean(SRdata.clean$RTLX.Score)+3*sd(SRdata.clean$RTLX.Score))
which (SRdata.clean$RTLX.Score < mean(SRdata.clean$RTLX.Score)-3*sd(SRdata.clean$RTLX.Score))
## no outliers for both variables

## descriptive statistics of two columns.
mean (SRdata.clean$SUS.Score)
sd(SRdata.clean$SUS.Score)
max (SRdata.clean$SUS.Score)
min (SRdata.clean$SUS.Score)
median (SRdata.clean$SUS.Score)
IQR (SRdata.clean$SUS.Score)

mean (SRdata.clean$RTLX.Score)
sd(SRdata.clean$RTLX.Score)
max (SRdata.clean$RTLX.Score)
min (SRdata.clean$RTLX.Score)
median (SRdata.clean$RTLX.Score)
IQR (SRdata.clean$RTLX.Score)

## plot SUS and RTLX against each other to visually look for a pattern like a correlation with a scatterplot. give each axis a meaningful label, and add a  main title.
plot (SRdata.clean$SUS.Score, SRdata.clean$RTLX.Score, xlim= c(0,100), ylim =c(20, 70), xlab="SUS.Score", ylab="RTLX.Score", main="Scatterplot of SUS.Score and RTLX.Score")
## a clear linear relationship 

## check statistically for a correlation using cor.test
cor.test(SRdata.clean$RTLX.Score, SRdata.clean$SUS.Score)
## Our correlation test provide evidence of a linear relationshp.  There is a strong positive correlation between RTLX and SUS. r(96) = 0.68, p < 0.001.

## find the regression equation
SRModel = lm (SRdata.clean$RTLX.Score~SRdata.clean$SUS.Score)
## get a description of it using the summary() function
summary(SRModel)
## add the regression equation line to the scatterplot
abline(SRModel, col="red")

