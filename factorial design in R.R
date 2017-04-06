
#############################
##Full Factorial Designs
#############################

#Factorial design using R
#I am using a data set which comprises of offers along with features associated with each offer
#I am interested specifically in offer 5 which contains a free shipping threshold
#So we will design an experiement using offer 1 which doesn't contain special features
# and offer 5 which contains a free shipping threshold

#import the data 
toolsfullexpandedData <- read.csv("tools_full_expanded.csv", head=TRUE, sep=",") 
head(toolsfullexpandedData, n=20) 

#keep only offers 1 and 5 because we are only interested in looking at an experiment for the free shipping threshold
toolsst200Data<- toolsfullexpandedData[ which(toolsfullexpandedData$offer==1 | toolsfullexpandedData$offer==5),]
toolsst200Data<-subset(toolsst200Data, select=c("st200", "sale"))
nrow(toolsst200Data)
head(toolsst200Data, n=20)

#create a 2-way table
toolsst200tbl <- xtabs(~ st200 + sale, data=toolsst200Data) 
toolsst200tbl 

#cross-tabulation with margins (i.e. totals)
addmargins(toolsst200tbl) 

#showing percentages
prop.table(toolsst200tbl,1)

#chi-square test
chisq.test(toolsst200tbl, correct=FALSE) # disable  Yates' continuity correction 
#(it's controversial whether this correction should be done or not)

#reading in collapsed data directly
toolsst200tbl <- matrix(c(9500,500,9420,580),ncol=2,byrow=TRUE) 
colnames(toolsst200tbl) <- c('0','1')
rownames(toolsst200tbl) <- c('0','1')
toolsst200tbl <- as.table(toolsst200tbl)
names(attributes(toolsst200tbl)$dimnames) <- c('st200', 'sale') #add dimension names to table
toolsst200tbl

#confidence interval for difference in proportions and hypothesis test
prop.test(x=c(500, 580), n=c(10000, 10000), alternative="two.sided", conf.level=0.95, correct=FALSE) 

##logistic regression with st200

#Option 1: use collapsed data
toolsst200df<-as.data.frame(toolsst200tbl) #coerce toolsst200tbl to a data frame (xtabs reverses the process)
class(toolsst200df)
toolsst200df
reg1 <- glm(sale ~ st200, weights = Freq, data=toolsst200df, family = binomial)
summary(reg1)

#Option 2: use raw data
reg2 <- glm(sale ~ st200, data=toolsst200Data, family = binomial)
summary(reg2)
exp(cbind(OR = coef(reg2), confint(reg2))) #to obtain odds ratios


##logistic regression with st200 and dis20

#keep only offers 1, 3, 5 and 8 because we are only interested in looking at an experiment for the free
#shipping threshold and the tools discount
toolsst200dis20Data<- toolsfullexpandedData[ which(toolsfullexpandedData$offer==1 | toolsfullexpandedData$offer==3| toolsfullexpandedData$offer==5 | toolsfullexpandedData$offer==8),]
toolsst200dis20Data<-subset(toolsst200dis20Data, select=c("st200", "dis20", "sale"))
nrow(toolsst200dis20Data)

#create summary table with all possible combinations
toolsst200dis20tbl <- xtabs(~ st200 + dis20 + sale, data=toolsst200dis20Data) 
toolsst200dis20df<-as.data.frame(toolsst200dis20tbl)
toolsst200dis20df

#logistic regression
reg3 <- glm(sale ~ st200 + dis20 + st200:dis20, data=toolsst200dis20Data, family = binomial)
summary(reg3)


exp(cbind(OR = coef(reg3), confint(reg3))) #to obtain odds ratios

