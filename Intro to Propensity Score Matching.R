#In this analysis I will use propensity score matching to understand the treatment effect
#I am using an observation data set comprising of subjects and their repsonses


library(tableone)


securityData <-read.csv("securitynational.csv")
head(securityData)

#table with standardized mean difference

tab1 <- CreateTableOne(data = securityData)

## Step 0: Summary Statistics of the variables

# generate variables 
securityData$cov4_sq = securityData$cov4*securityData$cov4     # (cov4)^2
securityData$cov5cov1 = securityData$cov5*securityData$cov1    # cov5*cov1
securityData$cov5cov2 = securityData$cov5*securityData$cov2    # cov5*cov2


## Step 1: Estimate the Propensity Scores

#obtain a vector with the estimated propensity scores after estimating a logistic regression
securityData$PS<-glm(fancy~cov1+cov2+cov3+cov4+cov4_sq+cov5cov1+cov5cov2+cov5+cov6, data=securityData, family = "binomial")$fitted.values


## Step 2: Check if Propensity Scores overlap enough to permit matching and, if yes, do the matching

# plot
library(ggplot2)
securityData$fancylabl <- factor(securityData$fancy, levels=c(0,1), labels=c("Usual", "Fancy"))
ggplot(securityData, aes(x = PS)) +  geom_histogram(color = "white") + facet_wrap(~fancylabl) + xlab("Pr(Fancy)") +theme_bw() +   coord_flip() 


# sort individuals randomly before matching; use seed for replication
set.seed(24)
securityData$random_id=runif(nrow(securityData),0,1)
securityData <- securityData[order(securityData$random_id),]  

# match each FANCY subject to a USUAL subject with similar propensity score
library("Matching")
m.out <- Match(Y=securityData$response, Tr=securityData$fancy, X=securityData$PS, M=1, caliper = 0.25, replace=F)
summary(m.out)

matched_data <- securityData[unlist(m.out[c("index.treated","index.control")]),]


## Step 3: Evaluate the quality of the balance

mb <- MatchBalance(fancy~cov1 + cov2+ cov3+ cov4+ cov4_sq+ cov5+ cov6+ cov5cov1+cov5cov2, data=securityData, match.out=m.out , nboots=0)

## Table and Visual inspection of the standardized differences

# Construct table of SMD for matched and unmatched samples
library(tableone)
vars <- c("cov1", "cov2", "cov3","cov4", "cov5", "cov6", "cov4_sq", "cov5cov1", "cov5cov2" )
tabUnmatched <- CreateTableOne(vars = vars, strata = "fancy", data = securityData, test = FALSE)
tabMatched <- CreateTableOne(vars = vars, strata = "fancy", data = matched_data, test = FALSE)


# Column bind tables    
resCombo <- cbind(print(tabUnmatched, smd=TRUE, printToggle = FALSE),
                  print(tabMatched, smd=TRUE,  printToggle = FALSE))
## Add group name row, and rewrite column names
resCombo <- rbind(Group = rep(c("Usual","Fancy", "SMD"), 2), resCombo)
colnames(resCombo) <- c("Unmatched","","","Matched","","")
print(resCombo, quote = FALSE)

## Combine matched and unmatched data for plot
dataPlot <- data.frame(variable  = names(ExtractSmd(tabUnmatched)), Unmatched = ExtractSmd(tabUnmatched), Matched = ExtractSmd(tabMatched))
print(dataPlot, smd = TRUE)

# Create long-format data for ggplot2
library(reshape2)
dataPlotMelt <- melt(data= dataPlot, id.vars= c("variable"), variable.name = "Sample", value.name= "SMD")

# Plot using ggplot2
#yintercept = 0.1 because a standardized mean difference greater than 0.1 is often considered the sign of important covariate imbalance (http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3144483/#s11title )
ggplot(data = dataPlotMelt, mapping = aes(x = variable, y = SMD, group = Sample, color = Sample)) +
  geom_line() +geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) + coord_flip() +
  theme_bw() + theme(legend.key = element_blank())  + labs(x="Variable", y="Standardized Mean Difference") 


# Step 4: Estimate the Treatment Effect

##tables
#create a 2-way table
tab1<-addmargins(xtabs(~ fancy + response, data=securityData))
tab1
#create a 2-way table
tab2<-addmargins(xtabs(~ fancy + response, data= matched_data)) 
tab2


# Unadjusted data
logit=glm(response ~ fancy, data =securityData, family = "binomial")
summary(logit)
exp(cbind(OddsRatio = coef(logit), confint(logit)))
# Odds Ratio & confidence interval
table(securityData$response, securityData$fancy)

# PS-Matched data
logit=glm(response ~ fancy, data =matched_data, family = "binomial")
summary(logit)
exp(cbind(OddsRatio = coef(logit), confint(logit)))
# Odds Ratio & confidence interval
table(matched_data$response, matched_data$fancy)

#confidence interval for difference in proportions and hypothesis test
prop.test(x=c(80, 26), n=c(209, 209), alternative="two.sided", conf.level=0.95, correct=FALSE) 


#treatment effect based on whole data
library(dplyr)

t_effect <- securityData %>%
  group_by(fancy) %>%
  summarise(effect = sum(response)/n())

#finding the best and worst matches

securityData <- securityData %>%
  mutate(row_index = row.names(.))%>%
  mutate(row_index = row.names(.))

matches <- data.frame(treat = m.out$index.treated, control = m.out$index.control)

matches_PS <- merge(matches,securityData[c("row_index", "PS","subject")], by.x = "treat", by.y = "row_index")

names(matches_PS)[names(matches_PS) == "PS"] <-"treated_PS" 
names(matches_PS)[names(matches_PS) == "subject"] <-"treated_subject" 

matches_PS <- merge(matches_PS,securityData[c("row_index", "PS","subject")], by.x = "control", by.y = "row_index")

names(matches_PS)[names(matches_PS) == "PS"] <-"control_PS" 
names(matches_PS)[names(matches_PS) == "subject"] <-"control_subject" 

matches_PS <- transform(matches_PS,diff = abs(treated_PS - control_PS))
