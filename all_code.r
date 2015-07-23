

library('foreign')
# We'll assume having Ghana/Ethiopia data in the directory
household = read.dta('GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8/GHA_2005_H.dta')
expenses = read.dta('GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8/GHA_2005_E.dta')
individuals = read.dta('GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8/GHA_2005_I.dta')


#household = read.dta('ETH_2004_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2004_H_P.dta')
#expenses = read.dta('ETH_2004_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2004_E_P.dta')
#individuals = read.dta('ETH_2004_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2004_I_P.dta')

#ETH_2000_HICES_v01_M_v01_A_SHIP_Stata8

#household = read.dta('ETH_2000_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2000_H_P.dta')
#expenses = read.dta('ETH_2000_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2000_E_P.dta')
#individuals = read.dta('ETH_2000_HICES_v01_M_v01_A_SHIP_Stata8/ETH_2000_I_P.dta')

household$INC_TOT_G_PPP_DAILY = household$INC_TOT_G / expenses$PPP2005[1] / 365

household$XPOV = household$INC_TOT_G_PPP_DAILY
household$INC_TOT_G_PPP_DAILY = household$INC_TOT_G / expenses$PPP2005[1] / 365

household$XPOV = household$INC_TOT_G_PPP_DAILY



library(ggplot2)
library(scales)
distPlot = ggplot(household, aes(x=XPOV, nrow=nrow(household), y=..count.. / nrow)) +
geom_line(stat='bin', binwidth=0.1) + scale_y_continuous(labels = percent_format()) + 
geom_vline(xintercept=2.0, color='red') + geom_vline(xintercept=1.25, color='blue')

distPlot + scale_x_continuous(limits=c(0, 20), breaks=0:20)  + labs(x='Gross Income in PPP $', y='% households')

sum(household$INC_TOT_G_PPP_DAILY == 0.0) / nrow(household)


# EXPENDTIURES
if('PCEXPDR_PPP' %in% names(household)){ # for Ghana
  #household$XPOV = household$PCEXP / expenses$PPP2005 / 365
  household$XPOV = household$PCEXPDR_PPP / 365

}else{ # for  Ethiopia
  household$XPOV = household$PCEXP_PPP / 365
}

distPlot = ggplot(household, aes(x=XPOV, nrow=nrow(household), y=..count.. / nrow)) +
geom_line(stat='bin', binwidth=0.1) + scale_y_continuous(labels = percent_format()) + 
geom_vline(xintercept=2.0, color='red') + geom_vline(xintercept=1.25, color='blue')
distPlot + scale_x_continuous(limits=c(0, 20), breaks=0:20)  + labs(x='Total Expendtuires $', y='% households')

sum(household$XPOV == 0.0) / nrow(household)

distPlot + aes(y=cumsum(..count..)/nrow) + scale_x_continuous(limits=c(0, 20), breaks=0:20)  +
labs(x='Gross Income in PPP $', y='Cumulative % of households') +
geom_hline(yintercept=0.51, color='red') + 
geom_hline(yintercept=0.28, color='blue')

# VISUAL ROUGH CALCULATION (just multiplied the hlines by a factor)
distPlot + aes(y=cumsum(..count..)/nrow) + scale_x_continuous(limits=c(0, 20), breaks=0:20)  +
labs(x='Gross Income in PPP $', y='Cumulative % of households') +
geom_hline(yintercept=0.51, color='red') + 
geom_hline(yintercept=0.28, color='blue') + 
geom_vline(xintercept=3.5, color='red') + geom_vline(xintercept=2.18, color='blue')

household$poor = factor(household$XPOV < 3.5)
# for households levels
table(household$poor)
sum(household$poor == TRUE, na.rm=T) / nrow(household)

# for individuals levels
sum(household$poor == T * household$HHSIZE, na.rm=T) / sum(household$HHSIZE)

household$poor = factor(household$XPOV < 2.7)
# for households levels
table(household$poor)
sum(household$poor == T, na.rm=T) / nrow(household)
sum(household$poor == T * household$HHSIZE, na.rm=T) / sum(household$HHSIZE)

# the default levels are 'YES', 'NO', let's change them to make the table more readble
levels(household$TV) = c('NOTV', 'TV')
table(household$poor, household$TV)

tv_xpov = ggplot(subset(household, !is.na(TV)), aes(y=TV, x=XPOV, col=TV)) +
  geom_point(alpha=0.5, position=position_jitter(0.05)) + geom_vline(xintercept=2.7, color='red')
tv_xpov

tv_xpov + scale_x_continuous(limits=c(1,10)) 

library(caTools) # for sampling
set.seed(2000) # to ensure consistent runs
split = sample.split(household$poor, SplitRatio=0.7)

# adding poor factorial
household$poorfac = factor(household$poor, levels=c('TRUE', 'FALSE'))
levels(household$poorfac) = c('POOR', 'NOT_POOR')

train.household = subset(household, split)
test.household = subset(household, !split)

# you can use '.' to refer to all variable or . - VAR1 - VAR2 to exclude some variables
logit1 = glm(poor ~ TV + TOILET, data=train.household, family=binomial)
summary(logit1)

logit1.poor.prediction = predict(logit1, newdata=test.household, type="response")
head(logit1.poor.prediction)

table(prediction=logit1.poor.prediction > 0.5, reference = test.household$poor)

# or you can use the more polished way of confusionMatrix in caret library

library(caret)
library(e1071)

confusionMatrix(logit1.poor.prediction > 0.5, test.household$poor)

library(ROCR)

logit1.ROCRpred = prediction(logit1.poor.prediction, test.household$poor) # prediction object

perf = performance(logit1.ROCRpred, 'tpr', 'fpr')
rocplot = function(perf){
  qplot(perf@x.values[[1]], perf@y.values[[1]], geom='line') + labs(x=perf@x.name, y=perf@y.name, title=perf@alpha.name) # plot ROC
}
rocplot(perf)

# this way you can get auc, precision, recall as values or as plots where the 
# the x axis is the cutoff and the xaxis is the metric. 
# to get the area under the curve
as.numeric(performance(logit1.ROCRpred, 'auc')@y.values)

# in helpers.R
source('helpers.R')
drawPrecisionRecall(logit1.ROCRpred)

logit2 = glm(poor ~ WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.household, family=binomial) 
logit2.poor.prediction = predict(logit2, newdata=test.household, type="response")

logit2.ROCRpred = prediction(logit2.poor.prediction, test.household$poor) # prediction object
drawPrecisionRecall(logit2.ROCRpred)

confusionMatrix(logit2.poor.prediction > 0.4, test.household$poor)


logit1.auc = performance(logit1.ROCRpred, 'auc')
logit2.auc = performance(logit2.ROCRpred, 'auc')
logit1.auc@y.values[[1]]
logit2.auc@y.values[[1]]

logit1.tpr.fpr = performance(logit1.ROCRpred, 'tpr', 'fpr')
logit2.tpr.fpr = performance(logit2.ROCRpred, 'tpr', 'fpr')

# not the cleanest way, but will make generating graphs for exploring much much better
df1 = data.frame(logit1.x = logit1.tpr.fpr@x.values[[1]], logit1.y = logit1.tpr.fpr@y.values[[1]])
df2 = data.frame(logit2.x = logit2.tpr.fpr@x.values[[1]], logit2.y = logit2.tpr.fpr@y.values[[1]])
ggplot(df1, aes(x = logit1.x, y=logit1.y)) +
  geom_line() +
  geom_line(data=df2, aes(logit2.x, logit2.y), color='red') +
  labs(title="Comparison between logit1 and logit2", x='False Positive Rate', y='True Positive Rate')

compareTwoModels(logit1, logit2, test.data=test.household, dep.var='poor',
                 model1.name='logit1', model2.name='logit2')

  summary(logit2)

library(randomForest)
# note in randomForests, NA values must be imputed or totally ignored 
#forest1 = randomForest(poor ~ HHSEX + HHAGEY + HHMARST + HHEDLEV + HHEMSTAT + HHEMTYPE  + OWNHOUSE+ ROOMS + GARBDISP + TOILET + WALLS +  SEWMACH + STOVE + RADIO + TV + AGLAND +  OTHLNDOW, data=train.household, nodesize=25, ntree=200)

forest1 = randomForest(poor ~ WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.household, nodesize=25, ntree=200)

# random forest models output TRUE or FALSE directly for classification. The models
# knew that it is a classification problem because the column poor is a factor. To
# Treat as response, make the poor column logical

# The predict function works here as well (there is an alias for each algorithm)
forest1.poor.prediction = predict(forest1, newdata=test.household)
confusionMatrix(forest1.poor.prediction, test.household$poor)

library(plyr)


all_info = ddply(individuals, .(HID), function(x){ 
                c(
                  sum(x$AGEY < 6) / x[1,]$HHSIZE_S, 
                  sum(x$AGEY < 15) / x[1,]$HHSIZE_S, 
                  sum(x$AGEY < 21) / x[1,]$HHSIZE_S,
                  sum(x$AGEY > 60) / x[1,]$HHSIZE_S,
                  sum(x$SEX == 'Female') / x[1,]$HHSIZE_S,
                  sum(x$LITERACY == 'Cannot read or write') / x[1,]$HHSIZE_S
                )
                 })
names(all_info) = c('HID', 'UND6RAT', 'UND15RAT', 'UND21RAT', 'ABV60RAT', 'FEMRAT', 'LITRAT')
extra.household = merge(all_info, household, by='HID')

train.extra.household = extra.household[split, ]
test.extra.household = extra.household[!split, ]

forest1 = randomForest(poor ~ WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.extra.household) 

forest2 = randomForest(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.extra.household) 
logit3 = glm(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.extra.household, family=binomial) 


library('mice')
povCols = c('HHSEX', 'HHAGEY', 'HHMARST', 'HHEDLEV', 'HHEMTYPE', 'OWNHOUSE',  'ROOMS', 'GARBDISP', 'TOILET', 'WALLS', 'SEWMACH', 'STOVE', 'RADIO', 'TV', 'AGLAND', 'OTHLNDOW', 'poor')

subset.household = household[, povCols]

subset.train.household  = subset(subset.household, split)
subset.test.household  = subset(subset.household, !split)

logit4 = glm(poor ~ ., subset.train.household, family=binomial)

# takes a lot of time, not worth it
# imputed.household = complete(mice(household[, povCols]))
imputed.household = household

imputed.train.household = subset(imputed.household, split)
imputed.test.household = subset(imputed.household, !split)

#logit4 = glm(poor ~ ., imputed.train.household, family=binomial)
#logit4.poor.prediction = predict(logit4, imputed.test.household)
#table(prediction=logit4.poor.prediction > 0.4, reference = imputed.test.household$poor)

library(kernlab)
svm1 = ksvm(poor ~ WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.extra.household)
svm1.predictions = predict(svm1, test.extra.household)
confusionMatrix(svm1.predictions, test.extra.household$poor)

svm3 = ksvm(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF, data=train.extra.household) 

svm3.predictions = predict(svm3, test.extra.household)
confusionMatrix(svm3.predictions, test.extra.household$poor)

# fitControl <- trainControl(## 10-fold CV
#                            method = "repeatedcv",
#                            number = 10,
#                            ## repeated ten times
#                            repeats = 5)
# 
# 
# gbmFit1 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
#                  data = train.extra.household,
#                  method = "gbm",
#                  trControl = fitControl,
#                  ## This last option is actually one
#                  ## for gbm() that passes through
#                  verbose = FALSE)
# gbmFit1
# 


# svmFit1 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
#                  data = train.extra.household,
#                  method = "svmRadial",
#                  trControl = fitControl,
#                  tuneLength = 8)
# svmFit1


# descrimination 
# rdaFit1 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
#                  data = train.extra.household,
#                  method = "rda",
#                  trControl = fitControl,
#                  tuneLength = 4,
#                  metric = "ROC")
# rdaFit
# 
# 
# 
# logFit1 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
#                  data = train.extra.household,
#                  method = "LogitBoost",
#                  trControl = fitControl)
# logFit1

ggplot(extra.household, aes(x=TOILET, fill=poorfac)) + geom_bar(position='fill') + coord_flip()
ggplot(extra.household, aes(x=TV, fill=poorfac)) + geom_bar(position='fill') + coord_flip()
ggplot(extra.household, aes(x=GARBDISP, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Grabage disposal') + coord_flip()
ggplot(extra.household, aes(x=SEWMACH, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Sewing machine') + coord_flip()
ggplot(extra.household, aes(x=WALLS, fill=poorfac)) + geom_bar(position='fill') + coord_flip()
ggplot(extra.household, aes(x=STOVE, fill=poorfac)) + geom_bar(position='fill') + coord_flip()
ggplot(extra.household, aes(x=RADIO, fill=poorfac)) + geom_bar(position='fill') + coord_flip()

ggplot(extra.household, aes(x=HHAGEY, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Household Head Age') + coord_flip()
ggplot(extra.household, aes(x=HHSEX, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Household Head Sex') + coord_flip()
ggplot(extra.household, aes(x=HHMARST, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Household Head Martial Status') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
ggplot(extra.household, aes(x=HHEDLEV, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Household Head Ed level') + scale_x_discrete(labels= c('No Education', 'Pre school', 'Primary', 'Secondary', 'Upper secondary ', 'Post secondary (technical or vocational)', 'University', 'Literacy Program', 'Other', 'NA')) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
ggplot(extra.household, aes(x=OWNHOUSE, fill=poorfac)) + geom_bar(position='fill') + ggtitle('Is the house owned') + coord_flip()

ggplot(extra.household, aes(x=UND6RAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title ='Under 6 ratio') + scale_x_continuous(limits=c(0.0, 1.0))
ggplot(extra.household, aes(x=UND15RAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title = 'Under 15 ratio') + scale_x_continuous(limits=c(0.0, 1.0))
ggplot(extra.household, aes(x=UND21RAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title = 'Under 21 ratio') + scale_x_continuous(limits=c(0.0, 1.0))
ggplot(extra.household, aes(x=ABV60RAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title = 'Above 60 ratio') + scale_x_continuous(limits=c(0.0, 1.0))
ggplot(extra.household, aes(x=FEMRAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title = 'Female  ratio') + scale_x_continuous(limits=c(0.0, 1.0))
ggplot(extra.household, aes(x=LITRAT, fill=poorfac)) + geom_bar(position='fill', binwidth=0.1) + labs(title = 'Illiteracy ratio') + scale_x_continuous(limits=c(0.0, 1.0))

ggplot(extra.household, aes(x=HHEDLEV, fill=poorfac)) + geom_bar() + ggtitle('Household Head Ed level') + scale_x_discrete(labels= c('No Education', 'Pre school', 'Primary', 'Secondary', 'Upper secondary ', 'Post secondary (technical or vocational)', 'University', 'Literacy Program', 'Other', 'NA')) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(extra.household, aes(x=LITRAT, fill=poorfac)) + geom_bar(binwidth=0.1) + labs(title = 'Illiteracy ratio') + scale_x_continuous(limits=c(0.0, 1.0))

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 5)


#gbmFit1 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
gbmFit1 <- train(poor ~ TOILET + TV + GARBDISP + STOVE + HHMARST + HHEDLEV + OWNHOUSE + UND15RAT  + UND21RAT  + FEMRAT +LITRAT,
                 data = train.extra.household,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

gbmFit2 <- train(poor ~ UND6RAT + FEMRAT + WALLS + FLOOR + WATER + OWNHOUSE + ROOMS + ROOF,
                 data = train.extra.household,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit2

