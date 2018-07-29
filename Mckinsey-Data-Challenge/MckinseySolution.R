#***********************************************************************************************************
# This Code was submitted for a data Challencge Hosted by Analytics Vidhya partnered with Mckinsey Analytics
# Submitted By : Sumeet Kukreja
# emailid : skukre3@uic.edu
#***********************************************************************************************************


#****************************************************************************
# Import the data set

data <- read.csv(file.choose(), header = T)

#Exploration and Manipulation

str(data)

data$renewal <- as.factor(data$renewal)

install.packages('DataExplorer') 
library(DataExplorer)

plot_str(data)
plot_missing(data)

#handling missing values

plot_density(data$Count_3.6_months_late)
plot_density(data$Count_6.12_months_late)
plot_density(data$Count_more_than_12_months_late)

# mice for imputing missing values
install.packages("mice")
library(mice)

md.pattern(data)
imputed_Data <- mice(data, m=5, maxit = 10, method = 'cart', seed = 500)
summary(imputed_Data)
imputed_Data$imp$Count_3.6_months_late

#get complete data (using the 4th one)
completeData <- complete(imputed_Data,4)
summary(completeData)

#EDA
plot_density(completeData$perc_premium_paid_by_cash_credit)
plot_density(completeData$age_in_days)
plot_density(completeData)
plot_bar(completeData)
plot_correlation(completeData, type = 'continuous')


# Remove the Id variable
completeData <- completeData[,-1]

#Need to normalize the data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

completeData_norm <- as.data.frame(lapply(completeData[,c(1:8,11)], normalize))
df_norm<- cbind(completeData_norm, completeData[,c(9,10,12)])

#****************************************************************************
# Splt the data into train and test(validation)
smp_size <- floor(0.70 * nrow(df_norm))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_norm)), size = smp_size)

train <- df_norm[train_ind, ]
test <- df_norm[-train_ind, ]

#****************************************************************************
# Decision tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
train.rpart <- rpart(renewal~., data = train, method = "class",
                     control = rpart.control(minsplit = 5), parms = list(split = "gini"))
train.rpart
rpart.plot(train.rpart)
summary(train.rpart)
pred.rpart <- predict(train.rpart, newdata = test)

# opt <- which.min(train.rpart$cptable[ ,"xerror"])
# cp <- train.rpart$cptable[opt,"CP"]

# Check the accuracy on test data
library(ROSE)
accuracy.meas(test$renewal, pred.rpart[,2])
roc.curve(test$renewal, pred.rpart[,2], plotit = F) #AUC : 0.633


# Since the data is unbalanced, need to do balance it first

#over sampling
data_balanced_over <- ovun.sample(renewal ~ ., data = train, method = "over",N = 104900)$data
table(data_balanced_over$renewal)

#Under Sampling
data_balanced_under <- ovun.sample(renewal ~ ., data = train, method = "under", N = 6894, seed = 1)$data
table(data_balanced_under$renewal)

#Both
data_balanced_both <- ovun.sample(renewal ~ ., data = train, method = "both", p=0.5,N=55897, seed = 1)$data
table(data_balanced_both$renewal)

#Generating data synthetically
data.rose <- ROSE(renewal ~ ., data = train, seed = 1)$data
table(data.rose$renewal)

#Now lets build tree models for all of the sampling methods
tree.rose <- rpart(renewal ~ ., data = data.rose)
tree.over <- rpart(renewal ~ ., data = data_balanced_over)
tree.under <- rpart(renewal ~ ., data = data_balanced_under)
tree.both <- rpart(renewal ~ ., data = data_balanced_both)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)

#AUC ROSE
roc.curve(test$renewal, pred.tree.rose[,2])
# Area under the curve (AUC): 0.679

#AUC Oversampling
roc.curve(test$renewal, pred.tree.over[,2])
# Area under the curve (AUC): 0.743

#AUC Undersampling
roc.curve(test$renewal, pred.tree.under[,2])
# Area under the curve (AUC): 0.743

#AUC Both
roc.curve(test$renewal, pred.tree.both[,2])
# Area under the curve (AUC): 0.743

#Check for variance
ROSE.holdout <- ROSE.eval(renewal ~ ., data = train, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout

#I'll use the Over sampling method
# Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
# Calculating the values for ROC curve
pred = prediction(pred.tree.over[,1], test$renewal)
perf = performance(pred,"tpr","fpr")
# Plotting the ROC curve
plot(perf, col = "black", lty = 3, lwd = 3)

# Calculating AUC
auc = performance(pred, "auc")
# Now converting S4 class to a vector
auc = unlist(slot(auc, "y.values"))
# Adding min and max ROC AUC to the center of the plot
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) =" ), minauc, sep = "")
maxauct = paste(c("max(AUC) =" ), maxauc, sep = "")
legend(0.7, 0.5, c(minauct, maxauct, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)

#Get the optimal cut-off
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))
#Cut off probability is 0.7984245

#****************************************************************************
# Apply the same method for random forest
library(randomForest)
mtry=if (!is.null(data.rose$renewal) && !is.factor(data.rose$renewal)) 
{max(floor(ncol(data.rose)/3), 1)} else floor(sqrt(ncol(data.rose)))
rf.rose <- randomForest(renewal ~ . , data = data.rose, ntree = 200, mtry = 3, proximity =
                               FALSE, replace = TRUE, sampsize = nrow(data.rose), importance = TRUE ) 

table(predict(rf.rose), data.rose$renewal)
plot(rf.rose)
rndF1.legend <- if (is.null(rf.rose$data.rose$err.rate)) {colnames(rf.rose$err.rate)} else
{colnames(rf.rose$data.rose$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)
varImpPlot(rf.rose)


#****************************************************************************
#predict on test
pred.rf.rose <- predict(rf.rose, newdata = test, type = "prob")
roc.curve(test$renewal, pred.rf.rose[,1]) #Area under the curve (AUC): 0.845

pred.rf = prediction(pred.rf.rose[,1], test$renewal)
perf.rf = performance(pred.rf,"tpr","fpr")
print(opt.cut(perf.rf, pred.rf))
# Load the test Data

unseendata <- read.csv(file.choose(), header = T)

str(unseendata)

# add the predicted probability

#Check the AUC for unseen data
unseen.tree.rose <- predict(tree.rose, newdata = unseendata)
unseen.tree.over <- predict(tree.over, newdata = unseendata)
unseen.tree.under <- predict(tree.under, newdata = unseendata)
unseen.tree.both <- predict(tree.both, newdata = unseendata)

unseendata$renewal_rose <- predict(tree.rose, newdata = unseendata)[,1]
unseendata$renewal_under <- predict(tree.under, newdata = unseendata)[,1]
unseendata$renewal_over <- predict(tree.over, newdata = unseendata)[,1]
unseendata$renewal_both <- predict(tree.both, newdata = unseendata)[,1]
if (unseendata$renewal_rose < 0.67) {
  unseendata$incentives = 2800
} else if (unseendata$renewal_rose > 0.7984245) {
  unseendata$incentives = 0
} 


# For Randomforest
rf.newdata<- read.csv(file.choose(), header = T)
rf.newdata$renewal_rose <- predict(rf.rose, newdata = rf.newdata, type = "prob")[,1]
for(i in 1:nrow(rf.newdata)) {
  if(rf.newdata$renewal_rose <0.3425)
    {rf.newdata$incentives[i] = 100}
  else if (rf.newdata$renewal_rose <0.385)
    {rf.newdata$incentives[i] = 150}
  else if (rf.newdata$renewal_rose <0.4275)
    {rf.newdata$incentives[i] = 200}
  else if (rf.newdata$renewal_rose <0.47)
    {rf.newdata$incentives[i] = 250}
  else if (rf.newdata$renewal_rose <0.5125)
    {rf.newdata$incentives[i] = 300}
  else if (rf.newdata$renewal_rose <0.555)
    {rf.newdata$incentives[i] = 400}
  else if (rf.newdata$renewal_rose <0.5975)
    {rf.newdata$incentives[i] = 500}
  else if (rf.newdata$renewal_rose <0.64)
    {rf.newdata$incentives[i] = 550}
  else if (rf.newdata$renewal_rose <0.6825)
    {rf.newdata$incentives[i] = 800}
  else if (rf.newdata$renewal_rose <0.725)
    {rf.newdata$incentives[i] = 1900}
}
#write solution to csv

write.csv(rf.newdata, file = "Solution4.csv")
#Further manipulation and incentive plans are done on excel
