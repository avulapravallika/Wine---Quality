library(tidyverse)
library(dplyr)
library(GGally)
library(corrplot)
library(rpart)
library(rpart.plot)
library(DMwR2)   # Contains rt.prune
library(ISLR)    
library(MASS)
library(randomForest)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(maditr)
library(ggpubr)
library(Metrics)
library(glmnet)
library(mlbench)
library(caret) # use createDataPartition() function 
library(gridExtra)

#Data reading
white_df <- read.csv(file = "winequality-white.csv", as.is = TRUE,sep = ";", header = TRUE)
head(white_df)
dim(white_df)

#Data overview
str(white_df)
summary(white_df)

#Checking missing values
which(is.na(white_df))
colSums(is.na(white_df))
#Build correlation and order by decreasing
set.seed(123)

white_dfcor <- cor(white_df)
corrplot(white_dfcor, method = "color", addCoef.col = "black",number.cex = .6,
         tl.col = "black", tl.srt = 90, diag = FALSE)

#Build correlation atts with Quality variable
dfcor <- cor(white_df)
quality_cor <- dfcor[,12]
absoutcome_cor <- abs(quality_cor)
head(absoutcome_cor[order(absoutcome_cor, decreasing = TRUE)],12)

#Analysing the overall quality
ggplot(white_df, aes(quality))+ 
  geom_histogram() + 
  labs(title = "Histogram of quality of white wine") + 
  theme(plot.title=element_text(hjust=0.5)) +
  geom_vline(aes(xintercept=mean(quality)), color="blue", linetype="dashed", size=1) +
  geom_text(aes(x=5.6, label="Mean Value", y=400), colour="red", angle=90, vjust = 1.2)

mean(white_df$quality)
#Plotting the marginal distributions of numerical quantities of interest using density plots

d0 <- ggplot(white_df, aes(x=quality)) + 
  geom_density()
d0 <- d0 + geom_vline(aes(xintercept=mean(quality)),
                      color="blue", linetype="dashed")
d1 <- ggplot(white_df, aes(x=fixed.acidity)) + 
  geom_density()
d1 <- d1 + geom_vline(aes(xintercept=mean(fixed.acidity)),
                      color="blue", linetype="dashed")
d2 <- ggplot(white_df, aes(x=volatile.acidity)) + 
  geom_density()
d2 <- d2 + geom_vline(aes(xintercept=mean(volatile.acidity)),
                      color="blue", linetype="dashed")
d3 <- ggplot(white_df, aes(x=citric.acid)) + 
  geom_density()
d3 <- d3 + geom_vline(aes(xintercept=mean(citric.acid)),
                      color="blue", linetype="dashed")
d4 <- ggplot(white_df, aes(x=residual.sugar)) + 
  geom_density()
d4 <- d4 + geom_vline(aes(xintercept=mean(residual.sugar)),
                      color="blue", linetype="dashed")
d5 <- ggplot(white_df, aes(x=chlorides)) + 
  geom_density()
d5 <- d5 + geom_vline(aes(xintercept=mean(chlorides)),
                      color="blue", linetype="dashed")
d6 <- ggplot(white_df, aes(x=free.sulfur.dioxide)) + 
  geom_density()
d6 <- d6 + geom_vline(aes(xintercept=mean(free.sulfur.dioxide)),
                      color="blue", linetype="dashed")
d7 <- ggplot(white_df, aes(x=total.sulfur.dioxide)) + 
  geom_density()
d7 <- d7 + geom_vline(aes(xintercept=mean(total.sulfur.dioxide)),
                      color="blue", linetype="dashed")
d8 <- ggplot(white_df, aes(x=density)) + 
  geom_density()
d8 <- d8 + geom_vline(aes(xintercept=mean(density)),
                      color="blue", linetype="dashed")
d9 <- ggplot(white_df, aes(x=pH)) + 
  geom_density()
d9 <- d9 + geom_vline(aes(xintercept=mean(pH)),
                      color="blue", linetype="dashed")
d10 <- ggplot(white_df, aes(x=sulphates)) + 
  geom_density()
d10 <- d10 + geom_vline(aes(xintercept=mean(sulphates)),
                        color="blue", linetype="dashed")
d11 <- ggplot(white_df, aes(x=alcohol)) + 
  geom_density()
d11 <- d11 + geom_vline(aes(xintercept=mean(alcohol)),
                        color="blue", linetype="dashed")
ggarrange(d0, d1, d2, d3, nrow = 2, ncol =2)
ggarrange(d4, d5, d6, d7, nrow = 2, ncol =2)
ggarrange(d8, d9, d10, d11, nrow = 2, ncol =2)

#Plotting the marginal distributions of numerical quantities of interest using box plots
b1 <- boxplot(white_df$fixed.acidity, col="slategray2", pch=20)
b2 <- boxplot(white_df$volatile.acidity, col="slategray2", pch=20)
b3 <- boxplot(white_df$citric.acid, col="slategray2", pch=20)
b4 <- boxplot(white_df$residual.sugar, col="slategray2", pch=20)
b5 <- boxplot(white_df$chlorides, col="slategray2", pch=20)
b6 <- boxplot(white_df$free.sulfur.dioxide, col="slategray2", pch=20)
b7 <- boxplot(white_df$total.sulfur.dioxide, col="slategray2", pch=20)
b8 <- boxplot(white_df$density, col="slategray2", pch=20)
b9 <- boxplot(white_df$pH, col="slategray2", pch=20)
b10 <- boxplot(white_df$sulphates, col="slategray2", pch=20)
b11 <- boxplot(white_df$alcohol, col="slategray2", pch=20)
b12 <- boxplot(white_df$quality, col="slategray2", pch=20)

#Plotting quality against numerical variables
bp1 <- ggplot(white_df, aes(factor(quality), fixed.acidity, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "fixed.acidity", title = "Boxplot of Quality vs. fixed.acidity") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp1
bp2 <- ggplot(white_df, aes(factor(quality), volatile.acidity, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "volatile.acidity", title = "Boxplot of Quality vs. volatile.acidity") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp2
bp3 <- ggplot(white_df, aes(factor(quality), citric.acid, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "citric.acid", title = "Boxplot of Quality vs. citric.acid") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp3
bp4 <- ggplot(white_df, aes(factor(quality), residual.sugar, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "residual.sugar", title = "Boxplot of Quality vs. residual.sugar") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp4
ggarrange(bp1, bp2, bp3, bp4, nrow = 2, ncol =2)

bp5 <- ggplot(white_df, aes(factor(quality), chlorides, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "Quality", y = "chlorides", title = "Boxplot of Quality vs. chlorides") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp5
bp6 <- ggplot(white_df, aes(factor(quality), free.sulfur.dioxide, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "free.sulfur.dioxide", title = "Boxplot of quality vs. free.sulfur.dioxide") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp6
bp7 <- ggplot(white_df, aes(factor(quality), total.sulfur.dioxide, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "total.sulfur.dioxide", title = "Boxplot of quality vs. total.sulfur.dioxide") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp7
bp8 <- ggplot(white_df, aes(factor(quality), density, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "density", title = "Boxplot of quality vs. density") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp8
ggarrange(bp5, bp6, bp7, bp8, nrow = 2, ncol =2)

bp9 <- ggplot(white_df, aes(factor(quality), pH, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "pH", title = "Boxplot of Quality vs. pH") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp9
bp10 <- ggplot(white_df, aes(factor(quality), sulphates, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "sulphates", title = "Boxplot of quality vs. sulphates") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp10
bp11 <- ggplot(white_df, aes(factor(quality), alcohol, fill=factor(quality))) + 
  geom_boxplot() +
  labs(x = "quality", y = "alcohol", title = "Boxplot of quality vs. alcohol") + 
  theme(legend.position = 'none', plot.title = element_text(size = 10, hjust=0.5))
bp11
ggarrange(bp9, bp10, bp11, nrow = 2, ncol =2)

#Analysing relationship among numerical variables
gg1 <- ggplot(white_df, aes(x=residual.sugar, y=density)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="density vs. residual sugar") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg1

gg2 <- ggplot(white_df, aes(x=alcohol, y=density)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="density vs. alcohol") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg2

gg3 <- ggplot(white_df, aes(x=fixed.acidity, y=pH)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="pH vs. fixed.acidity") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg3

gg4 <- ggplot(white_df, aes(x=volatile.acidity, y=pH)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="pH vs. volatile.acidity") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg4

gg5 <- ggplot(white_df, aes(x=citric.acid, y=pH)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="pH vs. citric.acid") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg5

gg6 <- ggplot(white_df, aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="free.sulfur.dioxide vs. total.sulfur.dioxide") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg6

gg7 <-ggplot(white_df, aes(x=sulphates, y=free.sulfur.dioxide)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="free.sulfur.dioxide vs. sulphates") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg7

gg8 <-ggplot(white_df, aes(x=sulphates, y=total.sulfur.dioxide)) +
  geom_point(color="blue",size=0.7) + 
  labs(title="total.sulfur.dioxide vs. sulphates") +
  geom_smooth(formula=y~x,method=lm, color="red") +
  theme(plot.title=element_text(hjust=0.5))
gg8

ggarrange(gg1, gg2, gg3, gg4, nrow = 2, ncol =2)
ggarrange(gg5, gg6, gg7, gg8, nrow = 2, ncol =2)


#Baseline Random Forest Model
whitewineRF<-randomForest(quality ~ .,white_df,ntree=150)

whitewineRF
# Get importance
Importance    <- importance(whitewineRF)
varImportance <- data.frame(Variables = row.names(Importance), 
                            Importance = (Importance))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
importance(whitewineRF)
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() 

#importance plot
importance(whitewineRF)
varImpPlot(whitewineRF, main="Plots of importance measures")

ggpairs(white_df)

#Linear regression model with top 4 highest correlation with Total Incidents
white_lm <- lm(quality ~ alcohol + volatile.acidity +  sulphates + total.sulfur.dioxide, data = white_df)
summary(white_lm)

# Define training control
set.seed(555)
train <- white_df[1:800, ]
test <- white_df[801:nrow(white_df), ]
# Train the model
white_model <- lm(quality ~ alcohol + volatile.acidity +  sulphates + total.sulfur.dioxide, data=train)
summary(white_model)

#calculate MSE
mean(white_model$residuals^2)
#calculate RMSE
sqrt(mean(white_model$residuals^2))
#calculate MAE
predValues <- predict(white_model,test)
#MAE for the model
mean(abs(test$quality -predValues)) 

#Lasso regression
x <- model.matrix(quality~., white_df)[,-1]
y <- white_df$quality
mod <- cv.glmnet(as.matrix(x), y, alpha=1)

#coefficients with the minimum cross-validation error
as.matrix(coef(mod, mod$lambda.min))
#coefficients with the "largest value of lambda such that error is 
#within 1 standard error of the minimum
as.matrix(coef(mod, mod$lambda.1se))

#Lasso regression model using above independent variables
white_lm1 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide 
              + sulphates + alcohol, data=white_df)
summary(white_lm1)

# Define training control
set.seed(555)
train <- white_df[1:800, ]
test <- white_df[801:nrow(white_df), ]
# Train the model
white_model1 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide 
                 + sulphates + alcohol, data=train)
summary(white_model1)

#calculate MSE
mean(white_model1$residuals^2)
#calculate RMSE
sqrt(mean(white_model1$residuals^2))
#calculate MAE
predValues <- predict(white_model1,test)
#MAE for the model
mean(abs(test$quality -predValues)) 

#Random forest
set.seed(555)
train <- white_df[1:800, ]
test <- white_df[801:nrow(white_df), ]
white_model3 <- randomForest(quality ~ ., train, mtry = 3, 
                           importance = TRUE, na.action = na.omit)
print(white_model3)
#Plot the error vs the number of trees graph 
plot(white_model3) 

importance(white_model3)
varImpPlot(white_model3,type=2)

# MSE for the model
white_model3$mse[length(white_model3$mse)]
# RMSE for the model
sqrt(white_model3$mse[length(white_model3$mse)])

predValues <- predict(white_model3,test)
#MAE for the model
mean(abs(test$quality -predValues)) 

#Comparing models

Model <- c("Linear", "Lasso", "Random-Forest")
R_squared <- c(0.2507, 0.2541, 0.5209)
RMSE <- c(0.7718813, 0.7700965, 0.6172284)
MAE <- c(0.6127622, 0.6097995, 0.5946836)
MSE <- c(0.5958007, 0.5930486, 0.3809709)
ml <- data.frame(Model, R_squared, RMSE, MAE, MSE)


p1 <- ggplot(ml, aes(Model, RMSE)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="RMSE") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
p2 <- ggplot(ml, aes(Model, R_squared)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="R-Squared") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
p3 <- ggplot(ml, aes(Model, MAE)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="MAE") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
p4 <- ggplot(ml, aes(Model, MSE)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="MSE") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
ggarrange(p2,p1,p3,p4, nrow=2, ncol=2)

#clustering to find relation between predictors
set.seed(1941)
white_k <- kmeans(white_df, 2, nstart = 25)
names(white_k)
white_k$centers
c1 <- ggplot(white_df)+geom_point(aes(x=pH,y=alcohol ,color=white_k$cluster))
c1
c2 <- ggplot(white_df)+geom_point(aes(x=pH,y=total.sulfur.dioxide ,color=white_k$cluster))
c2
c3 <- ggplot(white_df)+geom_point(aes(x=pH,y=citric.acid ,color=white_k$cluster))
c3
c4 <- ggplot(white_df)+geom_point(aes(x=alcohol,y=volatile.acidity ,color=white_k$cluster))
c4

ggarrange(c2,c1,c3,c4, nrow=2, ncol=2)

#clustering
set.seed(1406)
kmeans.re <- kmeans(white_df, centers = 3, nstart = 25)
kmeans.re

plot(white_df[c("pH", "quality")], 
     col = kmeans.re$cluster, 
     main = "white Wine clusters")

#Classification trees
library(party)
tree1=ctree(quality~alcohol+volatile.acidity,data=white_df) #~target variable,predicting 
tree1

library(rpart)
mytree=rpart(quality~alcohol+volatile.acidity,data=white_df,method="class")
mytree

library(rattle)
library(RColorBrewer)
fancyRpartPlot(mytree,caption="white wine Classification")

