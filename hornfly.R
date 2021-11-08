### hornfly counts for just control over time
boxplot(hornflydata$flies[hornflydata$trt=='control'] ~ hornflydata$week[hornflydata$trt=='control'])
#possible seasonal effect
#increase in the last 2 weeks?


### hornfly counts for treatments over time
boxplot(hornflydata$flies[hornflydata$trt!='control'] ~ hornflydata$week[hornflydata$trt!='control'])
#gradual increase from week 15


#### looking at various counties individually 
boxplot(flies[cty=='smith']~trt[cty=='smith'])

county <- unique(hf$cty)
par(mfrow=c(3,2), mai=c(.53,.53,.53,.53))
for (c in 1:15) {
  boxplot(hf$flies[hf$cty==county[c]]~hf$trt[hf$cty==county[c]], main = county[c], ylab= "Number of Flies", xlab= "Pesticide")
}


tb <- table(week, trt)
p <- ggplot(data= hornflydata, aes(x=trt,y=flies, group=factor(cty))) + geom_line(color='red')
p2 <- ggplot(data= hornflydata, aes(x=trt,y=flies)) + geom_line(color='red')
fitdist(hornflydata$flies[hornflydata$trt != 'control'], "norm")


par(mfrow=c(1,1))
pairs(hf)

####### START HERE! ########
hf <- data.frame(hornflydata)
hf <- na.omit(hf)

hf$cty <- as.factor(hf$cty)
hf$trt <- as.factor(hf$trt)
hf$week <- as.factor(hf$week)
#str(hf)

#hf_cont <- hf[hf$trt == 'control',]
#hf_treat <- hf[hf$trt != 'control',]
#pairs(hf_cont)
#pairs(hf_treat)
hist(hf$flies)#data looks highly right skewed

par(mfrow=c(2,1))
hist(hf_cont$flies, main = 'Histogram of Horn Fly Counts for the "Control Herds"', xlab = 'Control Herds: Horn Fly Counts')
hist(hf_treat$flies, main = 'Histogram of Horn Fly Counts for all "Pesticide-Treated Herds"', xlab = 'Pesticide-Treated Herds: Horn Fly Counts')

####
fit.4 <- lmer(sqrt(hf$flies) ~ hf$trt + (1|hf$cty))
fit.5 <- lm(sqrt(hf$flies) ~ hf$trt)

# take the mean of fly counts by 
avg_hf<-aggregate(hf$flies, by=list(txcounty=hf$cty, treat=hf$trt, txweek=hf$week), mean)
avg_treatment<- unique(avg_hf$treat)
avg_week <- unique(avg_hf$txweek)
avg_county <- unique(avg_hf$txcounty)

#install.packages("dplyr")
#avg_hf %>% 
#  group_by(txcounty, txweek) %>% 
#   diffs=x[match('control',treat)]-x[treat!='control'] -> d2

avg_hf_cty<-aggregate(avg_hf$x, by=list(county.avg=avg_hf$txcounty, treat.avg=avg_hf$treat), mean)

model_1 <- data.frame(model_one)
model_1$county <- as.factor(model_1$county)
model_1$treatment <- as.factor(model_1$treatment)

boxplot(model_1$percentage.reduction ~ model_1$treatment)

boxplot(model_1$percentage.reduction ~ model_1$treatment + model_1$county)

#fit.1 <- lm(model_1$percentage.reduction ~ model_1$treatment)
fit.1.d <- lm(model_1$percentage.reduction ~ model_1$treatment - 1) #we remove the intercept because of control
fit.aov.d <- aov(model_1$percentage.reduction ~ model_1$treatment -1)
########################

### combined pythonstrips to python
model_2 <- model_1[-1,]
model_2[model_2 == 'pythonstrips'] <- 'python'
View(model_2)

### same as earlier, treatment is significant - good
fit.2 <- lm(model_2$percentage.reduction ~ model_2$treatment- 1)
summary(fit.2)
plot(fit.2)

### county is significant - good
fit.3 <- lm(model_2$percentage.reduction ~ model_2$county - 1)
summary(fit.3)

#####  because dependent variable is continuous (and mostly normal), but all independent variables are categorical
fit.4 <- aov(model_2$percentage.reduction ~ model_2$treatment + model_2$county - 1)
summary(fit.4)


fit.5 <- lm(model_2$percentage.reduction ~ model_2$treatment + model_2$county - 1)
anova(fit.5)
plot(fit.5)
summary(fit.5)

##### does not work!!!
fit.6 <- lm(model_2$percentage.reduction ~ model_2$treatment*model_2$county - 1)
anova(fit.6)
plot(fit.6)
summary(fit.6)

fit.7 <- lm(model_2$percentage.reduction ~ model_2$treatment:model_2$county - 1)
anova(fit.7)
plot(fit.7)
summary(fit.7)

fit.8 <- lm(model_2$percentage.reduction ~ model_2$treatment -1 + model_2$county -1 )
anova(fit.8)
plot(fit.8)
summary(fit.8)

fit.9 <- lmer(model_2$percentage.reduction ~ model_2$treatment + (1 | model_2$county) - 1)
summary(fit.9)
anova(fit.9)
plot(fit.9)

fit.10 <- lmer(model_2$percentage.reduction ~ model_2$treatment*(1|model_2$county) - 1)
summary(fit.10)
anova(fit.10)
plot(fit.10)

fit.11 <- glm(model_2$percentage.reduction ~ model_2$treatment + model_2$county - 1)
anova(fit.11)
plot(fit.11)
summary(fit.11)

model_2$predicted_11 <- predict(fit.11)
model_2$residuals_11 <- residuals(fit.11)
library(dplyr)
library("ggplot2")

par(mfrow=c(2,2))
par(mfrow=c(1,1))
#ggplot(model_2, aes(x = (model_2$treatment + model_2$county -1), y = model_2$percentage.reduction))

plot(predict(fit.11), residuals(fit.11))
plot(model_2$predicted, model_2$residuals, col = as.numeric(col(model_2$predicted)), pch = 19)


fit.12 <- lm(model_2$percentage.reduction ~  model_2$treatment -1 + as.numeric(model_2$county))
summary(fit.12)
anova(fit.12)
plot(fit.12)
model_2$predicted <- predict(fit.12)
model_2$residuals <- residuals(fit.12)
plot(model_2$predicted, model_2$residuals, col = model_2$county)

plot(model_2$predicted, model_2$residuals, col= as.factor(model_2$county))

ggplot(model_2, aes(predicted, residuals, col = as.factor(county))) + geom_point()
ggplot(model_2, aes(predicted, residuals, col = as.factor(treatment))) + geom_point()


hist(model_2$residuals)
hist(model_2$residuals_11)
plot(density(resid(fit.12)))
qqnorm(resid(fit.12))
qqline(resid(fit.12))

