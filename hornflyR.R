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

#### checking if class is significant
fit.3b <- lm(model_2$percentage.reduction ~ model_2$class -1)
summary(fit.3b)

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

fit.13 <- lm(model_2$percentage.reduction ~  model_2$treatment -1 + model_2$county)
summary(fit.13)
anova(fit.13)
plot(fit.13)

model_2$class <- ifelse(model_2$treatment == 'warrior' | model_2$treatment == 'optimizer', 'organophosphate', ifelse(model_2$treatment == 'python' | model_2$treatment == 'magnum' | model_2$treatment == 'gardstar', 'pyrethroid', ifelse(model_2$treatment == 'xp820', 'macrocyclic_lactone', ifelse(model_2$treatment == 'vetgun', 'vetgun', '-'))))
model_2$class <- as.factor(model_2$class)


par(mfrow=c(2,2))
fit.14 <- lm(model_2$percentage.reduction ~ model_2$class + model_2$county - 1)
summary(fit.14)
anova(fit.14)
plot(fit.14)

fit.15 <- lm(model_2$percentage.reduction ~ model_2$class + as.numeric(model_2$county) - 1)
summary(fit.15)
anova(fit.15)
plot(fit.15)
model_2$predicted_15 <- predict(fit.15)
model_2$residuals_15 <- residuals(fit.15)
#### Residual Plot by County
g5 <- ggplot(model_2, aes(predicted_15, residuals_15, col = as.factor(county))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g6 = g5 + scale_color_manual(values=c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))
#### Residual Plot by Treatment Class
g7 <- ggplot(model_2, aes(predicted_15, residuals_15, col = as.factor(class))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g8 = g7 + scale_color_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066"))

ggplot2.multiplot(g6,g8, cols=2)


hf_new$predicted <- predict(fit.21)
hf_new$residuals <- residuals(fit.21)
ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(county))) + geom_point()
ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(class))) + geom_point()
ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(county))) + geom_point() + geom_smooth(method='lm', se = FALSE)

ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(class))) + geom_point() + geom_smooth(method='lm', se = FALSE)
ggplot(model_2, aes(predicted_14, residuals_14)) + geom_point() + geom_smooth(method='lm', col = 'red')

par(mfrow=c(1,2))

#### Residual Plot by County
g1 <- ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(county))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g4 = g1 + scale_color_manual(values=c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))
#### Residual Plot by Treatment Class
g2 <- ggplot(model_2, aes(predicted_14, residuals_14, col = as.factor(class))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g3 = g2 + scale_color_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066"))
#ggplot(model_2, aes(predicted_14, residuals_14, group = class)) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)


install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
# library grid
ggplot2.multiplot(g3,g4, cols=2)

hist(model_2$residuals_14)
plot(density(resid(fit.14)))

qqnorm(resid(fit.14))
qqline(resid(fit.14))

####### Maybe don't use this?!
boxplot(model_2$percentage.reduction ~ model_2$class)
par(mfrow=c(1,1))
par(mfrow=c(2,1))

boxplot(hf_new$percentage.reduction ~ hf_new$treatment_class, ylab = "Percentage Reduction", xlab = "Pesticide Class", main = "Percentage Reduction in Hornfly Counts by Class")

c1 <- rainbow(4)
c2 <- rainbow(4, alpha=0.2)
c3 <- rainbow(4, v=0.7)
boxplot(hf_new$percentage.reduction ~ hf_new$treatment_class, col=c2, 
        medcol=c3, whiskcol=c1, staplecol=c3, boxcol=c3, outcol=c3, pch=23, cex=2, 
        ylab = "Percentage Reduction", xlab = "Pesticide Class", 
        main = "Percentage Reduction in Hornfly Counts by Class")

b1 = ggplot(model_2, aes( model_2$class, model_2$percentage.reduction, fill = model_2$county)) + geom_boxplot()
b2 = b1 + scale_fill_manual(values=c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))
b2

b3 = ggplot(model_2, aes( model_2$county, model_2$percentage.reduction)) + geom_boxplot(aes(fill = model_2$class))
b4 = b3 + scale_fill_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066"))
b4

ggplot(model_2) + geom_boxplot(aes(x=model_2$class,y=model_2$percentage.reduction)) + facet_wrap(~as.factor(model_2$county), nrow=5)


b10 = ggplot(model_2, aes(model_2$county, model_2$percentage.reduction, fill=model_2$class)) + geom_bar(stat = "identity") + facet_wrap(~as.factor(model_2$class), nrow=5)
b11 = b10 + scale_fill_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066")) 
b11

b10 = ggplot(model_2, aes(model_2$treatment, model_2$percentage.reduction, fill=model_2$treatment)) + geom_bar(stat = "identity") + facet_wrap(~as.factor(model_2$county), nrow=5)
b11 = b10 + scale_fill_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066", "#99CC33", "#CC0000", "#000066",
                                       "#FFFF33")) 
b11


View(model_2)

ggplot(model_2, aes(sample=model_2$percentage.reduction)) + stat_qq()

q1 = ggplot(model_2, aes(sample = model_2$residuals_14)) + geom_qq(aes(color = model_2$class))
q2 = q1 + scale_color_manual(values=c("#336600", "#E69F00", "#56B4E9", "#990066"))
q2

q3 = ggplot(model_2, aes(sample = fit.14.stdres)) + geom_qq(aes(color = model_2$county))
q4 = q3 + scale_color_manual(values=c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))
q4

fit.14.stdres = rstandard(fit.14)

install.packages("ggpubr")
library("ggpubr")
ggboxplot(hf_new, x = "county_class", y = "percentage.reduction", color = "treatment_class",
          palette = c("#336600", "#E69F00", "#56B4E9", "#990066"))


interaction.plot(x.factor = model_2$class, trace.factor = model_2$county, 
                 response = model_2$percentage.reduction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Class", ylab="Percent Reduction",
                 pch=c(1,19), col = c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))

interaction.plot(x.factor = model_2$county, trace.factor = model_2$class, 
                 response = model_2$percentage.reduction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Class", ylab="Percent Reduction",
                 pch=c(1,19), col = c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))

fit.17 <- lm(model_2$percentage.reduction ~ model_2$treatment - 1)
Anova(fit.17)
anova(fit.17)

model_2 <- model_2[]

fit.17 <- lm(model_2$percentage.reduction ~ model_2$county - 1)
Anova(fit.17)
anova(fit.17)

fit.17 <- lm(model_2$percentage.reduction ~ model_2$class - 1)
Anova(fit.17)
Anova(fit.17)

fit.17 <- lm(model_2$percentage.reduction ~ model_2$class + model_2$county - 1)
Anova(fit.17, type = "III")
anova(fit.17)

###################
## http://texas.resiliencesystem.org/ru/texas-climate-divisions

hf <- data.frame(final_hornfly_a)
hf$treatment_class <- as.factor(hf$treatment_class)
hf$county_class <- as.factor(hf$county_class)

#fit.20 <- lm(hf$percentage.reduction ~ hf$treatment_class + hf$county_class -1)
#anova(fit.20)
#summary(fit.20)
#par(mfrow=c(2,2))
#plot(fit.20)



par(mfrow=c(1,1))
interaction.plot(x.factor = hf_new$treatment_class, trace.factor = hf_new$county_class, 
                 response = hf_new$percentage.reduction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Treatment Class", ylab="Percent Reduction",
                 pch=c(19,19), lty=1, col = c("#FF9933", "#99CC33"))

interaction.plot(x.factor = hf_new$county_class, trace.factor = hf_new$treatment_class, 
                 response = hf_new$percentage.reduction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "County Class", ylab="Percent Reduction",
                 pch=c(19,19), lty=1, col = c("steelblue", "violetred3", "orange2", "springgreen4"))

###### we remove the -3% reduction value as an outlier
hf_new <- hf[-5,]
fit.21 <- lm(hf_new$percentage.reduction ~ hf_new$treatment_class + hf_new$county_class -1)
anova(fit.21)
summary(fit.21)
par(mfrow=c(2,2))
plot(fit.21)

hf_new$predicted <- predict(fit.21)
hf_new$residuals <- residuals(fit.21)

q1 = ggplot(hf_new, aes(sample = hf_new$residuals)) + geom_qq(aes(color = hf_new$treatment_class))
q2 = q1 + scale_color_manual(values=c("steelblue", "violetred3", "orange2", "springgreen4")) + theme(axis.text=element_text(size=12))
q2

b3 = ggplot(hf_new, aes(hf_new$county_class, hf_new$percentage.reduction)) + geom_boxplot(aes(fill = hf_new$treatment_class))
b4 = b3 + scale_fill_manual(values=c("steelblue", "violetred3", "orange2", "springgreen4")) + 
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=12), 
        axis.title.y=element_text(size=12), legend.text=element_text(size=12), legend.title =element_text(size=12)) + xlab('County Region') +
  ylab('Percentage Reduction in Horn Fly counts') + 
  labs(fill='Treatment Class')
  

b4

g2 <- ggplot(hf_new, aes(predicted, residuals, col = as.factor(treatment_class))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g3 = g2 + scale_color_manual(values=c("steelblue", "violetred3", "orange2", "springgreen4"))
g3


g4 <- ggplot(hf_new, aes(predicted, residuals, col = as.factor(county_class))) + geom_point() + geom_smooth(method='lm', col = 'red', size = 0.2)
g5 = g4 + scale_color_manual(values=c("#FF9933", "#99CC33"))
g5
#######
#q3 = ggplot(model_2, aes(sample = fit.14.stdres)) + geom_qq(aes(color = model_2$county))
q4 = q3 + scale_color_manual(values=c("#CC3300", "#660000", "#FFCC00", "#FF6600", "#003333", "#333399", "#CC0099",
                                      "#006666", "#3399FF", "#33CC00", "#FF9933", "#99CC33", "#CC0000", "#000066",
                                      "#FFFF33"))
#q4


final_hornfly_a <- final_hornfly_a[-5,]
fit.22 <- lm(final_hornfly_a$percentage.reduction ~  final_hornfly_a$treatment_class + final_hornfly_a$county_class -1)
aov.22 <- aov(final_hornfly_a$percentage.reduction ~  final_hornfly_a$treatment_class + final_hornfly_a$county_class -1)
anova(fit.22)
summary(fit.22)
par(mfrow=c(2,2))
plot(fit.22)
print(lsmeans(fit.22, list(pairwise ~ treatment_class)), adjust = c("tukey"))
TUKEY <- TukeyHSD(x=anova(fit.22), 'final_hornfly_a$treatment_class', conf.level=0.95)
tuk <- TukeyHSD(x=aov.22)
par(mar = c(4,10,4,2) + 0.1)
