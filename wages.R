#load the libraries
library(ggplot2)
library(arm)
library(pROC)
library(e1071)
library(caret)
require(gridExtra)
require(rms)

#loading the data
data <- read.csv("wages.txt")


#####preparing the data
data$treat <- factor(data$treat, levels=c(1,0), labels=c("training","no training"))
data$black <- factor(data$black, levels=c(1,0), labels=c("black","not black"))
data$hispan <- factor(data$hispan, levels=c(1,0), labels=c("hispanic","not hispanic"))
data$married <- factor(data$married, levels=c(1,0), labels=c("married","not married"))
data$nodegree <- factor(data$nodegree, levels=c(1,0), labels=c("no degree","degree"))

# factor re78 if income is not zero
data$re78_fact <-ifelse(data$re78>0,1,0)
data$re78_fact <- factor(data$re78_fact, levels=c(1,0), labels=c("income","no income"))



####EDA
tapply(data$re74, list(data$treat_fact), mean)
tapply(data$re75, list(data$treat_fact), mean)
tapply(data$re78, list(data$treat_fact), mean)

p = ggplot(data, aes(x=re74, y=re78)) 
p + geom_point(aes(colour = treat_fact))

p = ggplot(data, aes(x=re74, y=re78)) + geom_point(color = "steelblue")
p + facet_wrap(~treat_fact)

p = ggplot(data, aes(x=re75, y=re78)) + geom_point(color = "steelblue")
p + facet_wrap(~treat_fact)


### Visualizing the interaction
final_1 <- glm(re78_fact ~ treat + age + black + treat:age, data = data, family = binomial)
summary(final_1)

age <- seq(from = min(data$age), to = max(data$age), by = 1)
new_wages <- data.frame(age)

#black = 1, treat = training, educnew = 1, married = 1
new_wages$treat <- factor("training",levels=levels(data$treat))
new_wages$black <- factor("black",levels=levels(data$black))
new_wages$educnew <- 1
new_wages$married <- factor("married",levels=levels(data$married))
predprob_training <- predict(another_model,new_wages,type='response')

#black = 1, treat = no training
new_wages$treat <- factor("no training",levels=levels(data$treat))
new_wages$black <- factor("black",levels=levels(data$black))
new_wages$educnew <- 1
new_wages$married <- factor("married",levels=levels(data$married))
predprob_notraining <- predict(another_model,new_wages,type='response')

#black = 0, treat = training
new_wages$treat <- factor("training",levels=levels(data$treat))
new_wages$black <- factor("not black",levels=levels(data$black))
new_wages$educnew <- 1
new_wages$married <- factor("married",levels=levels(data$married))
predprob_training_nb <- predict(another_model,new_wages,type='response')

#black = 0, treat = no training
new_wages$treat <- factor("no training",levels=levels(data$treat))
new_wages$black <- factor("not black",levels=levels(data$black))
new_wages$educnew <- 1
new_wages$married <- factor("married",levels=levels(data$married))
predprob_notraining_nb <- predict(another_model,new_wages,type='response')


plot(y = predprob_training, x = age, ylab = "Predicted probability", xlab = "Age", 
     main = "Predicted Probability vs. Age by Treat",ylim=c(0,1),pch= 1,
     col="hotpink4")
points(y=predprob_notraining, x=age, pch= 2,col="hotpink1")
points(y=predprob_training_nb, x=age, pch= 3,col="deepskyblue4")
points(y=predprob_notraining_nb, x=age, pch= 4,col="deepskyblue2")
legend('topright',pch=c(1,2,3,4),bty = "n",col=c('hotpink4','hotpink1','deepskyblue4','deepskyblue2'),
       c('black = 0; treat = training','black = 0; treat = no training','black = 1; treat = training','black = 1; treat = no training'))



###NEW model with educnew
data$educnew <- ifelse(data$educ>8,1,0)

# re-fit the model with educnew
model2 <- glm(re78_fact ~ age + treat + married + hispan + black + nodegree + educnew, data = data, family = binomial)
summary(model2)

FullModel2 <- glm(re78_fact ~ age * (treat + black + hispan + married + nodegree + educnew), data = data, family = binomial)
null_model <- glm(re78_fact~treat,data=data,family=binomial)
another_model <- step(null_model,scope=formula(FullModel2),direction="both",
                      trace=0)
summary(another_model)



### INTERESTING INSIGHT
int_data <- subset(data , re74 > 0 & re75 == 0 & re78 > 0)


int_data$subs <- int_data$re78 - int_data$re74
int_data$row_num <- seq.int(nrow(int_data))
# Change baseline
print(ggplot(int_data, aes(x=X, y=subs)) +
  geom_segment( aes(x=X, xend=X, y=1, yend=subs), color="lightpink1") +
  geom_point( color="lightpink3", size=4) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("ID") +
  ylab("re78 - re74")
+ ggtitle("Difference between 1978 and 1974 Incomes for Workers who had 0 Income in 1975"))

test_data <- subset(data , re74 > 0 & re75 == 0 & re78 > 0)


                                              