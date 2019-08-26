#cross-sectional dataframe
##from df "compcut"

library(dplyr)
w1 <- data.frame(filter(compcut, wave==1))
w2 <- data.frame(filter(compcut, wave==2))
w3 <- data.frame(filter(compcut, wave==3))
w4 <- data.frame(filter(compcut, wave==4))

library(psych)
partial.r()

mmse1 <-lm(mmse_c ~ age + sex+ edu, data=w1)
summary(mmse1)
mmse2 <-lm(mmse_c ~ age + sex+ edu, data=w2)
summary(mmse2)
mmse3 <-lm(mmse_c ~ age + sex+ edu, data=w3)
summary(mmse3)
mmse4 <-lm(mmse_c ~ age + sex+ edu, data=w4)
summary(mmse4)

ind1 <-lm(mmse_c ~ n_degree_in + age + sex+ edu, data=w1)
summary(ind1)
ind2 <-lm(mmse_c ~ n_degree_in + age + sex+ edu, data=w2)
summary(ind2)
ind3 <-lm(mmse_c ~ n_degree_in + age + sex+ edu, data=w3)
summary(ind3)
ind4 <-lm(mmse_c ~ n_degree_in + age + sex+ edu, data=w4)
summary(ind4)


indegree1 <- glm(Cutoff ~ n_degree_in + sex+age+edu, data = w1, family=binomial)
summary(indegree1)


#paired t-test
t.test(w1$mmse_c, w4$mmse_c, paired =TRUE, var.equal=FALSE) #p>.05

#corr
cor.test(w1$age, w1$mmse_c)
cor.test(w2$age, w2$mmse_c)
cor.test(w3$age, w3$mmse_c)
cor.test(w4$age, w4$mmse_c)

#plot
plot(w1$age, w1$mmse_c)
plot(w2$age, w2$mmse_c)
plot(w3$age, w3$mmse_c)
plot(w4$age, w4$mmse_c)

#boxplot
library(lattice)
bwplot(mmse_c ~ age|wave, data=compcut,
       ylab="MMSE", xlab="Wave", 
       main="MMSE change by age",
       layout=(c(2,2)))

