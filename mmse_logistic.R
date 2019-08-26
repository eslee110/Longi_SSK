#logistic regression
##using df "compcut"
compcut$Cutoff <-ifelse(compcut$cutoff =="Impaired", 1, 0)

##cross-sectional
library(dplyr)
w1 <- data.frame(filter(compcut, wave==1))
w1m <-data.frame(filter(w1, sex==1))
w1f <-data.frame(filter(w1, sex==2))
w2 <- data.frame(filter(compcut, wave==2))
w3 <- data.frame(filter(compcut, wave==3))
w4 <- data.frame(filter(compcut, wave==4))

glm.inout1 <-glm(Cutoff ~ n_degree_in+n_degree_out+sex+age+edu, data=w1, family=binomial)
summary(glm.inout1)

glm.inout2 <-glm(Cutoff ~ n_degree_in+n_degree_out+sex+age+edu, data=w2, family=binomial)
summary(glm.inout2)

glm.inout3 <-glm(Cutoff ~ n_degree_in+n_degree_out+sex+age+edu, data=w3, family=binomial)
summary(glm.inout3)

glm.inout4 <-glm(Cutoff ~ n_degree_in+n_degree_out+sex+age+edu, data=w4, family=binomial)
summary(glm.inout4)


glm.kcore1 <-glm(Cutoff ~ n_kcore_all+sex+age+edu, data=w1, family=binomial)
summary(glm.kcore1)

glm.kcore2 <-glm(Cutoff ~ n_kcore_all+sex+age+edu, data=w2, family=binomial)
summary(glm.kcore2)

glm.kcore3 <-glm(Cutoff ~ n_kcore_all+sex+age+edu, data=w3, family=binomial)
summary(glm.kcore3)

glm.kcore4 <-glm(Cutoff ~ n_kcore_all+sex+age+edu, data=w4, family=binomial)
summary(glm.kcore4)

glm.constraint1 <-glm(Cutoff ~ n_constraint+sex+age+edu, data=w1, family=binomial)
summary(glm.constraint1)


glm.feel1 <-glm(Cutoff ~ n_feel_g+sex+age+edu, data=w1, family=binomial)
summary(glm.feel1)

glm.comm1 <-glm(Cutoff ~ n_comm_g+sex+age+edu, data=w1, family=binomial)
summary(glm.comm1)

glm.cent1 <-glm(Cutoff ~ n_feel_g+n_degree_in+n_degree_out+sex+age+edu, data=w1, family=binomial)
summary(glm.cent1)


#training_prediction tryout
##data=compcut
train=(compcut$wave<4)
pred<- compcut[!train, ]

table(w3$Cutoff==1)
View(w3)

glm.indegree <-glm(Cutoff ~ n_degree_in,data=compcut, family=binomial, subset=train)
glm.probs=predict(glm.degree, pred, type="response")
glm.pred=rep("Impaired", 397)
glm.pred[glm.probs>.5]="Impaired"
table(glm.pred, w3)
mean(glm.pred==w3)


predicted=predict(glm.indegree, newdata=w3, type="response")

plot(Cutoff~n_degree_in, data=train, col="red4")
lines(Cutoff~n_degree_in, predicted, col="green4", lwd=2)


fit = glm(output ~ maxhr, data=heart, family=binomial)
predicted = predict(fit, newdata=heart, type="response")
plot(output~maxhr, data=heart, col="red4")
lines(heart$maxhr, predicted, col="green4", lwd=2)


#compcut as crosssectional...(meaningless)
glm.indegree <-glm(Cutoff ~ n_degree_in + sex+age+edu, data = compcut, family=binomial)
summary(glm.indegree)
glm.outdegree <-glm(Cutoff ~ n_degree_out + sex+age+edu, data = compcut, family=binomial)
summary(glm.outdegree)
glm.kcore <-glm(Cutoff ~ n_kcore_all + sex+age+edu, data = compcut, family=binomial)
summary(glm.kcore)
glm.constraint <- glm(Cutoff ~ n_constraint + sex+age+edu, data = compcut, family=binomial)
summary(glm.constraint)

glm.indegree <-glm(Cutoff ~ n_degree_in, data = compcut, family=binomial)
newdat <-data.frame(n_degree_in = seq(min(0), max(11), len=100))
newdat$Cutoff = predict(glm.indegree, newdata=newdat, type="response")
predicted = predict(glm.indegree, data=compcut, type="response")


#plotting data (outcome: "impaired"=1)
plot(Cutoff~ n_degree_in, data=compcut, col="red4")#tendency
lines(compcut$n_degree_in, predicted, col="green4", lwd=2)

plot(Cutoff~ n_degree_out, data=compcut, col="red4")
plot(Cutoff~ n_kcore_all, data=compcut, col="red4")
plot(Cutoff~ n_constraint, data=compcut, col="red4")
plot(Cutoff~ n_comm_g, data=compcut, col="red4") #opposite
plot(Cutoff~ n_feel_g, data=compcut, col="red4") #opposite

plot(Cutoff~age, data=compcut, col="red4")