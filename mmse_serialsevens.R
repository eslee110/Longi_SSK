#mmse_serialsevens
compd$mmse_ss <- compd$mmse_14 + compd$mmse_15 +compd$mmse_16 + compd$mmse_17 + compd$mmse_18
comps <- subset(compd, mmse_ss<6)
table(comps$dbid) #number of waves per id
subs <- names(which(table(comps$dbid) == 4)) #names==dbid
compss = subset(comps, subset=comps$dbid %in% subs) #data of dbid with all waves
subjects <- names(which(table(compss$dbid)==4))
length(compss)

#with "compss"
#serial sevens trajectory
library(lme4)

yangsa.ss <- lmer(mmse_ss ~ wave + sex + age + edu + (1 + wave| dbid), compss, REML=FALSE)
summary(yangsa.ss)
slope.ss<-coef(yangsa.ss)$dbid
colnames(slope.ss) <-c("intercept", "slope")
View(slope.ss$slope)

yangsa.indegree <-lmer(n_degree_in ~ wave + sex + age + edu + (1 + wave |dbid), compss, REML=FALSE)

yangsa.outdegree <-lmer(n_degree_out ~ wave + sex + age + edu + (1 + wave |dbid), compss, REML=FALSE)

yangsa.kcore <-lmer(n_kcore_all ~ wave + sex + age + edu +  (1 + wave |dbid), compss, REML=FALSE)

yangsa.constraint <-lmer(n_constraint ~ wave + sex + age + edu + (1 + wave |dbid), compss, REML=FALSE)

yangsa.comm <-lmer(n_comm ~ wave + sex + age + edu + (1 + wave |dbid), compss, REML=FALSE)

yangsa.feel <-lmer(n_feel ~ wave + sex + age + edu + (1 + wave |dbid), compss, REML=FALSE)

#individual slope
slope.indegree<-coef(yangsa.indegree)$dbid
colnames(slope.indegree) <-c("intercept", "slope")

slope.outdegree<-coef(yangsa.outdegree)$dbid
colnames(slope.outdegree) <-c("intercept", "slope")

slope.kcore<-coef(yangsa.kcore)$dbid
colnames(slope.kcore) <-c("intercept", "slope")

slope.constraint<-coef(yangsa.constraint)$dbid
colnames(slope.constraint) <-c("intercept", "slope")

slope.comm<-coef(yangsa.comm)$dbid
colnames(slope.comm) <-c("intercept", "slope")

slope.feel<-coef(yangsa.feel)$dbid
colnames(slope.feel) <-c("intercept", "slope")

#slope-slope correlation >nothing
cor.test(slope.ss$slope, slope.indegree$slope)
cor.test(slope.ss$slope, slope.outdegree$slope)
cor.test(slope.ss$slope, slope.kcore$slope)
cor.test(slope.ss$slope, slope.constraint$slope)

#cross-sectional dataframe
##from df "compss"
library(dplyr)
w1 <- data.frame(filter(compss, wave==1))
w2 <- data.frame(filter(compss, wave==2))
w3 <- data.frame(filter(compss, wave==3))
w4 <- data.frame(filter(compss, wave==4))
male <-data.frame(filter(compss, sex==1))
fem <-data.frame(filter(compss, sex==2))

View(slope.ss$slope)
View(w1)
View(compss)

#slope-wave1 correlation >nothing
cor.test(slope.ss$slope, w1$n_kcore_all)
cor.test(slope.ss$slope, w1$n_degree_all)
cor.test(slope.ss$slope, w1$n_degree_in)
cor.test(slope.ss$slope, w1$n_degree_out)
cor.test(slope.ss$slope, w1$n_comm_g)
cor.test(slope.ss$slope, w1$n_comm)
cor.test(slope.ss$slope, w1$n_feel_g)
cor.test(slope.ss$slope, w1$n_feel)
cor.test(slope.ss$slope, w1$n_constraint)

#binarize mstatus, work
compss$Mstatus <-ifelse(compss$mstatus<2, "1", "0")
compss$work <-ifelse(compss$work==2, "0", "1")
View(compss$Mstatus)

##hierarchical regression (DV:ss_slope) > nothing 
s1 <-lm(slope.ss$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work)
summary(s1)
