#with "compccut"
#cross-sectional (DV:w1mmse)
#cross-sectional dataframe from df "compcut"
library(dplyr)
w1 <- data.frame(filter(compcut, wave==1))
w2 <- data.frame(filter(compcut, wave==2))
w3 <- data.frame(filter(compcut, wave==3))
w4 <- data.frame(filter(compcut, wave==4))
male <-data.frame(filter(compcut, sex==1))
fem <-data.frame(filter(compcut, sex==2))

cc1 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work)
summary(cc1)

cc2 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_kcore_all)
summary(cc2) #<.05

cc3 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out + w1$n_feel)
summary(cc3) #both <.05

cc4 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_kcore_all+ w1$n_degree_out + w1$n_feel)
summary(cc4) #<.05


cc5 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_feel)
summary(cc5) #.

cc6<-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_in)
summary(cc6) #.

cc7 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_comm)
summary(cc7) #<.05

cc8 <-lm(w1$mmse_c ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_all)
summary(cc8) #<.05


#with "compcut"
##trajectory
library(lme4)

yangsa.mmse <- lmer(mmse_c ~ wave + sex + age + edu + (1 + wave| dbid), compcut, REML=FALSE)
summary(yangsa.mmse)

#yangsa.mmse.all <-lmer(mmse_c ~ wave + sex + age + edu + (1 + wave| dbid), data, REML=FALSE)
#summary(yangsa.mmse.all)

#soc
yangsa.indegree <-lmer(n_degree_in ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE)
summary(yangsa.indegree)

yangsa.outdegree <-lmer(n_degree_out ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE)
summary(yangsa.outdegree)

yangsa.degree <-lmer(n_degree_all ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE)
summary(yangsa.degree)

yangsa.kcore <-lmer(n_kcore_all ~ wave + sex + age + edu +  (1 + wave |dbid), compd, REML=FALSE)
summary(yangsa.kcore)

yangsa.constraint <-lmer(n_constraint ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE)
summary(yangsa.constraint)

yangsa.comm <-lmer(n_comm_g ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE)
yangsa.feel <-lmer(n_feel_g ~ wave + sex + age + edu + (1 + wave |dbid), compd, REML=FALSE) #singular fit

#individual slope
slope.mmse<-coef(yangsa.mmse)$dbid
colnames(slope.mmse) <-c("intercept", "slope")

slope.indegree<-coef(yangsa.indegree)$dbid
colnames(slope.indegree) <-c("intercept", "slope")

slope.outdegree<-coef(yangsa.outdegree)$dbid
colnames(slope.outdegree) <-c("intercept", "slope")

slope.degree<-coef(yangsa.degree)$dbid
colnames(slope.degree) <-c("intercept", "slope")

slope.kcore<-coef(yangsa.kcore)$dbid
colnames(slope.kcore) <-c("intercept", "slope")

slope.constraint<-coef(yangsa.constraint)$dbid
colnames(slope.constraint) <-c("intercept", "slope")

slope.comm<-coef(yangsa.comm)$dbid
colnames(slope.comm) <-c("intercept", "slope")

slope.feel<-coef(yangsa.feel)$dbid
colnames(slope.feel) <-c("intercept", "slope")

#slope-slope correlation 
cor.test(slope.mmse$slope, slope.indegree$slope)
cor.test(slope.mmse$slope, slope.outdegree$slope)
cor.test(slope.mmse$slope, slope.degree$slope) #<.05
cor.test(slope.mmse$slope, slope.kcore$slope) #<.05
cor.test(slope.mmse$slope, slope.constraint$slope)

#slope-wave1 correlation
cor.test(slope.mmse$slope, w1$n_kcore_all)
cor.test(slope.mmse$slope, w1$n_degree_all) #.06
cor.test(slope.mmse$slope, w1$n_degree_in)
cor.test(slope.mmse$slope, w1$n_degree_out) #<.05 neg
cor.test(slope.mmse$slope, w1$n_comm_g)
cor.test(slope.mmse$slope, w1$n_comm)
cor.test(slope.mmse$slope, w1$n_feel_g) #<.05 neg
cor.test(slope.mmse$slope, w1$n_feel) #<.05 neg
cor.test(slope.mmse$slope, w1$n_constraint)

#plotting correlation
plot(slope.mmse$slope, slope.kcore$slope, 
     xlab="MMSE Change", ylab="K-core Change") #0.13

plot(w1$n_degree_out, slope.mmse$slope, 
     xlab="Wave 1 Out-degree", ylab="MMSE Change")
plot(w1$n_feel, slope.mmse$slope,
     xlab="Wave1 Feeling Close", ylab="MMSE Change")
plot(slope.mmse$slope,
     ylab="MMSE Change")
summary(slope.mmse$slope)
sd(slope.mmse$slope)
#spaghettiplot
xyplot(mmse_c~ wave, groups=dbid, type="b", data=compcut,
       main="Cognitive Function Wave 1-4", 
       ylab="MMSE total", xlab="Wave")



##hierarchical regression (DV:mmseslope)
a1 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work)
summary(a1)

a2 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_feel)
summary(a2) #<.05

a3 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out + w1$n_feel)
summary(a3) #both <.05

a4 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out)
summary(a4) #<.05

a6<-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_in)
summary(a6)

a5 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_comm)
summary(a5)

a6 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_kcore_all)
summary(a6)





##hierarchical regression with intercept (DV:mmseslope)
in1 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + slope.mmse$intercept)
summary(in1)

in2 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_feel + slope.mmse$intercept)
summary(in2) #<.05

in3 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out + w1$n_feel + slope.mmse$intercept)
summary(in3) #both <.05

in4 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out+ slope.mmse$intercept)
summary(in4) #<.05

in6<-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_in+ slope.mmse$intercept)
summary(in6) #null

in5 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_comm+ slope.mmse$intercept)
summary(in5) #null

##hierarchical regression with baseline (DV:mmseslope)
mm1 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$mmse_c)
summary(mm1)

mm2 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_feel + w1$mmse_c)
summary(mm2) #<.05

mm3 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out + w1$n_feel + w1$mmse_c)
summary(mm3) #both <.05

mm4 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_out+ w1$mmse_c)
summary(mm4) #<.05

mm6<-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_degree_in+ w1$mmse_c)
summary(mm6) #null

mm5 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$n_comm + w1$mmse_c)
summary(mm5) #null


#opposite direction? (DV:indslope)
sn2 <-lm(slope.degree$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$Work +w1$myeon + w1$mmse_c)
summary(sn2) #null




#gender#
yangsa.mmse.m <- lmer(mmse_c ~ wave + sex + age + edu + (1 + wave| dbid), male, REML=FALSE)
slope.mmse.m<-coef(yangsa.mmse.m)$dbid
colnames(slope.mmse.m) <-c("intercept", "slope")
yangsa.mmse.f <- lmer(mmse_c ~ wave + sex + age + edu + (1 + wave| dbid), fem, REML=FALSE)
slope.mmse.f<-coef(yangsa.mmse.f)$dbid
colnames(slope.mmse.f) <-c("intercept", "slope")
#gender>>null
m1m <-lm(slope.mmse.m$slope ~ w1m$edu + w1m$age + w1m$sf_1 + w1m$Mstatus + w1m$work)
summary(m1m)
m2m <-lm(slope.mmse.m$slope ~ w1m$edu + w1m$age + w1m$sf_1 + w1m$Mstatus + w1m$work + w1m$n_feel)
summary(m2m)
m4m <-lm(slope.mmse.m$slope ~ w1m$edu + w1m$age + w1m$sf_1 + w1m$Mstatus + w1m$work + w1m$n_degree_out)
summary(m4m)

m2f <-lm(slope.mmse.f$slope ~ w1f$edu + w1f$age + w1f$sf_1 + w1f$Mstatus + w1f$work + w1f$n_feel)
summary(m2f)
m2f <-lm(slope.mmse.f$slope ~ w1f$edu + w1f$age + w1f$sf_1 + w1f$Mstatus + w1f$work + w1f$n_feel)
summary(m2f)
m4f <-lm(slope.mmse.f$slope ~ w1f$edu + w1f$age + w1f$sf_1 + w1f$Mstatus + w1f$work + w1f$n_degree_out)
summary(m4f)



#tryouts
m7<-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$Mstatus + w1$work + w1$myeon + w1$n_kcore_all)
summary(m7) #<.05 w/o Mstatus

m8 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$n_size)
summary(m8)

m9 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$work + w1$myeon + w1$n_kin_p)
summary(m9)

m10 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$work + w1$myeon + w1$n_degree_all)
summary(m10) #<.05

m11 <-lm(slope.mmse$slope ~ w1$sex + w1$edu + w1$age + w1$sf_1 + w1$work + w1$myeon + w1$n_degree_out + w1$n_kcore_all)
summary(m11)