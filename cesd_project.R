data<-kshap_merge_international_20180810_13ver

#cesd_total
data$cesd_m <- data$cesd_1+data$cesd_2+data$cesd_3+data$cesd_4+(5-(data$cesd_5))+data$cesd_6+data$cesd_7+data$cesd_8+data$cesd_9+(5-(data$cesd_10))+
  data$cesd_11+data$cesd_12+data$cesd_13+data$cesd_14+(5-(data$cesd_15))+data$cesd_16+data$cesd_17+data$cesd_18+data$cesd_19+data$cesd_20
data$cesd_r <- (data$cesd_m - 20) #real_cesd
cesd.all <- subset(data, cesd_m>=20) #missing
View(cesd.all)

#####missing/fallout????
cesd.miss <-subset(data, cesd_m<21)
w1all <- data.frame(filter(cesd.miss, wave==1))
View(w1all$dbid)
w3all <- data.frame(filter(cesd.miss, wave==3))
View(w3all$dbid)

#allparticipants_bywave
w1all <- data.frame(filter(cesd.all, wave==1))
w3all <- data.frame(filter(cesd.all, wave==3))
w4all <- data.frame(filter(cesd.all, wave==4))
View(w1all$dbid)

#allwaves: 
###final df = "cesdd"
table(cesd.all$dbid) #number of waves per id
subs <- names(which(table(cesd.all$dbid) == 3)) #names==dbid
cesdd = subset(cesd.all, subset=cesd.all$dbid %in% subs) #data of dbid with all waves
subjects <- names(which(table(cesdd$dbid)==3))
length(subjects)

#binarize mstatus, work
cesdd$Mstatus <-ifelse(cesdd$mstatus<2, "1", "0")
cesdd$Work <-ifelse(cesdd$work==2, "0", "1")
#young-old/old-old
cesdd$yo <-ifelse(cesdd$age<75, "1", "0")

###complete_bywave
w1c <- data.frame(filter(cesdd, wave==1))
w3c <- data.frame(filter(cesdd, wave==3))
w4c <- data.frame(filter(cesdd, wave==4))



#desc
mean(w1c$age); sd(w1c$age)
mean(w1c$myeon); sd(w1c$myeon)
mean(w1c$sf_1); sd(w1c$sf_1)
mean(w1c$n_degree_all); sd(w1c$n_degree_all)
mean(w1c$n_kcore_all); sd(w1c$n_kcore_all)
mean(w1c$cesd_c); sd(w1c$cesd_c)
sum(w1c$sex==1)
sum(w1c$sex==2)
sum(w1c$Mstatus==1); sum(w1c$Mstatus==0)
sum(w1c$Work==1); sum(w1c$Work==0)
table(w1c$edu==1)
table(w1c$edu==2)
table(w1c$edu==3)
table(w1c$edu==4)
table(w1c$edu==5)
table(w1c$edu>5)
hist(w1c$cesd_r, 
     xlab="CES-D Score", main="Wave 1 CES-D Histogram")

summary(w1c$n_kcore_all)



#correlation table
library(devtools)
library(psycho)
library(tidyverse)
devtools::install_github("neuropsychology/psycho.R")

wwa <-subset(cesdd, select=c(7, 10, 13, 1077))
cor4 <- correlation(wwa, type="partial")
summary(cor4)
plot(cor4)

w1a <-subset(w1com, select=c(7, 10, 13, 1077))
View(w1a)
cor1 <- correlation(w1a, type="partial")
summary(cor1)
plot(cor1)

w3a <-subset(w3com, select=c(7, 10, 13, 1077))
cor2 <- correlation(w3a, type="partial")
summary(cor2)
plot(cor2)

w4a <-subset(w4com, select=c(7, 10, 13, 1077))
cor3 <- correlation(w4a, type="partial")
summary(cor3)
plot(cor3)

#correlation table all participants
cesdata <-subset(cesdd, select=c("sex", "age", "edu", "Mstatus", "Work", "myeon", "sf_1", 
                                 "n_degree_all", "n_kcore_all", "cesd_r"))
cor5 <- correlation(cesdata, type="partial")
summary(cor5)
plot(cor5)

w4c$Work <-as.numeric(w4c$Work)
sum(w4c$Work)
w1c$Work <-as.numeric(w1c$Work)
sum(w1c$Work)
w3$Work <-as.numeric(w3c$Work)
sum(w3c$Work)



  
#df "cesdd"
#trajectory&slope
yangsa.cesdall <- lmer(cesd_r ~ wave + sex + age + edu + (1 + wave| dbid), cesd.all, REML=FALSE)
summary(yangsa.cesdall)

yangsa.cesd <- lmer(cesd_r ~ wave + sex + age + edu + (1 + wave| dbid), cesdd, REML=FALSE)
summary(yangsa.cesd)
slope.cesd<-coef(yangsa.cesd)$dbid
colnames(slope.cesd) <-c("intercept", "slope")

plot(w1c$dbid, slope.cesd$slope, 
     ylab="CES-D Change")
xyplot(cesd_c ~ wave, groups=dbid, type="b", data=cesdd,
       ylab="CES-D total", xlab="Wave")
summary(slope.cesd$slope)
sd(slope.cesd$slope)

#soc
yangsa.ind <-lmer(n_degree_in ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE)
slope.ind<-coef(yangsa.ind)$dbid
colnames(slope.ind) <-c("intercept", "slope")

yangsa.ind <-lmer(n_degree_out ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE)
yangsa.outd <-lmer(n_degree_out ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE)
yangsa.kcor <- lmer(n_kcore_all ~ wave + sex + age + edu +  (1 + wave |dbid), cesdd, REML=FALSE)
yangsa.const <-lmer(n_constraint ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE) #convergence issues

yangsa.kcorf <- lmer(n_kcore_all*n_feel_g ~ wave + sex + age + edu +  (1 + wave |dbid), cesdd, REML=FALSE)
summary(yangsa.kcorf)
slope.kcorf<-coef(yangsa.kcorf)$dbid
colnames(slope.kcorf) <-c("intercept", "slope")

yangsa.com <-lmer(n_comm ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE)
slope.com<-coef(yangsa.com)$dbid
colnames(slope.com) <-c("intercept", "slope")

yangsa.fl <-lmer(n_feel ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE) #singular fit
summary(yangsa.fl)


#slope-slope correlation
cor.test(slope.cesd$slope, slope.ind$slope)
cor.test(slope.cesd$slope, slope.com$slope) 
cor.test(slope.cesd$slope, slope.kcorf$slope) #kcorf has 457

#slope-baseline correlation
cor.test(slope.cesd$slope, w1c$n_kcore_all) #<.05
cor.test(slope.cesd$slope, w1c$n_degree_out)
cor.test(slope.cesd$slope, w1c$n_degree_in)
cor.test(slope.cesd$slope, w1c$n_degree_all)
cor.test(slope.cesd$slope, w1c$n_comm)
cor.test(slope.cesd$slope, w1c$n_feel)

#plot corr
plot(w1c$n_kcore_all, slope.cesd$slope,
     xlab="Baseline K-core", ylab="CES-D Change")




###cross-sectional regression (DV: W1 CESD)
c1 <-lm(w1c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(c1)

c2 <-lm(w1c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all)
summary(c2)

c3 <-lm(w1c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all)
summary(c3)

c5 <-lm(w1c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_in)
summary(c5)

c4 <- lm(w1c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all +w1c$n_kcore_all)
summary(c4) #no result for degree_all, but kcore survives with indegree


#checking number of participants
View(slope.cesd$slope)
View(w1c$cesd_r)

## longi regression (DV:cesdslope)
m1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(m1)

m2 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all)
summary(m2) #null

m3 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all)
summary(m3) #<.05

m4 <- lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all +w1c$n_kcore_all)
summary(m4)

#m5 <- lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_broker*w1c$n_feel_g)
#summary(m5)






##regression (DV:cesdintercept)
int1 <-lm(slope.cesd$intercept ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(int1)

int2 <-lm(slope.cesd$intercept ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all)
summary(int2) #<.05

int3 <-lm(slope.cesd$intercept ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all)
summary(int3) #<.05

int4 <- lm(slope.cesd$intercept ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all +w1c$n_kcore_all)
summary(int4)

##classic longi (DV:w3cesd/w4cesd)
l1 <-lm(w3c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(l1)

l2 <-lm(w4c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all)
summary(l2) #null

l3 <-lm(w4c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all)
summary(l3) #null

l4 <-lm(w4c$cesd_r ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_in)
summary(l4) #.

#opposite direction? (DV:indslope)
s1 <-lm(slope.ind$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(s1)

s2 <-lm(slope.ind$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$cesd_r)
summary(s2) #<.05???

#feelcomm
t1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all + w1c$n_comm +w1c$n_feel)
summary(t1)

f1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all*w1c$n_cohab_p)
summary(f1)

f2 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon 
        + w1c$n_kcore_all*w1c$n_comm_g + w1c$n_kcore_all*w1c$n_feel_g)
summary(f2)

#regression with intercept (DV:cesdslope)
i1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + slope.cesd$intercept)
summary(i1)

i2 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + slope.cesd$intercept + w1c$n_degree_all)
summary(i2) #null

i3 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all+ slope.cesd$intercept)
summary(i3) #<.05

#regression with baseline (DV:cesdslope)
b1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$cesd_r)
summary(b1)

b2 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$cesd_r + w1c$n_degree_all)
summary(b2) #null

b3 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$cesd_r + w1c$n_kcore_all*w1c$n_feel)
summary(b3)

##regression without sf_1 (DV:cesdslope)
r1 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(r1)

r2 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all)
summary(r2)

r3 <-lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_kcore_all)
summary(r3)

r4 <- lm(slope.cesd$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$Mstatus + w1c$Work +w1c$myeon + w1c$n_degree_all +w1c$n_kcore_all)
summary(r4)

cor.test(w1c$n_kcore_all, w1c$n_degree_all)