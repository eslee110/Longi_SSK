##depression cutoff##
#cesd cutoff
cesdd$cutoff <-ifelse(cesdd$cesd_r<22, "0", "1")
cesdd$cutoff <-as.numeric(cesdd$cutoff)
View(cesdd$cutoff)
sum(cesdd$cutoff) #101 cases ("cases" not dbid)
#not/depressed
View(cesdd$dbid)
dcesd <- subset(cesdd, cesdd$cutoff==1) #101
ncesd <- subset(cesdd, cesdd$cutoff==0) #1273
View(dcesd)
View(ncesd)
#n/d bywave
w1cd <- data.frame(filter(dcesd, wave==1)) #37
w1cn <- data.frame(filter(ncesd, wave==1)) #421
###selecting w1 n/d from "cesd"
dep1 <- w1cd$dbid
cw1d <- subset(cesdd, subset = cesdd$dbid %in% dep1)
no1 <- w1cn$dbid
cw1n <- subset(cesdd, subset = cesdd$dbid %in% no1)

#slope for w1d/n
yangsa.dcesd <- lmer(cesd_r ~ wave + sex + age + edu + (1 | dbid), cw1d, REML=FALSE) #1+wave leads to singular fit
summary(yangsa.dcesd)
slope.d<-coef(yangsa.dcesd)$dbid
colnames(slope.d) <-c("intercept", "slope")
View(slope.d) #37 slopes

yangsa.ncesd <- lmer(cesd_r ~ wave + sex + age + edu + (1+wave| dbid), cw1n, REML=FALSE)
summary(yangsa.ncesd)
slope.n<-coef(yangsa.ncesd)$dbid
colnames(slope.n) <-c("intercept", "slope")
View(slope.n) #421 slopes
View(slope.cesd)


#regession for normal at baseline (DV:cesdslope of w1 normal)
n1 <-lm(slope.n$slope ~ w1c$sex + w1c$edu + w1c$age + w1c$sf_1 + w1c$Mstatus + w1c$Work +w1c$myeon)
summary(n1)

n2 <-lm(slope.n$slope ~ w1cn$sex + w1cn$edu + w1cn$age + w1cn$sf_1 + w1cn$Mstatus + w1cn$Work +w1cn$myeon + w1cn$n_degree_all)
summary(n2) #null

n3 <-lm(slope.n$slope ~ w1cn$sex + w1cn$edu + w1cn$age + w1cn$sf_1 + w1cn$Mstatus + w1cn$Work +w1cn$myeon + w1cn$n_kcore_all*w1cn$n_feel_g)
summary(n3) #null