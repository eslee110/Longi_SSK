#baseline-trajectory mediation
## from "cesd.all" to "cesdd"
table(cesd.all$dbid) #number of waves per id
subs <- names(which(table(cesd.all$dbid) == 3)) #names==dbid
cesdd = subset(cesd.all, subset=cesd.all$dbid %in% subs) #data of dbid with all waves
subjects <- names(which(table(cesdd$dbid)==3))
length(subjects)

#slope
library(lme4)
yangsa.cesd <- lmer(cesd_r ~ wave + sex + age + edu + (1 + wave| dbid), cesdd, REML=FALSE)
summary(yangsa.cesd)
slope.cesd<-coef(yangsa.cesd)$dbid
colnames(slope.cesd) <-c("intercept", "slope")
#soc slope
yangsa.nsize <-lmer(n_size ~ wave + sex + age + edu + (1 + wave |dbid), cesdd, REML=FALSE)

###complete_bywave
w1c <- subset(cesdd, wave==1)
w3c <- subset(cesdd, wave==3)
w4c <- subset(cesdd, wave==4)


#IV:baseline, mediator: w4, DV: trajectory
nsd <- model4(iv = "w1c$Mstatus", dv = "yangsa.cesd$slope", med = "w4c$n_size")

kable(nsd)
degd <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_degree_all", data = cesd.all)
kable(degd)
ind <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_degree_in", data = cesd.all)
kable(ind)
outd <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_degree_out", data = cesd.all)
kable(outd)
kcrd <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_kcore_all", data = cesd.all)
kable(kcrd)
comd <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_comm", data = cesd.all)
kable(comd)
feeld <- model4(iv = "Mstatus", dv = "cesd_r", med = "n_feel", data = cesd.all)
kable(feeld)
nsfd <- model4(iv = "Mstatus", dv = "cesd_r", med = "nsf", data = cesd.all)
kable(nsfd)