data<-kshap_merge_international_20180810_13ver

#exclusion_mmse
all_data = subset(data, mmse_c >= 10) #severe dementia & NA excluded

table(all_data$dbid) #number of waves per id
subs <- names(which(table(all_data$dbid) == 4)) #names==dbid
compd = subset(all_data, subset=all_data$dbid %in% subs) #data of dbid with all waves
subjects <- names(which(table(compd$dbid)==4))
length(subjects) #N

#agecat and educat
compd$agecat <- ifelse(compd$age<70, "1",  
                    ifelse(compd$age>69 & compd$age<75, "2", 
                      ifelse(compd$age>74 & compd$age<80, "3", 
                       ifelse(compd$age>79, "4", NA))))
compd$educat <- ifelse(compd$edu==1, "1",  
                       ifelse(compd$edu<4, "2", 
                              ifelse(compd$edu<6, "3", 
                                     ifelse(compd$edu>5, "4", NA)))) 

#cutoff
norm<-mmsecutoff

#merge with normative data
compcut <-merge(x = compd, y = norm, by = c("sex", "agecat", "educat"), all.x = TRUE, all.y = TRUE)
compcut$cutoff <- ifelse (compcut$cut < compcut$mmse_c, "Normal", "Impaired")
table(compcut$cutoff=="Impaired")
compcut <- compcut[-c(1801, 1802), ]

#binarize mstatus, work
compcut$Mstatus <-ifelse(compcut$mstatus<2, "1", "0")
compcut$Work <-ifelse(compcut$work==2, "0", "1")
#young-old/old-old
compcut$yo <-ifelse(compcut$age<75, "1", "0")

##ruleout
na <- subset(data, mmse_c=NA)
imp <- subset(data, mmse_c <10)