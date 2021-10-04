#Trajectory analysis
packs = c("crimCV","tidyverse","reshape2","readxl")

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

lapply(packs, library, character.only = T)

eu = read_xlsx(path = "trajectory_analysis_data.xlsx",sheet = 1,trim_ws = T)


eu$date = as.Date.character(eu$date,format = "%Y%m%d")

eu$year = gsub("-[0-9][0-9]-[0-9][0-9]","",eu$date)


eu.trimmed = select(eu, date,a_party,a_name, i_organization, i_position)

eu.trimmed$date = as.Date.character(eu.trimmed$date,format = "%Y%m%d")

eu.trimmed$year = gsub("-[0-9][0-9]-[0-9][0-9]","",eu.trimmed$date)

eu.trimmed[,4:6] = lapply(eu.trimmed[,4:6],as.numeric)

eu.trimmed = filter(.data = eu.trimmed, i_organization >= 600 & i_organization <=700)

eu.count.ind = eu.trimmed%>%group_by(year,a_name,i_position)%>%tally(sort = T)

#positive only:

eu.count.ind.posonly = filter(eu.count.ind, i_position == 1)

eu.count.ind.posonly.w = eu.count.ind.posonly%>%spread(i_position, n)

eu.count.pos.only.w.ind = eu.count.ind.posonly.w %>% spread(year,`1`)

eu.count.pos.only.w.ind[is.na(eu.count.pos.only.w.ind)] = 0

eu.count.pos.only.w.ind.m = as.matrix(x = eu.count.pos.only.w.ind[,2:15])

poly_1 = vector(mode = "list",length = 10)
poly_2 = vector(mode = "list",length = 10)
poly_3 = vector(mode = "list",length = 10)

models.posonly = list(poly_1,poly_2,poly_3)


measures.posonly <- data.frame(cbind(poly = c(),groups=c(),llike=c(),AIC=c(),BIC=c(),CVE=c()))

for (j in 1:3) {

  print(paste0("polynomial order ", j))

  for (i in 1:10){
  
  print(paste0("Group number ", i))
  models.posonly[[j]][[i]] <- crimCV(eu.count.pos.only.w.ind.m,i,rcv=TRUE,dpolyp=j)
  measures.posonly <- rbind(measures.posonly,data.frame(cbind(poly_ord = j,groups=i,llike=models.posonly[[j]][i]$llike,
                                              AIC=models.posonly[[j]][i]$AIC,BIC=models.posonly[[j]][i]$BIC,CVE=models.posonly[[j]][i]$cv)))
}
}



for (j in 1:3) {
  
  print(paste0("polynomial order ", j))
  
  for (i in 1:10){
    
    print(paste0("Group number ", i))
    measures.posonly <- rbind(measures.posonly,data.frame(cbind(poly_ord = j,groups=i,llike=models.posonly[[j]][[i]]$llike,
                                                                AIC=models.posonly[[j]][[i]]$AIC,BIC=models.posonly[[j]][[i]]$BIC,CVE=models.posonly[[j]][[i]]$cv)))
  }
}

measures.posonly = na.omit(measures.posonly)
write.csv(measures.posonly,file = "pos_only_fit_stats.csv",dec = ",")


saveRDS(measures.posonly,file = "selection_measures_pos_only.RDS")
saveRDS(models,file = "models_pos_only.RDS")


long_data_posonly = long_traj(model = models.posonly[[3]][[3]],data = eu.count.pos.only.w.ind.m)

pred_data_posonly = pred_means(model = models.posonly[[3]][[3]])
pred_data_posonly$x = as.numeric(pred_data_posonly$x+2003)
pred_data_posonly$Group = as.factor(pred_data_posonly$Group)

traj_plot_posonly = ggplot(data = pred_data_posonly, aes(x = x, y = pred_mean, group = Group))+
  geom_line(aes(color = Group), size = 1.5)+ geom_vline(xintercept = c(2004,2006,2013,2017),color = "red")+
  geom_text(aes(x=2005, label="bumpy\n start", y=0.5), colour="black", size = 4) +
  geom_text(aes(x=2010, label="enthusiasm", y=0.5), colour="black", size = 4)+
  geom_text(aes(x=2015, label="accession\n fatigue", y=0.5), colour="black", size=4)+
  labs(y = "predicted average number of positive statements", x = "year")

traj_plot_posonly

mep_identy = select(long_data_posonly, GMax, Ord,x)

mep_identy$x = as.numeric(mep_identy$x+2003)

mep_names =data.frame(Ord = seq.int(1,nrow(eu.count.pos.only.w.ind),1),
                      a_name = eu.count.pos.only.w.ind$a_name)

actor_party = read_xlsx(path = "party_mp_lex.xlsx",sheet = 1,na = c("", " "),trim_ws = T)

mep_names = left_join(x = mep_names, y = actor_party,by = "a_name")

mep_identy_party = left_join(mep_identy,mep_names,by = "Ord")

mep_identy_party = na.omit(mep_identy_party)

colnames(mep_identy_party) = c("Group","ID","year","a_name","a_party")


mep_identy_party$year = as.character(mep_identy_party$year)


party_group = crosstab(mep_identy_party,row.vars = "Group",col.vars = "a_party", type = "r",dec.places = 1)

party_group = as.data.frame(party_group$crosstab)

party_group%>%spread(a_party,Freq)%>%write.csv(file="party_group.csv")

eu_parl_group = left_join(x = eu, y = mep_identy_party, by = c("a_name","a_party","year"))

eu_parl_group = filter(eu_parl_group, i_position == 1)

eu_parl_group$m_frames = ifelse(eu_parl_group$Normative == 1 & eu_parl_group$Utilitarian == 1, "both",
                        ifelse(eu_parl_group$Normative == 1 & eu_parl_group$Utilitarian == 0, "Normative",
                               ifelse(eu_parl_group$Normative == 0 & eu_parl_group$Utilitarian == 1, "Utilitarian", "Neither")))

group_frame = crosstab(eu_parl_group, row.vars = "Group",col.vars = "m_frames",type = "r")

group_frame = as.data.frame(group_frame$crosstab)

group_frame%>%spread(m_frames,Freq)%>%write.csv(file="group_frames.csv")

#----
#Conditional

eu.count.ind.cononly = filter(eu.count.ind, i_position == 0)

eu.count.ind.cononly.w = eu.count.ind.cononly%>%spread(i_position, n)

eu.count.ind.cononly.w[is.na(eu.count.ind.cononly.w)] = 0

eu.count.con.only.w.ind = eu.count.ind.cononly.w %>% spread(year,`0`)

eu.count.con.only.w.ind[is.na(eu.count.con.only.w.ind)] = 0

eu.count.con.only.w.ind.m = as.matrix(x = eu.count.con.only.w.ind[,2:11])

models.cononly = vector(mode = "list",length = 10)

measures.cononly <- data.frame(cbind(groups=c(),llike=c(),AIC=c(),BIC=c(),CVE=c()))

for (i in 1:10){
  print(paste0("iteration", i))
  models.cononly[[i]] <- crimCV(eu.count.con.only.w.ind.m,i,rcv=TRUE,dpolyp=3,dpolyl=3)
  measures.cononly <- rbind(measures.cononly,data.frame(cbind(groups=i,llike=models.cononly[[i]]$llike,
                                                              AIC=models.cononly[[i]]$AIC,BIC=models.cononly[[i]]$BIC,CVE=models.cononly[[i]]$cv)))
}

saveRDS(models.cononly,"models_cononly.RDS")
saveRDS(measures.cononly,"selection_measures_con_only.RDS")


long_data_cononly = long_traj(model = models.cononly[[4]],data = eu.count.con.only.w.ind.m)

pred_data_cononly = pred_means(model = models.cononly[[4]])
pred_data_cononly$x = as.numeric(pred_data_cononly$x+2003)
pred_data_cononly$Group = as.factor(pred_data_cononly$Group)

traj_plot_cononly = ggplot(data = pred_data_cononly, aes(x = x, y = pred_mean, group = Group))+
  geom_line(aes(color = Group), size = 1.5)+ geom_vline(xintercept = c(2004,2006,2013,2017),color = "red")+
  geom_text(aes(x=2005, label="bumpy\n start", y=0.8), colour="black", size = 4) +
  geom_text(aes(x=2010, label="enthusiasm", y=0.9), colour="black", size = 4)+
  geom_text(aes(x=2015, label="accession\n fatigue", y=0.8), colour="black", size=4)+
  labs(y = "predicted average number of conditional statements", x = "year")

traj_plot_cononly

mep_identy_con = select(long_data_cononly, GMax, Ord,x)

mep_identy_con$x = as.numeric(mep_identy$x+2003)

mep_names_con =data.frame(Ord = seq.int(1,130,1),
                      a_name = eu.count.con.only.w.ind$a_name)

mep_names_con = left_join(x = mep_names_con, y = actor_party,by = "a_name")

mep_identy_party_con = left_join(mep_identy_con,mep_names_con,by = "Ord")

mep_identy_party_con = na.omit(mep_identy_party_con)

con_traj_crosstab = crosstab(mep_identy_party,row.vars = c("GMax"),col.vars = "a_party",type = "row.pct")

con_traj_crosstab = as.data.frame(x = con_traj_crosstab$crosstab)

con_traj_crosstab = con_traj_crosstab %>% spread(a_party,Freq)

write.csv(con_traj_crosstab,file = "group_party_composition_con.csv")

#Negative only



eu.count.ind.negonly = filter(eu.count.ind, i_position == 2)

eu.count.ind.negonly.w = eu.count.ind.negonly%>%spread(i_position, n)

eu.count.ind.negonly.w[is.na(eu.count.ind.negonly.w)] = 0

eu.count.neg.only.w.ind = eu.count.ind.negonly.w %>% spread(year,`2`)

eu.count.neg.only.w.ind[is.na(eu.count.neg.only.w.ind)] = 0

eu.count.neg.only.w.ind.m = as.matrix(x = eu.count.neg.only.w.ind[,2:9])

models.negonly = vector(mode = "list",length = 10)

measures.negonly <- data.frame(cbind(groups=c(),llike=c(),AIC=c(),BIC=c(),CVE=c()))

for (i in 1:10){
  print(paste0("iteration", i))
  models.negonly[[i]] <- crimCV(eu.count.neg.only.w.ind.m,i,rcv=TRUE,dpolyp=3,dpolyl=3)
  measures.negonly <- rbind(measures.negonly,data.frame(cbind(groups=i,llike=models.negonly[[i]]$llike,
                                                              AIC=models.negonly[[i]]$AIC,BIC=models.negonly[[i]]$BIC,CVE=models.negonly[[i]]$cv)))
}

saveRDS(models.negonly,"models_negonly.RDS")
saveRDS(measures.negonly,"selection_measures_neg_only.RDS")


long_data_negonly = long_traj(model = models.negonly[[4]],data = eu.count.neg.only.w.ind.m)

pred_data_negonly = pred_means(model = models.negonly[[4]])
pred_data_negonly$x = as.numeric(pred_data_negonly$x+2003)
pred_data_negonly$Group = as.factor(pred_data_negonly$Group)

traj_plot_negonly = ggplot(data = pred_data_negonly, aes(x = x, y = pred_mean, group = Group))+
  geom_line(aes(color = Group), size = 1.5)+ geom_vline(xintercept = c(2004,2006,2013,2017),color = "red")+
  geom_text(aes(x=2005, label="bumpy\n start", y=0.8), colour="black", size = 4) +
  geom_text(aes(x=2010, label="enthusiasm", y=0.9), colour="black", size = 4)+
  geom_text(aes(x=2015, label="accession\n fatigue", y=0.8), colour="black", size=4)+
  labs(y = "predicted average number of negative statements", x = "year")

traj_plot_negonly

mep_identy_neg = select(long_data_negonly, GMax, Ord,x)

mep_identy_neg$x = as.numeric(mep_identy$x+2003)

mep_names_neg =data.frame(Ord = seq.int(1,130,1),
                          a_name = eu.count.neg.only.w.ind$a_name)

mep_names_neg = left_join(x = mep_names_neg, y = actor_party,by = "a_name")

mep_identy_party_neg = left_join(mep_identy_neg,mep_names_neg,by = "Ord")

mep_identy_party_neg = na.omit(mep_identy_party_neg)

neg_traj_crosstab = crosstab(mep_identy_party,row.vars = c("GMax"),col.vars = "a_party",type = "row.pct")

neg_traj_crosstab = as.data.frame(x = neg_traj_crosstab$crosstab)

neg_traj_crosstab = neg_traj_crosstab %>% spread(a_party,Freq)

write.csv(neg_traj_crosstab,file = "group_party_composition_neg.csv")
