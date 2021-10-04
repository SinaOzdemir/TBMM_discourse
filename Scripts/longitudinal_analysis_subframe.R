#longitudinal analysis subframes:

subframe_lex = readRDS(file = paste0(getwd(),"/Data/","subframe_lexicon.RDS"))
data = readRDS(file = paste0(getwd(),"/Data/","EU_only_data.RDS"))

data$year = as.numeric(data$year)

data$id = seq.int(1,nrow(data),1)

names(data)[names(data)=="belonging to eu"] = "belonging_to_eu"
names(data)[names(data)=="liberal democ"] = "liberal_democracy"
names(data)[names(data)=="fairnes"] = "fairness"

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

data_subframe = select(data,id,date,year,a_name,a_party,a_party_string,
                       i_country,i_country_string,i_organization,i_organization_string,
                       i_position,i_position_string,cost_benefit,modernization,geopolitics,
                       security,other,belonging_to_eu,liberal_democracy,
                       multiculturalism,sovereignty,fairness)

idvars = colnames(data_subframe)[1:12]
measurevars = colnames(data_subframe)[13:22]

data_subframe = melt(data_subframe,id.vars = idvars,measure.vars = measurevars,variable.name = "subframes")

data_subframe$mark1 = paste(data_subframe$subframes,data_subframe$value, sep = "-")


data_subframe = left_join(data_subframe,subframe_lex,by = "mark1")

data_subframe = filter(data_subframe, C_Frame != 99)

#Overall sub frame use by year
subframe_crs = crosstab(data_subframe, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

subframe_crs = as.data.frame(subframe_crs$crosstab)

subframe_crs = filter(subframe_crs, C_Frame != "Sum" & Freq != 0)

sub_frame_plt = ggplot(subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "Overall use of subframes",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ",nrow(subframe_crs)))

#overall use with positive
data_subframe_pos = filter(data_subframe, i_position ==1)

subframe_pos_crs = crosstab(data_subframe_pos, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

subframe_pos_crs = as.data.frame(subframe_pos_crs$crosstab)

subframe_pos_crs = filter(subframe_pos_crs, C_Frame != "Sum" & Freq != 0)

subframe_pos_plt = ggplot(subframe_pos_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "Use of subframes with positive statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ",nrow(subframe_pos_crs)))

#overall use with negative

data_subframe_neg = filter(data_subframe, i_position ==2)

subframe_neg_crs = crosstab(data_subframe_neg, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

subframe_neg_crs = as.data.frame(subframe_neg_crs$crosstab)

subframe_neg_crs = filter(subframe_neg_crs, C_Frame != "Sum" & Freq != 0)

subframe_neg_plt = ggplot(subframe_neg_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "Use of subframes with negative statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ",nrow(subframe_neg_crs)))


#overall use with conditional


data_subframe_cond = filter(data_subframe, i_position ==0)

subframe_cond_crs = crosstab(data_subframe_cond, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

subframe_cond_crs = as.data.frame(subframe_cond_crs$crosstab)

subframe_cond_crs = filter(subframe_cond_crs, C_Frame != "Sum" & Freq != 0)

subframe_cond_plt = ggplot(subframe_cond_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "Use of subframes with conditional statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ",nrow(subframe_cond_crs)))


#Akp's subframe use:

#positive

data_akp_pos = filter(data_subframe, a_party == 5301 & i_position == 1)

akp_pos_subframe_crs = crosstab(data_akp_pos, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

akp_pos_subframe_crs = as.data.frame(akp_pos_subframe_crs$crosstab)

akp_pos_subframe_crs = filter(akp_pos_subframe_crs, C_Frame != "Sum" & Freq != 0)

akp_pos_subframe_plt = ggplot(akp_pos_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "AKP's use of subframes with positive statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(akp_pos_subframe_crs)))


#negative


data_akp_neg = filter(data_subframe, a_party == 5301 & i_position == 2)

akp_neg_subframe_crs = crosstab(data_akp_neg, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

akp_neg_subframe_crs = as.data.frame(akp_neg_subframe_crs$crosstab)

akp_neg_subframe_crs = filter(akp_neg_subframe_crs, C_Frame != "Sum" & Freq != 0)

akp_neg_subframe_plt = ggplot(akp_neg_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "AKP's use of subframes with negative statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(akp_neg_subframe_crs)))

#conditional


data_akp_cond = filter(data_subframe, a_party == 5301 & i_position == 0)

akp_cond_subframe_crs = crosstab(data_akp_cond, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

akp_cond_subframe_crs = as.data.frame(akp_cond_subframe_crs$crosstab)

akp_cond_subframe_crs = filter(akp_cond_subframe_crs, C_Frame != "Sum" & Freq != 0)

akp_cond_subframe_plt = ggplot(akp_cond_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "AKP's use of subframes with conditional statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(akp_cond_subframe_crs)))

#CHP use of subframes

#positive

data_CHP_pos = filter(data_subframe, a_party == 5302 & i_position == 1)

CHP_pos_subframe_crs = crosstab(data_CHP_pos, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

CHP_pos_subframe_crs = as.data.frame(CHP_pos_subframe_crs$crosstab)

CHP_pos_subframe_crs = filter(CHP_pos_subframe_crs, C_Frame != "Sum" & Freq != 0)

CHP_pos_subframe_plt = ggplot(CHP_pos_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "CHP's use of subframes with positive statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(CHP_pos_subframe_crs)))


#negative


data_CHP_neg = filter(data_subframe, a_party == 5302 & i_position == 2)

CHP_neg_subframe_crs = crosstab(data_CHP_neg, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

CHP_neg_subframe_crs = as.data.frame(CHP_neg_subframe_crs$crosstab)

CHP_neg_subframe_crs = filter(CHP_neg_subframe_crs, C_Frame != "Sum" & Freq != 0)

CHP_neg_subframe_plt = ggplot(CHP_neg_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "CHP's use of subframes with negative statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(CHP_neg_subframe_crs)))

#conditional


data_CHP_cond = filter(data_subframe, a_party == 5302 & i_position == 0)

CHP_cond_subframe_crs = crosstab(data_CHP_cond, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

CHP_cond_subframe_crs = as.data.frame(CHP_cond_subframe_crs$crosstab)

CHP_cond_subframe_crs = filter(CHP_cond_subframe_crs, C_Frame != "Sum" & Freq != 0)

CHP_cond_subframe_plt = ggplot(CHP_cond_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "CHP's use of subframes with conditional statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(CHP_cond_subframe_crs)))

#MHP use of frames


#positive

data_MHP_pos = filter(data_subframe, a_party == 5303 & i_position == 1)

MHP_pos_subframe_crs = crosstab(data_MHP_pos, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

MHP_pos_subframe_crs = as.data.frame(MHP_pos_subframe_crs$crosstab)

MHP_pos_subframe_crs = filter(MHP_pos_subframe_crs, C_Frame != "Sum" & Freq != 0)

MHP_pos_subframe_plt = ggplot(MHP_pos_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "MHP's use of subframes with positive statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(MHP_pos_subframe_crs)))


#negative


data_MHP_neg = filter(data_subframe, a_party == 5303 & i_position == 2)

MHP_neg_subframe_crs = crosstab(data_MHP_neg, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

MHP_neg_subframe_crs = as.data.frame(MHP_neg_subframe_crs$crosstab)

MHP_neg_subframe_crs = filter(MHP_neg_subframe_crs, C_Frame != "Sum" & Freq != 0)

MHP_neg_subframe_plt = ggplot(MHP_neg_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "MHP's use of subframes with negative statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(MHP_neg_subframe_crs)))

#conditional


data_MHP_cond = filter(data_subframe, a_party == 5303 & i_position == 0)

MHP_cond_subframe_crs = crosstab(data_MHP_cond, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

MHP_cond_subframe_crs = as.data.frame(MHP_cond_subframe_crs$crosstab)

MHP_cond_subframe_crs = filter(MHP_cond_subframe_crs, C_Frame != "Sum" & Freq != 0)

MHP_cond_subframe_plt = ggplot(MHP_cond_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "MHP's use of subframes with conditional statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(MHP_cond_subframe_crs)))

#KRDS use of frames


#positive

data_KRDS_pos = filter(data_subframe, a_party == 5304 | a_party == 5308 & i_position == 1)

KRDS_pos_subframe_crs = crosstab(data_KRDS_pos, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

KRDS_pos_subframe_crs = as.data.frame(KRDS_pos_subframe_crs$crosstab)

KRDS_pos_subframe_crs = filter(KRDS_pos_subframe_crs, C_Frame != "Sum" & Freq != 0)

KRDS_pos_subframe_plt = ggplot(KRDS_pos_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "KRDS's use of subframes with positive statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(KRDS_pos_subframe_crs)))


#negative


data_KRDS_neg = filter(data_subframe, a_party == 5304 | a_party == 5308 & i_position == 2)

KRDS_neg_subframe_crs = crosstab(data_KRDS_neg, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

KRDS_neg_subframe_crs = as.data.frame(KRDS_neg_subframe_crs$crosstab)

KRDS_neg_subframe_crs = filter(KRDS_neg_subframe_crs, C_Frame != "Sum" & Freq != 0)

KRDS_neg_subframe_plt = ggplot(KRDS_neg_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "KRDS's use of subframes with negative statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(KRDS_neg_subframe_crs)))

#conditional


data_KRDS_cond = filter(data_subframe, a_party == 5304 | a_party == 5308 & i_position == 0)

KRDS_cond_subframe_crs = crosstab(data_KRDS_cond, row.vars = "C_Frame", col.vars = "year", dec.places = 1,percentages = T,type = "c")

KRDS_cond_subframe_crs = as.data.frame(KRDS_cond_subframe_crs$crosstab)

KRDS_cond_subframe_crs = filter(KRDS_cond_subframe_crs, C_Frame != "Sum" & Freq != 0)

KRDS_cond_subframe_plt = ggplot(KRDS_cond_subframe_crs,aes(x = year, y = Freq, group = C_Frame))+geom_bar(aes(fill= C_Frame,alpha = Freq),stat = "identity")+
  geom_text(aes(label = paste0(round(Freq,0),"%")),color = "white", position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(title = "KRDS's use of subframes with conditional statements",y = "Percentage of Subframe",fill = "Subframes",subtitle = paste0("N = ", nrow(KRDS_cond_subframe_crs)))


party_pos_plt = gridExtra::grid.arrange(akp_pos_subframe_plt,CHP_pos_subframe_plt,MHP_pos_subframe_plt,KRDS_pos_subframe_plt, nrow = 2)

party_cond_plt = gridExtra::grid.arrange(akp_cond_subframe_plt,CHP_cond_subframe_plt,MHP_cond_subframe_plt,KRDS_cond_subframe_plt, nrow = 2)

party_neg_plt = gridExtra::grid.arrange(akp_neg_subframe_plt,CHP_neg_subframe_plt,MHP_neg_subframe_plt,KRDS_neg_subframe_plt, nrow = 2)



if(isFALSE(exists(paste0(getwd(),"/Subframes by year/","subframes_year.png")))){
  
  ggsave(plot = sub_frame_plt,filename = paste0(getwd(),"/Subframes by year/","subframes_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = subframe_pos_plt,filename = paste0(getwd(),"/Subframes by year/","subframes_pos_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = subframe_neg_plt,filename = paste0(getwd(),"/Subframes by year/","subframes_neg_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = subframe_cond_plt,filename = paste0(getwd(),"/Subframes by year/","subframes_cond_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = akp_pos_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","akp_pos_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = akp_neg_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","akp_neg_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = akp_cond_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","akp_cond_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = CHP_pos_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","chp_pos_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = CHP_neg_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","chp_neg_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = CHP_cond_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","chp_cond_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = MHP_pos_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","mhp_pos_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = MHP_neg_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","mhp_neg_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = MHP_cond_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","mhp_cond_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = KRDS_pos_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","krds_pos_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = KRDS_neg_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","krds_neg_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = KRDS_cond_subframe_plt,filename = paste0(getwd(),"/Subframes by year/","krds_cond_subframe_year.png"),width = 185.208333333 ,height = 132.291666667, units = "mm")
  ggsave(plot = party_pos_plt,filename = paste0(getwd(),"/Subframes by year/","party_pos_subframe_year.png"),width = 238.125 ,height = 185.208333333, units = "mm")
  ggsave(plot = party_cond_plt,filename = paste0(getwd(),"/Subframes by year/","party_cond_subframe_year.png"),width = 238.125 ,height = 185.208333333, units = "mm")
  ggsave(plot = party_neg_plt,filename = paste0(getwd(),"/Subframes by year/","party_neg_subframe_year.png"),width = 238.125 ,height = 185.208333333, units = "mm")
  
}

rm(list = ls())