#Logitudinal analysis - issue position:

data = readRDS(file = paste0(getwd(),"/Data/","replication_data_w_frames.RDS"))

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

data = dplyr::filter(data, i_organization == 613| i_organization == 610|i_organization == 618 | i_organization == 620 | i_organization == 614 & i_country == 53)

colors = c("Against (membership)" = "red","Conditional" = "yellow", "For (membership)" = "darkgreen")

#Overall position by year

pos_eu = crosstab(data, col.vars = "year",row.vars = "i_position_string",dec.places = 1,percentages = T,type = "c")

pos_eu = as.data.frame(pos_eu$crosstab)

pos_eu = filter(pos_eu, i_position_string != "Sum")

pos_eu_plt = ggplot(data = pos_eu, aes(x= year, y = Freq, group = i_position_string))+ geom_smooth(aes(linetype = i_position_string), se = F, size = 2, method = "loess")+
  theme(axis.text.x = element_text(angle =45, hjust = 1))+ scale_colour_manual(values = colors)+ labs(title = "",y = "Percentage of positions")+ labs(colour = "Position")


if (isFALSE(exists()))

#AKP position by year

data_akp = filter(data, a_party == 5301)

akp_pos_eu = crosstab(data_akp, col.vars = "year",row.vars = "i_position_string",dec.places = 1, percentages = T, type = "r")

akp_pos_eu = as.data.frame(akp_pos_eu$crosstab)
akp_pos_eu = filter(akp_pos_eu, i_position_string != "Sum")

akp_pos_eu_plt = ggplot(data = akp_pos_eu,aes(x = year,y = Freq, group = i_position_string))+ geom_smooth(aes(linetype = i_position_string),se = F,method = "loess",size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+
  labs(title = "AKP's position on the EU membership over time ",
       y = "Percentage of positions") + labs(colour = "Position")

#CHP position by year


data_chp = filter(data, a_party == 5302)

chp_pos_eu = crosstab(data_chp, col.vars = "year",row.vars = "i_position_string",dec.places = 1, percentages = T, type = "c")

chp_pos_eu = as.data.frame(chp_pos_eu$crosstab)
chp_pos_eu = filter(chp_pos_eu, i_position_string != "Sum")

chp_pos_eu_plt = ggplot(data = chp_pos_eu,aes(x = year,y = Freq, group = i_position_string))+ geom_smooth(aes(linetype = i_position_string),se = F,method = "loess",size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+
  labs(title = "CHP's position on the EU membership over time",
       y = "Percentage of positions") +labs(colour = "Position")

#MHP position by year


data_mhp = filter(data, a_party == 5303)

mhp_pos_eu = crosstab(data_mhp, col.vars = "year",row.vars = "i_position_string",dec.places = 1, percentages = T, type = "c")

mhp_pos_eu = as.data.frame(mhp_pos_eu$crosstab)
mhp_pos_eu = filter(mhp_pos_eu, i_position_string != "Sum")

mhp_pos_eu_plt = ggplot(data = mhp_pos_eu,aes(x = year,y = Freq, group = i_position_string))+ geom_smooth(aes(linetype = i_position_string),se = F,method = "loess",size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+
  labs(title = "MHP's position on the EU membership over time",
       y = "Percentage of positions") +labs(colour = "Position")

#Kurdish Party

data_krd = filter(data, a_party == 5304 | a_party == 5308)

krd_pos_eu = crosstab(data_krd, col.vars = "year",row.vars = "i_position_string",dec.places = 1, percentages = T, type = "c")

krd_pos_eu = as.data.frame(krd_pos_eu$crosstab)
krd_pos_eu = filter(krd_pos_eu, i_position_string != "Sum")

krd_pos_eu_plt = ggplot(data = krd_pos_eu,aes(x = year,y = Freq, group = i_position_string))+ geom_smooth(aes(linetype = i_position_string),se = F,method = "loess",size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+
  labs(title = "Kurdish Parties' position on the EU membership over time",
       y = "Percentage of positions") +labs(colour = "Position")

position_plot = grid.arrange(akp_pos_eu_plt,chp_pos_eu_plt,mhp_pos_eu_plt,krd_pos_eu_plt, nrow = 2)

if(isFALSE(exists(paste0(getwd(),"/Position by year/","party_pos_eu.png")))){
  ggsave(filename = paste0(getwd(),"/Position by year/","party_pos_eu.png"),plot = position_plot,width = 384.439583333, height = 180.710416667 ,units = "mm")
  ggsave(filename = paste0(getwd(),"/Position by year/","akp_pos_eu.png"),plot = akp_pos_eu_plt,width = 244.475 , height = 160.3375  ,units = "mm")
  ggsave(filename = paste0(getwd(),"/Position by year/","chp_pos_eu.png"),plot = chp_pos_eu_plt,width = 244.475 , height = 160.3375  ,units = "mm")
  ggsave(filename = paste0(getwd(),"/Position by year/","mhp_pos_eu.png"),plot = mhp_pos_eu_plt,width = 244.475 , height = 160.3375  ,units = "mm")
  ggsave(filename = paste0(getwd(),"/Position by year/","krd_pos_eu.png"),plot = krd_pos_eu_plt,width = 244.475 , height = 160.3375  ,units = "mm")
  ggsave(filename = paste0(getwd(),"/Position by year/","pos_eu.png"),plot = pos_eu_plt,width = 244.475 , height = 160.3375  ,units = "mm")
  
}


saveRDS(data,file = paste0(getwd(),"/Data/","EU_only_data.RDS"))

remove(list = ls())