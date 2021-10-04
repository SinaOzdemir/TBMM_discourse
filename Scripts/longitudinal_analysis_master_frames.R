#longitudinal analysis - master frames
library(tidyverse)
library(gridExtra)

data = readRDS(file = paste0(getwd(),"/Data/","EU_only_data.RDS"))

data$year = as.numeric(data$year)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

colors = c("Normative" = "darkgreen", "Utilitarian" = "violet")
#Frames over time

mframes_time = crosstab(data, col.vars = "year", row.vars = "m_frames", dec.places = 1, percentages = T, type = "c")

mframes_time = as.data.frame(mframes_time$crosstab)

mframes_time = filter(mframes_time, m_frames != "Sum")

mframes_time = filter(mframes_time, m_frames %in% c("Normative","Utilitarian"))


mframes_time_plt = ggplot(data = mframes_time, aes(x = year, y = Freq, group = m_frames))+geom_smooth(aes(linetype = m_frames),size = 2, se = F, method = "loess")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+ labs(y = "Percentage of master frame",linetype = "Master Frames")

#Frames by party

data_akp = filter(data, a_party == 5301)

akp_mframe_year = crosstab(data_akp, col.vars = "year", row.vars = "m_frames", dec.places = 1, percentages = T, type = "c")

akp_mframe_year = as.data.frame(akp_mframe_year$crosstab)

akp_mframe_year = filter(akp_mframe_year, m_frames != "Sum")

akp_mframe_year = filter(akp_mframe_year, m_frames %in% c("Normative","Utilitarian"))

akp_mframe_year_plt = ggplot(data = akp_mframe_year, aes(x = year, y = Freq, group = m_frames))+geom_smooth(aes(linetype = m_frames),size = 2, se = F, method = "loess")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+ labs(title = "AKP's use of master frames over years",y = "Percentage of master frame", color = "Master Frames")

#Frames by chp

data_CHP = filter(data, a_party == 5302)

CHP_mframe_year = crosstab(data_CHP, col.vars = "year", row.vars = "m_frames", dec.places = 1, percentages = T, type = "c")

CHP_mframe_year = as.data.frame(CHP_mframe_year$crosstab)

CHP_mframe_year = filter(CHP_mframe_year, m_frames != "Sum")

CHP_mframe_year = filter(CHP_mframe_year, m_frames %in% c("Normative","Utilitarian"))

CHP_mframe_year_plt = ggplot(data = CHP_mframe_year, aes(x = year, y = Freq, group = m_frames))+geom_smooth(aes(linetype = m_frames),size = 2, se = F, method = "loess")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+ labs(title = "CHP's use of master frames over years",y = "Percentage of master frame", color = "Master Frames")

#Frames by mhp


data_MHP = filter(data, a_party == 5303)

MHP_mframe_year = crosstab(data_MHP, col.vars = "year", row.vars = "m_frames", dec.places = 1, percentages = T, type = "c")

MHP_mframe_year = as.data.frame(MHP_mframe_year$crosstab)

MHP_mframe_year = filter(MHP_mframe_year, m_frames != "Sum")

MHP_mframe_year = filter(MHP_mframe_year, m_frames %in% c("Normative","Utilitarian"))

MHP_mframe_year_plt = ggplot(data = MHP_mframe_year, aes(x = year, y = Freq, group = m_frames))+geom_smooth(aes(linetype = m_frames),size = 2, se = F, method = "loess")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+ labs(title = "MHP's use of master frames over years",y = "Percentage of master frame", color = "Master Frames")

#Frames by krd


data_KRD = filter(data, a_party == 5304 | a_party == 5308)

KRD_mframe_year = crosstab(data_KRD, col.vars = "year", row.vars = "m_frames", dec.places = 1, percentages = T, type = "c")

KRD_mframe_year = as.data.frame(KRD_mframe_year$crosstab)

KRD_mframe_year = filter(KRD_mframe_year, m_frames != "Sum")

KRD_mframe_year = filter(KRD_mframe_year, m_frames %in% c("Normative","Utilitarian"))

KRD_mframe_year_plt = ggplot(data = KRD_mframe_year, aes(x = year, y = Freq, group = m_frames))+geom_smooth(aes(linetype = m_frames),size = 2, se = F, method = "loess")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_colour_manual(values = colors)+ labs(title = "Kurdish parties's use of master frames over years",y = "Percentage of master frame", color = "Master Frames")

party_frame_year = grid.arrange(akp_mframe_year_plt,CHP_mframe_year_plt,MHP_mframe_year_plt,KRD_mframe_year_plt, nrow = 2)

if(isFALSE(exists(paste0(getwd(),"/Frames by year","/overall_frames_year.png")))){
  ggsave(plot = mframes_time_plt,filename = paste0(getwd(),"/Frames by year","/overall_frames_year.png"),width = 244.475 , height = 160.3375,units = "mm")
  ggsave(plot = party_frame_year,filename = paste0(getwd(),"/Frames by year","/party_frames_year.png"),width = 384.439583333, height = 180.710416667,units = "mm")
  ggsave(plot = akp_mframe_year_plt,filename = paste0(getwd(),"/Frames by year","/akp_frames_year.png"),width = 244.475 , height = 160.3375,units = "mm")
  ggsave(plot = CHP_mframe_year_plt,filename = paste0(getwd(),"/Frames by year","/chp_frames_year.png"),width = 244.475 , height = 160.3375,units = "mm")
  ggsave(plot = MHP_mframe_year_plt,filename = paste0(getwd(),"/Frames by year","/mhp_frames_year.png"),width = 244.475 , height = 160.3375,units = "mm")
  ggsave(plot = KRD_mframe_year_plt,filename = paste0(getwd(),"/Frames by year","/krds_frames_year.png"),width = 244.475 , height = 160.3375,units = "mm")
  
}

rm(list = ls())