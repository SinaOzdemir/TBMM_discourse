##Preprocessing


data = readxl::read_xlsx(path = paste0(getwd(),"/Data/","TR 2004-2017 with new frames.xlsx"),
                        sheet = 1,col_names = T,na = c(""," "),trim_ws = T)

v.lex = readxl::read_xlsx(path = paste0(getwd(),"/Data/","TR 2004-2017 with new frames.xlsx"),
                          sheet = 2,col_names = T,na = c(""," "),trim_ws = T)

data = data[,-c(13:23)]

#Text variables----
#a_party recode
v.lex.a = select(v.lex, a_party, a_party_string)

data1 = left_join(data,v.lex.a, by= "a_party")

data1$a_party_string = factor(x = data1$a_party_string,ordered = F)

#i_organization
v.lex.org = select(v.lex, i_organization, i_organization_string)

data1 = left_join(data1, v.lex.org, by = "i_organization")

data1$i_organization_string = factor(x = data1$i_organization_string,ordered = F)

#i_country

v.lex.cntry = select(v.lex, i_country, i_country_string)

data1 = left_join(data1, v.lex.cntry, by = "i_country")

data1$i_country_string = factor(data1$i_country_string, ordered = F)

#i_position

v.lex.pos = select(v.lex, i_position, i_position_string)

data1 = left_join(data1, v.lex.pos, by = "i_position")

data1$i_position_string = factor(data1$i_position_string,ordered = F)

#date

data1$date = as.Date.character(data1$date,format = "%Y%m%d")

#Year

data1$year = gsub("-[0-9][0-9]","",data1$date)

#create institutional position and electoral position variables----

data1$inst_pos = ifelse(data1$a_party == 5301, "GOV", ifelse(data1$a_party == 9900, "External", "OPP"))

data1$elec_pos = ifelse(data1$a_party_string == 5301 | data1$a_party == 5302 | data1$a_party == 5303, "Mainstream",ifelse(data1$a_party == 9900, "External","Marginal"))

data1$a_party_string = car::recode(data1$a_party_string,recodes = "'Demokratik Toplum Partisi/Bağımsız Demokrasi Partisi/Halkların Demokratik Partisi' = 'Kurdish Party Family'")

data1$lr_ideo = ifelse(data1$a_party == 5303, "Extreme-Right/Nationalist",
                       ifelse(data1$a_party == 5301, "Center-Right",
                              ifelse(data1$a_party == 5302, "Center-Left",
                                     ifelse(data1$a_party == 5306, "Center-Right",
                                            ifelse(data1$a_party == 5304 | data1$a_party ==5308, "Extreme-Left","No Info")))))


data1$m_frames = ifelse(data1$Normative == 1 & data1$Utilitarian == 1, "both",
                        ifelse(data1$Normative == 1 & data1$Utilitarian == 0, "Normative",
                               ifelse(data1$Normative == 0 & data1$Utilitarian == 1, "Utilitarian", "Neither")))

saveRDS(data1, file = paste0(getwd(),"/","Data/","replication_data_w_frames.RDS"))
write.csv(data1, file = paste0(getwd(),"/","Data/","replication_data_w_frames.csv"),fileEncoding = "UTF-8")

rm(list = ls())