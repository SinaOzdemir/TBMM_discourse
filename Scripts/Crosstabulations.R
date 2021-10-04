#crosstabulation:

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

pdata = readRDS(file = paste0(getwd(),"/Data/","replication_data_w_frames.RDS"))

# percentage of statements by organization

nstate_org = as.data.frame(table(pdata$i_organization_string))

nstate_org$Freq = round(((nstate_org$Freq/nrow(pdata))*100),2)

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","n_state_i_org.RDS")))){
  
  saveRDS(nstate_org,file = paste0(getwd(),"/Crosstabulation/","n_state_i_org.RDS"))
  write.csv(nstate_org,file = paste0(getwd(),"/Crosstabulation/","n_state_i_org.csv"),fileEncoding = "UTF-8")
  
}

#percentage of statements by position

nstate_pos = as.data.frame(table(pdata$i_position_string))

nstate_pos$Freq = round(((nstate_pos$Freq/nrow(pdata))*100),2)

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","n_state_i_pos.RDS")))){
  
  saveRDS(nstate_pos,file = paste0(getwd(),"/Crosstabulation/","n_state_i_pos.RDS"))
  write.csv(nstate_pos,file = paste0(getwd(),"/Crosstabulation/","n_state_i_pos.csv"),fileEncoding = "UTF-8")
  
}

# percentage of statements by party

nstate_party = as.data.frame(table(pdata$a_party_string))

nstate_party$Freq = round(((nstate_party$Freq/nrow(pdata))*100),2)

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","n_state_a_party.RDS")))){
  
  saveRDS(nstate_party,file = paste0(getwd(),"/Crosstabulation/","n_state_a_party.RDS"))
  write.csv(nstate_party,file = paste0(getwd(),"/Crosstabulation/","n_state_a_party.csv"),fileEncoding = "UTF-8")
  
}


# percentage of party position on EU


pdataeu = dplyr::filter(pdata, i_organization == 613| i_organization == 610|i_organization == 618 | i_organization == 620 | i_organization == 614 & i_country == 53)

party_pos_eu = crosstab(pdataeu, col.vars = "i_position_string",row.vars = "a_party_string",dec.places = 1, percentages = T, type = "r")

party_pos_eu = as.data.frame(party_pos_eu$crosstab)

party_pos_eu = spread(party_pos_eu, key = "i_position_string", value = "Freq")


if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","party_pos_eu.RDS")))){
  
  saveRDS(party_pos_eu,file = paste0(getwd(),"/Crosstabulation/","party_pos_eu.RDS"))
  write.csv(party_pos_eu,file = paste0(getwd(),"/Crosstabulation/","party_pos_eu.csv"),fileEncoding = "UTF-8")
  
}


##percentage of statementes on organizations by year----


i_org_claims = crosstab(pdata,row.vars = "i_organization_string", col.vars = "year",percentages = T,dec.places = 1,type = "c") 

i_org_year = as.data.frame(i_org_claims$crosstab)

i_org_year = spread(i_org_year,key = "year",value = "Freq")

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","i_org_year.RDS")))){

  saveRDS(i_org_year,file = paste0(getwd(),"/Crosstabulation/","i_org_year.RDS"))
  write.csv(i_org_year,file = paste0(getwd(),"/Crosstabulation/","i_org_year.csv"),fileEncoding = "UTF-8")
    
}
  
# percentage of position by year
i_pos_claims = crosstab(pdata,row.vars = "i_position_string", col.vars = "year",percentages = T,dec.places = 1,type = "c") 

i_pos_year = as.data.frame(i_pos_claims$crosstab)

i_pos_year = spread(i_pos_year,key = "year",value = "Freq")

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","i_pos_year.RDS")))){
  
  saveRDS(i_pos_year,file = paste0(getwd(),"/Crosstabulation/","i_pos_year.RDS"))
  write.csv(i_pos_year,file = paste0(getwd(),"/Crosstabulation/","i_pos_year.csv"),fileEncoding = "UTF-8")
  
}

# percentage of party statements by year

a_party_claims = crosstab(pdata,row.vars = "a_party_string", col.vars = "year",percentages = T,dec.places = 1,type = "c") 

a_party_year = as.data.frame(a_party_claims$crosstab)

a_party_year = spread(a_party_year,key = "year",value = "Freq")

if(isFALSE(exists(paste0(getwd(),"/Crosstabulation/","a_party_year.RDS")))){
  
  saveRDS(a_party_year,file = paste0(getwd(),"/Crosstabulation/","a_party_year.RDS"))
  write.csv(a_party_year,file = paste0(getwd(),"/Crosstabulation/","a_party_year.csv"),fileEncoding = "UTF-8")
  
}



rm(list = ls())
