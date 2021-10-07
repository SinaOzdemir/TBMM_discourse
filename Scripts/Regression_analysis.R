#################################
# Title: Regression analysis    #
# Author: Sina Özdemir          #
#         PhD Candidate         #
#         NTNU, Norway          #
#         sina.ozdemir@ntnu.no  #
# Date: 07/10/2021              #
#################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","psych","here")

p_load(char = packs, install = T)

data_path<- here("Data")

graph_path<- here("Graphs")

results_path <- here("Analysis","Regression")

data<- readRDS(file = file.path(data_path,"replication_data_w_frames.RDS")) %>%
  filter(i_organization_string %in% "European Union") %>% filter(i_country_string %in%"Turkey")

data$a_party_string<- car::recode(var = data$a_party_string,recodes = "'Anavatan Partisi' = 'AP';'Bağımsızlar' = 'Independents';'Cumhuriyet Halk Partisi' = 'CHP';'Kurdish Party Family' = 'Kurdish Parties';'Milliyetçi Hareket Partisi' = 'MHP';'Not specified' = 'External';'Platforma Obywatelska (PO)' = 'External'",as.factor = F,as.numeric = F)
data$a_party_string<- ifelse(data$a_party == 5301, "AKP",data$a_party_string)

  
data$i_position_pos<- ifelse(data$i_position == 1, 1,0)
data$i_position_cond<- ifelse(data$i_position == 0, 1,0)
data$i_position_neg <- ifelse(data$i_position == 2, 1,0)

parties <- unique(as.character(data$a_party_string))
years<- unique(data$year)

phi_coef<- function(var1,var2,data){
  cont_table<- table(data[[var1]],data[[var2]])
  cont_matrix<- matrix(cont_table,nrow = 2, ncol = 2)
  phi <- psych::phi(t = cont_matrix,digits = 2)
  return(phi)
}

#function test

phi_val<- phi_coef(var1 = "i_position_pos",var2 = "Utilitarian",data = data)


# phi coefficient ---------------------------------------------------------
frames<- colnames(data)[15:24]
positions<- colnames(data)[34:36]

party_data<- group_split(.tbl = group_by(data, a_party_string,year),.keep = T)

#positive x frames:
phi_pos<- list()
for (i in 1:length(party_data)) {
  if(nrow(party_data[[i]])==0){
    next
  }else{
  phi_val<-map2(.x = rep(positions[1],times = length(frames)),.y = frames,.f = phi_coef,data = party_data[[i]]) %>% unlist()
  phi_df<- tibble(party = rep(unique(party_data[[i]]$a_party_string),times = length(frames)),
                      year = rep(unique(party_data[[i]]$year),times = length(frames)),
                      position = rep(positions[1],times = length(frames)),
                      frame = frames,
                      phi = phi_val)
  phi_pos[[i]]<-phi_df}
}

dv_pos<- do.call("rbind",phi_pos)

#cond x frames:

phi_cond<- list()
for (i in 1:length(party_data)) {
  if(nrow(party_data[[i]])==0){
    next
  }else{
    phi_val<-map2(.x = rep(positions[2],times = length(frames)),.y = frames,.f = phi_coef,data = party_data[[i]]) %>% unlist()
    phi_df<- tibble(party = rep(unique(party_data[[i]]$a_party_string),times = length(frames)),
                    year = rep(unique(party_data[[i]]$year),times = length(frames)),
                    position = rep(positions[2],times = length(frames)),
                    frame = frames,
                    phi = phi_val)
    phi_cond[[i]]<-phi_df}
}

dv_cond<- do.call("rbind",phi_cond)

#negative x frames:

phi_neg<- list()
for (i in 1:length(party_data)) {
  if(nrow(party_data[[i]])==0){
    next
  }else{
    phi_val<-map2(.x = rep(positions[3],times = length(frames)),.y = frames,.f = phi_coef,data = party_data[[i]]) %>% unlist()
    phi_df<- tibble(party = rep(unique(party_data[[i]]$a_party_string),times = length(frames)),
                    year = rep(unique(party_data[[i]]$year),times = length(frames)),
                    position = rep(positions[3],times = length(frames)),
                    frame = frames,
                    phi = phi_val)
    phi_neg[[i]]<-phi_df}
}

dv_neg<- do.call("rbind",phi_neg)


dv_reg<- rbind(dv_pos,dv_neg,dv_cond)


saveRDS(dv_reg,file = file.path(data_path,"regression_data.RDS"))



# Regression --------------------------------------------------------------


