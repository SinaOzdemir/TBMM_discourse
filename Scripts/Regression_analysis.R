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

phi_coef<- function(var1,var2,data){
  cont_table<- table(data[[var1]],data[[var2]])
  cont_matrix<- matrix(cont_table,nrow = 2, ncol = 2)
  phi <- psych::phi(t = cont_matrix,digits = 2)
  chi_test<- chisq.test(x = cont_matrix,simulate.p.value = T,B = 5000)
  chi_p_val<- chi_test[["p.value"]]
  phi_res<- data.frame(phi_value = phi,
                       chi_sq_pval = chi_p_val)
  return(phi_res)
}

# data --------------------------------------------------------------------


data<- readRDS(file = file.path(data_path,"replication_data_w_frames.RDS")) %>%
  filter(i_organization_string %in% c("European Union",
                                      "Candidacy (for membership)",
                                      "Europe – Belonging",
                                      "Customs Union",
                                      "Schengen Area",
                                      "(Stabilisation and) Association agreement")) %>% filter(i_country_string %in%"Turkey")

data$a_party_string<- car::recode(var = data$a_party_string,recodes = "'Anavatan Partisi' = 'AP';'Bağımsızlar' = 'Independents';'Cumhuriyet Halk Partisi' = 'CHP';'Kurdish Party Family' = 'Kurdish Parties';'Milliyetçi Hareket Partisi' = 'MHP';'Not specified' = 'External';'Platforma Obywatelska (PO)' = 'External'",as.factor = F,as.numeric = F)
data$a_party_string<- ifelse(data$a_party == 5301, "AKP",data$a_party_string)
data$a_party<-as.character(data$a_party)
  
data$i_position_pos<- ifelse(data$i_position == 1, 1,0)
data$i_position_cond<- ifelse(data$i_position == 0, 1,0)
data$i_position_neg <- ifelse(data$i_position == 2, 1,0)

parties <- grep(x = unique(as.character(data$a_party)),pattern = "5301|5302|5303|5304",value = T)
years<- unique(data$year)


# phi coefficient[DEPRICATED DO NOT RUN!!!] ---------------------------------------------------------
frames<- colnames(data)[15:24]
positions<- colnames(data)[34:36]

party_data<- group_split(.tbl = group_by(data, a_party,year),.keep = T)

#position x frames:

phi_pos<- list()
reg_dv<- data.frame()
for (k in 1:length(positions)) {
  

for (i in 1:length(party_data)) {
  if(nrow(party_data[[i]])==0){
    warning("data has no observations")
    next
  }else{
  phi_val<-map2_dfr(.x = rep(positions[k],times = length(frames)),.y = frames,.f = phi_coef,data = party_data[[i]])
  
  phi_df<- tibble(party = rep(unique(party_data[[i]]$a_party),times = nrow(phi_val)),
                      year = rep(unique(party_data[[i]]$year),times = nrow(phi_val)),
                      position = rep(positions[k],times = nrow(phi_val)),
                      frame = frames) %>% bind_cols(.,phi_val)
  phi_pos[[i]]<-phi_df}
}
  phi_pos_df<- do.call("rbind",phi_pos)
  reg_dv<- rbind(reg_dv,phi_pos_df)
}

saveRDS(reg_dv,file = file.path(data_path,"regression_data.RDS"))



# DV plots --------------------------------------------------------------


dvs<- data %>%
  select(a_party,year, i_position,cost_benefit:fairnes) %>%
  pivot_longer(.,cols = cost_benefit:fairnes,names_to = "frames") %>% 
  group_by(a_party,year,i_position,frames) %>%
  summarise(n_frame = sum(value)) %>%
  ungroup() %>% filter(n_frame >0)

dvs$i_position<- ifelse(dvs$i_position == 0, "i_position_cond",
                             ifelse(dvs$i_position == 1, "i_position_pos",
                                    ifelse(dvs$i_position == 2, "i_position_neg","W")))

colnames(dvs)<- c("party","year","position","frame","n")



akp<- dvs %>% 
  filter(party%in%"5301") %>% mutate(text_label = paste0("n=",n)) %>% 
  ggplot(aes(x = frame, y = position,fill = n))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = text_label),color = "white",size = 3)+
  coord_fixed()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_discrete(labels = c("i_position_pos" = "Positive","i_position_cond" = "Conditional","i_position_neg" = "Negative"))+
  scale_x_discrete(labels = c("cost_benefit" = "Cost/Benefit calc","fairnes"="fairness"))+labs(title = "Purpose of frames for AKP",x = "Frames",y = "Positions")+
  facet_wrap(~year)

chp<- dvs %>% 
  filter(party%in%"5302") %>% mutate(text_label = paste0("n=",n)) %>% 
  ggplot(aes(x = frame, y = position,fill = n))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = text_label),color = "white",size = 3)+
  coord_fixed()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_discrete(labels = c("i_position_pos" = "Positive","i_position_cond" = "Conditional","i_position_neg" = "Negative"))+
  scale_x_discrete(labels = c("cost_benefit" = "Cost/Benefit calc","fairnes"="fairness"))+labs(title = "Purpose of frames for CHP",x = "Frames",y = "Positions")+
  facet_wrap(~year)


mhp<-dvs %>% 
  filter(party%in%"5303") %>% mutate(text_label = paste0("n=",n)) %>% 
  ggplot(aes(x = frame, y = position,fill = n))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = text_label),color = "white",size = 3)+
  coord_fixed()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_discrete(labels = c("i_position_pos" = "Positive","i_position_cond" = "Conditional","i_position_neg" = "Negative"))+
  scale_x_discrete(labels = c("cost_benefit" = "Cost/Benefit calc","fairnes"="fairness"))+labs(title = "Purpose of frames for MHP",x = "Frames",y = "Positions")+
  facet_wrap(~year)

kp<- dvs %>% 
  filter(party%in%"5304") %>% mutate(text_label = paste0("n=",n)) %>% 
  ggplot(aes(x = frame, y = position,fill = n))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = text_label),color = "white",size = 3)+
  coord_fixed()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_discrete(labels = c("i_position_pos" = "Positive","i_position_cond" = "Conditional","i_position_neg" = "Negative"))+
  scale_x_discrete(labels = c("cost_benefit" = "Cost/Benefit calc","fairnes"="fairness"))+
  labs(title = "Purpose of frames for Ethnic Parties",x = "Frames",y = "Positions")+
  facet_wrap(~year)

ggsave(filename = "akp_frame_purp.jpeg",plot = akp,path = graph_path)
ggsave(filename = "chp_frame_purp.jpeg",plot = chp,path = graph_path)
ggsave(filename = "mhp_frame_purp.jpeg",plot = mhp,path = graph_path)
ggsave(filename = "kp_frame_purp.jpeg",plot = kp,path = graph_path)


#Phi coefficient cannot capture colienarity between two variables, either consider alternatives or use count data for regression