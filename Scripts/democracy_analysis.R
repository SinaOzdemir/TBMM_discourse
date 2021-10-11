#################################
# Title: Democratic backlash    #
# Author: Sina Özdemir          #
#         PhD Candidate         #
#         NTNU, Norway          #
#         sina.ozdemir@ntnu.no  #
# Date: 07/10/2021              #
#################################




# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here")

p_load(char = packs, install = T)

data_path<- here("Data")

graph_path<- here("Graphs")

results_path <- here("Analysis","Regression")


# data --------------------------------------------------------------------


data<- readRDS(file = file.path(data_path,"replication_data_w_frames.RDS")) %>%
  filter(i_organization_string %in% c("European Union",
                                      "Candidacy (for membership)",
                                      "Europe – Belonging",
                                      "Customs Union",
                                      "Schengen Area",
                                      "(Stabilisation and) Association agreement")) %>% filter(i_country_string %in%"Turkey")

data$a_party<-as.character(data$a_party)

data$i_position<- ifelse(data$i_position == 1 ,1,
                         ifelse(data$i_position == 0 , 0,
                                ifelse(data$i_position == 2, -1,NA)))

parties <- grep(x = unique(as.character(data$a_party)),pattern = "5301|5302|5303|5304",value = T)
years<- unique(data$year)

v_dem<-readRDS(file = file.choose())

v_dem<-v_dem %>% filter(country_name %in% "Turkey" & year >= 2004 & year<= 2017) %>% mutate(year = as.character(year))

v_dem_party<- readRDS(file.choose())

v_dem_party<- v_dem_party %>% filter(country_name %in% "Turkey" & year >= 2004 & year<= 2017)
# Position x Democracy ----------------------------------------------------

pos_avg<- data %>%
  mutate(i_position = as.numeric(i_position)) %>% 
  group_by(a_party,year) %>%
  summarise(avg_pos = mean(i_position)) %>%
  ungroup() %>% 
  filter(a_party%in%parties)
  
pos_dem<- left_join(pos_avg,v_dem,by = c("year")) %>% select(a_party,year,avg_pos,matches("v2x_*")) %>% select(-matches("sd|low|high"))


pos_dem %>%
  select(a_party,year,avg_pos,v2x_polyarchy:v2x_egaldem) %>%
  pivot_longer(cols = avg_pos:v2x_egaldem,names_to = "v_names",values_to = "values") %>% 
  ggplot(aes(x = year,y = values, colour = v_names,group = v_names))+
  geom_smooth(se = F)+facet_wrap(~a_party)+
  theme_linedraw()


# frame perc. share democracy ---------------------------------------------


frame_perc<- data %>%
  mutate(i_position = factor(i_position,levels = c(-1,0,1))) %>%
  group_by(a_party,year,i_position) %>%
  summarise(across(cost_benefit:fairnes,~sum(.)),
            claim_n = n()) %>%
  rowwise() %>%
  mutate(total_frame = sum(c_across(cost_benefit:fairnes))) %>% 
  mutate(across(cost_benefit:fairnes,~.x/total_frame))

frame_perc[is.na(frame_perc)]<- 0

frame_dem<- left_join(frame_perc,v_dem,by=c("year")) %>%
  select(a_party,year,i_position,cost_benefit:fairnes,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem) %>% 
  pivot_longer(cols = cost_benefit:fairnes, names_to = "frames",values_to = "frame_values")

akp<-frame_dem %>% filter(a_party == "5301"&i_position == 1) %>% 
  pivot_longer(cols = v2x_polyarchy:v2x_egaldem,names_to = "dem_ind",values_to = "dem_vals") %>% 
  ggplot(aes(x = year))+
  geom_bar(aes(y = frame_values,fill= frames),position="dodge",stat = "identity",show.legend = F)+
  geom_smooth(aes(y = dem_vals, color = dem_ind,group =dem_ind),se = F)+
  theme_bw()+
  facet_wrap(~frames)+
  labs(title = "AKP's use of frames for positive position",
       subtitle = "barplot values are percentage share of a particular frame\nin all used frames \nLine graphs indicate V-Dem scores given as z-scores",
       x= "year", y= "")


chp<-frame_dem %>% filter(a_party == "5302"&i_position == 1) %>% 
  pivot_longer(cols = v2x_polyarchy:v2x_egaldem,names_to = "dem_ind",values_to = "dem_vals") %>% 
  ggplot(aes(x = year))+
  geom_bar(aes(y = frame_values,fill= frames),position="dodge",stat = "identity",show.legend = F)+
  geom_smooth(aes(y = dem_vals, color = dem_ind,group =dem_ind),se = F)+
  theme_bw()+
  facet_wrap(~frames)+
  labs(title = "CHP's use of frames for positive position",
       subtitle = "barplot values are percentage share of a particular frame\nin all used frames \nLine graphs indicate V-Dem scores given as z-scores",
       x= "year", y= "")


mhp<- frame_dem %>% filter(a_party == "5303"&i_position == 1) %>% 
  pivot_longer(cols = v2x_polyarchy:v2x_egaldem,names_to = "dem_ind",values_to = "dem_vals") %>% 
  ggplot(aes(x = year))+
  geom_bar(aes(y = frame_values,fill= frames),position="dodge",stat = "identity",show.legend = F)+
  geom_smooth(aes(y = dem_vals, color = dem_ind,group =dem_ind),se = F)+
  theme_bw()+
  facet_wrap(~frames)+
  labs(title = "MHP's use of frames for positive position",
       subtitle = "barplot values are percentage share of a particular frame\nin all used frames \nLine graphs indicate V-Dem scores given as z-scores",
       x= "year", y= "")


kp<-frame_dem %>% filter(a_party == "5304"&i_position == 1) %>% 
  pivot_longer(cols = v2x_polyarchy:v2x_egaldem,names_to = "dem_ind",values_to = "dem_vals") %>% 
  ggplot(aes(x = year))+
  geom_bar(aes(y = frame_values,fill= frames),position="dodge",stat = "identity",show.legend = F)+
  geom_smooth(aes(y = dem_vals, color = dem_ind,group =dem_ind),se = F)+
  theme_bw()+
  facet_wrap(~frames)+
  labs(title = "Kurdish Parties' use of frames for positive position",
       subtitle = "barplot values are percentage share of a particular frame\nin all used frames \nLine graphs indicate V-Dem scores given as z-scores",
       x= "year", y= "")


ggsave(filename = "akp_frame_pos_dem.jpeg",plot = akp,path = graph_path)

ggsave(filename = "chp_frame_pos_dem.jpeg",plot = chp,path = graph_path)

ggsave(filename = "mhp_frame_pos_dem.jpeg",plot = mhp,path = graph_path)

ggsave(filename = "kp_frame_pos_dem.jpeg",plot = kp,path = graph_path)

