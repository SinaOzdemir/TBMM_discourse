packs = c("crimCV","tidyverse","reshape2","readxl")

lapply(packs, library, character.only = T)

eu = read_xlsx(path = "trajectory_analysis_data.xlsx",sheet = 1,trim_ws = T)


eu.trimmed = select(eu, date,a_party,a_name, i_organization, i_position)

eu.trimmed$date = as.Date.character(eu.trimmed$date,format = "%Y%m%d")

eu.trimmed$year = gsub("-[0-9][0-9]-[0-9][0-9]","",eu.trimmed$date)

eu.trimmed[,4:6] = lapply(eu.trimmed[,4:6],as.numeric)

eu.trimmed = filter(.data = eu.trimmed, i_organization >= 600 & i_organization <=700)

a = eu.trimmed%>%group_by(a_name,i_position)%>%tally(sort = T)

a = a %>% spread(i_position,n)

a[is.na(a)] = 0

a$net_pos = (a$`0`+a$`1`-a$`2`)/(a$`0`+a$`1`+a$`2`)+1

a$net_pos = log(a$net_pos)

qplot(a$net_pos,geom = "histogram",bins = 3)
