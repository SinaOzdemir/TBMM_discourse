#Run

packs = c("tidyverse","readxl","gridExtra","reshape2")

install.packages(packs)

lapply(packs, library, character.only = T)

source(file = "preprocessing.R",local = T)
source(file = "Crosstabulations.R", local = T)
source(file = "longitudinal_analysis_issue_position.R",local = T)
source(file = "longitudinal_analysis_master_frames.R", local = T)
source(file = "longitudinal_analysis_subframe.R", local = T)
