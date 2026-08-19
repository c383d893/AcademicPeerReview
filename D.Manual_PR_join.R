# Join transpose cleaned + joined with our manual assessment of survey journals
# data downloaded from drive: "Full_CitizenScience_Peer-review_Study"
# extracted April 22 2026; replace " " with "_"; replace "" with "NA"; keep only ISSN, assigned, peer_review_model

#ISSN, assigned, peer_review_model

library(tidyverse)

dat <- readRDS("data/merged_blinding_wdjtrans_final.rds")

dat.man <- read.table("data/Full_CitizenScience_Peer-review_Study_April232026.txt", header =TRUE) %>%
  rename(peer_review_model_man = peer_review_model) 

dat.fin <- dat %>% 
  left_join(dat.man, by = "ISSN") %>%
  mutate(peer_review_model= ifelse(is.na(peer_review_model), peer_review_model_man, peer_review_model)) %>%
  select(-peer_review_model_man) %>%
  mutate(peer_review_model = case_when(peer_review_model == "Singel blind" ~ "Single-blind",
                                           peer_review_model == "Single blind" ~ "Single-blind",
                                           peer_review_model == "Single_blind" ~ "Single-blind",
                                           peer_review_model == "Single blinding"~ "Single-blind",
                                           peer_review_model == "Double/triple blind"  ~ "Double/triple-blind",
                                           peer_review_model == "Double/triple_blind"  ~ "Double/triple-blind",
                                           peer_review_model == "No blinding" ~ "None",
                                           peer_review_model == "No_blinding" ~ "None",
                                           peer_review_model == "Unknown" ~ "Unknown"))
                                           
  
#how many peer_review_model do we still need from Y?
dat.fin %>% filter(in_editor_list == "Y") %>% group_by(peer_review_model) %>% tally()
#1288

#save
saveRDS(dat.fin, "data/merged_blinding_wdjtrans_final_peerreview.rds")
write.csv(dat.fin, "data/merged_blinding_wdjtrans_final_peerreview.csv")
  
