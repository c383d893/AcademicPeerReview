# Clean transpose study: https://transpose-publishing.github.io/#/
# Merge with WDJ, assign editor contact list column
# ISSN, then eISSN, cbind

# Output
# transpose study (ISSN only) + full dataset + whether sent for survey: used to fill additional blinding manually (if sent = Y)

library(tidyverse)

df  <- read.csv("data/Transposedownload.csv") %>% 
  select(-c(Group, Description.Instructions, Allowed.entries, Example)) 

# Transpose and convert back to data frame
df_t <- as.data.frame(t(df))
   
# Step 2: Set the first row as column names
colnames(df_t) <- df_t[1, ]

# Step 3: Remove the first row (since it's now the column names)
df_t <- df_t[-1, ]

# Optionally, move row names to a column
df_t <- df_t %>% select("issn","title", "pr-type") %>% rename(ISSN = issn, peerreview = "pr-type") %>% 
        filter(!ISSN == "") %>% filter(!ISSN == "TESTING") %>%
        filter(!peerreview == "") %>% filter(!peerreview == "#N/A")

# Clean divergent ISSN formats
# Can't tell which is ISSN v eISSN so will need to check all.
df_t_clean <- df_t %>% separate(ISSN, into = c("ISSN", "ISSN2"), sep = ", ") %>%
  mutate(ISSN = sub("^([0-9]{4})([0-9])", "\\1-\\2", ISSN)) %>%
  mutate(ISSN2 = sub("^([0-9]{4})([0-9])", "\\1-\\2", ISSN2)) 

write.csv(df_t_clean, "data/Transposeclean.csv")
# in Excel did some little cleaning/checking

df2 <- read.csv("data/Transposeclean2.csv") %>%
       distinct(title, .keep_all =TRUE) %>%
       filter(!title == "Test") %>%
       select(-title) %>%
       mutate(peer_review_model = case_when(peerreview == "Double blind" ~ "Double/triple blind", 
                                            peerreview == "Single blind" ~ "Single blind", 
                                            peerreview == "Not blinded" ~ "No blinding",
                                            peerreview == "Open (not blinded) Post-Publication"  ~ "No blinding",
                                            peerreview == "Triple blind" ~ "Double/triple blind",
                                            peerreview == "Double Blind and Single Blind and Not blined in rare cases - see \"Peer review policy\" URL"  ~ NA,
                                            peerreview == "double blind for longer articles in open issues, not blinded in special issues and for shorter contributions"  ~ "No blinding",
                                            peerreview == "Single blind, but reviewers have the freedom to sign their reviews."  ~ "Single blind",
                                            peerreview == "open (not blinded) post publication " ~"No blinding",
                                            peerreview == "Unsure/not specified" ~ "Unknown",
                                            peerreview == "Consultative peer review (reviewers discuss their independent reports with one another and can optionally reveal their names to the authors)" ~ "No blinding",
                                            peerreview == "Optional single blind (reviewers can choose to name themselves)" ~ "Single blind",
                                            peerreview == "Single blind but double blind optional" ~ "No blinding",
                                            peerreview == "Single blind but double-blind offered" ~ "No blinding", 
                                            peerreview == "Referees can choose to stay anonymous" ~ "No blinding",
                                            peerreview == "Single Blind" ~ "Single blinding",
                                            peerreview == "Unsure"  ~ "Unknown",
                                            peerreview == "Other" ~ "Unknown",
                                            peerreview == "Transparent" ~ NA )) %>%
         drop_na(peer_review_model) %>%
         select(-peerreview)

saveRDS(df2, "data/Transposeclean_final.rds")

#join with full cleaned wdj data: ISSN based
#ISSN + eISSN = 21471; ISSN = 20114
mdat <- readRDS("data/merged_blinding.RDS") %>% distinct(ISSN, .keep_all = TRUE)
tdat <- df2 %>% rename(tpeer_review_model = peer_review_model)

#clean mdat model
mdat <- mdat %>%
  mutate(peer_review_model = case_when(model == "Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review" ~ "Unknown",
                                       model == "Peer_review" ~ "Unknown",
                                       model == "Open_peer_review" ~ "Unknown",
                                       model == "Post-publication_peer_review,_Open_peer_review" ~ "Unknown",
                                       model == "Double_anonymous_peer_review,_Open_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review,_Open_peer_review" ~ "",
                                       model == "Peer_review,_Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "single_blind_peer_review" ~ "Singel blind",
                                       model == "Peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Editorial_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Editorial_review,_Peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review" ~ "Unknown",
                                       model == "Anonymous_peer_review,_Double_anonymous_peer_review,_Open_peer_review" ~ "Double/triple blind",
                                       model == "Double_anonymous_peer_review,_Post-publication_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Open_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind")) %>%
  select(-model)

#join with ISSN and ISSN2 of tdat

tdatISSN <- tdat %>% select(ISSN, tpeer_review_model)
tdatISSN2 <- tdat %>% select(ISSN2, tpeer_review_model) %>% rename(ISSN = ISSN2, t2peer_review_model = tpeer_review_model) %>% drop_na()

# mdat v tdat issn
mdat_tdatISSN <- mdat %>%
  left_join(tdatISSN, by = "ISSN") 
sum(is.na(mdat_tdatISSN$peer_review_model) & !is.na(mdat_tdatISSN$tpeer_review_model)) # 591 additional journals

mdat_tdatISSN_con <- mdat_tdatISSN %>% 
  mutate(peer_review_model_f = ifelse(is.na(peer_review_model), tpeer_review_model, peer_review_model)) %>%
  select(-peer_review_model, -tpeer_review_model) %>%
  rename(peer_review_model = peer_review_model_f)

# mdat v tdat issn2
mdat_tdatISSN2 <- mdat_tdatISSN_con %>%
  left_join(tdatISSN2, by = "ISSN")
sum(is.na(mdat_tdatISSN2$peer_review_model) & !is.na(mdat_tdatISSN2$t2peer_review_model)) # 5 additional journals

mdat_tdatISSN2_con <- mdat_tdatISSN2 %>% 
  mutate(peer_review_model_f = ifelse(is.na(peer_review_model), t2peer_review_model, peer_review_model)) %>%
  select(-peer_review_model, -t2peer_review_model) %>%
  rename(peer_review_model = peer_review_model_f)

# add column for cat; add column if included in editor contact list
edat <- readRDS("data/merged_blinding_topj20&20perquartpercat.rds") #2100
edat256 <- readRDS("data/merged_blinding_topj256.rds")
edat_ISSN <- unique(c(edat$ISSN, edat256$ISSN)) # 2346

cat <- readRDS("data/journalcategories.rds") %>%
  select(ISSN, category) %>%
  drop_na(ISSN) %>%
  group_by(ISSN) %>%
  mutate(cat = paste0("short_category_", row_number())) %>%
  pivot_wider(names_from = cat, values_from = category)

fdat <- mdat_tdatISSN2_con %>%
  left_join(cat, by = "ISSN") %>%
  mutate(in_editor_list = ifelse(ISSN %in% edat_ISSN, "Y", "N")) 

#save output: this is what went to drive: "Full_CitizenScience_Peer-review_Study"
#20114
saveRDS(fdat, "data/merged_blinding_wdjtrans.RDS")
write.csv(fdat, "data/merged_blinding_wdjtrans.csv")

# add eISSN journals:
edat_eISSN <- unique(c(edat256$eISSN)) # 260

mdat.e <- readRDS("data/merged_blinding.RDS") %>% distinct(eISSN, .keep_all = TRUE) %>% filter(is.na(ISSN)) %>%
  mutate(peer_review_model = NA)

#clean mdat model
mdat.e <- mdat.e %>%
  mutate(peer_review_model = case_when(model == "Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review" ~ "Unknown",
                                       model == "Peer_review" ~ "Unknown",
                                       model == "Open_peer_review" ~ "Unknown",
                                       model == "Post-publication_peer_review,_Open_peer_review" ~ "Unknown",
                                       model == "Double_anonymous_peer_review,_Open_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review,_Open_peer_review" ~ "",
                                       model == "Peer_review,_Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "single_blind_peer_review" ~ "Singel blind",
                                       model == "Peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Editorial_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Anonymous_peer_review" ~ "Unknown",
                                       model == "Editorial_review,_Peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review" ~ "Unknown",
                                       model == "Anonymous_peer_review,_Double_anonymous_peer_review,_Open_peer_review" ~ "Double/triple blind",
                                       model == "Double_anonymous_peer_review,_Post-publication_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Open_peer_review,_Anonymous_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind",
                                       model == "Editorial_review,_Open_peer_review,_Double_anonymous_peer_review" ~ "Double/triple blind")) %>%
  select(-model)

cat.e <- readRDS("data/journalcategories.rds") %>%
  select(eISSN, category) %>%
  drop_na(eISSN) %>%
  group_by(eISSN) %>%
  mutate(cat = paste0("short_category_", row_number())) %>%
  pivot_wider(names_from = cat, values_from = category)

fdat.e <- mdat.e %>%
  left_join(cat.e, by = "eISSN") %>%
  mutate(in_editor_list = ifelse(eISSN %in% edat_eISSN, "Y", "N"))

#join
fdat.f <- rbind(fdat, fdat.e) 

# 21472
saveRDS(fdat.f, "data/merged_blinding_wdjtrans_final.RDS")
write.csv(fdat.f, "data/merged_blinding_wdjtrans_final.csv")

# exploring how we ended up with 2361 in_editor_list, when there should be 2360

top.js <- readRDS("data/merged_blinding_topj256.rds")
length(unique(top.js$journal)) #260
quart.js <- readRDS("data/merged_blinding_topj20&20perquartpercat.rds")
length(unique(quart.js$journal)) #2100

included.js <- rbind(top.js, quart.js)
length(unique(included.js$journal)) #2360
