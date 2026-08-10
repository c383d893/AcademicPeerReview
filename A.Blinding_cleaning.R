# Merge sources together: WOS (reference list), DOAJ (open access journals), and JIF (impact factors for WOS)
# Add impact factor information

# Outputs:
# Full list: WOJ + DOAJ + JIF
# top journal by category JIF_2022 (260); survey round 1

library(tidyverse)

###########################
###### INITIAL CLEAN ######
###########################

# get unique journals of wos
# 22216 journals
wos <- read.table("data/wos_full_202404_Rformat.txt", header = TRUE) %>% 
  distinct(journal, .keep_all = TRUE) 

# read in doaj (only open access)
# 20438 journals
doaj <- read.table("data/doaj_reduced_202404_Rformat.txt", header = TRUE) 
  
# unique doaj only ISSN
# 11814
doaj.issn <- doaj %>% select(!eISSN) %>%
  distinct(ISSN, .keep_all = TRUE) %>%
  drop_na(ISSN)

# doaj only eISSN without ISSN
# 8623
doaj.eissn <- doaj %>%
  filter(is.na(ISSN)) %>% select(!ISSN) %>%
  distinct(eISSN, .keep_all = TRUE) %>%
  drop_na(eISSN)

# bring in impact factors
jif <- read.table("data/Cat_JCR_JournalResults_04_2024_Clean_Rformat.txt", header = TRUE) %>%
  #journal with multiple categories repeated here
  select(-c("category")) %>%
  rename(JIF_2022 = X2022_JIF, JCI_2022 = X2022_JCI) %>%
  distinct(journal, .keep_all = TRUE) %>%
  select(-c("journal")) %>%
  mutate(JIF_2022 = as.numeric(JIF_2022), JIF_quartile = as.numeric(JIF_quartile), 
         citations = as.numeric(citations))
  
# unique jif only ISSN
# 20359
jif.issn <- jif %>% select(!eISSN) %>%
  distinct(ISSN, .keep_all = TRUE) %>%
  drop_na(ISSN)

# unique jif only eISSN without ISSN
# 1146
jif.eissn <- jif %>%
  filter(is.na(ISSN)) %>% select(!ISSN) %>%
  distinct(eISSN, .keep_all = TRUE) %>%
  drop_na(eISSN)

###########################
####### DATA MERGE ########
###########################

# a. issn matches
# matches wos & doaj issn
# 2953
wd.issn <- wos %>% 
  inner_join(doaj.issn, by = "ISSN") %>%
  filter(!(journal == "PHYSICAL_EDUCATION_OF_STUDENTS")) %>%
  filter(!(journal == "ULTRASOUND_JOURNAL"))

# matches wos & jif issn
# 20068
wj.issn <- wos %>% 
  inner_join(jif.issn, by = "ISSN") 

# b. eissn matches
# matches wos & doaj eissn: lots of doaj journals are not in the core wos
# 2808
wd.eissn <- wos %>% 
  inner_join(doaj.eissn, by = "eISSN") 

# matches wos & jif issn
# 1123
wj.eissn <- wos %>% 
  inner_join(jif.eissn, by = "eISSN") 

# c. the remainder of the journals
# those not matched to ISSN or eISSN

# 16455
wd.other <- wos %>%
  filter(!ISSN %in% wd.issn$ISSN) %>%
  filter(!eISSN %in% wd.eissn$eISSN) %>%
  mutate(publisher_country = NA,
         model = NA,  
         weekstopub = NA,
         APC = NA,
         APC_amount = NA,
         waiver = NA)

# 1025
wj.other <- wos %>%
  filter(!ISSN %in% wj.issn$ISSN) %>%
  filter(!eISSN %in% wj.eissn$eISSN) %>%
  mutate(citations = NA,
         JIF_2022 = NA,  
         JIF_quartile = NA,
         JCI_2022 = NA,
         percent_OAGold = NA)

# d. join these each for doaj and jif
wd <- rbind(wd.issn,wd.eissn,wd.other)

# wd had two duplicate journals:
#n_occur <- data.frame(table(wd$journal))
#n_occur_2 <- n_occur[n_occur$Freq > 1,]

#wdrep <- wd %>% filter(journal %in% n_occur_2$Var1)
#reported differntly in ISSN v eISSN, so drop duplicates and fill all rows with NA
#removed at ISSN so other filter captures these

# e. join these each for wos and jif
wj <- rbind(wj.issn,wj.eissn,wj.other)

###########################
###### DATA MERGE 2 #######
###########################

# bring wd and wj together
# merge by issn, then eissn again

# wj collapse to ISSN, eISSN and metadata associated with wj

wj.short <- wj %>% select(c("ISSN", "eISSN", "citations","JIF_2022","JIF_quartile","JCI_2022","percent_OAGold"))

# unique wj.short only ISSN
# and metadata to jif
# 19355
wj.short.issn <- wj.short %>% select(!eISSN) %>%
  distinct(ISSN, .keep_all = TRUE) %>%
  drop_na(ISSN) %>%
  drop_na(JIF_2022)

# wj.short only eISSN without ISSN
# and metadata to jif
# 1091
wj.short.eissn <- wj.short %>%
  filter(is.na(ISSN)) %>% select(!ISSN) %>%
  distinct(eISSN, .keep_all = TRUE) %>%
  drop_na(eISSN) %>%
  drop_na(JIF_2022)

# issn matches
wdj.issn <- wd %>% 
  inner_join(wj.short.issn, by = "ISSN") 

# eISSN
wdj.eissn <- wd %>% 
  inner_join(wj.short.eissn, by = "eISSN") 

# other
wdj.other <- wd %>%
  filter(!ISSN %in% wj.issn$ISSN) %>%
  filter(!eISSN %in% wj.eissn$eISSN) %>%
  mutate(citations = NA,
         JIF_2022 = NA,  
         JIF_quartile = NA,
         JCI_2022 = NA,
         percent_OAGold = NA)

wdj <- rbind(wdj.issn,wdj.eissn,wdj.other)

saveRDS(wdj, "data/merged_blinding.RDS")
write.csv(wdj, "data/merged_blinding.csv")

#find top journal of each category by JCI
#used for survey round 1 (top journal from 256 categories)
topj <- wdj %>% group_by(category_1) %>% slice_max(JIF_2022, n = 1) # 260 because of ties in IF.
saveRDS(topj, "data/merged_blinding_topj256.rds")
write.csv(topj, "data/merged_blinding_topj256.csv")
