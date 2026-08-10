# Assess Impact factor across broader categories
# Data manipulation results in several rows per journal, one for each short category assigned to that journal

# Outputs
# top 20 and 20% of quartiles for 21 categories (ISSN based); survey round 2

library(tidyverse)

###########################
##### JOIN CATEGORIES #####
###########################

#by hand:merge 256 categories into 21 larger categories: https://jcr.clarivate.com/jcr/browse-categories?app=jcr&referrer=target%3Dhttps:%2F%2Fjcr.clarivate.com%2Fjcr%2Fbrowse-categories%3Fapp%3Djcr%26referrer%3Dtarget%253Dhttps:%252F%252Fjcr.clarivate.com%252Fjcr%252Fbrowse-categories%26Init%3DYes%26authCode%3Dnull%26SrcApp%3DIC2LS&Init=Yes&authCode=null&SrcApp=IC2LS
cat <- read.table("data/Blinding_cat_merge_Nov202024.txt", header = TRUE) # v1 of categories
wdj <- readRDS("data/merged_blinding.RDS")

# reshape wdj to long format
wdj_long <- wdj %>%
  pivot_longer(cols = starts_with("category_"), names_to = "category_column", values_to = "catlong") %>%
  filter(!is.na(catlong)) 

# join with cat
merged <- wdj_long %>%
  left_join(cat, by = "catlong")

# reshape back to wide format 
merged_wide <- merged %>%
  pivot_wider(names_from = category_column, values_from = c(catlong, catshort1, catshort2))

# reshape the data to long format
# 49860
merged_long <- merged_wide %>%
  pivot_longer(cols = starts_with("catshort"), names_to = "category_type", values_to = "category") %>%
  filter(!is.na(category))  # Remove rows with NA categories

# keep only one category per journal and remove all other cols (still duplicate rows per journal)
# 42540
merged_final <- merged_long %>% 
  select(-c("cathshort4","catlong_category_1","catlong_category_2","catlong_category_3","catlong_category_4", "catlong_category_5", "catlong_category_6", "category_type")) %>%
  distinct(journal, category, .keep_all = TRUE)

saveRDS(merged_final, "data/journalcategories.rds")

###########################
##### JOIN CATEGORIES #####
###########################

#1. Remove 256 journals we already did.
#2. Select top 20 journals of each broad category
#3. Randomly select 20 in each quartile

#1.
# read in previous list
done <- read.csv("data/merged_blinding_topj256.csv")
# remove this from current list
merged_final_pull <- merged_final %>% filter(!ISSN %in% done$ISSN)

#2.
# Sort data by 'JIF_2022' in descending order
merged_final_pull <- merged_final_pull %>%
  arrange(desc(JIF_2022))

# Initialize an empty tibble for the final result
final_result <- tibble()

# Process each category
unique_categories <- unique(merged_final_pull$category)
for (category in unique_categories) {
  # Filter merged_final_pull for the current category, excluding previously selected ISSN values
  group <- merged_final_pull %>%
    filter(category == category)
  
  # Select the top 20 ISSN values for the current category
  top_20 <- group %>%
    distinct(ISSN, .keep_all = TRUE) %>% 
    slice_head(n = 20)
  
  # Append the result to the final DataFrame
  final_result <- bind_rows(final_result, top_20)
  
  # Remove selected Js from the original data
  merged_final_pull <- merged_final_pull %>%
    filter(!ISSN %in% top_20$ISSN)
}

# View the result
print(final_result)

merged_final_pull_top20percat <- final_result

#3.remove these journals also from the merged_final_pull
merged_final_pull_2 <- merged_final_pull %>% filter(!ISSN %in% merged_final_pull_top20percat$ISSN)
# now, take 20 from each category from each quartile

# Function to assign quartiles based on JIF_2022
merged_final_pull_2 <- merged_final_pull_2 %>%
  mutate(Quartile = ntile(JIF_2022, 4)) %>%  # Divide into 4 quartiles
  arrange(desc(JIF_2022))  # Sort by descending JIF_2022

# Initialize an empty DataFrame for the final result
final_result <- data.frame()

# Initialize a set to track selected Js
selected_ISSNs <- character()

# Process each category and each quartile
# ensure no replicate journals
unique_categories <- unique(merged_final_pull_2$category)
for (category in unique_categories) {
  for (quartile in 1:4) {
    # Filter rows for the current category and quartile, excluding already selected Js
    group <- merged_final_pull_2 %>%
      filter(category == category, Quartile == quartile, !ISSN %in% selected_ISSNs)
    
    # Select up to 20 unique Js for the current category and quartile
    top_20 <- group %>%
      distinct(ISSN, .keep_all = TRUE) %>%  # Ensure unique ISSN values
      slice_head(n = 20)
    
    # Append selected rows to the final result
    final_result <- bind_rows(final_result, top_20)
    
    # Update the set of selected Js
    selected_ISSNs <- c(selected_ISSNs, top_20$ISSN)
  }
}

# View the final result; without 256 original.
print(final_result)

merged_final_pull_20percatperquart <- final_result

#4 join together:  merged_final_pull_top20percat & merged_final_pull_20percatperquart
#used for survey round 2 (top 20 and 20 per quartile per category from 21 categories, after removal of 256 in round 1)
jnames_top20 <- merged_final_pull_top20percat %>% select(journal, ISSN)
jnames_quart <- merged_final_pull_20percatperquart %>% select(journal, ISSN)

journal_sample <- rbind(jnames_top20, jnames_quart)
write.csv(journal_sample, "data/merged_blinding_topj20&20perquartpercat.csv")
saveRDS(journal_sample, "data/merged_blinding_topj20&20perquartpercat.rds")
