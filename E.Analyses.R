# final final clean (group of categories to 7)
# Analyses by discipline, business model and Impact factor
# fist in_editor_list, then all

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(ggeffects)
library(multcomp)
library(ggalluvial)

# set colors
colors_model <- c(
  "Single-blind" = "grey45",
  "Double-blind" = "grey25",
  "Unknown" = "grey85"
)

colors_broad_group <- c(
  "Humanities and Social Sciences" = "orchid4",
  "Science, Technology, Engineering, and Mathematics" = "#33a02c"
)


cat_names <- c(
  "Arts&Humanities" =  "Arts & Humanities",
  "SocialSciences" = "Social Sciences",
  "Multidisciplinary"   ="Multidisciplinary",
  "LifeSciences"  =  "Life Sciences",
  "ClinicalPre-Clinical&Health" = "Clinical Pre-Clinical & Health",
  "PhysicalSciences" = "Physical Sciences",
  "Engineering&Technology" = "Engineering & Technology"
)

# read and add simple cats

cat <- read.table("data/Blinding_cat_merge_May132026.txt", header = TRUE) %>%  # final version of categories
  rename(category = catlong, short_category = catshort1) %>%
  dplyr::select(category, short_category)

dat <- readRDS("data/merged_blinding_wdjtrans_final_peerreview.rds") %>%
  dplyr::select(-starts_with("short_")) %>%
  rename(category = category_1) %>%
  dplyr::select(-starts_with("category_")) %>%
  left_join(cat, by = "category") %>%
  dplyr::select(-starts_with("language_")) %>%
  mutate(journal_super_category = case_when(short_category == "History&Archaeology" ~ "Arts&Humanities",
                                     short_category == "Psychiatry&Psychology" ~ "ClinicalPre-Clinical&Health",
                                     short_category == "EconomicsBusiness" ~ "SocialSciences",
                                     short_category == "ClinicalMedicine" ~ "ClinicalPre-Clinical&Health",
                                     short_category == "Multidisciplinary" ~ "Multidisciplinary",
                                     short_category == "SocialSciencesGeneral" ~ "SocialSciences",
                                     short_category == "Philosophy&Religion" ~ "Arts&Humanities",
                                     short_category == "Visual&PerformingArts" ~ "Arts&Humanities",
                                     short_category == "Biology&Biochemistry" ~ "LifeSciences",
                                     short_category == "EnvironmentEcology" ~ "LifeSciences",
                                     short_category == "Plant&AnimalScience" ~ "LifeSciences",
                                     short_category == "Geosciences" ~ "PhysicalSciences",
                                     short_category == "AgriculturalSciences" ~ "LifeSciences",
                                     short_category == "Chemistry" ~ "PhysicalSciences",
                                     short_category == "Mathematics" ~ "PhysicalSciences",
                                     short_category == "Physics" ~ "PhysicalSciences",
                                     short_category == "MaterialsScience" ~ "PhysicalSciences",
                                     short_category == "ComputerScience" ~ "Engineering&Technology",
                                     short_category == "Engineering" ~ "Engineering&Technology",
                                     short_category == "Literature&Language" ~ "Arts&Humanities",
                                     short_category == "Arts&Humanities" ~ "Arts&Humanities")) %>%
  drop_na(journal_super_category) %>%
  rename(pub_model = peer_review_model) %>%
  #dplyr::select("journal","ISSN","eISSN","JIF_2022", "JCI_2022","pub_model","journal_super_category", "region","in_editor_list", "citations") %>%
  mutate(pub_model_report = ifelse(pub_model == "Unknown", "N", "Y")) %>%
  mutate(broad_group = case_when(journal_super_category %in% c("Arts&Humanities","SocialSciences","Multidisciplinary" ) ~ "Humanities and Social Sciences",
                                 journal_super_category %in% c("ClinicalPre-Clinical&Health", "Engineering&Technology","PhysicalSciences", "LifeSciences") ~ "Science, Technology, Engineering, and Mathematics")) %>%
  mutate(pub_model = ifelse(pub_model == "Double/triple-blind", "Double-blind", pub_model))

# complete list
dat.b <- dat %>% drop_na(pub_model) %>% filter(!pub_model == "None")
# editor list
dat.b.el <- dat.b %>% filter(in_editor_list == "Y")

###############################
##### SUMMARY & VISUALIZE #####
###############################

# plot data: peer review model and reporting by discipline, colored by HSS & STEM

dat.b <- dat.b %>%
  mutate(
    journal_super_category = factor(
      journal_super_category,
      levels = names(cat_names),
      labels = cat_names
    )
  )

sankey_data <- dat.b %>%
  count(
    journal_super_category,
    pub_model,
    broad_group
  )


plot <- 
ggplot(
  sankey_data,
  aes(
    axis1 = journal_super_category,
    axis2 = pub_model,
    y = n
  )
) +
  geom_alluvium(
    aes(fill = broad_group),
    width = 0.2,
    alpha = 0.8
  ) +
  geom_stratum(
    width = 0.2,
    fill = "grey90",
    color = "black"
  ) +
  geom_text(
    stat = "stratum",
    aes(label = paste0(after_stat(stratum), "\n(n = ", after_stat(count), ")")),
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Humanities and Social Sciences" = "orchid4",
      "Science, Technology, Engineering, and Mathematics" = "#33a02c"
    ),
    name = NULL   # removes legend title
  ) +
  guides(
    fill = "none"   # removes legend entirely
  ) +
  scale_x_discrete(
    labels = c("Discipline", "Peer review model"),
    expand = c(.1, .1)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 30) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# save fig
png("figures/Figure1_complete_summary.jpg", width = 25, height = 10, units = 'in', res = 300)
plot
dev.off()

# summarize data

str(dat)
n <- dat %>% group_by(journal_super_category) %>% tally()
n.pr.cat <- dat %>% group_by(journal_super_category, pub_model) %>% tally()

dat.b.per <- dat.b %>%
  count(pub_model) %>%
  mutate(percent = n / sum(n) * 100)

dat.b.per.disc <- dat.b %>%
  count(journal_super_category, pub_model) %>%
  group_by(journal_super_category) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

########################
######## MODEL #########
########################

# M1: blinding by field (3561)
# predicted prob of double-blind

m1.dat <- dat.b %>% filter(pub_model != "Unknown") %>%
  mutate(pub_model = as.factor(pub_model)) %>%
  mutate(pub_model = fct_relevel(pub_model, "Single-blind")) 
m1 <- glm(pub_model ~ journal_super_category, family = "binomial", data = m1.dat)
summary(m1)

p <- ggeffect(m1, terms = "journal_super_category")

cld_res <- cld(emmeans(m1, ~ journal_super_category, type="response"), Letters=letters)
letters_df <- as.data.frame(cld_res)
letters_df$.group <- trimws(as.character(letters_df$.group))
p$journal_super_category <- p$x

plot_df <- merge(p, letters_df,
                 by = "journal_super_category",
                 all.x = TRUE) %>% 
  mutate(broad_group = case_when(journal_super_category %in% c("Arts & Humanities","Social Sciences","Multidisciplinary" ) ~ "Humanities and Social Sciences",
                                                             journal_super_category %in% c("Clinical Pre-Clinical & Health", "Engineering & Technology","Physical Sciences", "Life Sciences") ~ "Life Sciences")) 
  
plot_df$x <- fct_reorder(plot_df$x, plot_df$predicted, .desc = FALSE)
ordered_levels <- levels(plot_df$x)

plot <- ggplot(plot_df, aes(x = x, y = predicted, fill = broad_group)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_text(aes(label = .group, y = conf.high + 0.05), size = 5) +
  labs(x = "Discipline", y = "Probability of Double-blind") +
  scale_x_discrete(labels = cat_names) +
  scale_fill_manual(values = colors_broad_group) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  ylim(0, 1.05) 

# contrasts science vs humanities
means <- emmeans(m1, ~journal_super_category)
means

disc.contrasts <- list("Sci vs humanities" = c(-2,1,1,1,0,1,-2))

results <- lsmeans::contrast(means,disc.contrasts)
results.df <- as.data.frame(results)
results.df # p <.0001

# save fig
png("figures/Figure2A_predicted_discipline_pubmodel.jpg", width = 12, height = 10, units = 'in', res = 300)
plot
dev.off()

# M2: findability by field (6903)
# predicted prob of Y

m2.dat <- dat.b %>% 
  mutate(pub_model_report = as.factor(pub_model_report)) %>%
  mutate(pub_model_report = fct_relevel(pub_model_report, "N"))
m2 <- glm(pub_model_report ~ journal_super_category, family = "binomial", data = m2.dat)
summary(m2)

p <- ggeffect(m2, terms = "journal_super_category")

cld_res <- cld(emmeans(m2, ~ journal_super_category, type="response"), Letters=letters)
letters_df <- as.data.frame(cld_res)
letters_df$.group <- trimws(as.character(letters_df$.group))
p$journal_super_category <- p$x

plot_df <- merge(p, letters_df,
                 by = "journal_super_category",
                 all.x = TRUE) %>% 
  mutate(broad_group = case_when(journal_super_category %in% c("Arts & Humanities","Social Sciences","Multidisciplinary" ) ~ "Humanities and Social Sciences",
                                 journal_super_category %in% c("Clinical Pre-Clinical & Health", "Engineering & Technology","Physical Sciences", "Life Sciences") ~ "Life Sciences")) 

plot_df$x <- factor(plot_df$x, levels = ordered_levels)

plot <- ggplot(plot_df, aes(x = x, y = predicted, fill = broad_group)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_text(aes(label = .group, y = conf.high + 0.05), size = 5) +
  labs(x = "Discipline", y = "Probability of reporting model") +
  scale_x_discrete(labels = cat_names) +
  scale_fill_manual(values = colors_broad_group) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  ylim(0, 1.05)

# contrasts science vs humanities
means <- emmeans(m2, ~journal_super_category)
means

results <- lsmeans::contrast(means,disc.contrasts)
results.df <- as.data.frame(results)
results.df # p <.0001

# save fig
png("figures/Figure2B_predicted_discipline_pubmodelreport.jpg", width = 12, height = 10, units = 'in', res = 300)
plot
dev.off()

# M3: JIF by blinding and field (6332)
m3.dat <- dat.b %>% filter(!is.na(JIF_2022))
m3 <- glm(log(JIF_2022) ~ journal_super_category*pub_model, data = m3.dat)
summary(m3)

p <- ggeffect(m3, terms = c("journal_super_category","pub_model"))

p$predicted <- exp(p$predicted)
p$conf.low  <- exp(p$conf.low)
p$conf.high <- exp(p$conf.high)

emm <- emmeans(m3, ~ pub_model | journal_super_category)

cld_res <- as.data.frame(cld(emm, Letters = letters))
cld_res$.group <- trimws(as.character(cld_res$.group))

cld_res$emmean   <- exp(cld_res$emmean)
cld_res$lower.CL <- exp(cld_res$lower.CL)
cld_res$upper.CL <- exp(cld_res$upper.CL)

plot_df <- merge(p, cld_res,
                 by.x = c("x", "group"),
                 by.y = c("journal_super_category", "pub_model"),
                 all.x = TRUE)

cld_res$journal_super_category <- factor(
  cld_res$journal_super_category,
  levels = ordered_levels)

plot <- ggplot(cld_res, aes(x = journal_super_category, y = emmean, fill = pub_model)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8) +
  #geom_point(data = m3.dat, aes(x = journal_super_category, y = JIF_2022, color = pub_model), position = position_dodge(0.8), size = 1, show.legend = FALSE)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(label = .group, y = upper.CL),
            position = position_dodge(0.8), vjust = -0.5) +
  labs(x = "Discipline", y = "Impact factor (JIF)", fill = "Peer review model") +
  scale_fill_manual(values = colors_model) +
  scale_color_manual(values = colors_model) + 
  scale_x_discrete(labels = cat_names) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") 
  
# contrasts science vs humanities
means <- emmeans(m3, ~journal_super_category*pub_model)
means
  
disc.blind.contrasts <- list("Sci vs humanities vs blinding" = c(-2,1,1,1,0,1,-2, 2,-1,-1,-1,0,-1,2, 0,0,0,0,0,0,0),
                             "blinding" = c(1,1,1,1,1,0,1, -1,-1,-1,-1,-1,0,-1, 0,0,0,0,0,0,0))
  
results <- lsmeans::contrast(means,disc.blind.contrasts)
results.df <- as.data.frame(results)
results.df # p = 0.0011; <.0001

# save fig
png("figures/Figure3A_predicted_discipline_pubmodel_JIF.jpg", width = 12, height = 10, units = 'in', res = 300)
plot
dev.off()


# M4:JCI by blinding and field (6322)

m4.dat <- dat.b %>% filter(!is.na(JCI_2022)) %>% filter(!JCI_2022==0) 
m4 <- glm(log(JCI_2022) ~ journal_super_category*pub_model, data = m4.dat)
summary(m4)

p <- ggeffect(m4, terms = c("journal_super_category","pub_model"))

p$predicted <- exp(p$predicted)
p$conf.low  <- exp(p$conf.low)
p$conf.high <- exp(p$conf.high)

emm <- emmeans(m4, ~ pub_model | journal_super_category)

cld_res <- as.data.frame(cld(emm, Letters = letters))
cld_res$.group <- trimws(as.character(cld_res$.group))

cld_res$emmean   <- exp(cld_res$emmean)
cld_res$lower.CL <- exp(cld_res$lower.CL)
cld_res$upper.CL <- exp(cld_res$upper.CL)

plot_df <- merge(p, cld_res,
                 by.x = c("x", "group"),
                 by.y = c("journal_super_category", "pub_model"),
                 all.x = TRUE)

cld_res$journal_super_category <- factor(
  cld_res$journal_super_category,
  levels = ordered_levels)

plot <- ggplot(cld_res, aes(x = journal_super_category, y = emmean, fill = pub_model)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8) +
  #geom_point(data = m4.dat, aes(x = journal_super_category, y = JCI_2022, color = pub_model), position = position_dodge(0.8), size = 1, show.legend = FALSE)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(label = .group, y = upper.CL),
            position = position_dodge(0.8), vjust = -0.5) +
  labs(x = "Discipline", y = "Impact factor (JCI)", fill = "Peer review model") +
  scale_fill_manual(values = colors_model) +
  scale_color_manual(values = colors_model) + 
  scale_x_discrete(labels = cat_names) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") 
  
# contrasts science vs humanities
means <- emmeans(m4, ~journal_super_category*pub_model)
means
  
results <- lsmeans::contrast(means,disc.blind.contrasts)
results.df <- as.data.frame(results)
results.df # p = 0.3441; <.0001

# save fig
png("figures/Figure3B_predicted_discipline_pubmodel_JCI.jpg", width = 12, height = 10, units = 'in', res = 300)
plot
dev.off()

# M5: Citations by blinding and field (3445)

m5.dat <- dat.b %>% filter(!is.na(citations)) 
m5 <- glm(log(citations) ~ journal_super_category*pub_model, data = m5.dat)
summary(m5)

p <- ggeffect(m5, terms = c("journal_super_category","pub_model"))

p$predicted <- exp(p$predicted)
p$conf.low  <- exp(p$conf.low)
p$conf.high <- exp(p$conf.high)

emm <- emmeans(m5, ~ pub_model | journal_super_category)

cld_res <- as.data.frame(cld(emm, Letters = letters))
cld_res$.group <- trimws(as.character(cld_res$.group))

cld_res$emmean   <- exp(cld_res$emmean)
cld_res$lower.CL <- exp(cld_res$lower.CL)
cld_res$upper.CL <- exp(cld_res$upper.CL)

plot_df <- merge(p, cld_res,
                 by.x = c("x", "group"),
                 by.y = c("journal_super_category", "pub_model"),
                 all.x = TRUE)

cld_res$journal_super_category <- factor(
  cld_res$journal_super_category,
  levels = ordered_levels)

plot <- ggplot(cld_res, aes(x = journal_super_category, y = emmean, fill = pub_model)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8) +
  #geom_point(data = m5.dat, aes(x = journal_super_category, y = citations, color = pub_model), position = position_dodge(0.8), size = 1, show.legend = FALSE)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(label = .group, y = upper.CL),
            position = position_dodge(0.8), vjust = -0.5) +
  labs(x = "Discipline", y = "Citations", fill = "Peer review model") +
  scale_fill_manual(values = colors_model) +
  scale_color_manual(values = colors_model) + 
  scale_x_discrete(labels = cat_names) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") 

# contrasts science vs humanities
means <- emmeans(m5, ~journal_super_category*pub_model)
means

results <- lsmeans::contrast(means,disc.blind.contrasts)
results.df <- as.data.frame(results)
results.df # p = 0.0001; <.0001

# save fig
png("figures/FigureS1_predicted_discipline_pubmodel_citations.jpg", width = 12, height = 10, units = 'in', res = 300)
plot
dev.off()
