# 07_figS5_net_favorability.R
# Figure S5: net favorability collapsed to three categories (more challenges /
# equal / more benefits) per model, as a 100% stacked bar.
#
# Input:  data/blinding_survey_dat.csv
# Output: figures/FigureS5_net_favorability.png
# Run from the repo root: Rscript scripts/07_figS5_net_favorability.R

suppressPackageStartupMessages({ library(tidyverse); library(scales) })

# Benefit (5) and challenge (6) item columns
bens  <- c("benefit_a","benefit_b","benefit_c","benefit_d","benefit_e")
chals <- c("challenge_a","challenge_b","challenge_c","challenge_d","challenge_e","challenge_f")

# --- Net favorability per respondent: benefits endorsed minus challenges -----
# Analytical sample, restricted to editors routed to the benefit/challenge items
# (all double/triple-blind, plus single-blind editors with a considered-status).
# net > 0 / < 0 / = 0 -> "more benefits" / "more challenges" / "equal".
d <- read_csv("data/blinding_survey_dat.csv", show_col_types = FALSE) %>%
  filter(sample == "analytical", pub_model == "Double/triple blind" | !is.na(consid_status)) %>%
  mutate(nb = rowSums(across(all_of(bens))),
         nc = rowSums(across(all_of(chals))),
         net = nb - nc) %>%
  filter(!is.na(net)) %>%                       # item-complete respondents
  mutate(group = if_else(pub_model == "Double/triple blind",
                         "Experienced\n(double-blind)", "Anticipated\n(single-blind)"),
         cat = case_when(net > 0 ~ "More benefits than challenges",
                         net < 0 ~ "More challenges than benefits",
                         TRUE    ~ "Equal"))

# --- Share of editors in each category, within model (bars sum to 100%) ------
n_by <- d %>% count(group, name = "n_group")
plot_dat <- d %>% count(group, cat) %>% left_join(n_by, by = "group") %>%
  mutate(prop = n / n_group,
         cat = factor(cat, levels = c("More challenges than benefits", "Equal",
                                      "More benefits than challenges")),
         group = factor(group, levels = c("Anticipated\n(single-blind)",
                                          "Experienced\n(double-blind)")))

# --- Plot: horizontal 100% stacked bar, one bar per model --------------------
cat_cols <- c("More challenges than benefits" = "#762a83",  # purple
              "Equal" = "grey75",
              "More benefits than challenges" = "#1b9e77")   # green

p <- ggplot(plot_dat, aes(x = group, y = prop, fill = cat)) +
  geom_col(width = 0.62, color = "white") +
  geom_text(aes(label = ifelse(prop >= 0.04, percent(prop, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 4.2, color = "white", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_manual(values = cat_cols, name = NULL) +
  labs(x = NULL, y = "Share of editors") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold"),
        legend.text = element_text(size = 11))

# --- Console summary + export ------------------------------------------------
n_sb <- n_by$n_group[grepl("Anticipated", n_by$group)]
n_db <- n_by$n_group[grepl("Experienced", n_by$group)]
cat(sprintf("N item-complete: SB=%d, DB=%d, total=%d\n", n_sb, n_db, n_sb + n_db))
print(plot_dat %>% select(group, cat, prop) %>% mutate(prop = round(prop, 3)))

dir.create("figures", showWarnings = FALSE)
ggsave("figures/FigureS5_net_favorability.png", p, width = 8.5, height = 3.6, dpi = 300, bg = "white")
cat("done: figures/FigureS5_net_favorability.png\n")
