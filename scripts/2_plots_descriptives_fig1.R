##############################
##  
##  Author: Marshall A. Taylor and Dustin S. Stoltz
##  Notes: Code is for figure 1/2: USE
##############################

### BEGIN ###

##############################
## Packages and data
##############################

pacman::p_load(tidyverse, scales, ggpubr, srvyr, viridis, install = TRUE)

# Import prepared data
data_all <- read.csv("../data/20251014_combined_survey_responses.csv")

data_all <- data_all |>
  mutate(computational = ifelse(computational, "Yes", "No"))

## ----------------------------------------------------------------------------
# Figure 1 dataset
## ----------------------------------------------------------------------------

x_levs <- c("Never.", "At least once.", "At least monthly.", "At least weekly.", "Daily.")
x_labs <- gsub(".", "", x_levs, fixed = TRUE)

df_fig1a <- data_all |>
  as_survey_design(weights = weight_rake) |>
  group_by(use) |>
  filter(!is.na(use)) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  mutate(use = factor(use, levels = x_levs)) |>
  ungroup() 

df_fig1b <- data_all |>
  as_survey_design(weights = weight_rake) |>
  group_by(computational, use) |>
  filter(!is.na(use)) |> filter(!is.na(computational)) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  mutate(use = factor(use, levels = x_levs)) |>
  ungroup()


## ----------------------------------------------------------------------------
## Get N

## Fig 1-2 
n1 <- data_all |> dplyr::select(use) |> drop_na() |> nrow()
n1a <- data_all |> dplyr::select(use, computational) |> na.omit() |> nrow()

n12a <- data_all |> filter(use != "Never.") |> nrow()
n12b <- data_all |> filter(use != "Never.") |> dplyr::select(Q18_1:Q18_4) |> na.omit() |> nrow()
n12c <- data_all |> filter(use != "Never.") |> dplyr::select(Q19_1:Q19_5) |> na.omit() |> nrow()
n12d <- data_all |> filter(use != "Never.") |> dplyr::select(Q20_1:Q20_6) |> na.omit() |> nrow()
n12e <- data_all |> filter(use != "Never.") |> dplyr::select(Q21_1:Q21_4) |> na.omit() |> nrow()



# n_css_resp/(n_css + n_gen_samp)
cat("Ns:\n")
cat("# ---------------------------- #\n")
cat("All use: ", n1, "\n")
cat("All use, by type: ", n1a, "\n")

cat("All stages of use: ", n12a, "\n")
cat("Planning: ", n12b, "\n")
cat("Writing: ", n12c, "\n")
cat("Collection: ", n12d, "\n")
cat("Analysis: ", n12e, "\n")


## ----------------------------------------------------------------------------
# Fig 1. plot

fig1a <- df_fig1a |>
  ggplot(aes(x = use, y = prop)) +
  geom_col(color = "#2D2D2D", fill = viridis(1), width = 0.8, alpha = .8) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = .2) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, subtitle = NULL, title = "How often do you use GenAI in your research practices?") +
  scale_x_discrete(breaks = x_levs, labels = x_labs) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  theme_bw() +
  theme(title = element_text(size = 9.5),
        panel.border = element_blank(), 
        axis.text.x = element_blank(), 
        legend.position = "none", 
        plot.margin = margin(5,21.5,5,143.5, "points")) +
  coord_flip()

fig1b <- df_fig1b |>
  ggplot(aes(x = use, y = prop)) +
  geom_col(color = "#2D2D2D", aes(fill = computational), width = 0.8, alpha = .8, position = position_dodge(width = 0.8, preserve = "single")) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp, group = computational), 
      width = .2,
      position = position_dodge(width = 0.7, preserve = "single")) +
  guides(color = "none") +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, fill = "Do you use computational methods?") +
  scale_x_discrete(breaks = x_levs, labels = x_labs) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(ncol = 2, reverse = TRUE)) +
  theme_bw() +
  theme(
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin = margin(0,21.5,7,143.5, "points")) +
  coord_flip()

fig1_both <- ggpubr::ggarrange(fig1a, fig1b, ncol = 1)

## ----------------------------------------------------------------------------
# Fig 2
## ----------------------------------------------------------------------------


## ---- PLANNING
df_fig2a <- data_all |>
  dplyr::select(Q18_1:Q18_4, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q18")) |>
  mutate(value = case_match(value,
                   "Never" ~ "Never",
                   NA ~ "Never",
                   .default = "At least sometimes",
  ),
  name = case_match(name,
           "Q18_1" ~ "Brainstorm about a new idea",
           "Q18_2" ~ "Search for articles while reviewing the literature",
           "Q18_3" ~ "Summarize articles while reviewing the literature",
           "Q18_4" ~ "Translate articles while reviewing the literature"),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"),  "\\.", extra = "merge") |>
  ungroup() |>
  filter(value != "Never") |> 
  mutate(stage = "PLANNING") 

## ---- WRITING
df_fig2b <- data_all |>
  dplyr::select(Q19_1:Q19_5, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q19")) |>
  mutate(value = case_match(value,
                   "Never" ~ "Never",
                   NA ~ "Never",
                   .default = "At least sometimes"),
  name = case_match(name,
           "Q19_1" ~ "To check grammar and spelling in my own writing",
           "Q19_2" ~ "To help translate my own writing",
           "Q19_3" ~ "To paraphrase sections of my own writing",
           "Q19_4" ~ "To outline a section before I begin writing",
           "Q19_5" ~ "To write a section of a paper, which I edit afterward"),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"),  "\\.", extra = "merge") |>
  ungroup() |>
  filter(value != "Never") |> 
  mutate(stage = "WRITING")

## ---- COLLECTION
df_fig2c <- data_all |>
  dplyr::select(Q20_1:Q20_6, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q20")) |>
  mutate(value = case_match(value,
                   "Never" ~ "Never",
                   NA ~ "Never",
                   .default = "At least sometimes"),
  name = case_match(name,
           "Q20_1" ~ "To write survey questions or other stimuli",
           "Q20_2" ~ "To translate survey questions or other stimuli",
           "Q20_3" ~ "To transcribe interviews or other audio material",
           "Q20_4" ~ "To generate answers to survey questions",
           "Q20_5" ~ "To write code for the collection of data",
           "Q20_6" ~ "To create simulations"),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"),  "\\.", extra = "merge") |>
  ungroup() |>
  filter(value != "Never") |> 
  mutate(stage = "DATA COLLECTION")

## ---- ANALYSIS
df_fig2d <- data_all |>
  dplyr::select(Q21_1:Q21_4, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q21")) |>
  mutate(value = case_match(value,
                   "Never" ~ "Never",
                   NA ~ "Never",
                   .default = "At least sometimes"),
  name = case_match(name,
           "Q21_1" ~ "To write code for the analysis of data",
           "Q21_2" ~ "To fix or debug code for the analysis of data",
           "Q21_3" ~ "To classify or annotate unstructured data",
           "Q21_4" ~ "To explain the output of statistical analyses"),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
  ungroup() |>
  filter(value != "Never") |> 
  mutate(stage = "DATA ANALYSIS")

# -----------------------------------------------
# COMBINE datasets
df_fig2_all <- bind_rows(df_fig2a, df_fig2b, df_fig2c, df_fig2d) |>
    mutate(
      stage = factor(stage, 
        levels = c("PLANNING", "WRITING", "DATA COLLECTION", "DATA ANALYSIS"))
    )


df_fig2_overall <- data_all |>
      filter(use != "Never.") |>
      mutate_at(vars(starts_with('Q18')), ~ replace_na(., "Never"), 
                vars(starts_with('Q19')), ~ replace_na(., "Never"), 
                vars(starts_with('Q20')), ~ replace_na(., "Never"), 
                vars(starts_with('Q21')), ~ replace_na(., "Never")) |>
      mutate(`Planning` = case_when(
                (Q18_1 != "Never" | Q18_2 != "Never" | Q18_2 != "Never" | Q18_2 != "Never") ~ "At least sometimes",
                .default = "Never"),
             `Writing` = case_when(
                (Q19_1 != "Never" | Q19_2 != "Never" | Q19_2 != "Never" | Q19_2 != "Never") ~ "At least sometimes",
                .default = "Never"),
             `Data Collection` = case_when(
                (Q20_1 != "Never" | Q20_2 != "Never" | Q20_2 != "Never" | Q20_2 != "Never") ~ "At least sometimes",
                .default = "Never"), 
              `Data Analysis` = case_when(
                (Q21_1 != "Never" | Q21_2 != "Never" | Q21_2 != "Never" | Q21_2 != "Never") ~ "At least sometimes",
                .default = "Never")
      ) |>
      dplyr::select(Planning, Writing, `Data Collection`, `Data Analysis`, computational, use, weight_rake) |>
      pivot_longer(cols = c("Planning","Writing","Data Collection","Data Analysis")) |>
            mutate(value = case_match(value,
                       "Never" ~ "Never",
                       NA ~ "Never",
                       .default = "At least sometimes",
      )) |>
      group_by(computational, use, weight_rake, name) |>
      mutate(temp = paste0(value, ".", computational)) |>
      as_survey_design(weights = weight_rake) |>
      group_by(name, temp) |>
      filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
      summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
      separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
      ungroup() |>
      filter(value != "Never") |> 
      mutate(name = factor(toupper(name), 
             levels = rev(c("PLANNING", "WRITING", "DATA COLLECTION", "DATA ANALYSIS"))),
      )

## ----------------------------------------------------------------------------
# Figures
## ----------------------------------------------------------------------------

## fig2 plots
fig2_top <- df_fig2_overall |>
ggplot(aes(x = name, y = prop)) +
  geom_col(color = "#2D2D2D", aes(fill = computational), width = 0.8, alpha = .8, position = position_dodge(width = 0.8, preserve = "single")) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp, group = computational), 
      width = .2,
      position = position_dodge(width = 0.8, preserve = "single")) +
  labs(x = NULL, y = NULL,
  title = "How often do you use GenAI in the ______ stages of research?",
  fill = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(title = element_text(size = 9.5),
        panel.border = element_blank(),
        legend.position = "none", 
        axis.text.x = element_blank(), 
        plot.margin = margin(5,21,1,126, "points")) +
  coord_flip()

fig2_bot <- df_fig2_all |>
ggplot(aes(x = name, y = prop)) +
  geom_col(color = "#2D2D2D", aes(fill = computational), width = 0.8, alpha = .8, position = position_dodge(width = 0.8, preserve = "single")) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp, group = computational), 
      width = .2,
      position = position_dodge(width = 0.8, preserve = "single")) +
  labs(x = NULL, y = "Percent (%)",
  title = NULL,
  fill = "Do you use computational methods?") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(ncol = 2, reverse = TRUE)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.text = element_text(size = 7),
        legend.position = "inside",
        legend.position.inside = c(.7, .12),
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(t = 1, l = 1, r = 1), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  facet_grid(vars(stage), scales = "free_y") +
  coord_flip()

fig2_both <- ggpubr::ggarrange(fig2_top, fig2_bot, ncol = 1, heights = c(1.2, 4.2))


## ----------------------------------------------------------------------------

png("figures/paper_fig01_02_ds.png", res = 350, units = "in", height = 9, width = 7.5)
print(
ggpubr::ggarrange(fig1_both, fig2_both, ncol = 1, heights = c(1.7, 4))
)
dev.off()
