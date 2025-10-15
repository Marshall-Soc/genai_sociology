
##############################
##  
##  Author: Marshall A. Taylor and Dustin S. Stoltz
##  Notes: Code is for barcharts in Appendix
##############################


### BEGIN ###

##############################
## Packages and data
##############################

pacman::p_load(tidyverse, scales, srvyr, viridis, install = TRUE)

# Import data
data_all <- read.csv("../data/20251014_combined_survey_responses.csv")

data_all <- data_all |>
  mutate(computational = ifelse(computational, "Yes", "No"))

##############################
## Figures
##############################

## ----------------------------------------------------------------------------
# Function to create Likert Plots
## ----------------------------------------------------------------------------
mega_likert_plotter <- function(df, breaks, levels) {
  return(df |> 
  ggplot(aes(x = value, y = prop)) +
  geom_col(color = "#2D2D2D", aes(fill = computational), width = 0.8, alpha = .8, 
           position = position_dodge(width = 0.8, preserve = "single")) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp, group = computational), 
      width = .2,
      position = position_dodge(width = 0.8, preserve = "single")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(ncol = 2, reverse = TRUE)) +
  theme_bw() +
  theme(
    title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 8),
    panel.border = element_blank(),
    strip.background = element_rect(color = "#cccccc", fill = "#d9d9d9"),
    legend.position = "inside",
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(t = 1, l = 1, r = 1),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7)
  ) +
  facet_wrap(~name, ncol = 1) + 
  coord_flip() +
  labs(x = NULL,
    y = "Percent (%)",
    fill = "Do you use computational methods?"))
}
    
## ----------------------------------------------------------------------------
#fig3
df_fig3 <- data_all |>
  dplyr::select(Q15_1:Q15_5, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q15")) |>
  mutate(
  name = case_match(name,
                  "Q15_1" ~ "GenAI saves me time",
                  "Q15_2" ~ "GenAI saves me money",
                  "Q15_3" ~ "I am curious about GenAI",
                  "Q15_4" ~ "GenAI enables me to do research that is otherwise impossible",
                  "Q15_5" ~ "GenAI allows me to focus on aspects of research I find more meaningful",
  ),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
  ungroup() |>
  mutate(value = factor(value, levels = c("Strongly disagree",
                                 "Somewhat disagree",
                                 "Neither agree nor disagree",
                                 "Somewhat agree",
                                 "Strongly agree")))

png("figures/paper_fig03_appendix.png", res = 340, units = "in", height = 9, width = 6.5)
print(
mega_likert_plotter(df_fig3) +
  ggtitle("Indicate the extent you agree or disagree that the following are\nreasons for incorporating GenAI into your research practices.") + 
  theme(legend.position.inside = c(.76, .065))
)
dev.off()


## ----------------------------------------------------------------------------
#fig4
df_fig4 <- data_all |>
  dplyr::select(Q57_1:Q57_4, computational, use, weight_rake) |>
  filter(use != "Never.") |>
  pivot_longer(cols = starts_with("Q57")) |>
  mutate(name = case_match(name,
                  "Q57_1" ~ "I feel pressure from my institution to use GenAI",
                  "Q57_2" ~ "I feel pressure from my collaborators to use GenAI",
                  "Q57_3" ~ "I feel pressure from my field of study to use GenAI",
                  "Q57_4" ~ "Tools I typically use, such as search engines, now incorporate GenAI",
  ),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
  ungroup() |>
  mutate(value = factor(value, levels = c("Strongly disagree",
                                 "Somewhat disagree",
                                 "Neither agree nor disagree",
                                 "Somewhat agree",
                                 "Strongly agree")))

png("figures/paper_fig04_appendix.png", res = 340, units = "in", height = 7.5, width = 6.5)
print(
mega_likert_plotter(df_fig4) +
  ggtitle("Indicate the extent you agree or disagree that the following are\nreasons for incorporating GenAI into your research practices.") + 
  theme(legend.position.inside = c(.76, .08))
)
dev.off()


## ----------------------------------------------------------------------------
#fig5

df_fig5 <- data_all |>
  dplyr::select(Q22_1:Q22_4, computational, weight_rake) |>
  pivot_longer(cols = starts_with("Q22")) |>
  mutate(
  name = case_match(name, 
                  "Q22_1" ~ "Computational Costs",
                  "Q22_2" ~ "Economic Costs",
                  "Q22_3" ~ "Environmental Costs",
                  "Q22_4" ~ "Social Costs"),
  temp = paste0(value, ".", computational)) |>
  as_survey_design(weights = weight_rake) |>
  group_by(name, temp) |>
  filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
  separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
  ungroup() |>
  mutate(value = factor(value, levels = c("Unsure",
                                 "Not concerned",
                                 "Slightly concerned",
                                 "Concerned",
                                 "Very concerned"
                                 )))

png("figures/paper_fig05_appendix.png", res = 340, units = "in", height = 7.5, width = 6.5)
print(
mega_likert_plotter(df_fig5) +
  geom_vline(xintercept = 1.5, linetype = 5) +
  ggtitle("Indicate the extent you are concerned with the following.") + 
  theme(legend.position.inside = c(.78, .12))
)
dev.off()


## ----------------------------------------------------------------------------
# fig6

df_fig6 <- data_all |>
dplyr::select(Q59_1:Q59_5, computational, weight_rake) |>
pivot_longer(cols = starts_with("Q59")) |>
mutate(
name = case_match(name,
                "Q59_1" ~ "Biases GenAI may introduce into research practices",
                "Q59_2" ~ "Level of control large tech companies have over GenAI tools",
                "Q59_3" ~ "Data collection practices used in training GenAI models.",
                "Q59_4" ~ "Spread of low quality content or misinformation from GenAI",
                "Q59_5" ~ "Privacy of data transmitted or stored when using GenAI"),
temp = paste0(value, ".", computational)) |>
as_survey_design(weights = weight_rake) |>
group_by(name, temp) |>
filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
separate(temp, c("value", "computational"), "\\.", extra = "merge") |>
ungroup() |>
mutate(value = factor(value, levels = c("Unsure",
                              "Not concerned",
                               "Slightly concerned",
                               "Concerned",
                               "Very concerned"
                               )))

png("figures/paper_fig06_appendix.png", res = 340, units = "in", height = 7.5, width = 6.5)
print(
mega_likert_plotter(df_fig6) +
  geom_vline(xintercept = 1.5, linetype = 5) +
  ggtitle("Indicate the extent you are concerned with the following. Cont.") + 
  theme(legend.position.inside = c(.78, .12))
)
dev.off()

## ----------------------------------------------------------------------------
#fig 8
df_fig8 <- data_all |>
dplyr::select(Q6_4:Q6_5, Q27_1:Q27_4, computational, weight_rake) |>
pivot_longer(cols = starts_with(c("Q6","Q27"))) |>
mutate(
name = case_match(name,
                "Q6_4" ~ "I believe the outputs from GenAI can generally be trusted",
                "Q6_5" ~ "I am concerned about becoming overly dependent on GenAI",
                "Q27_1" ~ "The current advantages of GenAI outweigh their drawbacks for my field",
                "Q27_2" ~ "GenAI will have a net positive effect on my field over the next 2-3 years",
                "Q27_3" ~ "Current GenAI models require significant improvements in order to benefit my field",
                "Q27_4" ~ "GenAI will continue to improve in the next 2-3 years",
),
temp = paste0(value, ".", computational)) |>
as_survey_design(weights = weight_rake) |>
group_by(name, temp) |>
filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
summarize(prop = survey_prop(vartype = "ci", prop_method = "logit", na.rm = TRUE, proportion = TRUE)) |>
separate(temp, c("value", "computational"),  "\\.", extra = "merge") |>
ungroup() |>
mutate(value = factor(value, levels = c("Strongly disagree",
                               "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree",
                               "Strongly agree")))

png("figures/paper_fig07_08_appendix.png", res = 340, units = "in", height = 9, width = 6.5)
print(
mega_likert_plotter(df_fig8) +
ggtitle("Indicate the extent you agree or disagree with the following statements.") + 
  theme(title = element_text(size = 9),
        legend.position.inside = c(.76, .06))
)
dev.off()
