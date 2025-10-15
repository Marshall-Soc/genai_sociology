
##############################
##  
##  Author: Marshall A. Taylor and Dustin S. Stoltz
##  Notes: Code is for reasons, concerns, optimism
##############################


### BEGIN ###

##############################
## Packages and data
##############################
pacman::p_load(tidyverse, scales,
               ggpubr, srvyr, viridis, install = TRUE)

# Import data
data_all <- read.csv("../data/20251014_combined_survey_responses.csv")

data_all <- data_all |>
  mutate(computational = ifelse(computational, "Yes", "No"))


##############################
## Figures
##############################

theme_genai <- function() {

  theme_bw() +
  theme(
    title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 8),
    panel.border = element_blank(),
    strip.background = element_rect(color = "#cccccc", fill = "#d9d9d9"),
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(t = 1, l = 1, r = 1),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

}

plotter <- function(df) {
  return(df |>
  ggplot(aes(x = computational, y = mean, color = computational)) +
  geom_point(aes(color = computational), size = 3) +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp), width = .2) +
  theme_genai() +
  scale_color_viridis_d(guide = guide_legend(ncol = 2, reverse = TRUE)) + 
  facet_wrap(~name, ncol = 1) +
  coord_flip() +
  labs(color = "Do you use computational methods?",
      #  fill = "Do you use computational methods?",
      y = NULL, x = NULL)
  )
}

## ----------------------------------------------------------------------------
## N

## Fig 3-4 Reasons
n34 <- data_all |> dplyr::filter(use != "Never.") |> dplyr::select(Q15_1:Q15_5, Q57_1:Q57_4) |> na.omit() |> nrow()
## Fig 5-6 Concerns/Costs
n56 <- data_all |> dplyr::select(Q22_1:Q22_4, Q59_1:Q59_5) |> na.omit() |> nrow()
## Fig 7-8 Optimism
n78 <- data_all |> dplyr::select(Q6_4:Q6_5, Q27_1:Q27_4) |> na.omit() |> nrow()

# n_css_resp/(n_css + n_gen_samp)
cat("Ns:\n")
cat("# ---------------------------- #\n")
cat("Reasons for using: ", n34, "\n")
cat("Concerns about GenAI: ", n56, "\n")
cat("Optimism/Distrust: ", n78, "\n")


## ----------------------------------------------------------------------------
#3-4

df_fig03_04 <- data_all |>
  dplyr::select(Q15_1:Q15_5, Q57_1:Q57_4, computational, use, weight_rake) |>
  dplyr::filter(use != "Never.") |>
  pivot_longer(cols = starts_with(c("Q15","Q57"))) |>
  mutate(
    name = case_match(name,
                      "Q15_1" ~ "GenAI saves me time",
                      "Q15_2" ~ "GenAI saves me money",
                      "Q15_3" ~ "I am curious about GenAI",
                      "Q15_4" ~ "GenAI enables me to do research that is otherwise impossible",
                      "Q15_5" ~ "GenAI allows me to focus on aspects of research I find more meaningful",
                      "Q57_1" ~ "I feel pressure from my institution to use GenAI",
                      "Q57_2" ~ "I feel pressure from my collaborators to use GenAI",
                      "Q57_3" ~ "I feel pressure from my field of study to use GenAI",
                      "Q57_4" ~ "Tools I typically use, such as search engines, now incorporate GenAI"
    ),
    temp = paste0(value, ".", computational)) |>
  srvyr::as_survey_design(weights = weight_rake) |>
  group_by(name, computational) |>
  dplyr::filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  mutate(value = recode(value,
                        `Strongly disagree` = 1,
                        `Somewhat disagree` = 2,
                        `Neither agree nor disagree` = 3,
                        `Somewhat agree` = 4,
                        `Strongly agree` = 5)) |>
  dplyr::summarize(mean = srvyr::survey_mean(value, vartype = "ci", na.rm = TRUE)) |>
  ungroup() |>
  mutate(name = factor(name, levels = c(
    "GenAI saves me time",
    "I am curious about GenAI",
    "Tools I typically use, such as search engines, now incorporate GenAI",
    "GenAI allows me to focus on aspects of research I find more meaningful",
    "GenAI saves me money",
    "GenAI enables me to do research that is otherwise impossible",
    "I feel pressure from my field of study to use GenAI",
    "I feel pressure from my institution to use GenAI",
    "I feel pressure from my collaborators to use GenAI"
  )))




png("figures/paper_fig03_04_alt.png", res = 340, units = "in", height = 9*0.8, width = 5)
print(
df_fig03_04 |>
  plotter() +
  scale_y_continuous(breaks = c(1,2,3,4,5),
                     limits = c(1,5),
                     labels = c("Strongly\ndisagree",
                                "Somewhat\ndisagree",
                                "Neither agree\nnor disagree",
                                "Somewhat\nagree",
                                "Strongly\nagree")) +
  ggtitle("Indicate the extent you agree or disagree that the following are\nreasons for incorporating GenAI into your research practices.")
)
dev.off()

## ----------------------------------------------------------------------------
#5-6



df_fig05_06 <- data_all |>
  dplyr::select(Q22_1:Q22_4, Q59_1:Q59_5, computational, weight_rake) |>
  pivot_longer(cols = starts_with(c("Q22","Q59"))) |> 
  mutate(
    name = case_match(name,
                      "Q22_1" ~ "Computational Costs",
                      "Q22_2" ~ "Economic Costs",
                      "Q22_3" ~ "Environmental Costs",
                      "Q22_4" ~ "Social Costs",
                      "Q59_1" ~ "Biases GenAI may introduce into research practices",
                      "Q59_2" ~ "Level of control large tech companies have over GenAI tools",
                      "Q59_3" ~ "Data collection practices used in training GenAI models",
                      "Q59_4" ~ "Spread of low quality content or misinformation from GenAI",
                      "Q59_5" ~ "Privacy of data transmitted or stored when using GenAI")) |>
  srvyr::as_survey_design(weights = weight_rake) |>
  group_by(name, computational) |>
  dplyr::filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  mutate(value = dplyr::recode(value,
                               `Not concerned` = 1,
                               `Slightly concerned` = 2,
                               `Concerned` = 3,
                               `Very concerned` = 4,
                               `Unsure` = NULL,
                               .default = NULL
  )) |>
  dplyr::filter(!(is.na(value))) |>
  dplyr::summarize(mean = srvyr::survey_mean(value, vartype = "ci", na.rm = TRUE)) |>
  ungroup() |>
  mutate(name = factor(name, levels = c("Spread of low quality content or misinformation from GenAI",                                     
                                        "Level of control large tech companies have over GenAI tools",
                                        "Privacy of data transmitted or stored when using GenAI",
                                        "Data collection practices used in training GenAI models",
                                        "Biases GenAI may introduce into research practices",
                                      "Social Costs","Environmental Costs",
                                      "Economic Costs","Computational Costs")))


png("figures/paper_fig05_06_alt.png", res = 340, units = "in", height = 9*0.8, width = 5.5)
print(
df_fig05_06 |>
  plotter() +
  scale_y_continuous(breaks = c(1,2,3,4),
                     limits = c(1,4),
                     labels = c("Not\nConcerned",
                                "Slightly\nConcerned",
                                "Concerned",
                                "Very\nConcerned")) +
  ggtitle("Indicate the extent you are concerned with the following.")
)
dev.off()



## ----------------------------------------------------------------------------
#7-8


df_fig07_08 <- data_all |>
  dplyr::select(Q6_4:Q6_5, Q27_1:Q27_4, computational, weight_rake) |>
  pivot_longer(cols = starts_with(c("Q6","Q27"))) |>
  mutate(
    name = case_match(name,
                "Q6_4" ~ "I believe the outputs from GenAI can generally be trusted",
                "Q6_5" ~ "I am concerned about becoming overly dependent on GenAI",
                "Q27_1" ~ "The current advantages of GenAI outweigh their drawbacks for my field",
                "Q27_2" ~ "GenAI will have a net positive effect on my field over the next 2-3 years",
                "Q27_3" ~ "Current GenAI models require significant improvements in order to benefit my field",
                "Q27_4" ~ "GenAI will continue to improve in the next 2-3 years")) |>
  srvyr::as_survey_design(weights = weight_rake) |>
  group_by(name, computational) |>
  dplyr::filter(!(is.na(name) | is.na(value) | is.na(computational))) |>
  mutate(value = recode(value,
                        `Strongly disagree` = 1,
                        `Somewhat disagree` = 2,
                        `Neither agree nor disagree` = 3,
                        `Somewhat agree` = 4,
                        `Strongly agree` = 5)) |>
  dplyr::summarize(mean = srvyr::survey_mean(value, vartype = "ci", na.rm = TRUE)) |>
  ungroup() |> 
  mutate(name = factor(name, levels = c("GenAI will continue to improve in the next 2-3 years",
                                        "Current GenAI models require significant improvements in order to benefit my field",
                                        "I am concerned about becoming overly dependent on GenAI",
                                        "The current advantages of GenAI outweigh their drawbacks for my field",
                                        "GenAI will have a net positive effect on my field over the next 2-3 years",
                                        "I believe the outputs from GenAI can generally be trusted"
                                        )))

png("figures/paper_fig07_08_alt.png", res = 340, units = "in", height = 6*0.8, width = 5.5)
print(
df_fig07_08 |>
  plotter() +
  scale_y_continuous(breaks = c(1,2,3,4,5),
                     limits = c(1,5),
                     labels = c("Strongly\ndisagree",
                                "Somewhat\ndisagree",
                                "Neither agree\nnor disagree",
                                "Somewhat\nagree",
                                "Strongly\nagree")) +
  ggtitle("Indicate the extent you agree or disagree with the following statements.")
)
dev.off()

### END ###


