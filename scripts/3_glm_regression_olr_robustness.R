##############################
##  
##  Author: Marshall A. Taylor and Dustin S. Stoltz
##  Notes: Code is for regressions and predicted plots
##############################


### BEGIN ###

##############################
## Packages and data
##############################


pacman::p_load(tidyverse, scales, ggpubr, srvyr, survey, viridis, ggeffects, install = TRUE)


# Import prepared data
data_all <- read.csv("../data/20251014_combined_survey_responses.csv")

# Prep the data
data_lm <- data_all |>
  mutate(gender = ifelse(gender, "Cis-Man", "Not Cis-Man")) |>
  mutate_at(vars(starts_with(c("Q6", "Q27"))), 
        list(~case_match(.,
                      "Strongly agree" ~ 5,
                      "Somewhat agree" ~ 4,
                      "Neither agree nor disagree" ~ 3,
                      "Somewhat disagree" ~ 2,
                      "Strongly disagree" ~ 1
        ))) |>
  mutate_at(vars(starts_with(c("Q22","Q59"))), 
        list(~case_match(.,
                      "Very concerned" ~ 4,
                      "Concerned" ~ 3,
                      "Slightly concerned" ~ 2,
                      "Not concerned" ~ 1,
                      "Unsure" ~ NA
        ))) |>
  mutate(use = case_match(use, "Daily." ~ 1, "At least weekly." ~ 1, .default = 0)) |>
  mutate_at(vars(starts_with(c("Q18","Q19","Q20","Q21"))), 
        list(~case_match(.,
                      "Always" ~ 3,
                      "Most of the time" ~ 2,
                      "Sometimes" ~ 1,
                      "Never" ~ 0
        )))

## ----------------------------------------------------------------------------
cat("\nAssessing aggregate measure of 'familiarity'\n")
cat("# ----------------------------------------- #\n")

# Assess reliability for familiarity items
alph <- psych::alpha(data_lm[,c("Q6_1","Q6_2","Q6_3")], na.rm = TRUE) # Raw Alpha: 0.87
cat("Raw Alpha: ", unlist(alph)$total.raw_alpha, "\n")

# Assess dimensionality of familiarity items
eig <- FactoMineR::PCA(drop_na(data_lm[,c("Q6_1","Q6_2","Q6_3")]), graph= FALSE)$eig # eig_1 = 2.37
cat("1st Eigenvalue: ", eig[1,1], "\n")

## define aggregated familiarity
data_lm <- data_lm |> rowwise() |>
  mutate(familiar = sum(c_across(Q6_1:Q6_3))) |>
  ungroup()



##############################
## Regressions
##############################


# Create survey design object
design <- survey::svydesign(id =~ 1, weights =~ weight_rake, data = data_lm)

## ----------------------------------------------------------------------------
# Trust model
## ----------------------------------------------------------------------------

model1 <- survey::svyglm(Q6_4 ~ familiar*use + gender, data = data_lm, design = design)
  
  cat("\nModel 1: Trust\n")
  cat("# ----------------------------------------- #\n")
  print(broom::tidy(model1))
  cat("N: ", length(model1$residuals), "\n")
  cat("Pseudo R2:", attr(jtools::summ(model1), "rsq"), "\n")
  cat("AIC: ", AIC(model1)["AIC"], "\n")
  cat("Mod Dev:", model1$deviance, "\n")
  cat("Null Dev:", model1$null.deviance, "\n")

## ----------------------------------------------------------------------------
# Future improvement model
## ----------------------------------------------------------------------------

model2 <- survey::svyglm(Q27_4 ~ familiar*use + gender, data = data_lm, design = design)

  cat("\nModel 2: Future Improvement\n")
  cat("# ----------------------------------------- #\n")
  print(broom::tidy(model2))
  cat("N: ", length(model2$residuals), "\n")
  cat("Pseudo R2:", attr(jtools::summ(model2), "rsq"), "\n")
  cat("AIC: ", AIC(model2)["AIC"], "\n")
  cat("Mod Dev:", model2$deviance, "\n")
  cat("Null Dev:", model2$null.deviance, "\n")

## ----------------------------------------------------------------------------
# Net positive model
## ----------------------------------------------------------------------------

model3 <- survey::svyglm(Q27_2 ~ familiar*use + gender, data = data_lm, design = design)

  cat("\nModel 3: Net Positive\n")
  cat("# ----------------------------------------- #\n")
  print(broom::tidy(model3))
  cat("N: ", length(model3$residuals), "\n")
  cat("Pseudo R2:", attr(jtools::summ(model3), "rsq"), "\n")
  cat("AIC: ", AIC(model3)["AIC"], "\n")
  cat("Mod Dev:", model3$deviance, "\n")
  cat("Null Dev:", model3$null.deviance, "\n")

## ----------------------------------------------------------------------------
# Adjusted predictions
## ----------------------------------------------------------------------------

# Main
model1_pred <- ggeffects::predict_response(model1, terms = c("familiar", "use")) |>
  as.data.frame() |> mutate(model = 1)
model2_pred <- ggeffects::predict_response(model2, terms = c("familiar", "use")) |>
  as.data.frame() |>  mutate(model = 2)
model3_pred <- ggeffects::predict_response(model3, terms = c("familiar", "use")) |>
  as.data.frame() |> mutate(model = 3)

labs <- c('"I believe the outputs from\nGenAI can generally be trusted."',
          '"GenAI will continue to improve\nin the next 2-3 years."',
          '"GenAI will have a net positive effect\non my field over the next 2-3 years."')
names(labs) <- c(1,2,3)


## ---------------------------------------------------------------------------
# Main Predicted Outcomes Fig
## ----------------------------------------------------------------------------

png("figures/paper_fig09_pred.png", res = 340, units = "in", height = 4.5, width = 9.5)
print(
  rbind(model1_pred, model2_pred, model3_pred) |>
    ggplot(aes(x = x, y = predicted, fill = factor(group), color = factor(group))) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
    labs(x = "Familiarity with GenAI", y = NULL, fill = "",  color = "",
         title = "Predicted response to:") +
    scale_fill_viridis_d(breaks = c(0,1),
                         labels = c("Never or rarely uses GenAI",
                                    "Uses GenAI at least weekly")) +
    scale_color_viridis_d(breaks = c(0,1),
                          labels = c("Never or rarely uses GenAI",
                                     "Uses GenAI at least weekly")) +
    scale_x_continuous(breaks = c(3,15),
                       limits = c(2, 16),
                       labels = c("Less\nfamiliar",
                                  "More\nfamiliar")) +
    scale_y_continuous(breaks = c(1,2,3,4,5),
                       limits = c(.5,5.5),
                       labels = c("Strongly disagree",
                                  "Somewhat disagree",
                                  "Neither agree nor disagree",
                                  "Somewhat agree",
                                  "Strongly agree")) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          title = element_text(face = "bold"),
          legend.position = "bottom",
          strip.background = element_rect(color = "#cccccc", fill = "#d9d9d9"),
          panel.border = element_blank()) +
    facet_wrap(~model, labeller = labeller(model = labs))
)
dev.off()


## ---------------------------------------------------------------------------
# Robustness Check
## ---------------------------------------------------------------------------

## ----------------------------------------------------------------------------
# Ordered logits: robustness check

design_olr <- update(design, 
                  Q6_4_f = as.ordered(Q6_4),
                  Q27_4_f = as.ordered(Q27_4),
                  Q27_2_f = as.ordered(Q27_2))


## ----------------------------------------------------------------------------
# Trust model
## ----------------------------------------------------------------------------

model1_olr <- survey::svyolr(Q6_4_f ~ familiar*use + gender, design = design_olr)

    broom::tidy(model1_olr, conf.int= TRUE)

    model1_olr_null <- survey::svyolr(Q6_4_f ~ 1, design = design_olr)

    model1_olr_null$deviance - model1_olr$deviance #chi-square for lr test
    pchisq((model1_olr_null$deviance - model1_olr$deviance), #lr test
          (model1_olr$edf - model1_olr_null$edf),
          lower.tail= FALSE)

## ----------------------------------------------------------------------------
# Future improvement model
## ----------------------------------------------------------------------------

model2_olr <- survey::svyolr(Q27_4_f ~ familiar*use + gender, design = design_olr)

    broom::tidy(model2_olr, conf.int= TRUE)

    model2_olr_null <- survey::svyolr(Q27_4_f ~ 1, design = design_olr)

    model2_olr_null$deviance - model2_olr$deviance #chi-square for lr test
    pchisq((model2_olr_null$deviance - model2_olr$deviance), #lr test
          (model2_olr$edf - model2_olr_null$edf),
          lower.tail= FALSE)

## ----------------------------------------------------------------------------
# Net positive model
## ----------------------------------------------------------------------------

model3_olr <- survey::svyolr(Q27_2_f ~ familiar*use + gender, design = design_olr)

    broom::tidy(model3_olr, conf.int= TRUE)

    model3_olr_null <- survey::svyolr(Q27_2_f ~ 1, design = design_olr)

    model3_olr_null$deviance - model3_olr$deviance #chi-square for lr test
    pchisq((model3_olr_null$deviance - model3_olr$deviance), #lr test
          (model3_olr$edf - model3_olr_null$edf),
          lower.tail= FALSE)


## ----------------------------------------------------------------------------

# ologit robustness check
model_pred_data <- model1_pred |>
  select(x, group, model) |>
  rename("familiar" = x, "use" = group) |>
  mutate(gender = "Cis-Man")

model1_pred_olr <- model_pred_data |>
  mutate(use = ifelse(use == "0", 0, 1)) %>%
  cbind(predict(model1_olr, newdata = ., type = "probs")) |>
  pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`))

model2_pred_olr <- model_pred_data |>
  mutate(use = ifelse(use == "0", 0, 1),
         model = 2) %>%
  cbind(predict(model2_olr, newdata = ., type = "probs")) |>
  pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`))

model3_pred_olr <- model_pred_data |>
  mutate(use = ifelse(use == "0", 0, 1),
         model = 3) %>%
  cbind(predict(model3_olr, newdata = ., type = "probs")) |>
  pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`))

labs2 <- c("Strongly\nDisagree",
           "Somewhat\nDisagree",
           "Neither Agree\nnor Disagree",
           "Somewhat\nAgree",
           "Strongly\nAgree")
names(labs2) <- c(1,2,3,4,5)


## ---------------------------------------------------------------------------

png("figures/paper_figapp.png", res = 340, units = "in", height = 9.5, width = 9.5)
print(
  rbind(model1_pred_olr, model2_pred_olr, model3_pred_olr) |>
    mutate(name = factor(name, c("5","4","3","2","1"))) |>
    ggplot(aes(x = familiar, y = value, group = factor(use), color = factor(use))) +
    geom_line(linewidth = 2) +
    labs(y = "Predicted Probability",
         x = "Familiarity with GenAI",
         color = "",
         title = "Predicted response to:") +
    scale_color_viridis_d(breaks = c(0,1),
                          labels = c("Never or rarely uses GenAI",
                                     "Uses GenAI at least weekly")) +
    scale_x_continuous(breaks = c(3,15),
                       limits = c(2, 16),
                       labels = c("Less\nfamiliar",
                                  "More\nfamiliar")) +
    scale_y_continuous(limits = c(0,1),
                       breaks = c(0,.5,1)) +
    guides(color = guide_legend(override.aes = list(size = 20,
                                                    linewidth = 10))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          title = element_text(face = "bold", size = 16),
          # panel.border = element_blank(),
          strip.background = element_rect(color = "#cccccc", fill = "#d9d9d9"),
          legend.position = "bottom") +
    facet_grid(name~model,
               labeller = labeller(model = labs,
                                   name = labs2))
) 
dev.off()

