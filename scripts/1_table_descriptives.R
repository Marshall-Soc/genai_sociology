#############################
##  Descriptive Stats
##  Author: Dustin Stoltz
##  Notes: Code is for descriptive table in Appendix
##############################


##############################
## Packages and data
##############################

pacman::p_load(tidyverse, install = TRUE)

# Import prepared data
data_all <- read.csv("../data/20251014_combined_survey_responses.csv")


## ----------------------------------------------------------------------------
# Descriptives function
## ----------------------------------------------------------------------------

dscr <- function(data, var) {
    out <- data |> 
    dplyr::count(type, weight_rake, !!as.symbol(var) ) |> 
    dplyr::mutate(n_weight_rake = n * weight_rake) |> 
    dplyr::group_by(!!as.symbol(var)) |>
    dplyr::summarize(
        n_weight_rake = sum(n_weight_rake),
        n = sum(n)
        ) |>
    dplyr::mutate(
        prop = n / sum(n),
        prop_weight_rake = n_weight_rake/sum(n_weight_rake), 
        ) |> 
    dplyr::select(!!as.symbol(var), n, prop, prop_weight_rake)

    return(out)
}

## ----------------------------------------------------------------------------
#Q46	Do you consider yourself a sociologist?
sociologist <- dscr(data = data_all, var = "sociologist")

## ----------------------------------------------------------------------------
# Q33	Do you use computational methods in your research?
computational <- dscr(data = data_all, var = "computational")

## ----------------------------------------------------------------------------
# type  What sample did this person come from?
sample <- dscr(data = data_all, var = "type")

## ----------------------------------------------------------------------------
# Q43	What best describes the methods you typically use?
methods <- dscr(data = data_all, var = "quant")

# ----------------------------------------------------------------------------
# Q49	How would you rate your overall English language proficiency?
english <- dscr(data = data_all, var = "english")

## ----------------------------------------------------------------------------
#Q36	Which of these best describes your gender identity? - Selected Choice
gender <- dscr(data = data_all, var = "gender")

## ----------------------------------------------------------------------------
#white	Which of these best describes your racial/ethnic identity?
race <- dscr(data = data_all, var = "white")

## ----------------------------------------------------------------------------
#Q25	List of Countries
country <- dscr(data = data_all, var = "location") |> arrange(desc(n))

## ----------------------------------------------------------------------------
# Q26	Which best describes your current position?
tenure <- dscr(data = data_all, var = "tenured")


## ----------------------------------------------------------------------------
# Create Table
## ----------------------------------------------------------------------------
tab <- bind_rows(
list(
    `Gender (% Cis-Men)` = select(filter(gender, gender), -1),
    `Location (% US-Based)` = select(filter(country,  location), -1),
    `Computational` = select(filter(computational, computational), -1),
    `Sociologist` = select(filter(sociologist, sociologist), -1),
    `Quantitative` = select(filter(methods, quant), -1),
    `Language (% English)` = select(filter(english, english), -1),
    `Race/Ethnicity (% White)` = select(filter(race, white), -1),
    `Position (% Tenured Prof.)`= select(filter(tenure, tenured), -1)
), .id = "var"
) |> 
dplyr::select(var, prop, prop_weight_rake) |> 
dplyr::mutate_if(is.numeric, scales::label_percent())

# print table
## markdown
print(
knitr::kable(tab, caption = "Raw Proportions and Rake-Weighted Proportions on Select Variables")
)
## LaTeX
# kableExtra::kbl(tab, format = "latex", booktabs = TRUE, 
#                 caption = "Raw Proportions and Rake-Weighted Proportions on Select Variables")

