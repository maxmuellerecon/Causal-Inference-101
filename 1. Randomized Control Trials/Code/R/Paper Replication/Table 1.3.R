#Load necessary libraries.
library("tidyverse")
library("broom")
library("haven")
library("rlang")
library("clubSandwich")

#Function to calculate clustered standard errors and return a tidy data frame of the coefficients and standard errors.
cluster_se <- function(mod, cluster, type = "CR2") {
  vcov <- vcovCR(mod, cluster = cluster, type = type)
  coef_test(mod, vcov = vcov) %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>%
    select(term, estimate = beta, std.error = SE)
}

#Table 1.3

#Load the rand data.
data("rand_sample", package = "masteringmetrics")

#Rename the dataset
rand_sample <- rand_sample

#Calculate the number in each plan:
plantypes <- count(rand_sample, plantype)

#Give out a Table (HTML) to overview Plantypes:
knitr::kable(plantypes)

#Generate variable specification list:
varlist <- c("female", "blackhisp", "age", "educper",
             "income1cpi", "hosp", "ghindx", "cholest", "diastol",
             "systol", "mhi", "ghindxx",
             "cholestx", "diastolx", "systolx", "mhix")

#Create column (1) with the mean and standard deviation of the "Catastrophic" plan:
catastrophic_stats <- rand_sample %>%
  filter(plantype == "Catastrophic") %>%
  select(one_of(varlist)) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            `Std. Dev.` = sd(value, na.rm = TRUE))

#Display the Table with the Cathastrophic plan:
knitr::kable(catastrophic_stats, digits = 3)

#Calculate the difference in means between plans and the catastophic plan.
calc_diffs <- function(x) {
  # programmatically create the formula for lm
  f <- quo(!!sym(x) ~ plantype)    #quo(expresssion)--> Sym is responsible for take strings as input and turn them into symbols
  mod <- lm(f, data = rand_sample)  # lm = linear model formula
  out <- cluster_se(mod, cluster = rand_sample[["fam_identifier"]]) #Definition of the output
  out[["response"]] <- x 
  out   #View/Creation of the output
}

plantype_diffs <- map_dfr(varlist, calc_diffs) %>% #Map of the new data frame with variables and function
  select(response, term, estimate, std.error) %>%  #term=plantype
  mutate(term = str_replace(term, "^plantype", "")) #str_replace -> term(input vector),pattern to look for (plantype), replacement ("")

#Create a table similar to Angrist and Pischke (2014) Table 1.3.
fmt_num <- function(x) {
  prettyNum(x, digits = 3, format = "f", big.mark = ",", drop0trailing = FALSE) #numbers to appear all nicely formatted
}

plantype_diffs %>%
  mutate(estimate = str_c(fmt_num(estimate), " (", fmt_num(std.error), ")")) %>% #Join multiple strings into a single string (vector)
  select(-std.error) %>%
  spread(term, estimate) %>%
  knitr::kable(digits = 3)

#Addition: Plot the difference-in-means of each plantype vs. catastrophic insurance.
ggplot(filter(plantype_diffs, term != "(Intercept)"),
       aes(x = term, y = estimate,
           ymin = estimate - 2 * std.error,
           ymax = estimate + 2 * std.error)) +
  geom_hline(yintercept = 0, colour = "white", size = 1) +
  geom_pointrange() +
  facet_grid(response ~ ., scales = "free_y")