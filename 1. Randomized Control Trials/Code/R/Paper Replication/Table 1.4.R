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

#Table 1.4

#Load RAND Data
data("rand_person_spend", package = "masteringmetrics")

#Adjust year variable from annual expenditures data to correct calendar year in order to adjust for inflation.
rand_person_spend <- mutate(rand_person_spend,
                            expyear = indv_start_year + year - 1)

#Adjust spending for inflation: Add Inflation Rates
cpi <- tribble(  #tribble -> row by row layout
  ~ year, ~ cpi, #~ -> header
  1973, 3.07,
  1974, 2.76,
  1975, 2.53,
  1976, 2.39,
  1977, 2.24,
  1978, 2.09,
  1979, 1.88,
  1980, 1.65,
  1981, 1.5,
  1982, 1.41,
  1983, 1.37,
  1984, 1.31,
  1985, 1.27
)

#Adjust spending for inflation: Mutate Inflation rates with spending
rand_person_spend <- left_join(rand_person_spend, x =         #left_join= Join two tbls together
                               cpi, by = c("expyear" = "year")) %>%
  mutate(out_inf = outsum * cpi,
         inpdol_inf = inpdol * cpi)   #Adjust for Inflation

#Add a total spending variable.
rand_person_spend <- mutate(rand_person_spend,
                            tot_inf = inpdol_inf + out_inf)

#Add a variable for any health insurance (free, Individual deductible, or cost-sharing):
  rand_person_spend <- mutate(rand_person_spend,
                              any_ins = plantype != "Catastrophic") #any insurance, just not cathastrophic
  
#Count the number of observations in each plan-type,
count(rand_person_spend, plantype)

#and any-insurance,
count(rand_person_spend, any_ins)

#Create a list of response variables.
varlist <- c("ftf", "out_inf", "totadm", "inpdol_inf", "tot_inf")

#Calculate the mean and standard deviation for those receiving catastrophic insurance.
rand_person_spend %>%
  filter(plantype == "Catastrophic") %>%
  select(one_of(varlist)) %>%
  gather(response, value) %>%
  group_by(response) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            `Std. Dev.` = sd(value, na.rm = TRUE))

#Calculate the difference in means between plans and the catastophic plan.
calc_diffs <- function(x) {
  # programmatically create the formula
  f <- quo(!!sym(x) ~ plantype)
  mod <- lm(f, data = rand_person_spend)  # nolint
  out <- cluster_se(mod, cluster = rand_person_spend[["fam_identifier"]])
  out[["response"]] <- x
  out
}

person_diffs <- map_dfr(varlist, calc_diffs) %>%
  select(response, term, estimate, std.error) %>%
  mutate(term = str_replace(term, "^plantype", ""))

#Standard errors are clustered by family identifier using the clubSandwich package.
#Print the table. 

fmt_num <- function(x) {
  prettyNum(x, digits = 3, format = "f", big.mark = ",", drop0trailing = FALSE)
}

person_diffs %>%
  mutate(estimate = str_c(fmt_num(estimate), " (", fmt_num(std.error), ")")) %>%
  select(-std.error) %>%
  spread(term, estimate) %>%
  knitr::kable(digits = 3)

#Addition: We could plot the difference-in-means of each plan type vs. catastrophic insurance.
ggplot(filter(person_diffs, term != "(Intercept)"),
       aes(x = term, y = estimate,
           ymin = estimate - 2 * std.error,
           ymax = estimate + 2 * std.error)) +
  geom_hline(yintercept = 0, colour = "white", size = 1) +
  geom_pointrange() +
  facet_grid(response ~ ., scales = "free_y")