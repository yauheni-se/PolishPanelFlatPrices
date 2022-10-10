# 1. PRESETS: ----
library(tidyverse)
library(readxl)
library(foreign)
library(plm)
library(tseries)
library(lmtest)
library(DT)
library(plotly)
library(Metrics)
options(scipen = 999)
rm(list = ls())

show_beatiful_table <- function(data) {
  data %>% 
    datatable(
      rownames = FALSE,
      extensions = c("Buttons", "Select"),
      options = list(select = TRUE, dom = "Bfrtip", buttons = list("copy"))
  )
}

# 2. DATA PREPARATION: ----
data_raw_ext <- read_xlsx("data_raw.xlsx", sheet = 1)
data_raw_infl <- read_xlsx("data_raw.xlsx", sheet = 2)
data_raw_gdp <- read_xlsx("data_raw.xlsx", sheet = 3)

data_raw_ext %>% glimpse()
data_raw_infl %>% glimpse()
data_raw_gdp %>% glimpse()


data_raw_nested <- bind_rows(data_raw_ext, data_raw_infl, data_raw_gdp) %>% 
  rename(city = Miasto, variable = `Zmienna zakodowana`) %>%
  select(-c(`2021`, Kategoria, Jednostka, Zmienna)) %>% 
  group_by(city) %>% 
  nest() %>% 
  mutate(data = map(data, function(x) {
    x %>%
      pivot_longer(cols = -variable, names_to = "year", values_to = "value") %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      select(-c(marriages, divorses, newly_registered_entities, newly_deregistered_entities,
                commune_income_pc, commune_expenditure_pc)) %>% 
      mutate(across(any_of(c("co_emission", "night_stays", "cars", "shops", "marriages_increase",
                             "students", "newly_entities_increase")), function(x){x/population*1000}))
  }))
data_clean <- data_raw_nested %>% unnest(cols = c(data)) %>% ungroup()
data.table::fwrite(data_clean, "data_clean.csv")

data_prepared <- data_clean %>%
  select(-c(
    cars, shops, co_emission, commune_profit_pc, newly_entities_increase, population
  ))


# 3. VISUALIZATION: ----
data_for_plots_2 <- data_prepared %>% 
  select(-year) %>% 
  group_by(city) %>% 
  summarise_all(max) %>%
  mutate_if(is.numeric, round, 2) %>% 
  mutate(id = row_number()) %>% 
  select(-city)
plot_lst_2 <- list()
for (i in 1:(ncol(data_for_plots_2)-1)) {
  plot_lst_2[[i]] <- ggplot(data_for_plots_2, aes_string(x = "id", y = data_for_plots_2[, i] %>% colnames())) +
    geom_line() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(x = data_for_plots_2[, i] %>% colnames(), y = "")
}
plot_lst_2[[2]]
ggpubr::ggarrange(plotlist = plot_lst_2, nrows = 4)


data_for_plots <- data_prepared %>% 
  mutate(year = as.numeric(year))

plot_lst <- list()
for (i in 3:ncol(data_for_plots)) {
  plot_lst[[i-2]] <- ggplot(data_for_plots, aes_string(x = "year", y = data_prepared[, i] %>% colnames())) +
    geom_line() +
    facet_wrap(.~city)+ 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
}
plot_lst[[10]]


# 4. MODEL SELECTION: ----
# OLS:
right_formula <- data_prepared %>% colnames() %>% .[!. %in% c("city", "year", "house_price_per_m2")] %>% paste0(collapse = " + ")
left_formula <- "house_price_per_m2 ~ " 
full_formula <- paste0(left_formula, right_formula)

model_ols <- lm(as.formula(full_formula), data = data_prepared)
summary(model_ols)
summary(model_ols)[4]$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>% 
  show_beatiful_table()

# LSDV (OLS WITH CITY AS BINARY):
right_formula_lsdv <- paste0(right_formula, "+ factor(city) - 1")
full_formula_lsdv <- paste0(left_formula, right_formula_lsdv)
model_lsdv <- lm(as.formula(full_formula_lsdv), data_prepared)
summary(model_lsdv)


# FIXED EFFECTS:
model_fixed_effects <- plm(as.formula(full_formula), data = data_prepared,
                           index = c("city", "year"), model = "within")
summary(model_fixed_effects)

summary(model_fixed_effects)[1]$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>% 
  show_beatiful_table()


fixef(model_fixed_effects) %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>% 
  show_beatiful_table()
pFtest(model_fixed_effects, model_ols)


# RANDOM EFFECTS:
model_random_effects <- plm(as.formula(full_formula), data = data_prepared,
                            index = c("city", "year"), model = "random")
summary(model_random_effects)[1]$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>% 
  show_beatiful_table()


# POOLED:
model_pool <- plm(as.formula(full_formula), data = data_prepared,
                  index = c("city", "year"), model = "pooling")
summary(model_pool)


# 5. TESTS: ----
# FIXED OR RANDOM:
phtest(model_fixed_effects, model_random_effects)
plmtest(model_fixed_effects, c("time"), type = ("bp"))
pbgtest(model_fixed_effects, order = 3)

forecast::ggAcf(data_prepared %>% filter(city == "Warszawa") %>% .$house_price_per_m2) + labs(title = "")


# RANDOM OR POOLED:
plmtest(model_pool, type=c("bp"))

data_panel <- plm.data(data_prepared, index = c("city", "year"))
adf.test(data_panel$house_price_per_m2, k = 2)  # stat

# HC:
bptest(as.formula(full_formula), data = data_prepared, studentize = FALSE) # => use robust covariance matrix

summary(model_random_effects)


coeftest(model_random_effects, vcovHC(model_random_effects, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(model_random_effects, type = x)))))


# 6. FINAL MODEL: ----
significant_vars_tbl <- coeftest(model_random_effects, vcovHC(model_random_effects, type = "HC1"))
significant_vars_names <- significant_vars_tbl[, "Pr(>|t|)"] %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>% 
  `colnames<-`(c("var", "p_value")) %>%
  mutate(keep = ifelse(p_value <= 0.05, 1, 0)) %>% 
  filter(keep == 1, var != "(Intercept)") %>% 
  pull(var)

data_final <- data_prepared %>%
  select(-c(green_area, bike_roads, marriages_increase, balance_of_migration, pkb_pc))

right_formula <- data_final %>% colnames() %>% .[!. %in% c("city", "year", "house_price_per_m2")] %>% paste0(collapse = " + ")
left_formula <- "house_price_per_m2 ~ " 
full_formula <- paste0(left_formula, right_formula)
model_final <- plm(as.formula(full_formula), data = data_final, index = c("city", "year"), model = "random")
summary(model_final)

summary(model_final)[1]$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>% 
  show_beatiful_table()


# 7. PREDICTIONS: ----
data_raw_new <- read_xlsx("data_raw.xlsx", sheet = 4)
data_raw_infl_new <- read_xlsx("data_raw.xlsx", sheet = 2) %>% select(Miasto, `Zmienna zakodowana`, `2021`)

data_new <- bind_rows(data_raw_new, data_raw_infl_new) %>% 
  rename(city = Miasto, variable = `Zmienna zakodowana`) %>%
  group_by(city) %>% 
  nest() %>% 
  mutate(data = map(data, function(x) {
    x %>%
      pivot_longer(cols = -variable, names_to = "year", values_to = "value") %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      mutate(across(any_of(c("night_stays", "students")), function(x){x/population*1000}))})) %>%
  unnest(cols = c(data)) %>%
  ungroup()

data_full <- data_final %>% 
  rbind(data_new %>% select(-population)) %>% 
  arrange(city, year)
model_full <- plm(as.formula(full_formula), data = data_full, index = c("city", "year"), model = "random")
summary(model_full)


y_fitted_all <- as.numeric(model_full$model[[1]] - model_full$residuals)

y_fitted <- c()
for (i in seq(12, length(y_fitted_all), 12)) {
  y_fitted[i/12] <- y_fitted_all[i]
}

y_real_all <- data_full %>% arrange(city) %>% .$house_price_per_m2
y_real <- data_new %>% arrange(city) %>% .$house_price_per_m2

# all years:
comparisons_all <- tibble(
  city = data_full %>% arrange(city) %>% pull(city),
  actual = y_real_all,
  predicted = y_fitted_all) %>% 
  group_by(city) %>% 
  summarise(rmse = rmse(actual, predicted),
            mape = mape(actual, predicted)) %>% 
  mutate(mape = mape*100) %>% 
  mutate_if(is.numeric, round, 2)
comparisons_all %>% show_beatiful_table()

# last year:
comparisons <- tibble(
  city = data_full %>% arrange(city) %>% pull(city) %>% unique(),
  actual = y_real,
  predicted = y_fitted) %>% 
  group_by(city) %>% 
  mutate(rmse = rmse(actual, predicted),
         mape = mape(actual, predicted)) %>% 
  mutate(mape = mape*100) %>% 
  mutate_if(is.numeric, round, 2)
comparisons %>% show_beatiful_table()