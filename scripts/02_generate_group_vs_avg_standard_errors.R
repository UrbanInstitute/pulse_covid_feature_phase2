# Generate point estimates and SE for metrics of interest
# Also conduct significance tests bw subgroups and
# relevant natl/state/metro averages
library(tidyverse)
library(here)
library(srvyr)
library(survey)
library(fastDummies)

metrics <- c(
  "uninsured",
  "insured_public",
  "inc_loss",
  "expect_inc_loss",
  "rent_not_conf",
  "mortgage_not_conf",
  "food_insufficient",
  "depression_anxiety_signs",
  "spend_credit", 
  "spend_ui", 
  "spend_stimulus", 
  "spend_savings",
  "spend_snap",
  "rent_caughtup",
  "mortgage_caughtup",
  "eviction_risk",
  "foreclosure_risk",
  "telework",
  "mentalhealth_unmet",
  "learning_fewer",
  "expense_dif"
)

other_cols <- c(
  "cbsa_title",
  "state",
  "hisp_rrace",
  "week_num"
)

all_cols <- c(metrics, other_cols)

##  Read in and clean data
puf_all_weeks <- read_csv(here("data/intermediate-data", "pulse_puf2_all_weeks.csv")) %>%
  mutate(spend_credit = as.numeric(spend_credit),
         spend_savings = as.numeric(spend_savings),
         spend_stimulus = as.numeric(spend_stimulus),
         spend_ui = as.numeric(spend_ui))
                          
# Set parameters
CUR_WEEK <- puf_all_weeks %>%
  pull(week_x) %>%
  max()


puf_all_weeks2 <- puf_all_weeks %>%
  # For the uninsured variable, we filter out people over 65 from the denominator
  mutate(
    insured_public = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ insured_public
    ),
    uninsured = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ uninsured
    )
  ) %>%
  select(starts_with("pweight"), all_cols) %>%
  # Create one hot encoding of cbsas and states for easy use with survey pkg
  fastDummies::dummy_cols("cbsa_title") %>%
  fastDummies::dummy_cols("state") %>%
  janitor::clean_names() %>%
  # Add race indicator variables for easy use with survey package
  mutate(
    black = case_when(
      str_detect(hisp_rrace, "Black alone") ~ 1,
      TRUE ~ 0
    ),
    white = case_when(
      str_detect(hisp_rrace, "White") ~ 1,
      TRUE ~ 0
    ),
    hispanic = case_when(
      str_detect(hisp_rrace, "Latino") ~ 1,
      TRUE ~ 0
    ),
    asian = case_when(
      str_detect(hisp_rrace, "Asian") ~ 1,
      TRUE ~ 0
    ),
    other = case_when(
      str_detect(hisp_rrace, "Two or") ~ 1,
      TRUE ~ 0
    )
  )

# Set BRR survey design and specify replicate weights for single week numbers
svy_all <- puf_all_weeks2 %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )


get_se_diff <- function(..., svy = svy_all) {
  # Function to calculate all means/SEs and mean/SEs of the difference between
  # racial group mean and all other racial group mean for a given geography/race/
  # metric/week combinations (except US, which is handled separately)
  # INPUT:
  #    ...: Must be a dataframe with the following columns:
  #     metric, race_indicator(dummy race var), geography, week.
  #    svy: must be an object of the class tbl_svy returned by as_survey_rep()
  # OUTPUT:
  #    result: tibble containing mean mean/SE for the given geography/race
  #    metric/week combination, plus mean/SE for all other races and mean/SE for
  #    the difference between the given race and all other races
  dots <- list(...)

  metric_formula <- as.formula(paste0("~", dots$metric))
  race_formula <- as.formula(paste0("~", dots$race_indicator))

  if (dots$race_indicator == "total") {
    # if race indicator is total, then we compute mean of the geography,
    # and compare it to the mean of national estimates
    geo_formula <- as.formula(paste0("~", dots$geo_col))
    result <- tryCatch(
      {
        x <- svyby(metric_formula, geo_formula, svy %>%
          srvyr::filter(week_num == dots$week),
          svymean,
          na.rm = T,
          return.replicates = T,
          covmat = T
          )

      mean <- x %>%
        filter(!!sym(dots$geo_col) == 1) %>%
        pull(!!sym(dots$metric))
      se <- x %>%
        filter(!!sym(dots$geo_col) == 1) %>%
        pull(se) * 2
      other_mean <- x %>%
        filter(!!sym(dots$geo_col) == 0) %>%
        pull(!!sym(dots$metric))
      other_se <- x %>%
        filter(!!sym(dots$geo_col) == 0) %>%
        pull(se)
  
      # Use svycontrast to calculate se bw geography and national estimates
      contrast <- svycontrast(x, contrasts = list(diff = c(1, -1)))
      diff_mean <- contrast %>% as.numeric()
      diff_se <- contrast %>%
        attr("var") %>%
        sqrt() %>%
        {
          . * 2
        }  
  
      result <- tibble(
        mean = mean,
        se = se,
        other_mean = other_mean,
        other_se = other_se,
        diff_mean = diff_mean,
        diff_se = diff_se
      ) },
      error = function(err) {
        data <- tibble(
          mean = NA,
          se = 0,
          other_mean = NA,
          other_se = 0,
          diff_mean = 0,
          diff_se = 0
        )
        return(data)
      }
    )
  # if race_indicator isn't total, then compare subgroup to geography avg
   } else {
    # identify whether geography is state or cbsa
    geo_col_name <- ifelse(grepl("cbsa", dots$geo_col, fixed = TRUE), "cbsa_title", "state")

    # Use trycatch bc there are 4 metric-week-geogarphy combinations
    # where there are 0 respondents which return NA and error out
    result <- tryCatch(
      {
        # Use svyby to compute mean (and replicate means) for race and non race var population
        # (ie black and nonblack population)
        x <- svyby(metric_formula, race_formula, svy %>%
          srvyr::filter(!!sym(geo_col_name) == dots$geography) %>%
          srvyr::filter(week_num == dots$week),
        svymean,
        na.rm = T,
        return.replicates = T,
        covmat = T
        )

        mean <- x %>%
          filter(!!sym(dots$race_indicator) == 1) %>%
          pull(!!sym(dots$metric))
        se <- x %>%
          filter(!!sym(dots$race_indicator) == 1) %>%
          pull(se) * 2
        other_mean <- x %>%
          filter(!!sym(dots$race_indicator) == 0) %>%
          pull(!!sym(dots$metric))
        other_se <- x %>%
          filter(!!sym(dots$race_indicator) == 0) %>%
          pull(se)

        # Use svycontrast to calulate se bw race and nonrace (ie black and non black) population
        contrast <- svycontrast(x, contrasts = list(diff = c(1, -1)))
        diff_mean <- contrast %>% as.numeric()
        diff_se <- contrast %>%
          attr("var") %>%
          sqrt() %>%
          {
            . * 2
          }


        result <- tibble(
          mean = mean,
          se = se,
          other_mean = other_mean,
          other_se = other_se,
          diff_mean = diff_mean,
          diff_se = diff_se
        )
      },
      error = function(err) {
        # handle case where all NA responses for a given metric/geo/week/race
        data <- tibble(
          mean = NA,
          se = 0,
          other_mean = NA,
          other_se = 0,
          diff_mean = 0,
          diff_se = 0
        )
        return(data)
      }
    )
  }

  return(result)
}

generate_se_state_and_cbsas <- function(metrics, race_indicators, svy = svy_all) {
  # Wrapper function to calculate all means/SEs and mean/SEs of the difference between
  # racial group mean and all other racial group mean for all geography/race/
  # metric/week combinations (except US, which is handled separately)
  # INPUT:
  #    metrics: vector of metric column name strings
  #    race_indicator: vector of race dummy column name strings
  #    svy: must be an object of the class tbl_svy returned by as_survey_rep()
  # OUTPUT:
  #    full_combo_appended: dataframe with mean/SE for each geography/race
  #    metric/week combination, plus mean/SE for all other races and mean/SE for
  #    the difference between the given race and all other races


  cbsa_names <- svy %>%
    pull(cbsa_title) %>%
    unique() %>%
    na.omit()
  state <- svy %>%
    pull(state) %>%
    unique() %>%
    na.omit()
  geography <- c(cbsa_names, state)
  # mirror clean column names created by fastDummies
  geo_cols <- c(
    paste0("cbsa_title_", janitor::make_clean_names(cbsa_names)),
    paste0("state_", janitor::make_clean_names(state))
  )
  # crosswalk between geography names and geography dummy column names
  geo_xwalk <- tibble(geography = geography, geo_col = geo_cols)
  wks <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()

  # Create grid of all metric/race/geo/week combos
  full_combo <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators,
    geo_col = geo_cols,
    week = wks
  ) %>%
    left_join(geo_xwalk, by = "geo_col")

   #for testing (as running on all combinations takes up too much RAM)
   #full_combo = full_combo %>% 
  #   filter(metric %in% c("telework"))

  # get mean and se for diff bw subgroup and (total population -subgroup)
  # Call the get_se_diff function on every row of full_combo
  se_info <- full_combo %>% pmap_df(get_se_diff)
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(geo_type = ifelse(geography %in% cbsa_names, "msa", "state")) %>%
    select(-geo_col)

  return(full_combo_appended)
}

get_se_diff_us <- function(..., svy = svy_all) {
  # Function to calculate all means/SEs and mean/SEs of the difference between
  # racial group mean and all other racial group mean for a given race/
  # metric/week combination for the US
  # INPUT:
  #    ...: Must be a dataframe with the following columns:
  #     metric, race_indicator(dummy race var), week.
  #    svy: must be an object of the class tbl_svy returned by as_survey_rep()
  # OUTPUT:
  #    result: tibble containing mean mean/SE for the given race metric/week
  #    combination for the US, plus mean/SE for all other races and mean/SE for
  #    the difference between the given race and all other races
  dots <- list(...)

  metric_formula <- as.formula(paste0("~", dots$metric))
  race_formula <- as.formula(paste0("~", dots$race_indicator))

  x <- svyby(metric_formula, race_formula, svy %>%
    srvyr::filter(week_num == dots$week),
  svymean,
  na.rm = T,
  return.replicates = T,
  covmat = T
  )

  mean <- x %>%
    filter(!!sym(dots$race_indicator) == 1) %>%
    pull(!!sym(dots$metric))
  se <- x %>%
    filter(!!sym(dots$race_indicator) == 1) %>%
    pull(se) * 2
  other_mean <- x %>%
    filter(!!sym(dots$race_indicator) == 0) %>%
    pull(!!sym(dots$metric))
  other_se <- x %>%
    filter(!!sym(dots$race_indicator) == 0) %>%
    pull(se)

  # Use svycontrast to calulate se bw race and nonrace (ie black and non black) population
  contrast <- svycontrast(x, contrasts = list(diff = c(1, -1)))
  diff_mean <- contrast %>% as.numeric()
  diff_se <- contrast %>%
    attr("var") %>%
    sqrt() %>%
    {
      . * 2
    }

  result <- tibble(
    mean = mean,
    se = se,
    other_mean = other_mean,
    other_se = other_se,
    diff_mean = diff_mean,
    diff_se = diff_se
  )

  return(result)
}

generate_se_us <- function(metrics, race_indicators, svy = svy_all) {
  # Wrapper function to calculate all means/SEs and mean/SEs of the difference between
  # racial group mean and all other racial group mean for all race/metric/week
  # combinations for the united states
  # INPUT:
  #    metrics: vector of metric column name strings
  #    race_indicator: vector of race dummy column name strings
  #    svy: must be an object of the class tbl_svy returned by as_survey_rep()
  # OUTPUT:
  #    full_combo_appended: dataframe with mean/SE for each race/metric/week
  #    combination for the US, plus mean/SE for all other races and mean/SE for
  #    the difference between the given race and all other races

  wks <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()

  race_indicators <- race_indicators[-6]

  full_combo <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators,
    week = wks
  )
  
  # filter to new vars/ relevant weeks for testing
  #full_combo = full_combo %>% 
  #  filter(week %in% c("wk10_11", "wk11_12"))

  # get mean and se for diff bw subgroup and (total population -subgroup)
  se_info <- full_combo %>% pmap_df(get_se_diff_us)
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(
      geo_type = "national",
      geography = "US"
    )

  return(full_combo_appended)
}


race_indicators <- c("black", "asian", "hispanic", "white", "other", "total")

#This should be run on a reasonably large machine like a c5.2 instance
start <- Sys.time()
all_diff_ses <- generate_se_state_and_cbsas(metrics = metrics, race_indicators = race_indicators)
end <- Sys.time()
print(end - start)

write.csv(all_diff_ses, here("data/intermediate-data", "all_diff_ses.csv"))

start <- Sys.time()
us_diff_ses <- generate_se_us(metrics = metrics, race_indicators = race_indicators)
end <- Sys.time()
print(end - start)

write.csv(us_diff_ses, here("data/intermediate-data", "us_diff_ses.csv"))

# functions to calculate US total means/SEs
calculate_se_us_total <- function(metric, svy) {
  se_df <- svy %>%
    srvyr::filter(!is.na(!!sym(metric))) %>%
    group_by(week_num) %>%
    summarise(mean = survey_mean(!!sym(metric), na.rm = TRUE)) %>%
    # pull(out) %>%
    mutate(
      se = mean_se * 2,
      metric = metric,
      geography = "US",
      race_var = "total"
    ) %>%
    select(week_num, geography, race_var, mean, se, metric)

  return(se_df)
}

format_feature_total <- function(data, geo) {
  data <- data %>%
    mutate(
      moe_95 = se * 1.96,
      moe_95_lb = mean - moe_95,
      moe_95_ub = mean + moe_95,
      geo_type = geo
    ) %>%
    select(geography, metric, week_num, race_var, mean, se, moe_95, moe_95_lb, moe_95_ub, geo_type)

  return(data)
}

# calculate US-wide means for each metric/week
us_total <- map_df(metrics, calculate_se_us_total, svy = svy_all)
write.csv(us_total, here("data/intermediate-data", str_glue("us_total_se_to_week{CUR_WEEK}.csv")))

# Write out svy object as an RDS object for use in QC scipts
saveRDS(svy_all, file = "data/intermediate-data/svy.rds")

us_total_out <- format_feature_total(us_total, "national")

all_diff_ses_out <- all_diff_ses %>%
  mutate(
    moe_95 = se * 1.96,
    moe_95_lb = mean - moe_95,
    moe_95_ub = mean + moe_95,
    sigdiff = ifelse(abs(diff_mean / diff_se) > 1.96, 1, 0)
  ) %>%
  rename(
    race_var = race_indicator,
    week_num = week
  ) %>%
  select(-other_mean, -other_se, -diff_mean, -diff_se)

us_diff_ses_out <- us_diff_ses %>%
  mutate(
    moe_95 = se * 1.96,
    moe_95_lb = mean - moe_95,
    moe_95_ub = mean + moe_95,
    sigdiff = ifelse(abs(diff_mean / diff_se) > 1.96, 1, 0)
  ) %>%
  rename(
    race_var = race_indicator,
    week_num = week
  ) %>%
  select(-other_mean, -other_se, -diff_mean, -diff_se)


data_all <- bind_rows(all_diff_ses_out, us_diff_ses_out, us_total_out)

week_crosswalk <- tibble::tribble(
  ~week_num, ~date_int,
  "wk13", paste("8/19\u2013", "31", sep = ""),
  "wk14", paste("9/2\u2013", "14", sep = ""),
  "wk15", paste("9/16\u2013", "28", sep = ""),
  "wk16", paste("9/30\u2013", "10/12", sep = ""),
  "wk17", paste("10/14\u2013", "26", sep = ""),
  "wk18", paste("10/28\u2013", "11/9", sep = ""),
  "wk19", paste("11/11\u2013", "23", sep = "")
  
)

data_out <- left_join(data_all, week_crosswalk, by = "week_num") %>%
  arrange(metric, race_var, geography,
          factor(week_num, 
                 levels = c("wk13",  "wk14", "wk15", "wk16", "wk17", "wk18",  "wk19")))

# Create final-data directory if it doesn't exist
dir.create("data/final-data", showWarnings = F)

write_csv(data_out, here("data/final-data", "phase2_all_to_current_week.csv"))