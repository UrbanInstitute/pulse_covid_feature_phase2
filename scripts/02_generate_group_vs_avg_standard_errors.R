# Generate point estimates and SE for metrics of interest
# Also conduct significance tests bw subgroups and
# relevant natl/state/metro averages
library(tidyverse)
library(here)
library(srvyr)
library(survey)
library(fastDummies)



##  Read in and clean data


puf_all_weeks <- read_csv(here("data/intermediate-data", "pulse_puf_all_weeks.csv"))

# Set parameters
CUR_WEEK <- puf_all_weeks %>%
  pull(week_x) %>%
  max()


puf_all_weeks <- puf_all_weeks %>%
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
svy <- puf_all_weeks %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )


# Helper function to dynamically generate crosswalk for rolling average calculations
construct_overlap_intervals_df <- function(x) {
  # INPUT:
  #   x: integer

  ### Construct week number vector
  week_vec <- 1:x
  # Get middle weeks and repeat each twice
  middle_nums <- week_vec[c(-1, -length(week_vec))]
  middle_nums <- rep(middle_nums, each = 2)
  # Construct week_num vector where middle numbers are repeated twice
  wk_nums <- c(week_vec[1], middle_nums, tail(week_vec, n = 1))
  wk_nums <- paste0("wk", wk_nums)



  ### Construct week_Average vector
  interval_vec <- c()
  while (x != 1) {
    previous_num <- x - 1
    interval <- paste0("wk", previous_num, "_", x)
    interval_vec <- c(interval_vec, interval)

    x <- x - 1
  }
  # sort alphabetically and repeat each twice
  wk_int <- sort(interval_vec) %>% rep(each = 2)

  ### Create dataframe
  result <- tibble(
    week_num = wk_nums,
    week_int = wk_int
  )



  return(result)
}


week_crosswalk <- construct_overlap_intervals_df(CUR_WEEK)


puf_all_weeks_rolling <- puf_all_weeks %>%
  mutate_at(vars(starts_with("pweight")), funs(. / 2)) %>% # divide pweight and repweights by 2
  right_join(week_crosswalk, by = "week_num") %>% # create week range column
  rename("week_pt" = "week_num", "week_num" = "week_int")

# Set BRR survey design and specify replicate weights for rolling averages
svy_rolling <- puf_all_weeks_rolling %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )


get_se_diff <- function(..., svy = svy_rolling) {
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
    x <- svyby(metric_formula, geo_formula, svy %>%
      filter(week_num == dots$week),
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
    )
  }
  # if race_indicator isn't total, then compare subgroup to geography avg
  else {
    # identify whether geography is state or cbsa
    geo_col_name <- ifelse(grepl("cbsa", dots$geo_col, fixed = TRUE), "cbsa_title", "state")

    # Use trycatch bc there are 4 metric-week-geogarphy combinations
    # where there are 0 respondents which return NA and error out
    result <- tryCatch(
      {
        # Use svyby to compute mean (and replicate means) for race and non race var population
        # (ie black and nonblack population)
        x <- svyby(metric_formula, race_formula, svy %>%
          filter(!!sym(geo_col_name) == dots$geography) %>%
          filter(week_num == dots$week),
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

generate_se_state_and_cbsas <- function(metrics, race_indicators, svy = svy_rolling) {
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

  # Create grid of all metric/reace/geo/week combos
  full_combo <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators,
    geo_col = geo_cols,
    week = wks
  ) %>%
    left_join(geo_xwalk, by = "geo_col")

  # for testing (as running on all 4080 combintaions takes up too much RAM)
  #full_combo = full_combo %>% filter(race_indicator %in% c("black", "total") , metric == "rent_paid_last_month")

  # get mean and se for diff bw subgroup and (total population -subgroup)
  # Call the get_se_diff function on every row of full_combo
  se_info <- full_combo %>% pmap_df(get_se_diff)
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(geo_type = ifelse(geography %in% cbsa_names, "msa", "state")) %>%
    select(-geo_col)

  return(full_combo_appended)
}

get_se_diff_us <- function(..., svy = svy_rolling) {
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
    filter(week_num == dots$week),
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

generate_se_us <- function(metrics, race_indicators, svy = svy_rolling) {
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


metrics <- c(
  "uninsured",
  "insured_public",
  "inc_loss",
  "expect_inc_loss",
  "rent_not_conf",
  "mortgage_not_conf",
  "rent_not_paid",
  "mortgage_not_paid",
  "food_insufficient",
  "classes_cancelled",
  "depression_anxiety_signs"
)
race_indicators <- c("black", "asian", "hispanic", "white", "other", "total")

# Update: ran on c5.4xlarge instance and took 3 hours
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
    filter(!is.na(!!sym(metric))) %>%
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
us_rolling_total <- map_df(metrics, calculate_se_us_total, svy = svy_rolling)
write.csv(us_rolling_total, here("data/intermediate-data", str_glue("us_rolling_total_se_to_week{CUR_WEEK}.csv")))

# Write out svy_roling object as an RDS object for use in QC scipts
saveRDS(svy_rolling, file = "data/intermediate-data/svy_rolling.rds")

us_total_rolling_out <- format_feature_total(us_rolling_total, "national")

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


rolling_all <- bind_rows(all_diff_ses_out, us_diff_ses_out, us_total_rolling_out)
write_csv(rolling_all, here("data/final-data", "rolling_all_to_current_week.csv"))
