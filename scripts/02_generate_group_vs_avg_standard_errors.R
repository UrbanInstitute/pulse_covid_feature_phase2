# Generate point estimates and SE for metrics of interest
# Also conduct significance tests bw subgroups and
# relevant natl/state/metro averages
require(devtools)
install_version("survey", version = "4.0", repos = "http://cran.us.r-project.org")
install_version("srvyr", version = "1.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(here)
library(srvyr)
library(survey)
library(fastDummies)
library(parallel)
library(future)
library(furrr)
library(aws.s3)
library(data.table)

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
#puf_all_weeks <- read_csv(here("data/intermediate-data", "pulse_puf2_all_weeks.csv")) %>%
puf_all_weeks <- read_csv(here("data/intermediate-data", "pulse_puf2_cur_week.csv")) %>%
  mutate(spend_credit = as.numeric(spend_credit),
         spend_savings = as.numeric(spend_savings),
         spend_stimulus = as.numeric(spend_stimulus),
         spend_ui = as.numeric(spend_ui),
         inc_loss = as.numeric(inc_loss), 
         inc_loss_rv = as.numeric(inc_loss_rv)) %>%
#create combined inc_loss variable for efficient processing
mutate(inc_loss = case_when(week_x >= 28 ~ inc_loss_rv,
                             TRUE ~ inc_loss))


                          
# Set parameters
CUR_WEEK <- puf_all_weeks %>%
  pull(week_x) %>%
  max()

new_week_vec <- c(str_glue("wk{CUR_WEEK}"))

puf_all_weeks <- puf_all_weeks %>%
  mutate(tbirth_year = as.numeric(tbirth_year))


puf_all_weeks2 <- puf_all_weeks %>%
  # For the uninsured variable, we filter out people over 65 from the denominator
  mutate(
    insured_public = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ as.numeric(insured_public)
    ),
    uninsured = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ as.numeric(uninsured)
    )
  ) %>%
  select(starts_with("pweight"), all_cols) %>%
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
  ) %>%
  # only run metrics for current week
  filter(week_num %in% new_week_vec)

puf_all_weeks2_total <- puf_all_weeks2 %>%
  # Create one hot encoding of cbsas and states for easy use with survey pkg
  fastDummies::dummy_cols("cbsa_title") %>%
  fastDummies::dummy_cols("state") %>%
  janitor::clean_names()

#Set BRR survey design and specify replicate weights
svy_all <- puf_all_weeks2 %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )

state_list <- puf_all_weeks2 %>%
  mutate(geography = state,
         geo_type = "state") %>%
  split(list(puf_all_weeks2$state, puf_all_weeks2$week_num))

cbsa_list <- puf_all_weeks2 %>%
  mutate(geography = cbsa_title,
         geo_type = "msa") %>%
  split(list(puf_all_weeks2$cbsa_title, puf_all_weeks2$week_num))

all_list <- c(state_list, cbsa_list)

all_week_list <- puf_all_weeks2_total %>%
  split(puf_all_weeks2_total$week_num)

get_se_diff_total <- function(..., svy) {
  # if race indicator is total, then we compute mean of the geography,
  # and compare it to the mean of national estimates
  dots <- list(...)
  
  geo_formula <- as.formula(paste0("~", dots$geo_col))
  metric_formula <- as.formula(paste0("~", dots$metric))

  result <- tryCatch(
    {
      x <- svyby(metric_formula, geo_formula, svy,
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
}

get_se_diff <- function(..., svy) {
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

 
  #compare subgroup to geography avg

  # Use trycatch bc there are 4 metric-week-geogarphy combinations
  # where there are 0 respondents which return NA and error out
  result <- tryCatch(
    {
      # Use svyby to compute mean (and replicate means) for race and non race var population
      # (ie black and nonblack population)
      x <- svyby(metric_formula, race_formula, svy,
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
  return(result)
  }

generate_se_state_and_cbsas <- function(metrics, race_indicators, df) {
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


  svy <- df %>%
    as_survey_rep(
      repweights = dplyr::matches("pweight[0-9]+"),
      weights = pweight,
      type = "BRR",
      mse = TRUE
    )
  
  # cbsa_names <- svy %>%
  #   pull(cbsa_title) %>%
  #   unique() %>%
  #   na.omit()
  # state <- svy %>%
  #   pull(state) %>%
  #   unique() %>%
  #   na.omit()
  # geography <- c(cbsa_names, state)
  # # mirror clean column names created by fastDummies
  # geo_cols <- c(
  #   paste0("cbsa_title_", janitor::make_clean_names(cbsa_names)),
  #   paste0("state_", janitor::make_clean_names(state))
  # )
  # # crosswalk between geography names and geography dummy column names
  # geo_xwalk <- tibble(geography = geography, geo_col = geo_cols)
  
  wk <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()
  
  geo <- svy %>%
    pull(geography) %>%
    unique() %>%
    na.omit()
  
  geo_type <- svy %>%
    pull(geo_type) %>%
    unique() %>%
    na.omit()

  # Create grid of all metric/race combos for the geo/week dataframe
  full_combo <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators
  ) 

   #for testing (as running on all combinations takes up too much RAM)
   #full_combo = full_combo %>% 
  #   filter(metric %in% c("telework"))

  # get mean and se for diff bw subgroup and (total population -subgroup)
  # Call the get_se_diff function on every row of full_combo
  se_info <- full_combo %>% pmap_df(get_se_diff, svy = svy)
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(geo_type = geo_type,
           geography = geo,
           week_num = wk)

  return(full_combo_appended)
}

generate_se_state_and_cbsas_total <- function(metrics, df) {
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
  
  
  svy <- df %>%
    as_survey_rep(
      repweights = dplyr::matches("pweight[0-9]+"),
      weights = pweight,
      type = "BRR",
      mse = TRUE
    )
  
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
  
  wk_num <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()
  
  print(wk_num)
  
  
  # Create grid of all metric/race/geo/week combos
  full_combo <- expand_grid(
    metric = metrics,
    geo_col = geo_cols
  ) %>%
    left_join(geo_xwalk, by = "geo_col")
  
  #for testing (as running on all combinations takes up too much RAM)
  #full_combo = full_combo %>% 
  #   filter(metric %in% c("telework"))
  
  # get mean and se for diff bw subgroup and (total population -subgroup)
  # Call the get_se_diff function on every row of full_combo
  se_info <- full_combo %>% pmap_df(get_se_diff_total, svy = svy)
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(geo_type = ifelse(geography %in% cbsa_names, "msa", "state"),
           race_indicator = "total",
           week_num = wk_num)%>%
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
  
  result <- tryCatch(
    {
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
    )},
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

  #race_indicators <- race_indicators[-6]

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


race_indicators <- c("black", "asian", "hispanic", "white", "other")

#This should be run on a reasonably large machine like a c5.2 instance
start <- Sys.time()
plan(multisession, workers = parallel::detectCores() - 2)
all_diff_ses <- future_map_dfr(all_list, 
                               ~generate_se_state_and_cbsas(metrics = metrics,
                                                            race_indicators = race_indicators,
                                                            df = .x))
end <- Sys.time()
print(end - start)

start <- Sys.time()
plan(sequential)
all_diff_ses_total <- map_dfr(all_week_list, 
                               ~generate_se_state_and_cbsas_total(metrics = metrics,
                                                            df = .x))
end <- Sys.time()
print(end - start)

all_diff_ses <- rbind(all_diff_ses, all_diff_ses_total)

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
      group_by(week_num) 
    
    result <- se_df %>%
      summarise(mean = survey_mean(!!sym(metric), na.rm = TRUE)) %>%
      # pull(out) %>%
      mutate(
        se = mean_se * 2,
        metric = metric,
        geography = "US",
        race_var = "total"
      ) %>%
      select(week_num, geography, race_var, mean, se, metric)
  
  return(result)
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
# starting in week 34, don't include expect_inc_loss
metrics_total <- metrics[!metrics %in% c("telework", "learning_fewer", "expect_inc_loss")]
us_total <- map_df(metrics_total, calculate_se_us_total, svy = svy_all)
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
    race_var = race_indicator
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


# variables removed between start of questionnaire two and phase 3.2
phase_3_2_rem_metric <- c("inc_loss", "telework", "learning_fewer", "expect_inc_loss")

us_total_rem <- expand_grid(metric = c("telework", "learning_fewer", "expect_inc_loss"),
                            geography = "US",
                            race_var = "total",
                            geo_type = "national",
                            week_num = new_week_vec,
                            mean = NA_real_,
                            se = 0,
                            moe_95 = 0,
                            moe_95_ub = NA_real_,
                            moe_95_lb = NA_real_,
                            sigdiff = NA_real_) %>%
  select(geography, metric, week_num, race_var, mean, se, moe_95, moe_95_lb, moe_95_ub, sigdiff, geo_type)

data_all <- bind_rows(all_diff_ses_out, us_diff_ses_out, us_total_out, us_total_rem)

week_crosswalk <- tibble::tribble(
  ~week_num, ~date_int,
  "wk13", paste("8/19/20\u2013", "8/31/20", sep = ""),
  "wk14", paste("9/2/20\u2013", "9/14/20", sep = ""),
  "wk15", paste("9/16/20\u2013", "9/28/20", sep = ""),
  "wk16", paste("9/30/20\u2013", "10/12/20", sep = ""),
  "wk17", paste("10/14/20\u2013", "10/26/20", sep = ""),
  "wk18", paste("10/28/20\u2013", "11/9/20", sep = ""),
  "wk19", paste("11/11/20\u2013", "11/23/20", sep = ""),
  "wk20", paste("11/25/20\u2013", "12/7/20", sep = ""),
  "wk21", paste("12/9/20\u2013", "12/21/20", sep = ""),
  "wk22", paste("1/6/21\u2013", "1/18/21", sep = ""),
  "wk23", paste("1/20/21\u2013", "2/1/21", sep = ""),
  "wk24", paste("2/3/21\u2013", "2/15/21", sep = ""),
  "wk25", paste("2/17/21\u2013", "3/1/21", sep = ""),
  "wk26", paste("3/3/21\u2013", "3/15/21", sep = ""),
  "wk27", paste("3/17/21\u2013", "3/29/21", sep = ""),
  "wk28", paste("4/14/21\u2013", "4/26/21", sep = ""),
  "wk29", paste("4/28/21\u2013", "5/10/21", sep = ""),
  "wk30", paste("5/12/21\u2013", "5/24/21", sep = ""),
  "wk31", paste("5/26/21\u2013", "6/7/21", sep = ""),
  "wk32", paste("6/9/21\u2013", "6/21/21", sep = ""),
  "wk33", paste("6/23/21\u2013", "7/5/21", sep = ""),
  "wk34", paste("7/21/21\u2013", "8/2/21", sep = ""),
  "wk35", paste("8/4/21\u2013", "8/16/21", sep = ""),
  "wk36", paste("8/18/21\u2013", "8/30/21", sep = ""),
  "wk37", paste("9/1/21\u2013", "9/13/21", sep = ""),
  "wk38", paste("9/15/21\u2013", "9/27/21", sep = ""),
  "wk39", paste("9/29/21\u2013", "10/11/21", sep = ""),
  "wk40", paste("12/1/21\u2013", "12/13/21", sep = ""),
  "wk41", paste("12/29/21\u2013", "1/10/22", sep = ""),
  "wk42", paste("1/26/22\u2013", "2/7/22", sep = ""))

# create data for feature with combined inc_loss and inc_loss_rv metric
data_out_feature <- left_join(data_all, week_crosswalk, by = "week_num") 

inc_loss_rv <- data_out_feature %>%
  filter(metric == "inc_loss") %>%
  mutate(metric = "inc_loss_rv")

data_out <- rbind(data_out_feature, inc_loss_rv) %>%
  mutate(mean = if_else((metric == "inc_loss"), NA_real_, mean ),
         se = if_else((metric == "inc_loss"), 0, se ),
         moe_95 = if_else((metric == "inc_loss"), 0, moe_95),
         moe_95_lb = if_else((metric == "inc_loss"), NA_real_, moe_95_lb),
         moe_95_ub = if_else((metric == "inc_loss"), NA_real_, moe_95_ub),
         sigdiff= if_else((metric == "inc_loss"), NA_real_, sigdiff),
         var_removed = case_when((metric %in% phase_3_2_rem_metric)  ~ 1,
                                 TRUE ~ 0)
         )
  

# Create final-data directory if it doesn't exist
dir.create("data/final-data", showWarnings = F)

write_csv(data_out, here("data/final-data", str_glue("phase2_wk{CUR_WEEK}.csv")))
write_csv(data_out_feature, here("data/final-data", str_glue("phase2_wk{CUR_WEEK}_feature.csv")))

data_out_prev <- read_csv("https://ui-census-pulse-survey.s3.amazonaws.com/phase2_all_to_current_week.csv")
data_out_feature_prev <- read_csv("https://ui-census-pulse-survey.s3.amazonaws.com/phase2_all_to_current_week_feature.csv")


data_out <- rbind(data_out_prev, data_out) %>%
  arrange(metric, race_var, geography,
          factor(week_num,
                 levels = c("wk13",  "wk14", "wk15", "wk16", "wk17", "wk18",
                            "wk19", "wk20", "wk21", "wk22", "wk23", "wk24",
                            "wk25", "wk26",  "wk27", "wk28", "wk29", "wk30",
                            "wk31", "wk32", "wk33", "wk34", "wk35", "wk36",
                            "wk37", "wk38", "wk39", "wk40", "wk41", "wk42")))

data_out_feature <- rbind(data_out_feature_prev, data_out_feature) %>%
  arrange(metric, race_var, geography,
          factor(week_num,
                 levels = c("wk13",  "wk14", "wk15", "wk16", "wk17", "wk18",
                            "wk19", "wk20", "wk21", "wk22", "wk23", "wk24",
                            "wk25", "wk26",  "wk27", "wk28", "wk29", "wk30",
                            "wk31", "wk32", "wk33", "wk34", "wk35", "wk36",
                            "wk37", "wk38", "wk39", "wk40", "wk41", "wk42")))

write_csv(data_out, here("data/final-data", "phase2_all_to_current_week.csv"))
write_csv(data_out_feature, here("data/final-data", "phase2_all_to_current_week_feature.csv"))

n_row_feat_out <- length(metrics) * (15 + 51 + 1) * (CUR_WEEK - 13 + 1) * (length(race_indicators) + 1)
n_row_out <- (length(metrics) + 1) * (15 + 51 + 1) * (CUR_WEEK - 13 + 1) * (length(race_indicators) + 1)

assertthat::are_equal(n_row_feat_out, data_out_feature %>% nrow())
assertthat::are_equal(n_row_out, data_out %>% nrow())
