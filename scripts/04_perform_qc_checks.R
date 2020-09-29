library(tidyverse)
library(readxl)
library(testit)
library(rmarkdown)
library(stringr)
library(tidyverse)
library(urbnthemes)
library(here)
library(survey)
library(srvyr)



### For testing, we read in a few indicators from the Pulse Data Tables and ensure that we get the same numbers from our PUF
### ----------Read in cleaned data from disk -------------------------


rolling_all <- read_csv(here("data/final-data", "rolling_all_to_current_week.csv"))

all_diff_ses <- read_csv(here("data/intermediate-data", "all_diff_ses.csv"))

us_diff_ses = read_csv(here("data/intermediate-data", "us_diff_ses.csv"))

svy_rolling <- readRDS(here("data/intermediate-data", "svy_rolling.rds"))



### -----------Read in data from Pulse Data Table----------------------

### Defining data readin functions
# Need to define a seperate data readin function for every table
# of interest from the Census Pulse Survey, then add it to the tribble in
# `generate_table_data`.

readin_tech_availability_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table educ3, or computer and Internet availability. All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "device_always_available",
      "device_usually_available",
      "device_sometimes_available",
      "device_rarely_available",
      "device_never_available",
      "device_did_not_respond",
      "internet_always_available",
      "internet_usually_available",
      "internet_sometimes_available",
      "internet_rarely_available",
      "internet_never_available",
      "internet_did_not_respond"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      device_total_answered = total - device_did_not_respond,
      internet_total_answered = total - internet_did_not_respond,
      # NOTE: We only count rarely and never as unavailable
      perc_dev_unavail = (device_rarely_available + device_never_available) / (total - device_did_not_respond),
      perc_int_unavail = (internet_rarely_available + internet_never_available) / (total - internet_did_not_respond),
      device_percent_answered = device_total_answered / total,
      internet_percent_answered = internet_total_answered / total,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, device_rarely_available, device_never_available, device_total_answered,
      internet_rarely_available, internet_never_available, internet_total_answered,
      perc_dev_unavail, perc_int_unavail, total, device_percent_answered, internet_percent_answered
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_educ_affected_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table educ2, or education change table. The
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "classes_distance_using_online_materials",
      "classes_distance_using_paper_materials",
      "classes_cancelled",
      "classes_changed_another_Way",
      "classes_unchanged",
      "did_not_report"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    # NOTE: This was a multiple choice questions so columns
    # may not sum to total columns. We can only calculate percentages
    # for one columns
    mutate(
      total_answered = total - did_not_report,
      perc_classes_cancelled = classes_cancelled / total_answered,
      perc_using_distance_learning_online = classes_distance_using_online_materials / total_answered,
      perc_using_distance_learning_paper = classes_distance_using_paper_materials / total_answered,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up
  # assert(data_by_race %>%
  #          mutate(total_comp = income_loss_since_mar_13 + no_income_loss_since_mar_13 +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% food_insuff_data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  
  result <- data_by_race %>%
    select(
      variable, geography, total, total_answered, perc_classes_cancelled,
      perc_using_distance_learning_online, perc_using_distance_learning_paper
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_employ_loss_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table employ1, or lost employment income. All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  # OUPUT:
  #   result: A datafrmae where every column is a race/ethnicity and the columns are variables
  #     of interest
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "income_loss_since_mar_13",
      "no_income_loss_since_mar_13",
      "did_not_report_loss_since_mar_13",
      "income_loss_next_4_wks",
      "no_income_next_4_wks",
      "did_not_report_loss_next_4_wks"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate( # total_answered = total - did_not_report,
      perc_lost_income = income_loss_since_mar_13 / (total - did_not_report_loss_since_mar_13),
      total_answered_lost = (total - did_not_report_loss_since_mar_13),
      perc_lose_income = income_loss_next_4_wks / (total - did_not_report_loss_next_4_wks),
      total_answered_lose = (total - did_not_report_loss_next_4_wks),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up
  # assert(data_by_race %>%
  #          mutate(total_comp = income_loss_since_mar_13 + no_income_loss_since_mar_13 +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% food_insuff_data_by_race$total %>% discard(is.na) %>% round(1))

  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(variable, geography, total_answered_lose, total_answered_lost, income_loss_since_mar_13, income_loss_next_4_wks, perc_lost_income, perc_lose_income) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_avoided_medical_care_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table health1, or delayed medical care.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded

  # AS: changed did_get and did_not_get because question is negatively worded (so yes = did not get)
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "delayed_med_care_did_not_get",
      "delayed_med_care_did_get",
      "delayed_med_care_did_not_report",
      "needed_med_care_did_not_get",
      "needed_med_care_did_get",
      "needed_med_care_did_not_report"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate( # NOTE: No totals in the data
      perc_not_get_med_care = needed_med_care_did_not_get / (needed_med_care_did_not_get + needed_med_care_did_get),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))

  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(variable, geography, needed_med_care_did_get, needed_med_care_did_not_get, needed_med_care_did_not_report, perc_not_get_med_care) %>%
    # AS: updated week num to generalize to numbers with two digits
    mutate(week_num = wk_num)
  return(result)
}

readin_food_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table food2b, or food insecurity.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "enough",
      "enough_but_not_wanted",
      "sometimes_not_enough",
      "often_not_enough",
      "did_not_report"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      total_answered = total - did_not_report,
      # NOTE: We only count often note enough and sometimes not enough respondents as food insecure
      total_food_insecure = often_not_enough + sometimes_not_enough,
      percent_food_insecure = total_food_insecure / total_answered,
      percent_answered = total_answered / total,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, total_food_insecure, total_answered, total,
      percent_food_insecure, percent_answered
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_food_children_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table food3b, or food insecurity in households with children.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "enough",
      "enough_but_not_wanted",
      "sometimes_not_enough",
      "often_not_enough",
      "did_not_report"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      total_answered_children = total - did_not_report,
      # NOTE: We only count often note enough and sometimes not enough respondents as food insecure
      total_food_insecure_children = often_not_enough + sometimes_not_enough,
      percent_food_insecure_children = total_food_insecure_children / total_answered_children,
      percent_answered_children = total_answered_children / total,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, total_food_insecure_children, total_answered_children, total,
      percent_food_insecure_children, percent_answered_children
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_conf_pay_mortgage_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table housing2a, or confidence in paying mortgage next month.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded




  data <- tryCatch(
    {
      data <- read_excel(filepath,
        skip = skip,
        col_names = c(
          "variable",
          "total",
          "owned_free_clear",
          "no_confidence",
          "slight_confidence",
          "moderate_confidence",
          "high_confidence",
          "payment_deferred",
          "did_not_respond_conf",
          "did_not_respond_tenure"
        ),
        col_types = "text",
        sheet = sheet
      )
    },
    error = function(err) {

      # error handler picks up where error was generated
      print(paste("Column number error:  ", err))
      data <- read_excel(filepath,
        skip = skip,
        col_names = c(
          "variable",
          "total",
          "owned_free_clear",
          "no_confidence",
          "slight_confidence",
          "moderate_confidence",
          "high_confidence",
          "payment_deferred",
          "did_not_respond_conf",
          "renter_occupied",
          "did_not_respond_tenure"
        ),
        col_types = "text",
        sheet = sheet
      )
      return(data)
    }
  )


  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      total_answered = total - owned_free_clear - did_not_respond_conf,
      perc_conf_pay_mortgage = (no_confidence + slight_confidence + payment_deferred) / (total - owned_free_clear - did_not_respond_conf),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, perc_conf_pay_mortgage, no_confidence,
      slight_confidence, moderate_confidence, high_confidence, payment_deferred,
      total, did_not_respond_conf, did_not_respond_tenure, owned_free_clear
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_conf_pay_rent_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table housing 2b, or confidence in paying rent this month.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded



  data <- tryCatch(
    {
      data <- read_excel(filepath,
        skip = skip,
        col_names = c(
          "variable",
          "total",
          "occup_no_rent",
          "no_confidence",
          "slight_confidence",
          "moderate_confidence",
          "high_confidence",
          "payment_deferred",
          "did_not_respond_conf",
          "owner_occupied",
          "did_not_respond_tenure"
        ),
        col_types = "text",
        sheet = sheet
      )
    },
    error = function(err) {

      # error handler picks up where error was generated
      # print(paste("Column number error:  ", err))
      data <- read_excel(filepath,
        skip = skip,
        col_names = c(
          "variable",
          "total",
          "occup_no_rent",
          "no_confidence",
          "slight_confidence",
          "moderate_confidence",
          "high_confidence",
          "payment_deferred",
          "did_not_respond_conf",
          "did_not_respond_tenure"
        ),
        col_types = "text",
        sheet = sheet
      )
      return(data)
    }
  )


  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      perc_conf_pay_rent = (no_confidence + slight_confidence + payment_deferred) / (total - occup_no_rent - did_not_respond_conf),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, perc_conf_pay_rent, no_confidence,
      slight_confidence, moderate_confidence, high_confidence, payment_deferred,
      total, did_not_respond_conf, did_not_respond_tenure
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_rent_not_paid_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table housing 2b, or confidence in paying rent this month.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  
  
  
  data <- tryCatch(
    {
      data <- read_excel(filepath,
                         skip = skip,
                         col_names = c(
                           "variable",
                           "total",
                           "occup_no_rent",
                           "payment_yes",
                           "payment_no",
                           "payment_deferred",
                           "did_not_respond_conf",
                           "owner_occupied",
                           "did_not_respond_tenure"
                         ),
                         col_types = "text",
                         sheet = sheet
      )
    },
    error = function(err) {
      
      # error handler picks up where error was generated
      # print(paste("Column number error:  ", err))
      data <- read_excel(filepath,
                         skip = skip,
                         col_names = c(
                           "variable",
                           "total",
                           "occup_no_rent",
                           "payment_yes",
                           "payment_no",
                           "payment_deferred",
                           "did_not_respond_conf",
                           "did_not_respond_tenure"
                         ),
                         col_types = "text",
                         sheet = sheet
      )
      return(data)
    }
  )
  
  
  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      perc_rent_not_paid = payment_no + payment_deferred / (total - occup_no_rent - did_not_respond_conf),
      total_not_paid = payment_no + payment_deferred,
      total_answered = total - occup_no_rent - did_not_respond_conf,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)
  
  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, perc_rent_not_paid, payment_no, payment_yes, payment_deferred,
      total, did_not_respond_conf, did_not_respond_tenure, total_answered, total_not_paid
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_stimulus_expenses_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table housing 2b, or confidence in paying rent this month.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  
  
  
  data <- tryCatch(
    {
      data <- read_excel(filepath,
                         skip = skip,
                         col_names = c(
                           "variable",
                           "total",
                           "used_expenses",
                           "used_debt",
                           "used_savings",
                           "payment_not_received",
                           "did_not_respond"
                         ),
                         col_types = "text",
                         sheet = sheet
      )
    },
    error = function(err) {
      
      # error handler picks up where error was generated
      # print(paste("Column number error:  ", err))
      data <- read_excel(filepath,
                         skip = skip,
                         col_names = c(
                           "variable",
                           "total",
                           "used_expenses",
                           "used_debt",
                           "used_savings",
                           "payment_not_received",
                           "did_not_respond"
                         ),
                         col_types = "text",
                         sheet = sheet
      )
      return(data)
    }
  )
  
  
  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      perc_used_expenses = used_expenses / (total - payment_not_received - did_not_respond),
      total_answered = total - payment_not_received - did_not_respond,
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)
  
  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(
      variable, geography, perc_used_expenses, used_expenses, used_debt, used_savings,
      total, did_not_respond, total_answered, payment_not_received
    ) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}


readin_mental_health_anxiety_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table health2a, or symptoms of anxiety.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "nerv_anx_not_at_all",
      "nerv_anx_several_days",
      "nerv_anx_halfmore_days",
      "nerv_anx_nearlyall_days",
      "nerv_anx_no_response",
      "worried_not_at_all",
      "worried_several_days",
      "worried_halfmore_days",
      "worried_nearlyall_days",
      "worried_no_response"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      anxiety_signs = (nerv_anx_several_days + nerv_anx_halfmore_days + nerv_anx_nearlyall_days) / (nerv_anx_not_at_all + nerv_anx_several_days + nerv_anx_halfmore_days + nerv_anx_nearlyall_days),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(variable, geography, anxiety_signs) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_mental_health_depression_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table health 2b, or symptoms of depression. All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded


  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "little_pleasure_not_at_all",
      "little_pleasure_several_days",
      "little_pleasure_halfmore_days",
      "little_pleasure_nearlyall_days",
      "little_pleasure_no_response",
      "down_depress_not_at_all",
      "down_depress_several_days",
      "down_depress_halfmore_days",
      "down_depress_nearlyall_days",
      "down_depress_no_response"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      depressed_signs = (down_depress_several_days + down_depress_halfmore_days + down_depress_nearlyall_days) / (down_depress_not_at_all + down_depress_several_days + down_depress_halfmore_days + down_depress_nearlyall_days),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))

  # AS: store wk_num as separate varaiable in order to correctly collect week number for week 2
  # to create week_num variable in line 679
  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  wk_num <- ifelse(filepath == "data/raw-data/health2b_wk2-full.xlsx",
    "wk2",
    wk_num
  )

  result <- data_by_race %>%
    select(variable, geography, depressed_signs) %>%
    mutate(week_num = wk_num)
  return(result)
}

readin_health_insurance_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table health3, or types of health insurance coverage.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total_insured",
      "private",
      "public",
      "uninsured",
      "not_reported"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      insured_public = public / total_insured,
      uninsured = uninsured / (uninsured + total_insured),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))
  wk_num <- str_match(filepath, "week(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(variable, geography, insured_public, uninsured) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}

readin_employment_data <- function(sheet, filepath, skip = 5) {
  # Specific cleaning function for table employ2, or food emplyment status by sector.All the
  # inputs to this fxn should be automatically selected by the wrapper function
  # and should NOT have to be manually entered.
  #
  # INPUTS:
  #   sheet (chr): Name of sheet to read in. This usually does NOT
  #   have to be manually specified and is instead done automatically in
  #   wrapper functions
  #   filepath (chr): Local filepath to food insecurity table after its been downloaded
  data <- read_excel(filepath,
    skip = skip,
    col_names = c(
      "variable",
      "total",
      "government",
      "private",
      "nonprofit",
      "self_employed",
      "fam_business",
      "employed_did_not_respond",
      "not_employed",
      "did_not_respond"
    ),
    col_types = c(
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    sheet = sheet
  )

  data_by_race <- data %>%
    # Assumes only rows with Hispanic in title will be race vars. True for now
    filter(str_detect(
      variable,
      "Hispanic"
    )) %>%
    mutate_at(vars(-variable), as.numeric) %>%
    # Replace NA's with 0. DANGEROUS! But this seems correct after adding up Census figrues
    mutate_at(vars(-variable), replace_na, 0) %>%
    mutate(
      entrepreneurship_health = (self_employed + fam_business) / (total - employed_did_not_respond - did_not_respond),
      geography = sheet
    ) %>%
    # Removing Hispanic Origin and Race Header Row
    slice(-1)

  # Check that totals add up (In week 3 asian total is off by 1? Need to investigate)
  # assert(data_by_race %>%
  #          mutate(total_comp = enough + enough_but_not_wanted +
  #                   sometimes_not_enough + often_not_enough +
  #                   did_not_report) %>%
  #          pull(total_comp) %>%
  #          discard(is.na) %>%
  #          round(1) %==% data_by_race$total %>% discard(is.na) %>% round(1))

  wk_num <- str_match(filepath, "_(.*?).xlsx")[,2]
  result <- data_by_race %>%
    select(variable, geography, entrepreneurship_health) %>%
    # MM: this assumes the week is single digit, and that the file name is standardized
    mutate(week_num = wk_num)
  return(result)
}


generate_table_data <- function(table_var, week_num) {
  # Function to generate table data by reading in all sheets, keeping
  # just the race vars, and cleaning using specific cleaning functions
  # for every kind of table.
  # INPUTS:
  #   week_num (int): Week number for Pulse survey
  #   table (chr): Case sensitive name to read in (ie food2b)

  # if week_num is a vector, then map this function over the different weeks
  if (length(week_num) > 1) {
    table_data <- map_df(week_num, generate_table_data, table_var = table_var)
    return(table_data)
  }

  # Tribble showing Pulse table and thier accompanying cleaning function
  # Need to add to this tribble as we add more tables
  fxn_table_xwalk <- tribble(
    ~table, ~cleaning_fxn, ~metric,
    "educ3", "readin_tech_availability_data", "education_tech_availability",
    "educ2", "readin_educ_affected_data", "education_classes_cancelled",
    "employ1", "readin_employ_loss_data", "employment_income_loss",
    "health1", "readin_avoided_medical_care_data", "avoided_medical_care",
    "food2b", "readin_food_data", "food_insecurity",
    "food3b", "readin_food_children_data", "food_insecurity_children",
    "housing2a", "readin_conf_pay_mortgage_data", "confidence_paying_mortgage",
    "housing2b", "readin_conf_pay_rent_data", "confidence_paying_rent",
    "housing1b", "readin_rent_not_paid_data", "rent_not_paid",
    "health2a", "readin_mental_health_anxiety_data", "anxiety",
    "health2b", "readin_mental_health_depression_data", "depression",
    "health3", "readin_health_insurance_data", "health_insurance",
    "employ2", "readin_employment_data", "employment",
    "stimulus1", "readin_stimulus_expenses_data", "stimulus_expenses"
  )

  # Get right cleaning function for the inputted table
  cleaning_fxn <- fxn_table_xwalk %>%
    filter(table == table_var) %>%
    pull(cleaning_fxn)

  # Construct data url and downlaod
  data_url <- str_glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/2020/wk{week_num}/{table_var}_week{week_num}.xlsx")
  filepath <- str_glue("data/raw-data/{table_var}_wk{week_num}.xlsx")

  if (!file.exists(filepath)) {
    # Download new week files
    download.file(data_url,
      destfile = filepath,
      mode = "wb",
      method = "libcurl"
    )
  }




  # Get all sheets in table, which should be all geographies
  sheets <- excel_sheets(filepath)

  # Read in all sheets as a long dataframe
  table_data <- map_df(sheets,
    # Need get() bc cleaning_fxn is character vector representation of the function
    get(cleaning_fxn),
    filepath = filepath
  )

  msa_transalation_list <-
    tribble(
      ~data_table_msa_name, ~msa_name,
      "Chicago_Metro_Area", "Chicago-Naperville-Elgin, IL-IN-WI",
      "Atlanta_Metro_Area", "Atlanta-Sandy Springs-Roswell, GA",
      "Boston_Metro_Area", "Boston-Cambridge-Newton, MA-NH",
      "Dallas_Metro_Area", "Dallas-Fort Worth-Arlington, TX",
      "Detroit_Metro_Area", "Detroit-Warren-Dearborn, MI",
      "Philadelphia_Metro_Area", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
      "Houston_Metro_Area", "Houston-The Woodlands-Sugar Land, TX",
      "Los.Angeles_Metro_Area", "Los Angeles-Long Beach-Anaheim, CA",
      "Miami_Metro_Area", "Miami-Fort Lauderdale-West Palm Beach, FL",
      "New.York_Metro_Area", "New York-Newark-Jersey City, NY-NJ-PA",
      "Phoenix_Metro_Area", "Phoenix-Mesa-Scottsdale, AZ",
      "Riverside_Metro_Area", "Riverside-San Bernardino-Ontario, CA",
      "San.Francisco_Metro_Area", "San Francisco-Oakland-Hayward, CA",
      "Seattle_Metro_Area", "Seattle-Tacoma-Bellevue, WA",
      "Washington.DC_Metro_Area", "Washington-Arlington-Alexandria, DC-VA-MD-WV"
    )

  table_data <- table_data %>%
    mutate(race_var = case_when(
      variable == "Hispanic or Latino (may be of any race)" ~ "hispanic",
      variable == "White alone, not Hispanic" ~ "white",
      variable == "Black alone, not Hispanic" ~ "black",
      variable == "Asian alone, not Hispanic" ~ "asian",
      variable == "Two or more races + Other races, not Hispanic" ~ "other",
      TRUE ~ variable
    )) %>%
    left_join(
      msa_transalation_list,
      by = c("geography" = "data_table_msa_name")
    ) %>%
    mutate(geography = case_when(
      str_detect(geography, "Metro_Area") ~ msa_name,
      TRUE ~ geography
    ))


  return(table_data)
}

CUR_WEEK <- 12
week_num <- 1:CUR_WEEK
week_num_spend <- 7:CUR_WEEK


test_within_0.001 <- function(vec1, vec2) {
  # Helper fxn that checks if vec1 and vec2 are equal (+- 0.001)

  if (dplyr::between(vec1, vec2 - 0.002, vec2 + 0.002)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# vectorize above function
test_within_0.001_v <- Vectorize(test_within_0.001)

# Construct week overlap intervals
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
  
  #reverse order and repeat each twice
  wk_int <- rev(interval_vec) %>% rep(each = 2)

  ### Create dataframe
  result <- tibble(
    week_num = wk_nums,
    week_int = wk_int
  )



  return(result)
}

week_crosswalk <- construct_overlap_intervals_df(CUR_WEEK)


#### ----Define and run tests------

# Declare lists of states, metros and metrics for randomized testing of a subset
all_states <- svy_rolling %>%
  select(state, cbsa_title) %>%
  pull(state) %>%
  unique() %>%
  na.omit()
all_metros <- svy_rolling %>%
  select(state, cbsa_title) %>%
  pull(cbsa_title) %>%
  unique() %>%
  na.omit()

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
  "depression_anxiety_signs",
  "stimulus_expenses", 
  "spend_credit", 
  "spend_ui", 
  "spend_stimulus", 
  "spend_savings")


### Check that data from data tables match rolling average estimates we calculated

check_food_insuff_numbers <- function(tables = "food2b", point_df = point_all, wknum = week_num) {

  # Generate pulse data table
  pulse_data_tables <- tables %>%
    map_df(generate_table_data, week_num = wknum) %>%
    select(geography, percent_food_insecure, week_num, race_var, total_food_insecure, total_answered) %>%
    pivot_longer(cols = percent_food_insecure, names_to = "metric", values_to = "mean") %>%
    right_join(week_crosswalk) %>%
    group_by(week_int, geography, race_var) %>%
    summarize(
      sum_food_insecure = sum(total_food_insecure),
      sum_total_answered = sum(total_answered),
      metric = "food_insufficient",
      mean = sum_food_insecure / sum_total_answered
    ) %>%
    ungroup() %>%
    rename(week_num = week_int)

  # Join to our data
  data_comparisons_by_race <- rolling_all %>%
    filter(metric == "food_insufficient") %>%
    left_join(pulse_data_tables, by = c("geography", "week_num", "race_var", "metric")) %>%
    mutate(
      mean.x = round(mean.x, 4),
      mean.y = round(mean.y, 4)
    )

  # Check that race-geography numbers match up (within 0.001 to account for rounding errors)
  ind_race_nums <- data_comparisons_by_race %>%
    filter(!is.na(mean.y))
  assert("Food Insufficiency race numbers match up", test_within_0.001_v(ind_race_nums$mean.x, ind_race_nums$mean.y))
}


check_rent_not_paid_numbers <- function(tables = "housing1b", point_df = point_all, wknum = week_num) {
  
  # Generate pulse data table
  pulse_data_tables <- tables %>%
    map_df(generate_table_data, week_num = wknum) %>%
    select(geography, perc_rent_not_paid, week_num, race_var, payment_no, total_answered, total_not_paid) %>%
    pivot_longer(cols = perc_rent_not_paid, names_to = "metric", values_to = "mean") %>%
    right_join(week_crosswalk) %>%
    group_by(week_int, geography, race_var) %>%
    summarize(
      sum_rent_not_paid = sum(total_not_paid),
      sum_total_answered = sum(total_answered),
      metric = "rent_not_paid",
      mean = sum_rent_not_paid / sum_total_answered
    ) %>%
    ungroup() %>%
    rename(week_num = week_int)
  
  # Join to our data
  data_comparisons_by_race <- rolling_all %>%
    filter(metric == "rent_not_paid") %>%
    left_join(pulse_data_tables, by = c("geography", "week_num", "race_var", "metric")) %>%
    mutate(
      mean.x = round(mean.x, 4),
      mean.y = round(mean.y, 4)
    )
  
  # Check that race-geography numbers match up (within 0.001 to account for rounding errors)
  ind_race_nums <- data_comparisons_by_race %>%
    filter(!is.na(mean.y))
  assert("rent not paid race numbers match up", test_within_0.001_v(ind_race_nums$mean.x, ind_race_nums$mean.y))
}
check_income_numbers <- function(tables = "employ1", point_df = point_all, wknum = week_num) {

  # Generate pulse data table
  pulse_data_tables <- tables %>%
    map_df(generate_table_data, week_num = wknum) %>%
    select(-perc_lost_income, -perc_lose_income, week_num, race_var, everything()) %>%
    right_join(week_crosswalk) %>%
    group_by(week_int, geography, race_var) %>%
    summarize(
      inc_loss = sum(income_loss_since_mar_13) / sum(total_answered_lost),
      expect_inc_loss = sum(income_loss_next_4_wks) / sum(total_answered_lose)
    ) %>%
    pivot_longer(cols = inc_loss:expect_inc_loss, names_to = "metric", values_to = "mean") %>%
    ungroup() %>%
    rename(week_num = week_int)


  # Join to our data
  data_comparisons_by_race <- rolling_all %>%
    filter(metric == "inc_loss" | metric == "expect_inc_loss") %>%
    left_join(pulse_data_tables, by = c("geography", "week_num", "race_var", "metric")) %>%
    mutate(
      mean.x = round(mean.x, 4),
      mean.y = round(mean.y, 4)
    )

  # Check that race-geography numbers match up (within 0.001 to account for rounding errors)
  ind_race_nums <- data_comparisons_by_race %>%
    filter(!is.na(mean.y))
  assert("Income Loss and Expected Income Loss race numbers match up", test_within_0.001_v(ind_race_nums$mean.x, ind_race_nums$mean.y))
}

check_stimulus_expenses_numbers <- function(tables = "stimulus1", point_df = point_all, wknum = week_num_spend) {
  
  # Generate pulse data table
  pulse_data_tables <- tables %>%
    map_df(generate_table_data, week_num = wknum) %>%
    select(geography, perc_used_expenses, week_num, race_var, used_expenses, total_answered) %>%
    pivot_longer(cols = perc_used_expenses, names_to = "metric", values_to = "mean") %>%
    right_join(week_crosswalk) %>%
    group_by(week_int, geography, race_var) %>%
    summarize(
      sum_stimulus_expenses = sum(used_expenses),
      sum_total_answered = sum(total_answered),
      metric = "stimulus_expenses",
      mean = sum_stimulus_expenses / sum_total_answered
    ) %>%
    ungroup() %>%
    rename(week_num = week_int) %>%
    filter(week_num %in% c("wk7_8", "wk8_9", "wk9_10", "wk10_11", "wk11_12"))
  
  # Join to our data
  data_comparisons_by_race <- rolling_all %>%
    filter(metric == "stimulus_expenses") %>%
    filter(week_num %in% c("wk7_8", "wk8_9", "wk9_10", "wk10_11", "wk11_12")) %>%
    left_join(pulse_data_tables, by = c("geography", "week_num", "race_var", "metric")) %>%
    mutate(
      mean.x = round(mean.x, 4),
      mean.y = round(mean.y, 4)
    )
  
  # Check that race-geography numbers match up (within 0.001 to account for rounding errors)
  ind_race_nums <- data_comparisons_by_race %>%
    filter(!is.na(mean.y))
  assert("stimulus expenses numbers match up", test_within_0.001_v(ind_race_nums$mean.x, ind_race_nums$mean.y))
}


# TODO: More Checks
check_food_insuff_numbers()
check_income_numbers()
check_rent_not_paid_numbers()
check_stimulus_expenses_numbers()

### Check that SE from doing regressions matching SE we get using svyby and svycontrast
# Check Standard Errors for black inc_loss in wk1_2 in Atlanta

check_glm_se_match <- function(week_int, geo, race_ind, metr, se_df = all_diff_ses, svy = svy_rolling) {
  sd_calcs <- se_df %>%
    filter(
      week == week_int,
      geography == geo,
      race_indicator == race_ind,
      metric == metr
    ) %>%
    select(
      estimate = diff_mean,
      std_error = diff_se
    ) %>%
    mutate(
      estimate = estimate %>% abs() %>% round(4),
      std_error = std_error %>% round(4)
    )

  glm_formula <- as.formula(paste0(metr, " ~ ", race_ind))

  if (nchar(geo) == 2) {
    # if length geo = 2, then this is a state so filter to that state
    glm_calcs <- svyglm(glm_formula,
      svy %>%
        filter(week_num == week_int) %>%
        filter(state == geo),
      na.action = "na.omit"
    ) %>%
      summary() %>%
      coef() %>%
      # pull estimate and SE of the black coefficient from the regression. This
      # should match the diff mean and diff SE from the all_diff_ses object
      as_tibble() %>%
      janitor::clean_names() %>%
      slice(2) %>%
      select(estimate, std_error) %>%
      mutate(
        std_error = round(std_error * 2, 4),
        estimate = round(estimate, 4)
      )
  } else {
    # else its a metro, so filter to that metro
    glm_calcs <- svyglm(glm_formula,
      svy %>%
        filter(week_num == week_int) %>%
        filter(cbsa_title == geo),
      na.action = "na.omit"
    ) %>%
      summary() %>%
      coef() %>%
      # pull estimate and SE of the black coefficient from the regression. This
      # should match the diff mean and diff SE from the all_diff_ses object
      as_tibble() %>%
      janitor::clean_names() %>%
      slice(2) %>%
      select(estimate, std_error) %>%
      mutate(
        std_error = round(std_error * 2, 4),
        estimate = round(estimate, 4)
      )
  }


  testit::assert(all_equal(glm_calcs, sd_calcs))

  return(TRUE)
}

# Construct random list of 10 ge/race/metric/week combinations to test
random_test_list <- tibble(
  # replace with last two weeks 
  week_int = sample(c("wk10_11", "wk11_12"), size = 10, replace = TRUE),
  geo = c(sample(all_states, 7), sample(all_metros, 3)),
  race_ind = sample(c("black", "asian", "hispanic", "other"), 10, replace = TRUE),
  metr = sample(metrics, 10, replace = TRUE)
)

se_glm_test_results <- random_test_list %>% pmap_lgl(check_glm_se_match)


### Check that SE calculations match manual calculations using Census formulas

compute_replicate_diff <- function(mean_obj1, mean_obj2) {
  # function to compare means and SE from a svymean object using Census formulas
  replicates_1 <- mean_obj1$replicates
  replicates_2 <- mean_obj2$replicates

  theta_i <- replicates_1 - replicates_2
  theta_hat <- mean_obj1$mean %>%
    as.numeric() - mean_obj2$mean %>%
    as.numeric()
  var_theta <- 4 / 80 * sum((theta_i - theta_hat)^2)
  se_diff <- sqrt(var_theta)
  return(c(mean = theta_hat, se = se_diff))
}

test_against_manual <- function(svy = svy_rolling, data = all_diff_ses, metric_name, wk_num, race_name,
                                geo_name, geo_col) {
  # Function to test means, SEs and significance from data versus manually
  # calcaulted mean, SEs and significance values from svy. Us
  # INPUT:
  #   svy: Should be = svy_rolling, aka the raw survey dataset
  #    where SE/mean calculations will be done manually
  #   data: SHould be = all_diff_ses, aka the full SE file created at
  #     end of script 2
  #   metric_name: name of the metric to test
  #   wk_num: name of week interval to test (ie wk1_2)
  #   race_name: name of race indicator to test (ie black)
  #   geo_name: name of geography to test (ie CA or San Francisco....)
  #   geo_col: Name of geography column, should be state for states and
  #     cbsa_title for metros

  metric_formula <- as.formula(paste0("~", metric_name))

  tot <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num) %>%
    filter(!!sym(geo_col) == geo_name),
  na.rm = T, return.replicates = TRUE
  )
  replicates_total <- tot$replicates

  nr <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num) %>%
    filter(!!sym(race_name) != 1) %>%
    filter(!!sym(geo_col) == geo_name),
  na.rm = T, return.replicates = TRUE
  )
  replicates_not_race <- nr$replicates

  r <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num) %>%
    filter(!!sym(race_name) == 1) %>%
    filter(!!sym(geo_col) == geo_name),
  na.rm = T, return.replicates = TRUE
  )
  replicate_race <- r$replicates


  params_race_notrace <- compute_replicate_diff(r, nr)
  mean_race_notrace <- params_race_notrace[["mean"]] %>% round(8)
  se_race_notrace <- params_race_notrace[["se"]] %>% round(8)
  tstat_race_notrace <- mean_race_notrace / se_race_notrace

  params_race_total <- compute_replicate_diff(tot, r)
  mean_race_total <- params_race_total[["mean"]] %>% round(8)
  se_race_total <- params_race_total[["se"]] %>% round(8)
  tstat_race_total <- mean_race_total / se_race_total

  ra_stats <- data %>%
    filter(week == wk_num) %>%
    filter(race_indicator == race_name) %>%
    filter(metric == metric_name) %>%
    filter(geography == geo_name) %>%
    select(diff_mean, diff_se)

  ras_mean <- ra_stats$diff_mean %>%
    as.numeric() %>%
    round(8)
  ra_se <- ra_stats$diff_se %>%
    as.numeric() %>%
    round(8)
  tstat_rr_signif <- abs(tstat_race_notrace) > 1.96
  tstat_rt_signif <- abs(tstat_race_total) > 1.96
  is_same_signif <- tstat_rr_signif == tstat_rt_signif

  assert("means are equal", abs(ras_mean) == abs(mean_race_notrace))
  assert("SEs are equal", ra_se == se_race_notrace)

  comparison_groups_df <- tribble(
    ~comparison_group, ~mean, ~se, ~tstat,
    "total_minus_subgroup", abs(mean_race_notrace), se_race_notrace, abs(tstat_race_notrace),
    "total", mean_race_total, se_race_total, tstat_race_total
  )

  return(comparison_groups_df)
}

test_against_manual_us <- function(svy = svy_rolling, data = us_diff_ses, metric_name, wk_num, race_name) {
  # function to test against manual calculations for whole US

  metric_formula <- as.formula(paste0("~", metric_name))

  tot <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num),
  na.rm = T, return.replicates = TRUE
  )
  replicates_total <- tot$replicates

  nr <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num) %>%
    filter(!!sym(race_name) != 1),
  na.rm = T, return.replicates = TRUE
  )
  replicates_not_race <- nr$replicates

  r <- svymean(metric_formula, svy %>%
    filter(week_num == wk_num) %>%
    filter(!!sym(race_name) == 1),
  na.rm = T, return.replicates = TRUE
  )
  replicate_race <- r$replicates


  params_race_notrace <- compute_replicate_diff(r, nr)
  mean_race_notrace <- params_race_notrace[["mean"]] %>% round(8)
  se_race_notrace <- params_race_notrace[["se"]] %>% round(8)
  tstat_race_notrace <- mean_race_notrace / se_race_notrace

  params_race_total <- compute_replicate_diff(tot, r)
  mean_race_total <- params_race_total[["mean"]] %>% round(8)
  se_race_total <- params_race_total[["se"]] %>% round(8)
  tstat_race_total <- mean_race_total / se_race_total

  ra_stats <- data %>%
    filter(week == wk_num) %>%
    filter(race_indicator == race_name) %>%
    filter(metric == metric_name) %>%
    filter(geography == "US") %>%
    select(diff_mean, diff_se)

  ras_mean <- ra_stats$diff_mean %>%
    as.numeric() %>%
    round(8)
  ra_se <- ra_stats$diff_se %>%
    as.numeric() %>%
    round(8)
  tstat_rr_signif <- abs(tstat_race_notrace) > 1.96
  tstat_rt_signif <- abs(tstat_race_total) > 1.96

  assert("Means are same", abs(ras_mean) == abs(mean_race_notrace))
  assert("SEs are same", ra_se == se_race_notrace)

  comparison_groups_df <- tribble(
    ~comparison_group, ~mean, ~se, ~tstat,
    "total_minus_subgroup", abs(mean_race_notrace), se_race_notrace, abs(tstat_race_notrace),
    "total", mean_race_total, se_race_total, tstat_race_total
  )

  return(comparison_groups_df)
}


# Note that SE and means slightly differ when comparing subgroup to total vs subgroup to (total - subgroup).
# This could be due to unaccounted covariance between subgroup and total estimates in the replicate weights.
# We use and report the SE for difference bw subgroup and (total-subgroup)

random_test_list_manual <- tibble(
  metric_name = sample(metrics, 10, replace = TRUE),
  # replace with last two weeks
  wk_num = sample(c("wk10_11", "wk11_12"), size = 10, replace = TRUE),
  race_name = sample(c("black", "asian", "hispanic", "other", "white"), 10, replace = TRUE),
  geo_name = c(sample(all_states, 7), sample(all_metros, 3)),
  geo_col = c(rep("state", 7), rep("cbsa_title", 3))
)

# Test manual calculations for specific geographies and all US
se_manual_calc_test_results <- random_test_list_manual %>% pmap_df(test_against_manual)

random_test_list_us = tibble(
   metric_name = sample(metrics, 10, replace = TRUE),
   #replace last two weeks
   wk_num = sample(c("wk10_11", "wk11_12"), size = 10, replace = TRUE),
   race_name = sample(c("black", "asian", "hispanic", "other", "white"), 10, replace = TRUE),
 )

se_manual_calc_test_us_results = random_test_list_us %>% pmap_df(test_against_manual_us)
