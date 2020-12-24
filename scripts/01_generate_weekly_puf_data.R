# Generate cleaned Public Use File for Data Catalog and use in feature
library(tidyverse)
library(readxl)
library(testit)
library(tigris)
library(stringr)
library(tidyverse)
library(httr)
library(here)
library(janitor)

download_and_clean_puf_data <- function(week_num, output_filepath = "data/raw-data/public_use_files/") {
  # Function to download in Pulse Public Use File for a given week, and add:
  #   1) cleaned Non Hispanic Race variable, (hisp_rrace)
  #   2) MSA and state names (cbsa_title  and state_name)
  #   3) a week number variable (week_num)
  #   4) indicator variable for uninsured persons (uninsured)
  #   5) indicator variable for publicly insured persons (insured_public)
  #   6) indicator variable for if a person has experienced loss in employment income (inc_loss)
  #   7) indicator variable for if a person expects a loss in employment income (expect_inc_loss)
  #   8) indicator variable for if a person has low to no confidence in paying rent next month or has already deferred (rent_not_conf)
  #   9) indicator variable for if a person has low to no confidence in paying mortgage next month or has already deferred (mortgage_not_conf)
  #   10)joint indicator variable for if a person has either 8 or 9, (payment_not_conf)
  #   11)adjusted score columns for the mental health questions
  #   12)indicator variable for if a person displays signs of anxiety (anxiety_signs)
  #   13)indicator variable for if a person displays signs of depression (depression_signs)
  #   14) indicator variable for if a person is caught up on rent payments (rent_caughtup)
  #   15) indicator variable for if a person is caught up on mortgage payments (mortgage_caughtup)
  #   16) indicator variable for if a person used SNAP to meet spending needs in past 7 days (spend_snap)
  #   17) indicator variable for if a person used credit cards or loans to meet spending needs in past 7 days (spend_credit)
  #   18) indicator variable for if a person used savings to meet spending needs in past 7 days (spend_savings)
  #   19) indicator variable for if a person used UI benefits to meet spending needs in past 7 days (spend_ui)
  #   20) indicator variable for if a person used stimulus payment to meet spending needs in past 7 days (spend_stimulus)
  #   21) indicator variable for if a person not caught up on rent is somewhat likely or very likely to be evicted in next two months (eviction_risk)
  #   22) indicator variable for if a person not caught up on mortgage is somewhat likely or very likely to be foreclosed on in next two months (foreclosure_risk)


  # INPUT:
  #   week_num (num): week number of Pulse survey (ie 1, 2,... 12, etc). Can be a vector if
  #     you want to pull data for multiple weeks
  #   output_filepaths (chr): Output folder where puf file and data dictionary
  #     be written to
  # OUPUT:
  #   df_clean: cleaned pubilc use file
  #   This fxn will also write out the raw downlaoded public use files into the data/raw-data directory

  week_num_padded <- str_pad(week_num, width = 2, side = "left", pad = "0")


  puf_url <- str_glue("https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk{week_num}/HPS_Week{week_num_padded}_PUF_CSV.zip")

  # Create public_use_files directory if it doesn't exist
  dir.create("data/raw-data/public_use_files/", showWarnings = F)

  # Download zip file
  if(!file.exists(str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"))){
    download.file(puf_url,
                  destfile = str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"),
                  # By default uses winnet method which is for some reason very slow
                  method = "libcurl"
    )

  }
  
  #Note: data dictionaries change naming conventions within phase 2 due to December
  # update, we accordingly default to extracting all files without names
  unzip(str_glue("data/raw-data/public_use_files/week_{week_num_padded}.zip"),
        exdir = "data/raw-data/public_use_files")
  
  # Get MSA FIPS Codes for appending later
  fips_msa_url <- "https://query.data.world/s/vn4chhniqhslgt5fpb7swxbkcsq3oj"
  GET(fips_msa_url, write_disk(tf <- tempfile(fileext = ".xls")))
  msa_fips_codes <- read_excel(tf, skip = 2) %>%
    select("CBSA Code", "CSA Title", "CBSA Title") %>%
    # There seem to be some duplicate entries, so we remove them
    distinct(.keep_all = TRUE)

  ### Read in PUF file and weights file
  puf_filepath <- str_glue("{output_filepath}pulse2020_puf_{week_num_padded}.csv")
  rep_wt_filepath <- str_glue("{output_filepath}pulse2020_repwgt_puf_{week_num_padded}.csv")
  df <- read_csv(puf_filepath)
  rep_wt <- read_csv(rep_wt_filepath) %>%
    janitor::clean_names()

  # add missing variables for weeks before 7
  if (week_num < 7) {
    df <- df %>%
      mutate(eip = NA_real_,
             spndsrc1 = NA_real_,
             spndsrc2 = NA_real_,
             spndsrc3 = NA_real_,
             spndsrc4 = NA_real_,
             spndsrc5 = NA_real_,
             spndsrc6 = NA_real_,
             spndsrc7 = NA_real_)
  }  


  df_clean <- df %>%
    janitor::clean_names() %>%
    ### Append Urban specific columns
    mutate(
      # Generate hispanic/non-hispanic race variables
      hisp_rrace = case_when(
        rrace == 1 ~ "White alone, not Hispanic",
        rrace == 2 ~ "Black alone, not Hispanic",
        rrace == 3 ~ "Asian alone, not Hispanic",
        rrace == 4 ~ "Two or more races + Other races, not Hispanic",
        TRUE ~ NA_character_
      ),
      # Note: hispanic ethnicity overrides all other race variables
      hisp_rrace = case_when(
        rhispanic == 1 ~ hisp_rrace,
        rhispanic == 2 ~ "Hispanic or Latino (may be of any race)",
        TRUE ~ hisp_rrace
      ),
      # Dummy var for uninsured respondents
      uninsured = case_when(
        # Note the order of these case_when statements matter
        # First if they have any of the first 6 healath insurance options, they are insured (0)
        hlthins1 == 1 |
          hlthins2 == 1 |
          hlthins3 == 1 |
          hlthins4 == 1 |
          hlthins5 == 1 |
          hlthins6 == 1 ~ 0,
        # If they answer no to all 6 health insurance quetions, they are uninsured (1)
        (hlthins1 == 2 &
          hlthins2 == 2 &
          hlthins3 == 2 &
          hlthins4 == 2 &
          hlthins5 == 2 &
          hlthins6 == 2) ~ 1,
        # Note Census includes those who have Indian Health Service coverage as uninsured (1)
        hlthins7 == 1 ~ 1,

        TRUE ~ NA_real_
      ),
      # Dummy var for respondents with public insurance
      insured_public = case_when(
        # if they have medicare, medicaid, or VA insurance = 1
        hlthins3 == 1 | hlthins4 == 1 | hlthins6 == 1 ~ 1,

        # if they didn't answer any of the helath insurance questions& assign them NA
        hlthins1 == -99 & hlthins2 == -99 & hlthins3 == -99 &
          hlthins4 == -99 & hlthins5 == -99 & hlthins6 == -99 &
          hlthins7 == -99 & hlthins8 == -99 ~ NA_real_,

        hlthins1 == -88 & hlthins2 == -88 & hlthins3 == -88 &
          hlthins4 == -88 & hlthins5 == -88 & hlthins6 == -88 &
          hlthins7 == -88 & hlthins8 == -88 ~ NA_real_,
        # If they answered at least one question but were not publicly insured, mark as 0
        TRUE ~ 0
      ),
      # Dummy var for lost income in past 4 weeks? (1 = lost income, 0 = didn't lose income)
      inc_loss = case_when(
        wrkloss == 1 ~ 1,
        wrkloss == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for future Employment income loss
      expect_inc_loss = case_when(
        expctloss == 1 ~ 1,
        expctloss == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for not confident in housing payment next month? (1 = yes, 0 = no)
      payment_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for not confident in rent payment next month (1 = yes, 0 = no)
      rent_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) & tenure == 3 ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) & tenure == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      mortgage_not_conf = case_when(
        # slight or no confidnece or payment already deferred = 1
        mortconf %in% c(1, 2, 5) & tenure == 2 ~ 1,
        # moderate or high confidence = 0
        mortconf %in% c(3, 4) & tenure == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var caught up on rent (1 = yes, 0 = no)
      rent_caughtup = case_when(
        # caught up on rent payments = 1
        rentcur == 1 & tenure == 3 ~ 1,
        # not caught up on rent payments = 0
        rentcur == 2 & tenure == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for caught up on mortage (1 = yes, 0 = no)
      mortgage_caughtup = case_when(
        # caught up on mortgage payments = 1
        mortcur == 1 & tenure == 2 ~ 1,
        # not caught up on mortgage payments = 0
        mortcur == 2 & tenure == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      # Dummy var for Food Insufficient households
      food_insufficient = case_when(
        curfoodsuf %in% c(3, 4) ~ 1,
        curfoodsuf %in% c(1, 2) ~ 0,
        TRUE ~ NA_real_
      ),
      spend_credit = as.numeric(case_when(
        #set 1 if respondent answered they use credit cards or loans
        spndsrc2 == 1 ~ 1,
        # Set 0 if respondent answered atleast one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_savings = as.numeric(case_when(
        #set 1 if respondent answered they use savings or selling assets
        spndsrc3 == 1 ~ 1,
        # Set 0 if respondent answered at least one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_ui = as.numeric(case_when(
        #set 1 if respondent answered they use creditcards or loans
        spndsrc5 == 1 ~ 1,
        # Set 0 if respondent answered at least one of the spending questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      spend_stimulus = as.numeric(case_when(
        #set 1 if respondent answered they use creditcards or loans
        spndsrc6 == 1 ~ 1,
        # Set 0 if respondent answered atleast one of the child education questions
        (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | 
           spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0| spndsrc8 >= 0) ~ 0,
        # Set NA otherwise
        TRUE ~ NA_real_
      )),
      # Score variables for mental health qs. Note we use scoring scheme laid out here:
      # https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm which requires recoding
      # the 4 mental health questions to thier specific scores. If sum of sets of 2 qs
      # are greater than 3, that means the respondent has a sign of anxiety or depression
      anxious_score = case_when(
        anxious == 1 ~ 0,
        anxious == 2 ~ 1,
        anxious == 3 ~ 2,
        anxious == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      worry_score = case_when(
        worry == 1 ~ 0,
        worry == 2 ~ 1,
        worry == 3 ~ 2,
        worry == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      interest_score = case_when(
        interest == 1 ~ 0,
        interest == 2 ~ 1,
        interest == 3 ~ 2,
        interest == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      down_score = case_when(
        down == 1 ~ 0,
        down == 2 ~ 1,
        down == 3 ~ 2,
        down == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      #difficulty paying household expenses in past 7 days
      expense_dif= case_when(
      expns_dif >= 3 ~ 1,
      expns_dif %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
      ),
      #dummy for telework
      telework= case_when(tw_start == 1 ~ 1,
                          tw_start %in% c(2, 3) ~ 0,
                          TRUE ~ NA_real_
      ),
      #dummy for unmet need for mental health services in last 4 weeks
      mentalhealth_unmet= case_when(mh_notget == 1 ~ 1,
                                    mh_notget == 2 ~ 0,
                                    TRUE ~ NA_real_
      ),
      #dummy for eviction risk
      # this question was asked of everyone who answered rentcur == 2 and tenure == 3,
      # or that they are renters and they are not caught up on rent. 
      eviction_risk = case_when(evict %in% c(1, 2) ~ 1,
                                evict %in% c(3, 4) ~ 0,
                                TRUE ~ NA_real_
      ),
      #dummy for foreclosure risk
      # this question was asked of everyone who answered mortcur == 2 and tenure == 2,
      # or that they pay mortgages and are not caught up on mortgages. 
      foreclosure_risk = case_when(forclose %in% c(1, 2) ~ 1,
                                   forclose %in% c(3, 4) ~ 0,
                                   TRUE ~ NA_real_
      ),
      #dummy for Proportion of adults with children in school who spend fewer 
      #hours on learning activities in the past 7 days relative to before the pandemic
      learning_fewer= case_when(tch_hrs %in% c(1, 2) ~ 1,
                                tch_hrs >= 3 ~ 0,
                                TRUE ~ NA_real_
      ),
      #SNAP spending
      spend_snap = case_when(spndsrc8 == 1 ~ 1,
      (spndsrc1 >= 0 | spndsrc2 >= 0 | spndsrc3 >= 0 | spndsrc4 >= 0 | spndsrc5 >= 0 | spndsrc6 >= 0 | spndsrc7 >= 0 | spndsrc8 >= 0) ~ 0
      )
    ) %>%
    # Needed for rowwise sum calculations in anxiety_signs and depression_signs var
    rowwise() %>%
    mutate(
      # Dummy var for sign of anxiety (based on >=3 score)
      anxiety_signs = case_when(
        sum(anxious_score, worry_score, na.rm = T) >= 3 ~ 1,
        is.na(anxious_score) & is.na(worry_score) ~ NA_real_,
        TRUE ~ 0
      ),
      # Dummy var for sign of depression (based on >=3 score)
      depression_signs = case_when(
        sum(interest_score, down_score, na.rm = T) >= 3 ~ 1,
        is.na(interest_score) & is.na(down_score) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Dummy Var for any signs of anxiety/depression
      depression_anxiety_signs = case_when(
        # Set 1 if respondent has either anxiety signs or depression sigs
        anxiety_signs == 1 | depression_signs == 1 ~ 1,
        is.na(anxiety_signs == 1) & is.na(depression_signs) ~ NA_real_,
        TRUE ~ 0
      ),
      # Turn MSA column into character
      est_msa = as.character(est_msa),
      # Add week_number column
      week_num = paste0("wk", week_num)
    ) %>%
    ### Append full state names
    left_join(tigris::fips_codes %>% select(state, state_code, state_name) %>%
      distinct(state_code, .keep_all = TRUE),
    by = c("est_st" = "state_code")
    ) %>%
    ### Append MSA Names
    left_join(msa_fips_codes, by = c("est_msa" = "CBSA Code")) %>%
    ### Append Replicate Weights
    left_join(rep_wt, by = "scram") %>%
    janitor::clean_names()

  # Check that cleanded data has same number of rows as raw data
  assert("Cleaned df has same # of rows as raw data", nrow(df) == nrow(df_clean))

  return(df_clean)
}

calculate_response_rate_metrics <- function(df_clean) {
  # Function to calculate the following response rate metrics:
  #   1) rr_out: The proportion of racial group respondents who responded to
  #      each question (ie 75% of black survey takers responded to Question X in
  #      Week Y)
  #   2) job_loss_out: the proportion of respondents who that did and did not answer the question(s)
  #      answered that at least one member of their household had lost employment income since March 13.
  #      (ie 75% of survey respondents who answered Question X in Week Y responded that at least one member
  #      of their household had lost employment income since March 13 compared to 70% of survey respondents
  #      who didn't answer Question X in Week Y). We choose this metric because the overall item response 
  #      for this metric is very high (> 99%) though some respondents did not answer.
  #   3) prop_resp_by_race: the racial breakdown of respondents who answered
  #      each question  (ie 30% of survey takers who responded to Question X
  #      were black in Week Y)
  #   For metrics where all respondents answer the questions
  #   (metrics_no_elig) we use the question(s) that are used to calculate the
  #   metric. Where only certain respondents receive the questions
  #   (learning_fewer, rent_not_conf, mortgage_not_conf, rent_caughtup,
  #   mortgage_caughtup, eviction_risk, foreclosure_risk) we use the response
  #   rate to the question that determines eligibility to receive the
  #   question(s) used to calculate the metric to approximate response rate
  #
  # INPUTS:
  #   df_clean: dataframe output from download_and_clean_puf_data() function
  # OUTPUTS:
  #   list of dataframes with response rate metrics: rr_out, job_loss_out, prop_resp_by_race
  
  # Look into var: learning_fewer, using enrollment variable as proxy
  metrics_no_elig <- c(
    "uninsured",
    "insured_public",
    "inc_loss",
    "expect_inc_loss",
    "food_insufficient",
    "depression_anxiety_signs",
    "spend_credit", 
    "spend_ui", 
    "spend_stimulus", 
    "spend_savings",
    "spend_snap",
    "telework",
    "mentalhealth_unmet",
    "expense_dif"
  )
  
  answered_df <- df_clean %>% 
    mutate(across(metrics_no_elig, ~if_else(is.na(.), 0, 1), .names = "answered_{.col}"),
           # used to approximate response rate for rent_not_conf, mortgage_not_conf, rent_caughtup,
           #   mortgage_caughtup, eviction_risk, foreclosure_risk
           answered_tenure = if_else(tenure > 0, 1, 0),
           # used to approximate response rate for learning_fewer
           # the rr for enroll is very low because many respondents without
           # school age kids may skip this question. Some that didn't answer this
           # question (both coded -88 and -99) went on to answer subsequent questions.
           # 
           # When we exclude households without children under 18 by requiring thhld_numkid > 0,
           # the response rate for enroll is between 80-81% in weeks 13-15. However, there are some
           # respondents where thhld_numkid == 0 who do answer the enroll question, perhaps either because
           # they have a member of the household over 18 in school, or to answer "no" (enroll3 == 1).
           # Because households without children are in the universe of respondents, we decide to keep them
           # in the denominator for purposes of response rate, but recognize that this is an imperfect metric.
           answered_enroll = case_when(enroll1 > 0 | enroll2 > 0 | enroll3 > 0 ~ 1,
                                       TRUE ~ 0)) 
  

  # This calculates the racial breakdown of people who answered each of the
  # questions 
  prop_resp_by_race <- answered_df %>%
    # Add in answered_hisp_rrace to get overall survey prop resp by race
    mutate(answered_hisp_rrace = ifelse(is.na(hisp_rrace), 0, 1)) %>%
    select(week_num, hisp_rrace, starts_with("answered")) %>%
    pivot_longer(!c("hisp_rrace", "week_num"), names_to = "metric", values_to = "answered") %>%
    group_by(week_num, metric, hisp_rrace) %>%
    summarise(across(starts_with("answered"), ~sum(.x, na.rm = TRUE))) %>%
    mutate(across(starts_with("answered"), ~.x/sum(.x, na.rm =  TRUE))) %>%
    pivot_wider(names_from = metric, values_from = answered)
  
  rr_by_race <- answered_df %>%
    group_by(week_num, hisp_rrace) %>%
    summarise(across(starts_with("answered"), ~mean(.x, na.rm = TRUE)))
  
  rr_total <- answered_df %>%
    group_by(week_num) %>%
    summarise(across(starts_with("answered"), ~mean(.x, na.rm = TRUE))) %>%
    mutate(hisp_rrace = "Total")
  
  rr_out <- rbind(rr_by_race, rr_total)

  
  job_loss_non_answer_race <- answered_df %>%
    filter(!is.na(inc_loss)) %>%
    select(week_num, hisp_rrace, inc_loss, starts_with("answered")) %>%
    pivot_longer(!c("hisp_rrace", "week_num", "inc_loss"), names_to = "metric", values_to = "answered") %>%
    group_by(week_num, metric, hisp_rrace, answered) %>%
    summarise(inc_loss_pct = mean(inc_loss, na.rm = TRUE)) %>%
    pivot_wider(names_from = metric, values_from = inc_loss_pct)
  
  job_loss_non_answer_all <- answered_df %>%
    filter(!is.na(inc_loss)) %>%
    select(week_num, inc_loss, starts_with("answered")) %>%
    pivot_longer(!c("week_num", "inc_loss"), names_to = "metric", values_to = "answered") %>%
    group_by(week_num, metric, answered) %>%
    summarise(inc_loss_pct = mean(inc_loss, na.rm = TRUE)) %>%
    mutate(hisp_rrace = "Total") %>%
    pivot_wider(names_from = metric, values_from = inc_loss_pct)
  
  job_loss_out <- rbind(job_loss_non_answer_race, job_loss_non_answer_all)
  
  return(list(rr_out, job_loss_out, prop_resp_by_race))
  
}


CUR_WEEK <- 19
week_vec <- c(13:CUR_WEEK)

# Read in all PUF files for the specified weeks, and write out one big PUF file. There will be a column named
# week_num that differentiates microdata from each week.

puf_all_weeks <- map_df(week_vec, download_and_clean_puf_data)
metric_list <- calculate_response_rate_metrics(puf_all_weeks)
rr_out <- metric_list[[1]]
job_loss_out <- metric_list[[2]]
prop_resp_race_out <- metric_list[[3]]

# Create public_use_files directory if it doesn't exist
dir.create("data/intermediate-data", showWarnings = F)

write_csv(puf_all_weeks, str_glue("data/intermediate-data/pulse_puf2_week_13_to_{CUR_WEEK}.csv"))
# Write out most recent CSV
write_csv(puf_all_weeks, here("data/intermediate-data", "pulse_puf2_all_weeks.csv"))

write_csv(rr_out, here("data/intermediate-data", "pulse2_rr_metrics_race_all.csv"))
write_csv(job_loss_out, here("data/intermediate-data", "pulse2_rr_metrics_job_loss_all.csv"))
write_csv(prop_resp_race_out, here("data/intermediate-data", "pulse2_response_by_race_all.csv"))


# Manually generate and write out data dictionary for appended columns
appended_column_data_dictionary <-
  tibble::tribble(
    ~col_name, ~description,
    "hisp_rrace", "Combination of Hispanic and Race column. Groups respondents into the following categories: Hispanic, White non Hispanic, Black non Hispanic, Asian non Hispanic, and Other race/two or more races",
    "uninsured", "Indicator variable for if a respondent is uninsured. This is 1 if the respondent reported that they have none of the available insurnace ooptions or only have insurance through the Indian Health Service. It is 0 if the respondents have some type of health insurance (excluding the Indian Health service)",
    "insured_public", "Indicator variable for if a respondent has public insurance. This is 1 if the respondent reported thaty had Medicare, Medicaid, or VA Health Insurance",
    "inc_loss", "Indicator variable for if a respondent (or anyone in their houshold) experienced a loss in employment income since March 13, 2020. This is essentially a recoding of the wrkloss variable with -88 and -99 coded as NA, 1 coded as 1 and 2 coded as 0",
    "expect_inc_loss", "Indicator variable for if a respondent (or anyone in their household) expects to experience a loss in employment income in the next 4 weeks due to the coronavirus. this is essentially a recoding of the expctloss variable with -88 and -99 coded as NA, 1 coded as 1 and 2 coded as 0",
    "payment_not_conf", "Indicator variable for if a respondent has little or no confidence in paying rent/mortgage next month or has already deferred payment for next months rent/mortgage. Note this excludes people who oen their homes free and clear or occupy thier house without payment of rent. They are coded as 1 if mortconf is  1,2 or ; s 0 if mortconf is 3 or 4; and NA otherwise",
    "rent_not_conf", "Indicator variable for if a respondent has little or no confidence in paying thier rent next month or has already deferred. This is a limited to renters (ie tenure ==3)",
    "mortgage_not_conf", "Indicator variable for if a respondent has little or no confidence in paying thier mortgage next month or has already deferred. This is a limited to owners paying mortgage (ie tenure ==2)",
    "rent_caughtup", "Indicator variable for if a respondent's household is currently caught up on rent. This is a limited to renters (ie tenure ==3)",
    "mortgage_caughtup", "Indicator variable for if a respondent's household is currently caught up on mortage. This is a limited to owners paying mortgage (ie tenure ==2)",
    "food_insufficient", "Indicator variable for if a respondents household has sometimes or often had not enough to eat in the last 7 days. This is essentially a recoding of the curfoodsuff variable where 3 and 4 are coded as 1, 1 and 3 are coded as 0, and -88 and -99 are coded as NA",
    "spend_savings", "Indicator variable for if a respondent reported using money from savings or selling assets in last 7 days to meet spending needs",
    "spend_credit", "Indicator variable for if a respondent reported using money from credit cards or loans in last 7 days to meet spending needs",
    "spend_ui", "Indicator variable for if a respondent reported using money from unemployment insurance (UI) benefit payments in last 7 days to meet spending needs",
    "spend_stimulus", "Indicator variable for if a respondent reported using money from stimulus (economic impact) payment in last 7 days to meet spending needs",
    "anxious_score", "A recoding of the anxious variable to correctly reflect the numerical scores used to determine symptoms of generalized anxiety disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "worry_score", "A recoding of the worry variable to correctly reflect the numerical scores used to determine symptoms of generalized anxiety disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "interest_score", "A recoding of the interest variable to correctly reflect the numerical scores used to determine symptoms of major depresive disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "down_score", "A recoding of the worry variable to correctly reflect the numerical scores used to determine symptoms of major depressive disorder. Specifically not at all = 0, several days = 1, more than half the days = 2, and nearly every day = 3",
    "anxiety_signs", "Indicator variable for if the respondent is showing signs of generalized anxiety disorder. This is coded as 1 if the sum of anxious_score and worry_score is >= 3. Respondents with missing responses to both questions are coded as NA and 0 otherwise",
    "depression_signs", "Indicator variable for if the respondent is showing signs of major depressive disroder. This is coded as 1 if the sum of down_score and interest_score is >= 3.  Respondents with missing responses to both questions are coded as NA and 0 otherwise",
    "depression_anxiety_signs", " Indicator variable if the respondent is showing either signs of major depressive disorder or generalized anxiety disorder. Respondents with missing responses to both anxiety_signs and depression_signs are coded as NA",
    "expense_dif", "Indicator variable for if a respondent reported difficulty for their household to pay for usual household expense n the last 7 days ",
    "telework", "Indicator variable for if at least one adult in this household substitutes some or all of their typical in-person work for telework because of the coronavirus pandemic",
    "metalhealth_unmet", "Indicator variable for if respondent needed but did not get counseling or therapy from a mental health professional in the past 4 weeks, for any reason",
    "eviction_risk", "Indicator variable for if the household will very likely or extremely likely have to leave this home or apartment within the next two months because of eviction. ",
    "foreclosure_risk", "Indicator variable for if the houeshold will very likely or extremely likely have to leave this home within the next two months because of foreclosure",
    "learning_fewer", "Indicator variable for if the student(s) in the household spend less time on all learning activities relative to a school day before the coronavirus pandemic during the last 7 days ",
    "spend_snap", "Indicator variable for if household members are using SNAP to meet their spending needs in the past 7 days",
    "week_num", "The week number that the survey data is from",
    "state", "2 digit abbrevation of the state that respondents are from",
    "state_name", "The full name of the state that respondents are from",
    "csa_title", "The name of the larger Combined statistical area that the respondent is from. Note the Census only reports the Metropolitan Statistical Area (aks the CBSA)",
    "cbsa_title", "The full name of the Core based statistical area that the respondent is from"
    
  )

# Write out data dictionary
write_csv(
  appended_column_data_dictionary,
  "data/intermediate-data/pulse_puf2_appended_columns_data_dictionary.csv"
)

# Manually generate and write out data dictionary for rr metrics
rr_metrics_data_dictionary <-
  tibble::tribble(
    ~col_name, ~description,
    "hisp_rrace", "Combination of Hispanic and Race column. Groups respondents into the following categories: Hispanic, White non Hispanic, Black non Hispanic, Asian non Hispanic, and Other race/two or more races",
    "answered_uninsured", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for uninsured metric",
    "answered_insured_public", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for insured_public metric",
    "answered_inc_loss", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for inc_loss metric",
    "answered_expect_inc_loss", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for expect_inc_loss metric",
    "answered_food_insufficient", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for food_insufficient metric",
    "answered_spend_savings", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for spend_savings metric. All of the spending variables have the same response rate because they are calculated from different response choices from the same question.",
    "answered_spend_credit", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for spend_credit metric. All of the spending variables have the same response rate because they are calculated from different response choices from the same question.",
    "answered_spend_ui", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for spend_ui metric. All of the spending variables have the same response rate because they are calculated from different response choices from the same question.",
    "answered_spend_stimulus", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for spend_stimulus metric. All of the spending variables have the same response rate because they are calculated from different response choices from the same question.",
    "answered_depression_anxiety_signs", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for depression_anxiety_signs metric",
    "answered_expense_dif", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for expense_dif metric",
    "answered_telework", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for telework metric",
    "answered_metalhealth_unmet", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for mentalhealth_unmet metric",
    "answered_spend_snap", "Proportion of total respondents (overall and by race/ethnicity) that answered question(s) for spend_snap metric. All of the spending variables have the same response rate because they are calculated from different response choices from the same question.",
    "answered_tenure", "Proportion of total respondents (overall and by race/ethnicity) that answered tenure question. Used as proxy for housing variable response rate because tenure question determines eligibility for housing questions.",
    "answered_enroll", "Proportion of total respondents (overall and by race/ethnicity) that answered school enrollment question. Used as proxy for learning_fewer variable response rate because enrollment question determines eligibility for housing questions. Response rate is low because asked of all respondents, even those without children under 18 (some of whom answer the question).",
    "week_num", "The week number that the survey data is from"
  )

# Write out data dictionary
write_csv(
  rr_metrics_data_dictionary,
  "data/intermediate-data/pulse_puf2_rr_metrics_data_dictionary.csv"
)
