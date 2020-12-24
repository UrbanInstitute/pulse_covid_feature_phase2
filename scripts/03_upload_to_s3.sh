bucket_name="ui-census-pulse-survey"
endweek_padded="19"

sudo apt install awscli
aws s3 cp data/intermediate-data/pulse_puf2_all_weeks.csv s3://${bucket_name}/phase2_pulse_puf_most_recent.csv
aws s3 cp data/raw-data/public_use_files/pulse2020_data.dictionary_CSV_${endweek_padded}.xlsx s3://${bucket_name}/phase2_puf_week_most_recent_data_dictionary.xlsx
aws s3 cp data/final-data/phase2_all_to_current_week.csv s3://${bucket_name}/phase2_all_to_current_week.csv
aws s3 cp data/intermediate-data/pulse_puf2_appended_columns_data_dictionary.csv s3://${bucket_name}/phase2_puf_appended_column_data_dictionary.csv
aws s3 cp data/intermediate-data/pulse2_rr_metrics_race_all.csv s3://${bucket_name}/phase2_rr_metrics_race_all.csv
aws s3 cp data/intermediate-data/pulse2_rr_metrics_job_loss_all.csv s3://${bucket_name}/phase2_rr_metrics_job_loss_all.csv
aws s3 cp data/intermediate-data/pulse2_response_by_race_all.csv s3://${bucket_name}/phase2_rr_metrics_response_by_race_all.csv
aws s3 cp data/intermediate-data/pulse_puf2_rr_metrics_data_dictionary.csv s3://${bucket_name}/phase2_rr_metrics_data_dictionary.csv