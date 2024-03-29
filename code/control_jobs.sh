# ./code/prepare_data/remove_excluded_zm_data.R &&
# 
# ./code/prepare_data/create_features.R &&
# 
# ./code/prepare_data/extract_in_bed_data.R &&
# 
# ./code/create_models/chained_models/spend_data_budget.R &&
# 
# ./code/create_models/chained_models/in_bed_workflows.R &&

# ./code/create_models/chained_models/sleep_workflows.R &&

# ./code/create_models/chained_models/finalize_in_bed_workflows.R &&

# ./code/create_models/chained_models/finalize_sleep_workflows.R &&

./code/evaluate_models/create_pr_and_roc_data_all_models.R &&

./manuscript/code/plot_sleep_PR_and_ROC_data.R &&

echo "Job's done!"

sleep 5 && shutdown now