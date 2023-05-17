# ./code/prepare_data/remove_excluded_zm_data.R &&

# ./code/prepare_data/create_features.R &&

./code/prepare_data/extract_in_bed_data.R &&

./code/create_models/chained_models/spend_data_budget.R &&

./code/create_models/chained_classifiers/in_bed_workflows.R &&

./code/create_models/chained_classifiers/sleep_workflows.R &&

./code/create_models/chained_classifiers/finalize_in_bed_workflows.R &&

./code/create_models/chained_classifiers/finalize_sleep_workflows.R &&

echo "Job's done!"