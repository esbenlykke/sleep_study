import pandas as pd
import torch
from torch.utils.data import Dataset
from sklearn.model_selection import train_test_split

# Define the helper function
def prepare_data(data_path, sequence_length, step_size, target_var):
    # Load the data using pandas
    data = pd.read_parquet(data_path)
    data = data[['age', 'weekday', 'incl', 'theta', 'x_mean', 'y_mean', 'z_mean',
              'x_sd', 'y_sd', 'z_sd', 'x_sd_long', 'y_sd_long', 'z_sd_long', 'sd_max',
              'temp_mean', 'temp_sd', 'clock_proxy_cos', 'clock_proxy_linear',
              'temp_mean_lag_1min', 'temp_mean_lag_5min', 'temp_mean_lag_30min',
              'temp_mean_lead_1min', 'temp_mean_lead_5min', 'temp_mean_lead_30min',
              'theta_lag_1min', 'theta_lag_5min', 'theta_lag_30min',
              'theta_lead_1min', 'theta_lead_5min', 'theta_lead_30min',
              'incl_lag_1min', 'incl_lag_5min', 'incl_lag_30min',
              'incl_lead_1min', 'incl_lead_5min', 'incl_lead_30min',
              'x_sd_lag_1min', 'x_sd_lag_5min', 'x_sd_lag_30min',
              'y_sd_lag_1min', 'y_sd_lag_5min', 'y_sd_lag_30min',
              'z_sd_lag_1min', 'z_sd_lag_5min', 'z_sd_lag_30min',
              'x_sd_lead_1min', 'x_sd_lead_5min', 'x_sd_lead_30min',
              'y_sd_lead_1min', 'y_sd_lead_5min', 'y_sd_lead_30min',
              'z_sd_lead_1min', 'z_sd_lead_5min', 'z_sd_lead_30min',
              'vector_magnitude',
              'x_crossing_rate', 'y_crossing_rate', 'z_crossing_rate',
              'x_skewness', 'y_skewness', 'z_skewness',
              'x_kurtosis', 'y_kurtosis', 'z_kurtosis', target_var]]

    # Separate out the target variable
    target = data.pop(target_var)

    # Normalize the data
    data = (data - data.mean()) / data.std()

    # Add the target variable back to the dataframe
    data[target_var] = target

    # Convert to PyTorch tensors
    data_tensor = torch.tensor(data.values, dtype=torch.float32)

    # Create sequences and labels
    sequences = []
    labels = []

    for i in range(0, len(data) - sequence_length, step_size):
        sequences.append(data_tensor[i:i + sequence_length, :-1])
        labels.append(data_tensor[i + sequence_length - 1, -1])

    sequences_tensor = torch.stack(sequences)
    labels_tensor = torch.stack(labels)

    # Generate file names
    base_name = data_path.split("/")[-1].replace(".parquet", "")
    sequences_file = f"data/data_for_modelling/lstm/pytorch_{base_name}_sequences_{target_var}.pt"
    labels_file = f"data/data_for_modelling/lstm/pytorch_{base_name}_labels_{target_var}.pt"

    # Save tensors to files
    torch.save(sequences_tensor, sequences_file)
    torch.save(labels_tensor, labels_file)

    return sequences_file, labels_file

# Sequence length and step size
sequence_length = 20  # Corresponding to 10 minutes in 30 sec epoch data
step_size = 1  # Corresponding to 30 sec. Thus will give one prediction per data point when making predictions.

# Prepare the train and test data for each target variable
prepare_data("data/data_for_modelling/lstm/30_sec_training.parquet", sequence_length, step_size, 'score_simple')
prepare_data("data/data_for_modelling/lstm/30_sec_validation.parquet", sequence_length, step_size, 'score_simple')
prepare_data("data/data_for_modelling/lstm/30_sec_testing.parquet", sequence_length, step_size, 'score_simple')

prepare_data("data/data_for_modelling/lstm/30_sec_training.parquet", sequence_length, step_size, 'score_simple_median_5')
prepare_data("data/data_for_modelling/lstm/30_sec_validation.parquet", sequence_length, step_size, 'score_simple_median_5')
prepare_data("data/data_for_modelling/lstm/30_sec_testing.parquet", sequence_length, step_size, 'score_simple_median_5')

prepare_data("data/data_for_modelling/lstm/30_sec_training.parquet", sequence_length, step_size, 'score_simple_median_10')
prepare_data("data/data_for_modelling/lstm/30_sec_validation.parquet", sequence_length, step_size, 'score_simple_median_10')
prepare_data("data/data_for_modelling/lstm/30_sec_testing.parquet", sequence_length, step_size, 'score_simple_median_10')
