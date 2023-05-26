import pandas as pd
import torch
from torch.utils.data import Dataset
from sklearn.model_selection import train_test_split

# Define the helper function
def prepare_data(data_path, sequence_length, step_size):
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
              'x_kurtosis', 'y_kurtosis', 'z_kurtosis', 'score_simple']]

    # Separate out 'score_simple'
    score_simple = data.pop('score_simple')

    # Normalize the data
    data = (data - data.mean()) / data.std()

    # Add 'score_simple' back to the dataframe
    data['score_simple'] = score_simple

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

    return sequences_tensor, labels_tensor



# Sequence length and step size
sequence_length = 20  # Corresponding to 10 minutes in 30 sec epoch data
step_size = 1  # Corresponding to 30 sec. Thus will give one prediction per data point when making predictions.

# Prepare the train and test data
train_sequences, train_labels = prepare_data("data/data_for_modelling/lstm/30_sec_training.parquet", sequence_length, step_size)
valid_sequences, valid_labels = prepare_data("data/data_for_modelling/lstm/30_sec_validation.parquet", sequence_length, step_size)
test_sequences, test_labels = prepare_data("data/data_for_modelling/lstm/30_sec_testing.parquet", sequence_length, step_size)

print("train sequences have shape: {}".format(train_sequences.shape))
print("valid sequences have shape: {}".format(valid_sequences.shape))
print("test sequences have shape: {}".format(test_sequences.shape))

# Save the validation and test tensors
torch.save(train_sequences, "data/data_for_modelling/lstm/pytorch_train_sequences.pt")
torch.save(train_labels, "data/data_for_modelling/lstm/pytorch_train_labels.pt")
torch.save(valid_sequences, "data/data_for_modelling/lstm/pytorch_valid_sequences.pt")
torch.save(valid_labels, "data/data_for_modelling/lstm/pytorch_valid_labels.pt")
torch.save(test_sequences, "data/data_for_modelling/lstm/pytorch_test_sequences.pt")
torch.save(test_labels, "data/data_for_modelling/lstm/pytorch_test_labels.pt")