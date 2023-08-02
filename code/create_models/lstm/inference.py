import torch
import torch.nn as nn
import pandas as pd
from model import biLSTM
from torch.utils.data import TensorDataset
from torch.utils.data import DataLoader
from tqdm import tqdm

# Hyperparameters
input_size = 64
sequence_length = 20  # or n, depending on your data
step = 1
hidden_size = 128
num_layers = 4
num_classes = 3
learning_rate = 3e-4
batch_size = 64
num_epochs = 10

# Load the trained models
model_raw = biLSTM(input_size, hidden_size, num_layers, num_classes)
model_raw.load_state_dict(torch.load("/home/esbenlykke/projects/sleep_study/models/score_simple_lstm_model.pt"))

model_median_5 = biLSTM(input_size, hidden_size, num_layers, num_classes)
model_median_5.load_state_dict(torch.load("/home/esbenlykke/projects/sleep_study/models/score_simple_median_5_lstm_model.pt"))

model_median_10 = biLSTM(input_size, hidden_size, num_layers, num_classes)
model_median_10.load_state_dict(torch.load("/home/esbenlykke/projects/sleep_study/models/score_simple_median_10_lstm_model.pt"))

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

model_raw.to(device)
model_raw.eval()
model_median_5.to(device)
model_median_5.eval()
model_median_10.to(device)
model_median_10.eval()

# Load test tensors
test_predictors_raw = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_sequences_score_simple.pt")
test_labels_raw = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_labels_score_simple.pt")
test_predictors_median_5 = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_sequences_score_simple_median_5.pt")
test_labels_median_5 = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_labels_score_simple_median_5.pt")
test_predictors_median_10 = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_sequences_score_simple_median_10.pt")
test_labels_median_10 = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_testing_labels_score_simple_median_10.pt")

# Create test datasets
test_data_raw = TensorDataset(test_predictors_raw, test_labels_raw)
test_data_median_5 = TensorDataset(test_predictors_median_5, test_labels_median_5)
test_data_median_10 = TensorDataset(test_predictors_median_10, test_labels_median_10)

# Create data loaders (with no shuffling)
batch_size = 64
test_loader_raw = DataLoader(test_data_raw, shuffle=False, batch_size=batch_size)
test_loader_median_5 = DataLoader(test_data_median_5, shuffle=False, batch_size=batch_size)
test_loader_median_10 = DataLoader(test_data_median_10, shuffle=False, batch_size=batch_size)

# Make predictions
predictions_raw = []
predictions_median_5 = []
predictions_median_10 = []

# Wrap the test loaders with tqdm for progress tracking
with tqdm(test_loader_raw, desc='Predicting Raw', unit='batch') as progress_bar:
    for batch in progress_bar:
        inputs, _ = batch
        inputs = inputs.to(device)

        # Forward pass
        outputs = model_raw(inputs)

        # Append the batch predictions to the list
        predictions_raw.append(outputs.detach().cpu())

        # Free GPU memory
        del inputs, outputs
        torch.cuda.empty_cache()

with tqdm(test_loader_median_5, desc='Predicting Median 5', unit='batch') as progress_bar:
    for batch in progress_bar:
        inputs, _ = batch
        inputs = inputs.to(device)

        # Forward pass
        outputs = model_median_5(inputs)

        # Append the batch predictions to the list
        predictions_median_5.append(outputs.detach().cpu())

        # Free GPU memory
        del inputs, outputs
        torch.cuda.empty_cache()

with tqdm(test_loader_median_10, desc='Predicting Median 10', unit='batch') as progress_bar:
    for batch in progress_bar:
        inputs, _ = batch
        inputs = inputs.to(device)

        # Forward pass
        outputs = model_median_10(inputs)

        # Append the batch predictions to the list
        predictions_median_10.append(outputs.detach().cpu())

        # Free GPU memory
        del inputs, outputs
        torch.cuda.empty_cache()

# Concatenate the predictions from all batches
predictions_raw = torch.cat(predictions_raw, dim=0)
predictions_median_5 = torch.cat(predictions_median_5, dim=0)
predictions_median_10 = torch.cat(predictions_median_10, dim=0)

# Convert the predicted outputs to class labels
predicted_classes_raw = torch.argmax(predictions_raw, dim=1)
predicted_classes_median_5 = torch.argmax(predictions_median_5, dim=1)
predicted_classes_median_10 = torch.argmax(predictions_median_10, dim=1)

# Convert the outputs to probabilities
softmax = nn.Softmax(dim=1)
probabilities_raw = softmax(predictions_raw)
probabilities_median_5 = softmax(predictions_median_5)
probabilities_median_10 = softmax(predictions_median_10)

# Create new DataFrames with the predicted results
predictions_df_raw = pd.DataFrame({
    "Predicted_Class": predicted_classes_raw.tolist(),
    "Probability_Class_0": probabilities_raw[:, 0].tolist(),
    "Probability_Class_1": probabilities_raw[:, 1].tolist(),
    # Add more columns as needed
})

predictions_df_median_5 = pd.DataFrame({
    "Predicted_Class": predicted_classes_median_5.tolist(),
    "Probability_Class_0": probabilities_median_5[:, 0].tolist(),
    "Probability_Class_1": probabilities_median_5[:, 1].tolist(),
    # Add more columns as needed
})

predictions_df_median_10 = pd.DataFrame({
    "Predicted_Class": predicted_classes_median_10.tolist(),
    "Probability_Class_0": probabilities_median_10[:, 0].tolist(),
    "Probability_Class_1": probabilities_median_10[:, 1].tolist(),
    # Add more columns as needed
})

# Save the predictions to CSV files
predictions_df_raw.to_csv("/home/esbenlykke/projects/sleep_study/data/processed/predictions/biLSTM_simple_score_predictions_raw.csv", index=False)
predictions_df_median_5.to_csv("/home/esbenlykke/projects/sleep_study/data/processed/predictions/biLSTM_simple_score_predictions_median_5.csv", index=False)
predictions_df_median_10.to_csv("/home/esbenlykke/projects/sleep_study/data/processed/predictions/biLSTM_simple_score_predictions_median_10.csv", index=False)
