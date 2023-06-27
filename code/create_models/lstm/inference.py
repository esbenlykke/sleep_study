import torch
import torch.nn as nn
import pandas as pd
from model import biLSTM
from torch.utils.data import TensorDataset
from torch.utils.data import DataLoader
from tqdm import tqdm

# Load the trained model
model = torch.load("/home/esbenlykke/projects/sleep_study/models/lstm_model.pt")

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

model.to(device)
model.eval()

# Load test tensors
test_predictors = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_test_sequences.pt")
test_labels = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_test_labels.pt")

sequence_length = 20
input_size = 64

# Reshape your data before creating your DataLoader
test_predictors = test_predictors.view(-1, sequence_length, input_size)

test_data = TensorDataset(test_predictors, test_labels)

# Create data loader (with no shuffling)
batch_size = 64
test_loader = DataLoader(test_data, shuffle=False, batch_size=batch_size)

# Make predictions
predictions = []

# Wrap the test_loader with tqdm for progress tracking
with tqdm(test_loader, desc='Predicting', unit='batch') as progress_bar:
    for batch in progress_bar:
        inputs, _ = batch
        inputs = inputs.to(device)

        # Forward pass
        outputs = model(inputs)

        # Append the batch predictions to the list
        predictions.append(outputs.detach().cpu())

        # Free GPU memory
        del inputs, outputs
        torch.cuda.empty_cache()

# Concatenate the predictions from all batches
predictions = torch.cat(predictions, dim=0)

# Convert the predicted outputs to class labels
predicted_classes = torch.argmax(predictions, dim=1)

# Convert the outputs to probabilities
softmax = nn.Softmax(dim=1)
probabilities = softmax(predictions)

# Create a new DataFrame with the predicted results
predictions_df = pd.DataFrame({
    "Predicted_Class": predicted_classes.tolist(),
    "Probability_Class_0": probabilities[:, 0].tolist(),
    "Probability_Class_1": probabilities[:, 1].tolist(),
    # Add more columns as needed
})

# Save the predictions to a csv file
predictions_df.to_csv("/home/esbenlykke/projects/sleep_study/data/processed/predictions/biLSTM_simple_score_predictions.csv", index=False)
