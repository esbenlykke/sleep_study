# %%
import torch
import torch.nn as nn
import pandas as pd
# Prediction script (load the model)
from model import biLSTM
from torch.utils.data import TensorDataset
from torch.utils.data import DataLoader
from tqdm import tqdm

# %%
# Load the trained model
model = torch.load("/home/esbenlykke/projects/sleep_study/models/lstm_model.pt")

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

model.to(device)
model.eval()



# %%
# load test tensors
test_predictors = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/test_predictors.pt")
test_labels = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/test_labels.pt")

test_predictors = test_predictors.to(device)
test_labels = test_labels.to(device).long() + 1

# Create a TensorDataset from your tensors
test_data = TensorDataset(test_predictors, test_labels)

# Create data loader
batch_size = 64

test_loader = DataLoader(test_data, shuffle=True, batch_size=batch_size)

# %%
print(test_data)

# %%
sequence_length = 20
input_size = 68

test_predictors = test_predictors.view(-1, sequence_length, input_size)

print(test_predictors.size())

# %%
# Make predictions
predictions = []

# Set the model to evaluation mode
model.eval()

# Wrap the test_loader with tqdm for progress tracking
with tqdm(test_loader, desc='Predicting', unit='batch') as progress_bar:
    for batch in progress_bar:
        inputs, _ = batch
        inputs = inputs.to(device)

        # Forward pass
        outputs = model(inputs)

        # Process the outputs as needed
        # ...

        # Append the batch predictions to the list
        predictions.append(outputs.detach().cpu())

        # Free GPU memory
        del inputs, outputs
        torch.cuda.empty_cache()

# Concatenate the predictions from all batches
predictions = torch.cat(predictions, dim=0)



# %%
print(pd.DataFrame(predictions))

# %%
# Convert the predicted outputs to the desired format (e.g., classes, probabilities, etc.)
# Perform any necessary post-processing steps on the outputs

# Example: Convert the outputs to class labels
predicted_classes = torch.argmax(predictions, dim=1)

# %%
print(pd.DataFrame(predicted_classes).describe)

# %%
# Example: Convert the outputs to probabilities
softmax = nn.Softmax(dim=1)
probabilities = softmax(outputs)

# Create a new DataFrame with the predicted results
# Assume 'predictions_df' is a new DataFrame to store the predictions
predictions_df = pd.DataFrame({
    "Predicted_Class": predicted_classes.tolist(),
    "Probability_Class_0": probabilities[:, 0].tolist(),
    "Probability_Class_1": probabilities[:, 1].tolist(),
    # Add more columns as needed
})

# Use the predictions for further analysis or save them to a file
predictions_df.to_csv("predictions.csv", index=False)

# %%
print(test_predictors.shape)
out, _ = model.lstm(test_predictors)



