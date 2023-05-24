# Imports
import torch
import torch.nn as nn  
import torch.optim as optim  
import torch.nn.functional as F  
from torch.utils.data import DataLoader
from torch.utils.data import TensorDataset
from tqdm import tqdm  
from sklearn.metrics import f1_score
import pandas as pd

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

# Hyperparameters
input_size = 54
sequence_length = 20  # or n, depending on your data
step = 10
hidden_size = 128
num_layers = 2
num_classes = 5
learning_rate = 3e-4
batch_size = 64
num_epochs = 25
sequence_length = 20
step = 10

# Create a bidirectional LSTM
class biLSTM(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes):
        super(biLSTM, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(
            input_size, hidden_size, num_layers, batch_first=True, bidirectional=True
        )
        self.fc = nn.Linear(hidden_size * 2, num_classes)

    def forward(self, x):
        h0 = torch.zeros(self.num_layers * 2, x.size(0), self.hidden_size).to(device)
        c0 = torch.zeros(self.num_layers * 2, x.size(0), self.hidden_size).to(device)

        out, _ = self.lstm(x)
        out = self.fc(out[:, -1, :])

        return out

# define check_accuracy function
def check_accuracy(loader, model, dataset_name):
    print(f"Checking accuracy on {dataset_name}...")

    num_correct = 0
    num_samples = 0
    model.eval()
    
    all_predictions = []
    all_targets = []

    with torch.no_grad():
        for x, y in loader:
            x = x.to(device=device)
            y = y.to(device=device)

            scores = model(x)
            _, predictions = scores.max(1)
            num_correct += (predictions == y).sum().item()
            num_samples += predictions.size(0)

            # Store all predictions and targets for F1 score calculation
            all_predictions.extend(predictions.cpu().numpy())
            all_targets.extend(y.cpu().numpy())
            
        accuracy = float(num_correct) / float(num_samples) * 100
        print(f"Got {num_correct} / {num_samples} with accuracy {accuracy:.2f}")
        
        # Calculate and print F1 score
        f1 = f1_score(all_targets, all_predictions, average='weighted')
        print(f"F1 score on {dataset_name}: {f1:.2f}")

    model.train()

    return accuracy, f1


# Load train tensors
train_predictors = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_train_sequences.pt")
train_labels = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_train_labels.pt")

train_predictors = train_predictors.to(device)
train_labels = train_labels.to(device).long() +1

# load validation tensors
valid_predictors = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_valid_sequences.pt")
valid_labels = torch.load("/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_valid_labels.pt")

valid_predictors = valid_predictors.to(device)
valid_labels = valid_labels.to(device).long() + 1

# Create a TensorDataset from your tensors
train_data = TensorDataset(train_predictors, train_labels)
valid_data = TensorDataset(valid_predictors, valid_labels)
# test_data = TensorDataset(test_predictors, test_labels)

# Create your DataLoaders
train_loader = DataLoader(train_data, shuffle=True, batch_size=batch_size)
valid_loader = DataLoader(valid_data, shuffle=True, batch_size=batch_size)
# test_loader = DataLoader(test_data, shuffle=True, batch_size=batch_size)

# # Save validation tensors
# torch.save(valid_predictors, "/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_valid_sequences.pt")
# torch.save(valid_labels, "/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_valid_labels.pt")

# # Save test tensors
# torch.save(test_predictors, "/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_test_sequences.pt")
# torch.save(test_labels, "/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_test_labels.pt")

print(torch.unique(train_labels, return_counts=True))
print(torch.unique(valid_labels, return_counts=True))

# Initialize network
model = biLSTM(input_size, hidden_size, num_layers, num_classes).to(device)

# Loss and optimizer 
criterion = nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=learning_rate)

# Initialize DataFrame to store metrics
metrics_df = pd.DataFrame(columns=['Epoch', 'Train Loss', 'Valid Loss', 'Train Accuracy', 'Train F1', 'Test Accuracy', 'Test F1'])

# Train network
for epoch in range(num_epochs):
    running_train_loss = 0.0
    running_valid_loss = 0.0
    model.train()
    for batch_idx, (data, targets) in enumerate(train_loader):  # use train_loader here
        data = data.to(device=device)
        targets = targets.to(device=device)

        # Forward
        scores = model(data)
        loss = criterion(scores, targets)
        running_train_loss += loss.item()

        # Backward
        optimizer.zero_grad()
        loss.backward()

        # Gradient descent or Adam step
        optimizer.step()

    model.eval()
    with torch.no_grad():
        for batch_idx, (data, targets) in enumerate(valid_loader):
            data = data.to(device=device)
            targets = targets.to(device=device)

            scores = model(data)
            loss = criterion(scores, targets)
            running_valid_loss += loss.item()
            
    train_loss = running_train_loss / len(train_loader)
    valid_loss = running_valid_loss / len(valid_loader)

    print(f'Epoch {epoch+1}, Train Loss: {train_loss:.4f}, Valid Loss: {valid_loss:.4f}')

    # Evaluate model
    train_acc, train_f1 = check_accuracy(train_loader, model, "Train")
    test_acc, test_f1 = check_accuracy(valid_loader, model, "Validation")

    # Inside your training loop, after computing the metrics for each epoch...
    metrics_df = metrics_df._append({  # use append() instead of _append()
    'Epoch': epoch+1,
    'Train Loss': train_loss,
    'Valid Loss': valid_loss,
    'Train Accuracy': train_acc,
    'Train F1': train_f1,
    'Test Accuracy': test_acc,
    'Test F1': test_f1
    }, ignore_index=True)

# Save metrics DataFrame to a CSV file
metrics_df.to_csv("/home/esbenlykke/projects/sleep_study/models/lstm_model_metrics.csv", index=False)

# Save the entire model object
torch.save(model, "/home/esbenlykke/projects/sleep_study/models/lstm_model.pt")