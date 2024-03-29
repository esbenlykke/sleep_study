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

torch.manual_seed(42)
torch.cuda.manual_seed_all(42)

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

print("The device being used is: {}\n".format(device))

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

# Create a bidirectional LSTM
class biLSTM(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, num_classes):
        super(biLSTM, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(
            input_size, hidden_size, num_layers, batch_first=True, bidirectional=True
        )
        self.fc1 = nn.Linear(hidden_size * 2, hidden_size)  # Additional fully connected layer
        self.fc2 = nn.Linear(hidden_size, num_classes)  # Output layer

    def forward(self, x):
        h0 = torch.zeros(self.num_layers * 2, x.size(0), self.hidden_size).to(device)
        c0 = torch.zeros(self.num_layers * 2, x.size(0), self.hidden_size).to(device)

        out, _ = self.lstm(x, (h0, c0))
        out = out[:, -1, :]
        out = self.fc1(out)  # Apply linear transformation
        out = self.fc2(out)  # Output layer
        out = F.softmax(out, dim=1)  # Apply softmax

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


# List of targets
targets = ['score_simple', 'score_simple_median_5', 'score_simple_median_10']

for target in targets:
    print(f"\nTraining model for target: {target}")

    # Load train tensors
    train_predictors = torch.load(f"/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_training_sequences_{target}.pt")
    train_labels = torch.load(f"/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_training_labels_{target}.pt")

    # load validation tensors
    valid_predictors = torch.load(f"/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_validation_sequences_{target}.pt")
    valid_labels = torch.load(f"/home/esbenlykke/projects/sleep_study/data/data_for_modelling/lstm/pytorch_30_sec_validation_labels_{target}.pt")

    # Create a TensorDataset from your tensors
    train_data = TensorDataset(train_predictors, train_labels)
    valid_data = TensorDataset(valid_predictors, valid_labels)

    # Create your DataLoaders
    train_loader = DataLoader(train_data, shuffle=True, batch_size=batch_size)
    valid_loader = DataLoader(valid_data, shuffle=True, batch_size=batch_size)

    print(torch.unique(train_labels, return_counts=True))
    print(torch.unique(valid_labels, return_counts=True))

    # Initialize network
    model = biLSTM(input_size, hidden_size, num_layers, num_classes).to(device)

    # Loss and optimizer
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(model.parameters(), lr=learning_rate)

    # Initialize DataFrame to store metrics
    metrics_df = pd.DataFrame(
        columns=['Epoch', 'Train Loss', 'Valid Loss', 'Train Accuracy', 'Train F1', 'Test Accuracy', 'Test F1'])

    # Set early stopping parameters
    best_valid_loss = float('inf')
    patience = 3  # Number of epochs with no improvement before stopping
    patience_counter = 0
    best_model = None

    # Train network
    for epoch in range(num_epochs):
        running_train_loss = 0.0
        running_valid_loss = 0.0
        model.train()
        for batch_idx, (data, targets) in enumerate(train_loader):
            data = data.to(device=device)
            targets = targets.to(device=device, dtype=torch.int64)  # Convert targets to torch.int64

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
                targets = targets.to(device=device, dtype=torch.int64)  # Convert targets to torch.int64

                scores = model(data)
                loss = criterion(scores, targets)
                running_valid_loss += loss.item()

        train_loss = running_train_loss / len(train_loader)
        valid_loss = running_valid_loss / len(valid_loader)

        print(f'Epoch {epoch + 1}, Train Loss: {train_loss:.4f}, Valid Loss: {valid_loss:.4f}')

        # Save the model if validation loss has decreased
        if valid_loss < best_valid_loss:
            best_valid_loss = valid_loss
            patience_counter = 0  # Reset counter
            best_model = model.state_dict()
        else:
            patience_counter += 1  # Increment counter

        # Stop training if patience limit is reached
        if patience_counter >= patience:
            print(f"Early stopping at epoch {epoch}!")
            break

        # Evaluate model
        train_acc, train_f1 = check_accuracy(train_loader, model, "Train")
        test_acc, test_f1 = check_accuracy(valid_loader, model, "Validation")

        # Inside your training loop, after computing the metrics for each epoch...
        metrics_df = metrics_df._append({
            'Epoch': epoch + 1,
            'Train Loss': train_loss,
            'Valid Loss': valid_loss,
            'Train Accuracy': train_acc,
            'Train F1': train_f1,
            'Test Accuracy': test_acc,
            'Test F1': test_f1
        }, ignore_index=True)

    # Save metrics DataFrame to a CSV file
    metrics_df.to_csv(f"/home/esbenlykke/projects/sleep_study/models/{target}_lstm_model_metrics.csv", index=False)

    # Save the entire model object
    torch.save(best_model, f"/home/esbenlykke/projects/sleep_study/models/{target}_lstm_model.pt")
