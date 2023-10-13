# Imports
import torch
import torch.nn as nn  
import torch.optim as optim  
import torch.nn.functional as F  
from torch.utils.data import DataLoader
from torch.utils.data import TensorDataset
from tqdm import tqdm  
from sklearn.metrics import f1_score

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"

# Hyperparameters
input_size = 64
sequence_length = 20  # or n, depending on your data
step = 1
hidden_size = 128
num_layers = 2
num_classes = 3
learning_rate = 3e-4
batch_size = 64
num_epochs = 5
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
    
# Initialize the model
model = biLSTM(input_size, hidden_size, num_layers, num_classes).to(device)
model.eval()  # Set model to evaluation mode

# Create a dummy input of appropriate size
x = torch.randn(1, sequence_length, input_size).to(device)

# Convert to TorchScript via tracing
traced_model = torch.jit.trace(model, x)

# Save the traced model
traced_model.save("biLSTM_model.pt")
