import torch
from torchviz import make_dot
from model import biLSTM

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

# Set device
device = "cuda" if torch.cuda.is_available() else "cpu"


model = biLSTM(input_size, hidden_size, num_layers, num_classes).to(device)
x = torch.randn(sequence_length, input_size).unsqueeze(0).to(device)
out = model(x)
dot = make_dot(out)
dot.attr(size="15,15")  # Adjust the size of the entire graph
dot.render("model_graph")  # This will save the graph in a file called model_graph.pdf

