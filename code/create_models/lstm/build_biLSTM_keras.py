# Imports
from keras.models import Sequential
from keras.layers import LSTM, Dense, Dropout
from keras.utils import to_categorical
from keras.optimizers import Adam
from sklearn.metrics import f1_score
from keras.callbacks import EarlyStopping
import numpy as np


# Hyperparameters
input_size = 68
sequence_length = 20  # or n, depending on your data
step = 10
hidden_size = 128
num_layers = 2
num_classes = 5
learning_rate = 3e-4
batch_size = 64
num_epochs = 10
sequence_length = 20
step = 10

# Load data
train_predictors = np.load("train_predictors.npy")
train_labels = np.load("train_labels.npy")

test_predictors = np.load("test_predictors.npy")
test_labels = np.load("test_labels.npy")

# one-hot encode the labels
train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)

# Create a bidirectional LSTM
model = Sequential()
model.add(LSTM(hidden_size, return_sequences=True, input_shape=(None, input_size), dropout=0.2, recurrent_dropout=0.2, go_backwards=False))
model.add(LSTM(hidden_size, return_sequences=False, go_backwards=True))
model.add(Dense(num_classes, activation='softmax'))

# compile the model
model.compile(loss='categorical_crossentropy', optimizer=Adam(learning_rate), metrics=['accuracy'])

# fit the model
history = model.fit(train_predictors, train_labels, batch_size=batch_size, epochs=num_epochs, validation_data=(test_predictors, test_labels))

# evaluate the model
train_acc = model.evaluate(train_predictors, train_labels, verbose=0)
test_acc = model.evaluate(test_predictors, test_labels, verbose=0)

# predict labels for the test set
test_predictions = model.predict(test_predictors)

# convert predictions and test labels to 1D arrays
test_predictions = np.argmax(test_predictions, axis=1)
test_labels = np.argmax(test_labels, axis=1)

# calculate and print F1 score
f1 = f1_score(test_labels, test_predictions, average='weighted')
print(f"F1 score on Test set: {f1:.2f}")

# save the model
model.save('lstm_model.h5')
