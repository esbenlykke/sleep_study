# Load the necessary libraries
library(keras)
library(tensorflow)

# Hyperparameters
input_size <- 68
sequence_length <- 20  # or n, depending on your data
step <- 10
hidden_size <- 128
num_layers <- 2
num_classes <- 5
learning_rate <- 3e-4
batch_size <- 64
num_epochs <- 10
sequence_length <- 20
step <- 10

# Load data
train_predictors <- reticulate::np_load("train_predictors.npy")
train_labels <- reticulate::np_load("train_labels.npy")

test_predictors <- reticulate::np_load("test_predictors.npy")
test_labels <- reticulate::np_load("test_labels.npy")

# one-hot encode the labels
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

# Create a bidirectional LSTM
model <- keras_model_sequential() 
model %>%
  layer_lstm(units = hidden_size, return_sequences = TRUE, input_shape = c(NULL, input_size), dropout = 0.2, recurrent_dropout = 0.2, go_backwards = FALSE) %>%
  bidirectional(layer_lstm(units = hidden_size, return_sequences = FALSE, go_backwards = TRUE)) %>%
  layer_dense(units = num_classes, activation = 'softmax')

# compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(lr = learning_rate),
  metrics = c('accuracy')
)

# fit the model
history <- model %>% fit(
  train_predictors, 
  train_labels, 
  epochs = num_epochs, 
  batch_size = batch_size,
  validation_data = list(test_predictors, test_labels)
)

# evaluate the model
train_acc <- model %>% evaluate(train_predictors, train_labels)
test_acc <- model %>% evaluate(test_predictors, test_labels)

# predict labels for the test set
test_predictions <- model %>% predict_classes(test_predictors)

# calculate and print F1 score
f1 <- caret::f1_score(test_labels, test_predictions, average = 'weighted')
print(paste("F1 score on Test set:", round(f1, 2)))

# save the model
model %>% save_model_hdf5('lstm_model.h5')
