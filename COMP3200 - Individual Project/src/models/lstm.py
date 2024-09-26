from tensorflow.keras.models import Sequential # type: ignore
from tensorflow.keras.layers import Embedding, LSTM, GlobalMaxPooling1D, Dense # type: ignore
from tensorflow.keras.preprocessing.text import Tokenizer # type: ignore
from tensorflow.keras.preprocessing.sequence import pad_sequences # type: ignore
from tensorflow.keras.callbacks import Callback # type: ignore
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
import time
import json
import numpy as np

# Initialize empty lists
texts = []
cd_indices = []

# Open your JSONL file
start_time = time.time()
with open('training_set.jsonl', 'r') as f:
    for line in f:
        paper = json.loads(line)

        # Add the filtered "text" field to the texts list
        texts.append(paper["text"])

        # Add the "cd_index" field to the cd_indices list
        cd_indices.append(paper["cd_index"])
end_time = time.time()
print(f"Time elapsed for reading the file: {end_time - start_time} seconds")

# Convert texts to sequences of word indices
start_time = time.time()
tokenizer = Tokenizer(num_words=2000)
tokenizer.fit_on_texts(texts)
sequences = tokenizer.texts_to_sequences(texts)
end_time = time.time()
print(f"Time elapsed for converting texts to sequences: {end_time - start_time} seconds")

# Pad sequences to the same length
start_time = time.time()
data = pad_sequences(sequences, maxlen=1500)
end_time = time.time()
print(f"Time elapsed for padding sequences: {end_time - start_time} seconds")

# Create an LSTM model
start_time = time.time()
model = Sequential()
model.add(Embedding(2000, 50))
model.add(LSTM(128, activation='tanh'))
model.add(Dense(1, activation='linear'))
end_time = time.time()
print(f"Time elapsed for creating the model: {end_time - start_time} seconds")

# Compile the model
start_time = time.time()
model.compile(optimizer='adam', loss='mean_squared_error', metrics=['accuracy']) 
end_time = time.time()
print(f"Time elapsed for compiling the model: {end_time - start_time} seconds")

class TimeHistory(Callback):
    def on_train_begin(self, logs={}):
        self.times = []

    def on_epoch_begin(self, batch, logs={}):
        self.epoch_time_start = time.time()

    def on_epoch_end(self, batch, logs={}):
        self.times.append(time.time() - self.epoch_time_start)

time_callback = TimeHistory()

cd_indices = np.array(cd_indices)

# Split the data into training and validation sets (75/25 split)
data_train, data_val, cd_indices_train, cd_indices_val = train_test_split(data, cd_indices, test_size=0.25, random_state=0)

print ("Starting model training...")

# Train the model on the training data and validate on the validation data
start_time = time.time()
model.fit(data_train, cd_indices_train, validation_data=(data_val, cd_indices_val), epochs=20, batch_size=32, callbacks=[time_callback])
end_time = time.time()
print(f"Time elapsed for training the model: {end_time - start_time} seconds")

# Predict on the validation data
predictions = model.predict(data_val)

# Calculate R^2 score
r2 = r2_score(cd_indices_val, predictions)
print(f"R^2 Score: {r2}")