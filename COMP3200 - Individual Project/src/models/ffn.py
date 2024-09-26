from sklearn.feature_extraction.text import TfidfVectorizer
from keras.layers import Dense, Dropout
from keras.models import Sequential
from keras.callbacks import Callback
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split
import numpy as np
import time
import json

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

start_time = time.time()
vectorizer = TfidfVectorizer(max_features=10000)
X = vectorizer.fit_transform(texts)
end_time = time.time()
print(f"Time elapsed for converting texts to sequences: {end_time - start_time} seconds")

start_time = time.time()
model = Sequential()
model.add(Dense(128, activation='relu', input_shape=(10000,)))
model.add(Dropout(0.25))  # Dropout layer
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
X_train, X_val, y_train, y_val = train_test_split(X, cd_indices, test_size=0.25, random_state=1)

print ("Starting model training...")

# Train the model on the training data and validate on the validation data
start_time = time.time()
model.fit(X_train, y_train, validation_data=(X_val, y_val), epochs=25, batch_size=32, callbacks=[time_callback])
end_time = time.time()
print(f"Time elapsed for training the model: {end_time - start_time} seconds")

# Predict on the validation data
predictions = model.predict(X_val)

# Calculate R^2 score
r2 = r2_score(y_val, predictions)
print(f"R^2 Score: {r2}")
