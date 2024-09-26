from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.linear_model import LinearRegression
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

# Create a TfidfVectorizer
start_time = time.time()
vectorizer = TfidfVectorizer(max_features=1010)
X = vectorizer.fit_transform(texts)
end_time = time.time()
print(f"Time elapsed for creating the TfidfVectorizer: {end_time - start_time} seconds")

# Split the data into training and validation sets (75/25 split)
X_train, X_val, cd_indices_train, cd_indices_val = train_test_split(X, cd_indices, test_size=0.25, random_state=0)

# Create a Linear Regression model
start_time = time.time()
model = LinearRegression()
model.fit(X_train, cd_indices_train)
end_time = time.time()
print(f"Time elapsed for training the model: {end_time - start_time} seconds")

# Predict on the validation data
predictions = model.predict(X_val)

# Calculate Mean Squared Error
mse = mean_squared_error(cd_indices_val, predictions)
print(f"Mean Squared Error: {mse}")

# Calculate R^2 score
r2 = r2_score(cd_indices_val, predictions)
print(f"R^2 Score: {r2}")
