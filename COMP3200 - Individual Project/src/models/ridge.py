import json
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import mean_squared_error, r2_score

# Load JSONL file into a pandas DataFrame
with open('training_set.jsonl', 'r') as file:
    data = [json.loads(line) for line in file]
df = pd.DataFrame(data)

# Convert text data into TF-IDF vectors
vectorizer = TfidfVectorizer()
X = vectorizer.fit_transform(df['text'])

# Split data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, df['cd_index'], test_size=0.25, random_state=0)

# Define the Ridge model
ridge_model = Ridge()

# Define the hyperparameter grid for tuning
param_grid = {'alpha': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
              'solver': ['lsqr', 'sparse_cg', 'sag'] }

# Perform grid search using cross-validation
ridge_grid_search = GridSearchCV(ridge_model, param_grid, scoring='neg_mean_squared_error', cv=5)
ridge_grid_search.fit(X_train, y_train)

# Get the best parameters
best_params = ridge_grid_search.best_params_
print(f'Best Parameters: {best_params}')

# Train and predict using the model with the best parameters
best_grid = ridge_grid_search.best_estimator_
best_grid.fit(X_train, y_train)
y_pred = best_grid.predict(X_test)

# Calculate Mean Squared Error
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse}")

# Calculate R-squared
r2 = r2_score(y_test, y_pred)
print(f"R-squared: {r2}")
