import json
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, r2_score, mean_squared_error, confusion_matrix, ConfusionMatrixDisplay
from sklearn.model_selection import GridSearchCV
import matplotlib.pyplot as plt

# Load JSONL file into a pandas DataFrame
with open('training_set.jsonl', 'r') as file:
    data = [json.loads(line) for line in file]
df = pd.DataFrame(data)

# Convert text data into TF-IDF vectors
vectorizer = TfidfVectorizer()
X = vectorizer.fit_transform(df['text'])

# Make sure your target variable is categorical
y = pd.cut(df['cd_index'], bins=4, labels=False)


# Split data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)

# Define the parameter grid
param_grid = {
    'C': [1],
    'penalty': ['l2'],
    'solver': ['liblinear']
}

# Create a base model
logreg = LogisticRegression(max_iter=1000)

# Instantiate the grid search model
grid_search = GridSearchCV(estimator=logreg, param_grid=param_grid, cv=5, n_jobs=-1, verbose=2)

# Fit the grid search to the data
grid_search.fit(X_train, y_train)

# Get the best parameters
best_params = grid_search.best_params_
print(f'Best Parameters: {best_params}')

# Train and predict using the model with the best parameters
best_grid = grid_search.best_estimator_
best_grid.fit(X_train, y_train)
y_pred = best_grid.predict(X_test)

# Calculate Accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f'Accuracy: {accuracy}')

# Calculate Mean Squared Error
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse}")

# Calculate R^2 score
r2 = r2_score(y_test, y_pred)
print(f"R^2 Score: {r2}")

# Print Confusion Matrix
cm = confusion_matrix(y_test, y_pred)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels= ["-1 to -0.5", "-0.5 to 0", "0 to 0.5", "0.5 to 1"])
disp.plot()
plt.title("Random State 1")
plt.show()
print(f'Confusion Matrix: \n{cm}')
