# Import necessary libraries
from sklearn import svm
from sklearn.metrics import classification_report, accuracy_score
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
import pandas as pd

# Load the training data
train_data = pd.read_csv('TrainingDataBinary.csv')

# Load the testing data
test_data = pd.read_csv('TestingDataBinary.csv')

# Separate train features and labels
X = train_data.iloc[:, :-1]
y = train_data.iloc[:, -1]

# Split the data into training and validation sets
X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2, random_state=1)

# Separate test features
X_test = test_data.iloc[:, :]

# Define the parameter grid
param_grid = {'svm__C': [0.1, 1, 10, 100], 'svm__gamma': [1, 0.1, 0.01, 0.001], 'svm__kernel': ['linear', 'rbf']}

# Create a SVM classifier
clf = svm.SVC()

# Create a pipeline
pipe = Pipeline(steps=[('scale', StandardScaler()), ('svm', clf)])

# Create the GridSearchCV object
grid = GridSearchCV(pipe, param_grid, refit=True, n_jobs=-1, verbose=2)

# Fit the data to the GridSearchCV object
grid.fit(X_train, y_train)

# Print the best parameters
print("The best parameters are: ", grid.best_params_)

# Predict the response for validation dataset
y_val_pred = grid.predict(X_val)

# Evaluate the model
print("Validation Accuracy: ", accuracy_score(y_val, y_val_pred))
print("\nClassification Report:\n", classification_report(y_val, y_val_pred))

# Predict the response for test dataset
y_pred = grid.predict(X_test)

# Save the predictions to a CSV file
pd.DataFrame(y_pred).to_csv('TestingResultsBinary-svm.csv', index=False)
