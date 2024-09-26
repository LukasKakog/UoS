# Import necessary libraries
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, accuracy_score, confusion_matrix, ConfusionMatrixDisplay
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
import pandas as pd
import matplotlib.pyplot as plt

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
param_grid = {'rf__n_estimators': [100, 200, 300], 'rf__max_depth': [None, 10, 20, 30], 'rf__min_samples_split': [2, 5, 10]}

# Create a Random Forest classifier
clf = RandomForestClassifier()

# Create a pipeline
pipe = Pipeline(steps=[('scale', StandardScaler()), ('rf', clf)])

# Create the GridSearchCV object
grid = GridSearchCV(pipe, param_grid, refit=True, n_jobs=-1, verbose=3)

# Fit the data to the GridSearchCV object
grid.fit(X_train, y_train)

# Print the best parameters
print("The best parameters are: ", grid.best_params_)

# Predict the response for validation dataset
y_val_pred = grid.predict(X_val)

# Creation of confusion matrix for evaluation
cm = confusion_matrix(y_val, y_val_pred)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels= ["0", "1"])
disp.plot()
plt.title("Binary Classification")
plt.show()

# Evaluate the model
print("Validation Accuracy: ", accuracy_score(y_val, y_val_pred))
print("\nClassification Report:\n", classification_report(y_val, y_val_pred))

# Predict the response for test dataset
y_pred = grid.predict(X_test)

# Add the predictions to the test data
test_data['marker'] = y_pred

# Save the updated test data to a new CSV file
test_data.to_csv('TestingResultsBinary.csv', index=False)
