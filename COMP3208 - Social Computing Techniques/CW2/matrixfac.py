import numpy as np
import csv, time

######################################################################
#
# (c) Copyright University of Southampton, 2023
#
# Copyright in this software belongs to University of Southampton,
# Highfield, University Road, Southampton SO17 1BJ
#
# This work is based on the example created for COMP3208 Coursework
######################################################################

#
# load a set of ratings from file
#
def load_data_with_ratings(file, test_size=0.2):

    # Initialise empty lists to store user IDs, item IDs, ratings, and timestamp
    user_ids = []
    item_ids = []
    ratings = []
    timestamp = []

    # Open the CSV file and read its contents
    with open(file, 'r', newline='') as csvF:
        csv_reader = csv.reader(csvF)
        for row in csv_reader:
            user_ids.append(int(row[0]))  # Getting user ID from the first column
            item_ids.append(int(row[1]))  # Getting item ID from the second column
            ratings.append(float(row[2])) # Getting rating from the third column
            timestamp.append(int(row[3])) # Getting timestamp from fourth column

    # Convert lists to numpy arrays
    user_ids = np.array(user_ids)
    item_ids = np.array(item_ids)
    ratings = np.array(ratings)
    timestamp = np.array(timestamp)

    max_user_id = max(user_ids)
    max_item_id = max(item_ids)

    # Split the data into training and validation sets
    user_ids_train, item_ids_train, ratings_train, user_ids_val, item_ids_val, ratings_val = train_val_split(user_ids, item_ids, ratings, test_size)
    user_ids_train, item_ids_train, ratings_train, user_ids_val, item_ids_val, ratings_val = filter_validation_set(user_ids_train, item_ids_train, ratings_train, user_ids_val, item_ids_val, ratings_val)

    return user_ids_train, item_ids_train, ratings_train, user_ids_val, item_ids_val, ratings_val, max_user_id, max_item_id

def train_val_split(user_ids, item_ids, ratings, test_size=0.2):

    print("Started splitting")
    # Get unique users
    unique_users = np.unique(user_ids)

    # Initialize lists for training and validation data
    user_ids_train, item_ids_train, ratings_train = [], [], []
    user_ids_val, item_ids_val, ratings_val = [], [], []

    # For each user, split their data into training and validation sets
    for user in unique_users:
        # Get this user's data
        user_data_indices = np.where(user_ids == user)[0]
        user_item_ids = item_ids[user_data_indices]
        user_ratings = ratings[user_data_indices]

        # Determine the split index
        split_idx = int((1 - test_size) * len(user_data_indices))

        # Split the data
        user_ids_train.extend([user] * split_idx)
        item_ids_train.extend(user_item_ids[:split_idx])
        ratings_train.extend(user_ratings[:split_idx])

        user_ids_val.extend([user] * (len(user_data_indices) - split_idx))
        item_ids_val.extend(user_item_ids[split_idx:])
        ratings_val.extend(user_ratings[split_idx:])

    return np.array(user_ids_train), np.array(item_ids_train), np.array(ratings_train), np.array(user_ids_val), np.array(item_ids_val), np.array(ratings_val)

def filter_validation_set(user_ids_train, item_ids_train, ratings_train, user_ids_val, item_ids_val, ratings_val):

    print("Started filtering")
    # Convert training item IDs to a set for faster lookup
    train_items_set = set(item_ids_train)

    # Initialize lists for filtered validation data
    user_ids_val_filtered = []
    item_ids_val_filtered = []
    ratings_val_filtered = []

    # Iterate over the validation set
    for user, item, rating in zip(user_ids_val, item_ids_val, ratings_val):
        # If this item is in the training set, add it to the filtered validation set
        if item in train_items_set:
            user_ids_val_filtered.append(user)
            item_ids_val_filtered.append(item)
            ratings_val_filtered.append(rating)

    # Convert lists to numpy arrays
    user_ids_val_filtered = np.array(user_ids_val_filtered)
    item_ids_val_filtered = np.array(item_ids_val_filtered)
    ratings_val_filtered = np.array(ratings_val_filtered)

    return user_ids_train, item_ids_train, ratings_train, user_ids_val_filtered, item_ids_val_filtered, ratings_val_filtered

def load_data_without_ratings( file ) :

	# Initialise empty lists to store user IDs, item IDs, and timestamp
	user_ids = []
	item_ids = []
	timestamp = []

	# Open the CSV file and read its contents
	with open(file, 'r', newline='') as csvF:
	
		csv_reader = csv.reader(csvF)
	
		for row in csv_reader:
			user_ids.append(int(row[0]))  # Getting user ID from the first column
			item_ids.append(int(row[1]))  # Getting item ID from the second column
			timestamp.append(int(row[2])) # Getting timestamp from third column

	return user_ids, item_ids, timestamp

class SparseMatrix:
    def __init__(self):
        self.data = {}

    def insert(self, i, j, val):
        self.data[(i, j)] = val

    def get(self, i, j):
        return self.data.get((i, j), 0)

def create_user_item_matrix(users, items, ratings):
    # Determine unique users and items
    unique_users = np.unique(users)
    unique_items = np.unique(items)
    
    # Initialize empty user-item rating matrix with zero values
    user_item_matrix = SparseMatrix()
    
    # Create a mapping from user ID and item ID to index
    user_to_index = {user: i for i, user in enumerate(unique_users)}
    item_to_index = {item: i for i, item in enumerate(unique_items)}
    
    # Populate user-item matrix with ratings
    for idx in range(len(users)):
        user = users[idx]
        item = items[idx]
        rating = ratings[idx]
        
        user_index = user_to_index[user]
        item_index = item_to_index[item]
        user_item_matrix.insert(user_index, item_index, rating)
    
    return user_item_matrix, user_to_index, item_to_index

def matrix_factorization(R, P, Q, K, steps=25, alpha=0.002, beta=0.02):
    print("Starting matrix factorization")

    Q = Q.T

    for step in range(steps):
        start_time = time.time()
        # Print out the progress
        print(f"Step {step} of {steps}")
        for (i, j), r_ij in R.data.items():
            eij = r_ij - np.dot(P[i,:],Q[:,j])
            P[i, :] = P[i, :] + alpha * (2 * eij * Q[:, j] - beta * P[i, :])
            Q[:, j] = Q[:, j] + alpha * (2 * eij * P[i, :] - beta * Q[:, j])
        print("Stochastic gradient descent done.")
        e = 0
        for (i, j), r_ij in R.data.items():
            e = e + pow(r_ij - np.dot(P[i,:],Q[:,j]), 2)
            for k in range(K):
                e = e + (beta/2) * (pow(P[i][k],2) + pow(Q[k][j],2))
        if e < 0.001:
            break
        end_time = time.time()
        print(f"Time taken for step {step}: {end_time - start_time} seconds")
    return P, Q.T

def pred_ratings(user_item_matrix, max_user_id, max_item_id):
    # Calculate the number of users and items
    N = max_user_id + 1 
    M = max_item_id + 1 

    K = 12

    P = np.random.rand(N,K)
    Q = np.random.rand(M,K)

    nP, nQ = matrix_factorization(user_item_matrix, P, Q, K)

    return nP, nQ

#
# Helper function to get predictions from prediction matrix
#
def get_predictions(users, items, P, Q, user_to_index, item_to_index):
    predictions = []

    for index in range(len(users)):
        user_id = users[index]
        item_id = items[index]
        # Generate the prediction on-the-fly
        user_index = user_to_index[user_id]
        item_index = item_to_index[item_id]
        prediction = np.dot(P[user_index, :], Q[item_index, :])
        predictions.append((user_id, item_id, prediction))

    return predictions

#
# Function used to serialize a set of predictions to a .csv file
#
def serialize_predictions(output_file, P, Q, users, items, timestamps, user_to_index, item_to_index):
    # Open the file in write mode
    with open(output_file, 'w', newline='') as file:
        writer = csv.writer(file)
        
        # Write the predictions
        for user, item, timestamp in zip(users, items, timestamps):
            # Generate the prediction on-the-fly
            user_index = user_to_index[user]
            item_index = item_to_index[item]
            prediction = np.dot(P[user_index, :], Q[item_index, :])
            writer.writerow([user, item, prediction, timestamp])

def calculate_rmse(actual_ratings, predicted_ratings):
    return np.sqrt(np.mean((actual_ratings - predicted_ratings) ** 2))

def calculate_mae(actual_ratings, predicted_ratings):
    return np.mean(np.abs(actual_ratings - predicted_ratings))

if __name__ == '__main__':
     
    #
    # load test and training data into memory
    # INPUT = test and training files
    # OUTPUT = (user, item) rating matrix for test and training data
    #
    print("Started loading data...")
    start_time = time.time()
    users_train, items_train, ratings_train, users_val, items_val, ratings_val, max_user_id, max_item_id = load_data_with_ratings( file = 'train_20M_withratings.csv' )
    users_test, items_test, timestamp_test = load_data_without_ratings( file = 'test_20M_withoutratings.csv' )
    end_time = time.time()
    print(f"Time taken for loading data: {end_time - start_time} seconds")
    
    print("Creating user-item matrix...")
    start_time = time.time()
    user_item_matrix_train, user_to_index, item_to_index = create_user_item_matrix(users_train, items_train, ratings_train)
    end_time = time.time()
    print(f"Time taken for creating matrix: {end_time - start_time} seconds")
	
    #
	# call the train function to learn similarity weights for cosine similarity recommender system algorithm
	# INPUT = (user,item) rating matrix loaded from training dataset
	# OUTPUT = matrix (item to item simularity); vector (user average rating)
	#   
    print("Creating predictions matrix...")
    start_time = time.time()
    nP, nQ = pred_ratings(user_item_matrix_train, max_user_id, max_item_id)
    end_time = time.time()
    print(f"Time taken for creating predictions matrix: {end_time - start_time} seconds")

    print(f"P shape: {nP.shape}, Q shape: {nQ.shape}")

    print(list(user_to_index.items())[:5])
    print(list(item_to_index.items())[:5])

    # Assuming train_items, val_items, and test_items are your item sets
    # assert set(items_val).issubset(set(items_train)), "Validation items not in training set"
    # assert set(items_test).issubset(set(items_train)), "Test items not in training set"

	# #
	# # serialize the rating predictions to file ready for upload to ECS handin system
	# # INPUT = output CSV filename; (user,item) rating prediction matrix
	# # OUTPUT = None (predictions serialized to file)
	# #
    # print("Saving data to file.")
    # start_time = time.time()
    # serialize_predictions('submission2.csv', nP, nQ, users_test, items_test, timestamp_test, user_to_index, item_to_index)
    # end_time = time.time()
    # print(f"Time taken for serialising the rating predictions to file: {end_time - start_time} seconds")
     
    val_predictions = get_predictions(users_val, items_val, nP, nQ, user_to_index, item_to_index)

    actual_ratings = np.array(ratings_val)
    predicted_ratings = np.array([rating for user, item, rating in val_predictions])
    rmse = calculate_rmse(actual_ratings, predicted_ratings)
    print(f"RMSE for validation set: {rmse}")

    mae = calculate_mae(actual_ratings, predicted_ratings)
    print(f"MAE for validation set: {mae}")