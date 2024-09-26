import numpy as np
import csv, time

"""
This script implements a collaborative filtering method for recommendation systems called Matrix Factorization. 

Matrix Factorization is a class of collaborative filtering algorithms used in recommender systems. 
It works by factorizing the user-item interaction matrix into the product of two lower dimensionality rectangular matrices. 
One matrix can be seen as the user matrix where rows represent users and columns are latent factors. 
The other matrix is the item matrix where rows are items and columns are the same latent factors. 
The latent factors here are learned from the data.

The main idea is to capture the underlying patterns in the user-item interactions. 
These patterns can be thought of as 'tastes' and 'preferences' of the user. 
For example, a user's taste in movies might be a combination of liking action movies, comedies, and dramas, which are all latent factors.

The user-item interaction matrix (R) is factorized into two matrices P and Q. 
Matrix P represents the user-factors matrix and Q is the item-factors matrix. 
The predicted rating for a user-item pair is calculated by taking the dot product of the corresponding user-factor vector and item-factor vector.

The factorization is done using Stochastic Gradient Descent (SGD). 
The objective function that SGD tries to minimize is the mean squared error of the observed ratings,
and the ratings predicted by the dot product of the user-factor and item-factor vectors. 
A regularization term is added to avoid overfitting.

The learning process is iterative. 
For each iteration, the algorithm goes through every rating in the dataset, 
computes the prediction error for that rating (which is the difference between the observed and predicted rating), 
and updates the user-factor and item-factor vectors based on the gradient of the error with respect to the factors. 
This process is repeated for a number of iterations or until the error converges to a minimum.

After the factorization, the script predicts the ratings for the test set by taking the dot product 
of the corresponding user-factor and item-factor vectors from the factorized user-item matrix. 
The predicted ratings are then written to a CSV file.
"""

def load_data_with_ratings(file):
    """Loads a CSV file with user IDs, item IDs, ratings, and timestamps.

    Args:
        file (str): The name of the CSV file.

    Returns:
        tuple: A tuple containing numpy arrays of user IDs, item IDs, ratings, 
        the maximum user ID and the maximum item ID.
    """
    user_ids = []
    item_ids = []
    ratings = []
    timestamp = []

    with open(file, 'r', newline='') as csvF:
        csv_reader = csv.reader(csvF)
        for row in csv_reader:
            user_ids.append(int(row[0]))  
            item_ids.append(int(row[1]))  
            ratings.append(float(row[2])) 
            timestamp.append(int(row[3])) 

    user_ids_train = np.array(user_ids)
    item_ids_train = np.array(item_ids)
    ratings_train = np.array(ratings)
    timestamp = np.array(timestamp)

    max_user_id = max(user_ids)
    max_item_id = max(item_ids)

    return user_ids_train, item_ids_train, ratings_train, max_user_id, max_item_id

def load_data_without_ratings(file):
    """Loads a CSV file with user IDs, item IDs, and timestamps.

    Args:
        file (str): The name of the CSV file.

    Returns:
        tuple: A tuple containing lists of user IDs, item IDs, and timestamps.
    """
    user_ids = []
    item_ids = []
    timestamp = []

    with open(file, 'r', newline='') as csvF:
        csv_reader = csv.reader(csvF)
        for row in csv_reader:
            user_ids.append(int(row[0]))  
            item_ids.append(int(row[1]))  
            timestamp.append(int(row[2])) 

    return user_ids, item_ids, timestamp

class SparseMatrix:
    """A class used to represent a sparse matrix.

    Attributes:
        data (dict): A dictionary to store the non-zero elements of the matrix.
    """
    def __init__(self):
        self.data = {}

    def insert(self, i, j, val):
        """Inserts a value into the matrix at the specified indices.

        Args:
            i (int): The row index.
            j (int): The column index.
            val (float): The value to be inserted.
        """
        self.data[(i, j)] = val

    def get(self, i, j):
        """Gets the value at the specified indices in the matrix.

        Args:
            i (int): The row index.
            j (int): The column index.

        Returns:
            float: The value at the specified indices. Returns 0 if no value is present.
        """
        return self.data.get((i, j), 0)

def create_user_item_matrix(users, items, ratings):
    """Creates a user-item matrix from the given data.

    Args:
        users (list): A list of user IDs.
        items (list): A list of item IDs.
        ratings (list): A list of ratings.

    Returns:
        tuple: A tuple containing the user-item matrix, and dictionaries mapping user IDs and item IDs to indices.
    """
    unique_users = np.unique(users)
    unique_items = np.unique(items)
    
    user_item_matrix = SparseMatrix()
    
    user_to_index = {user: i for i, user in enumerate(unique_users)}
    item_to_index = {item: i for i, item in enumerate(unique_items)}
    
    for idx in range(len(users)):
        user = users[idx]
        item = items[idx]
        rating = ratings[idx]
        
        user_index = user_to_index[user]
        item_index = item_to_index[item]
        user_item_matrix.insert(user_index, item_index, rating)
    
    return user_item_matrix, user_to_index, item_to_index

def matrix_factorization(R, P, Q, K, steps=25, alpha=0.002, beta=0.02):
    """Performs matrix factorization using stochastic gradient descent.

    Args:
        R (SparseMatrix): The user-item matrix.
        P (numpy.ndarray): The initial user feature matrix.
        Q (numpy.ndarray): The initial item feature matrix.
        K (int): The number of latent factors.
        steps (int, optional): The number of iterations. Defaults to 25.
        alpha (float, optional): The learning rate. Defaults to 0.002.
        beta (float, optional): The regularization parameter. Defaults to 0.02.

    Returns:
        tuple: A tuple containing the final user feature matrix and item feature matrix.
    """
    print("Starting matrix factorization")

    Q = Q.T

    for step in range(steps):
        start_time = time.time()
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
    """Predicts the ratings for all user-item pairs.

    Args:
        user_item_matrix (SparseMatrix): The user-item matrix.
        max_user_id (int): The maximum user ID.
        max_item_id (int): The maximum item ID.

    Returns:
        tuple: A tuple containing the final user feature matrix and item feature matrix.
    """
    N = max_user_id + 1 
    M = max_item_id + 1 

    K = 12

    P = np.random.rand(N,K)
    Q = np.random.rand(M,K)

    nP, nQ = matrix_factorization(user_item_matrix, P, Q, K)

    return nP, nQ

def find_new_items_and_user_avg_rating(users_train, items_train, ratings_train, users_test, items_test):
    """In some cases, there might be items in the test set that are not present in the training set. 
    This is often referred to as the ‘cold start’ problem in recommendation systems. 
    Since these items have not been seen during the training phase, the model does not have information to accurately predict the ratings for these items.

    To handle this, the script identifies these new items and finds the users who have rated these items in the test set. 
    It then calculates the average rating these specific users have given to items in the training set. 
    This average rating is used as the predicted rating for the new items.

    This approach provides a reasonable estimate for the unknown ratings and helps to ensure 
    that the model can provide recommendations even for items that it has not seen during training. 

    Args:
        users_train (list): The user IDs in the training set.
        items_train (list): The item IDs in the training set.
        ratings_train (list): The ratings in the training set.
        users_test (list): The user IDs in the test set.
        items_test (list): The item IDs in the test set.

    Returns:
        dict: A dictionary where the keys are the new items and the values are the average user ratings.
    """
    # Extract item sets
    train_items = set(items_train)
    test_items = set(items_test)

    # Find new items
    new_items = test_items - train_items

    # Find the users who rated these new items
    users_who_rated_new_items = [user for user, item in zip(users_test, items_test) if item in new_items]

    # Calculate average rating for these users based on the training set
    user_ratings = {user: [] for user in users_train}
    for user, item, rating in zip(users_train, items_train, ratings_train):
        if user in users_who_rated_new_items:
            user_ratings[user].append(rating)

    # Calculate the average rating for each user
    user_avg_ratings = {user: sum(ratings)/len(ratings) for user, ratings in user_ratings.items() if ratings}

    # Create a dictionary where the keys are the new items and the values are dictionaries 
    # where the keys are the users who rated the new items and the values are their average ratings
    new_item_user_avg_ratings = {item: {user: user_avg_ratings[user] for user in users_who_rated_new_items} for item in new_items}

    return new_item_user_avg_ratings

def serialize_predictions(output_file, P, Q, users_test, items_test, timestamps, user_to_index, item_to_index, new_item_user_avg_ratings):
    """Writes the predicted ratings to a CSV file.

    Args:
        output_file (str): The name of the output file.
        P (numpy.ndarray): The user feature matrix.
        Q (numpy.ndarray): The item feature matrix.
        users_test (list): The user IDs in the test set.
        items_test (list): The item IDs in the test set.
        timestamps (list): A list of timestamps.
        user_to_index (dict): A dictionary mapping user IDs to indices.
        item_to_index (dict): A dictionary mapping item IDs to indices.
        new_item_user_avg_ratings (dict): A nested dictionary where the first key is the item and the second key is the user, and the value is the average rating.
    """
    with open(output_file, 'w', newline='') as file:
        writer = csv.writer(file)
        
        for user, item, timestamp in zip(users_test, items_test, timestamps):
            if item in item_to_index:
                user_index = user_to_index[user]
                item_index = item_to_index[item]
                prediction = np.dot(P[user_index, :], Q[item_index, :])
            else:
                # Use the average user rating for items not in the training set
                prediction = new_item_user_avg_ratings.get(item, {}).get(user, 2.5)  # Use a default value of 2.5 if the user or item is not in the dictionary
            writer.writerow([user, item, prediction, timestamp])

if __name__ == '__main__':
    """Main function to run the script."""
    print("Started loading data...")
    start_time = time.time()
    users_train, items_train, ratings_train, max_user_id, max_item_id = load_data_with_ratings( file = 'train_20M_withratings.csv' )
    users_test, items_test, timestamp_test = load_data_without_ratings( file = 'test_20M_withoutratings.csv' )
    end_time = time.time()
    print(f"Time taken for loading data: {end_time - start_time} seconds")
    
    print("Creating user-item matrix...")
    start_time = time.time()
    user_item_matrix_train, user_to_index, item_to_index = create_user_item_matrix(users_train, items_train, ratings_train)
    end_time = time.time()
    print(f"Time taken for creating matrix: {end_time - start_time} seconds")
	
    print("Creating predictions matrix...")
    start_time = time.time()
    nP, nQ = pred_ratings(user_item_matrix_train, max_user_id, max_item_id)
    end_time = time.time()
    print(f"Time taken for creating predictions matrix: {end_time - start_time} seconds")

    print("Calculating average user ratings...")
    start_time = time.time()
    user_avg_ratings = find_new_items_and_user_avg_rating(users_train, items_train, ratings_train, users_test, items_test)
    end_time = time.time()
    print(user_avg_ratings)
    print(f"Time taken for calculating average user ratings: {end_time - start_time} seconds")

    print("Saving data to file.")
    start_time = time.time()
    serialize_predictions('submission.csv', nP, nQ, users_test, items_test, timestamp_test, user_to_index, item_to_index, user_avg_ratings)
    end_time = time.time()
    print(f"Time taken for serialising the rating predictions to file: {end_time - start_time} seconds")
