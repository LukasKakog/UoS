import numpy as np
import csv

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
def load_data_with_ratings( file ) :

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

	return user_ids, item_ids, ratings, timestamp

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

#
# Function used to create a user-item rating matrix
#
def create_user_item_matrix( users, items, ratings ) :
    
    # Determine unique users and items
    unique_users = np.unique(users)
    max_item_id = max(items)
    
    # Initialize empty user-item rating matrix with zero values
    user_item_matrix = np.zeros((len(unique_users), max_item_id))
    
    # Create a mapping from user ID to index
    user_to_index = {user: i for i, user in enumerate(unique_users)}
    
    # Populate user-item matrix with ratings
    for idx in range(len(users)):
        user = users[idx]
        item = items[idx]
        rating = ratings[idx]
        
        user_index = user_to_index[user]
        item_index = item - 1  # Adjust item ID to 0-based index
        user_item_matrix[user_index, item_index] = rating
    
    return user_item_matrix

def adjusted_cosine_similarity(matrix, item1, item2):
    # Find users who have rated both items
    users_who_rated_both_items = np.where((matrix[:, item1] != 0) & (matrix[:, item2] != 0))[0]
    
    # If no users have rated both items, return 0
    if len(users_who_rated_both_items) == 0:
        return 0
    
    # Get the ratings given by the users who have rated both items
    user_ratings_item1 = matrix[users_who_rated_both_items, item1]
    user_ratings_item2 = matrix[users_who_rated_both_items, item2]
    
    # Calculate the average rating for each user
    user_averages = np.mean(matrix[users_who_rated_both_items], axis=1)
    
    # Calculate the numerator of the formula
    numerator = np.sum((user_ratings_item1 - user_averages) * (user_ratings_item2 - user_averages))
    
    # Calculate the denominator of the formula
    denominator = np.sqrt(np.sum((user_ratings_item1 - user_averages)**2)) * \
                  np.sqrt(np.sum((user_ratings_item2 - user_averages)**2))
    
    # If the denominator is 0, return 0
    if denominator == 0:
        return 0
    
    # Return the adjusted cosine similarity
    return numerator / denominator

#
# Training function for cosine similarity recommender system algorithm
#
def train_model(user_item_matrix) :

    user_avg_ratings = np.nanmean(np.where(user_item_matrix != 0, user_item_matrix, np.nan), axis=1)

    num_items = user_item_matrix.shape[1]
    item_similarity_matrix = np.zeros((num_items, num_items))

    for i in range(num_items):
        for j in range(num_items):
            if i != j:
                item_similarity_matrix[i, j] = adjusted_cosine_similarity(user_item_matrix, i, j)
            else:
                item_similarity_matrix[i, j] = 0

    return user_avg_ratings, item_similarity_matrix

#
# This function calculates a prediction for all the ratings in the user_item_matrix
#
def predict_ratings(user_item_matrix, item_similarity_matrix, user_avg_ratings):
    # Initialize a matrix to store the predicted ratings
    predicted_ratings = np.zeros(user_item_matrix.shape)

    # Calculate the overall average rating
    overall_avg_rating = np.nanmean(np.where(user_item_matrix != 0, user_item_matrix, np.nan))

    # Iterate over each user-item pair
    for user in range(user_item_matrix.shape[0]):
        for item in range(user_item_matrix.shape[1]):
            # Only predict ratings for user-item pairs that are missing a rating
            if user_item_matrix[user, item] == 0:
                # Calculate the numerator and denominator of the formula
                rated_items = np.where(user_item_matrix[user, :] != 0)[0]
                numerator = np.sum(item_similarity_matrix[item, rated_items] * (user_item_matrix[user, rated_items] - user_avg_ratings[user]))
                denominator = np.sum(np.abs(item_similarity_matrix[item, rated_items]))

                # Calculate the predicted rating
                if denominator != 0:
                    p_rating = user_avg_ratings[user] + 7 * (numerator / denominator) # Hyperparameter used for tuning
                    if p_rating > 5:
                        predicted_ratings[user, item] = 5
                    elif p_rating < 0.5:
                        predicted_ratings[user, item] = 0.5
                    else:
                        predicted_ratings[user, item] = p_rating
                else:
                    # Fall back on the overall average rating
                    predicted_ratings[user, item] = overall_avg_rating

    return predicted_ratings

#
# Helper function to get predictions from prediction matrix
#
def get_predictions(users, items, prediction_matrix):
    predictions = []

    for index in range(len(users)):
        user_id = users[index]
        item_id = items[index]
        prediction = prediction_matrix[user_id - 1, item_id - 1]
        predictions.append((user_id, item_id, prediction))

    return predictions

#
# Helper function to write predictions and timestamps to csv file
#
def write_predictions_to_csv(predictions, timestamps, filename):
    # Open the file in write mode
    with open(filename, 'w', newline='') as file:
        writer = csv.writer(file)
        
        # Write the predictions
        for (user, item, prediction), timestamp in zip(predictions, timestamps):
            writer.writerow([user, item, prediction, timestamp])

#
# Function used to serialize a set of predictions to a .csv file
#
def serialize_predictions( output_file, prediction_matrix, users, items, timestamps ) :
	
    predictions = get_predictions(users, items, prediction_matrix)

    write_predictions_to_csv(predictions, timestamps, output_file)

if __name__ == '__main__':
     
	#
	# load test and training data into memory
	# INPUT = test and training files
	# OUTPUT = (user, item) rating matrix for test and training data
	#
	users_train, items_train, ratings_train, timestamp_train = load_data_with_ratings( file = 'train_100k_withratings.csv' )
	users_test, items_test, timestamp_test = load_data_without_ratings( file = 'test_100k_withoutratings.csv' )
	
	user_item_matrix_train = create_user_item_matrix(users_train, items_train, ratings_train)

	#
	# call the train function to learn similarity weights for cosine similarity recommender system algorithm
	# INPUT = (user,item) rating matrix loaded from training dataset
	# OUTPUT = matrix (item to item simularity); vector (user average rating)
	#   
	user_average_rating_vector, item_sim_matrix = train_model( user_item_matrix_train )
	
	#
	# call the infer function to execute the cosine similarity recommender system algorithm
	# INPUT = (user,item) rating matrix loaded from test dataset; matrix (item to item simularity); vector (user average rating)
	# OUTPUT = (user,item) rating prediction matrix
	#
	pred_matrix = predict_ratings(user_item_matrix_train, item_sim_matrix, user_average_rating_vector)
    
	#
	# serialize the rating predictions to file ready for upload to ECS handin system
	# INPUT = output CSV filename; (user,item) rating prediction matrix
	# OUTPUT = None (predictions serialized to file)
	#
	serialize_predictions( 'submission.csv', pred_matrix, users_test, items_test, timestamp_test )

     