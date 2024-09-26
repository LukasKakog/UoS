import numpy as np

# # def find_common_users(user_item_matrix, item1, item2):
    
# #     common_users = []
# #     if item1 != item2:
# #         # Find users who rated both items
# #         for user in user_item_matrix:
# #             for x in range(len(user)):
# #                 if ((user_item_matrix[user[x], item1] != 0) & (user_item_matrix[user[x], item2] != 0)):
# #                     indices = np.where(user_item_matrix == user)[0]
# #                     common_users = common_users + indices[0]

# #     return common_users

# import numpy as np

# def find_common_users(user_item_matrix, item1, item2):

#     if item1 == item2:
#         return []  # Items are the same, no common users

#     # Find non-zero indices for item1 and item2
#     nonzero_indices_item1 = np.nonzero(user_item_matrix[:, item1])[0]
#     nonzero_indices_item2 = np.nonzero(user_item_matrix[:, item2])[0]

#     # Find intersection of indices
#     common_users = np.intersect1d(nonzero_indices_item1, nonzero_indices_item2)

#     if len(common_users.tolist()) == 1:
#         return [] # Only one user rated both items, no common users
    
#     return common_users.tolist()

# def find_ratings(user_item_matrix, item1, item2, common_users):
#     if item1 == item2:
#         return []  # Items are the same, no rating returned
    
#     ratings = []
#     for user_id in common_users:
#         rating_item1 = user_item_matrix[user_id, item1]
#         rating_item2 = user_item_matrix[user_id, item2]
#         if rating_item1 != 0 and rating_item2 != 0:
#             ratings.append(rating_item1)
#             ratings.append(rating_item2)

#     return ratings


# def compute_similarity_matrix3(items_array):
#     # Extract unique items
#     items_list = sorted(set(items_array))
#     num_items = len(items_list)
    
#     print(items_array)
#     print(items_list)
#     # # Create a dictionary to map item IDs to indices
#     # item_to_index = {item: i for i, item in enumerate(items_list)}
    
#     # # Initialize item-item similarity matrix
#     # similarity_matrix = np.zeros((num_items, num_items))
    
#     # # Compute item-item similarity
#     # for i, item1 in enumerate(items_list):
#     #     for j, item2 in enumerate(items_list):
#     #         if i != j:
#     #             # Find common users who rated both items
#     #             common_users = find_common_users(user_item_matrix, item1, item2)
#     #             if len(common_users) > 0:
#     #                 # Find ratings for common users
#     #                 ratings_item1 = find_ratings(user_item_matrix, item1, item2, common_users)
#     #                 ratings_item2 = find_ratings(user_item_matrix, item2, item1, common_users)
#     #                 # Calculate cosine similarity between items
#     #                 if len(ratings_item1) > 0 and len(ratings_item2) > 0:
#     #                     similarity = np.dot(ratings_item1, ratings_item2) / (np.linalg.norm(ratings_item1) * np.linalg.norm(ratings_item2))
#     #                     similarity_matrix[i, j] = similarity
    
#     # # Save matrix in a txt file for error checking
#     # with open('similarityMatrix.txt', 'wb') as f:
#     #     for line in similarity_matrix:
#     #         np.savetxt(f, line, fmt='%.8f')
    
#     # return similarity_matrix

# # Example usage:
# # Assume user_item_matrix is your user-item rating matrix where each row represents a user and each column represents an item,
# # and item1_id and item2_id are the IDs of the two items you want to find common users for.
# user_item_matrix = np.array([[1, 2, 3, 1],
#                              [0, 2, 0, 1],
#                              [1, 5, 4, 0],
#                              [0, 0, 4, 0],
#                              [0, 5, 3, 4],
#                              [1, 5, 2, 0]])

# item1_id = 0  # ID of the first item
# item2_id = 2  # ID of the second item

# common_users = find_common_users(user_item_matrix, item1_id, item2_id)
# print("Common users who rated both items {} and {}: {}".format(item1_id, item2_id, common_users))

# print("Ratings: ", find_ratings(user_item_matrix, item1_id, item2_id, common_users))

# compute_similarity_matrix3([1, 5, 2, 0])

#
# Example function to compute similarity matrix using cosine similarity
#
# def compute_similarity_matrix(items_array, ratings_array):
    
#     # Extract unique items and users 
#     items_list = sorted(set(items_array))  
    
#     # Create dictionaries to map item and user IDs to indices
#     item_to_index = {item: i for i, item in enumerate(items_list)}
    
#     # Initialize item-item similarity matrix
#     similarity_matrix = np.zeros((len(items_list), len(items_list)))
    
#     # Compute item-item similarity
#     for x in range(len(items_array)):
#         item, rating = items_array[x], ratings_array[x] 
#         i = item_to_index[item]
#         for other_item in items_list:
#             j = item_to_index[other_item]
#             if item != other_item:
#                 similarity_matrix[i, j] += rating  # Accumulate ratings for items rated by the same user
    
#     # Normalize similarity matrix
#     norms = np.linalg.norm(similarity_matrix, axis=1)
#     norms[norms == 0] = 1  # Handle division by zero
#     similarity_matrix = similarity_matrix / norms[:, np.newaxis]
    
#     #Save matrix in a txt file for error checkings
#     with open('similarityMatrix.txt','wb') as f:
#         for line in similarity_matrix:
#             np.savetxt(f, line, fmt='%.5f')
    
#     return similarity_matrix
		
# def compute_similarity_matrix2(items_array, ratings_array):
#     # Extract unique items
#     items_list = sorted(set(items_array))
    
#     # Create a dictionary to map item IDs to indices
#     item_to_index = {item: i for i, item in enumerate(items_list)}
    
#     # Initialize item-item similarity matrix
#     similarity_matrix = np.zeros((len(items_list), len(items_list)))
    
#     # Compute item-item similarity
#     for i, item1 in enumerate(items_list):
#         for j, item2 in enumerate(items_list):
#             # Find ratings for both items
#             ratings_item1 = ratings_array[items_array == item1]
#             ratings_item2 = ratings_array[items_array == item2]
#             # Calculate cosine similarity between items
#             similarity = np.dot(ratings_item1, ratings_item2) / (np.linalg.norm(ratings_item1) * np.linalg.norm(ratings_item2))
#             similarity_matrix[i, j] = similarity
    
#     # Save matrix in a txt file for error checking
#     with open('similarityMatrix.txt', 'wb') as f:
#         for line in similarity_matrix:
#             np.savetxt(f, line, fmt='%.5f')
    
#     return similarity_matrix

# Example user-item rating matrix (replace this with your actual data)
user_item_matrix = np.array([
    [5, 4, 0, 0],
    [0, 0, 3, 4],
    [2, 0, 0, 0],
    [0, 2, 0, 5]
])

# Calculate the user average rating vector
user_avg_ratings = np.mean(user_item_matrix, axis=1)

# Calculate item-to-item similarity matrix (example: using cosine similarity)
def cosine_similarity(a, b):
    dot_product = np.dot(a, b)
    norm_a = np.linalg.norm(a)
    norm_b = np.linalg.norm(b)
    similarity = dot_product / (norm_a * norm_b)
    return similarity

num_items = user_item_matrix.shape[1]
item_similarity_matrix = np.zeros((num_items, num_items))

for i in range(num_items):
    for j in range(num_items):
        item_similarity_matrix[i, j] = cosine_similarity(user_item_matrix[:, i], user_item_matrix[:, j])

print("User Average Rating Vector:")
print(user_avg_ratings)
print("\nItem-to-Item Similarity Matrix:")
print(item_similarity_matrix)

# #
# # Function used to perform knn imputation
# #
# def knn( user_item_matrix, k=5 ) :
# 	# Compute pairwise distances between users (cosine similarity)
#     norm_user_item_matrix = np.linalg.norm(user_item_matrix, axis=1, keepdims=True)
#     normalized_user_item_matrix = user_item_matrix / norm_user_item_matrix
#     user_distances = np.dot(normalized_user_item_matrix, normalized_user_item_matrix.T)
    
#     # Replace diagonal elements (self-distances) with 0
#     np.fill_diagonal(user_distances, 0)
    
#     # Find k nearest neighbors for each user
#     k_nearest_neighbors = np.argsort(user_distances, axis=1)[:, -k:]
    
#     # Perform KNN imputation
#     for i in range(len(user_item_matrix)):
#         for j in range(len(user_item_matrix[i])):
#             if user_item_matrix[i, j] == 0:
#                 nearest_neighbor_ratings = user_item_matrix[k_nearest_neighbors[i], j]
#                 non_zero_ratings = nearest_neighbor_ratings[nearest_neighbor_ratings != 0]
#                 if len(non_zero_ratings) > 0:
#                     user_item_matrix[i, j] = np.mean(non_zero_ratings)

#     # Save matrix in a txt file for error checking
#     with open('knnMatrix.txt', 'wb') as f:
#         for line in user_item_matrix:
#             np.savetxt(f, line, fmt='%.8f')
#     return user_item_matrix

# def find_common_users(user_item_matrix, item1, item2):

#     if item1 == item2:
#         return []  # Items are the same, no common users

#     # Find non-zero indices for item1 and item2
#     nonzero_indices_item1 = np.nonzero(user_item_matrix[:, item1])[0]
#     nonzero_indices_item2 = np.nonzero(user_item_matrix[:, item2])[0]

#     # Find intersection of indices
#     common_users = np.intersect1d(nonzero_indices_item1, nonzero_indices_item2)

#     if len(common_users.tolist()) == 1:
#         return [] # Only one user rated both items, no common users
    
#     return common_users.tolist()

# def find_ratings(user_item_matrix, item1, item2, common_users):
#     if item1 == item2:
#         return []  # Items are the same, no rating returned
    
#     ratings = []
#     for user_id in common_users:
#         rating_item1 = user_item_matrix[user_id, item1]
#         rating_item2 = user_item_matrix[user_id, item2]
#         if rating_item1 != 0 and rating_item2 != 0:
#             ratings.append(rating_item1)

#     return ratings

# def compute_similarity_matrix(user_item_matrix, users_array, items_array, ratings_array):
#     # Extract unique items
#     items_list = sorted(set(items_array))
#     num_items = len(user_item_matrix[0])
    
#     # Create a dictionary to map item IDs to indices
#     item_to_index = {item: i for i, item in enumerate(items_list)}
    
#     # Initialize item-item similarity matrix
#     similarity_matrix = np.zeros((num_items, num_items))
    
#     # Compute item-item similarity
#     for i, item1 in enumerate(items_list):
#         for j, item2 in enumerate(items_list):
#             if i != j:
#                 # Find common users who rated both items
#                 common_users = find_common_users(user_item_matrix, item1 - 1, item2 - 1)
#                 if len(common_users) > 0:
#                     # Find ratings for common users
#                     ratings_item1 = find_ratings(user_item_matrix, item1 - 1, item2 - 1, common_users)
#                     ratings_item2 = find_ratings(user_item_matrix, item2 - 1, item1 - 1, common_users)
#                     # Calculate cosine similarity between items
#                     if len(ratings_item1) > 0 and len(ratings_item2) > 0:
#                         similarity = np.dot(ratings_item1, ratings_item2) / (np.linalg.norm(ratings_item1) * np.linalg.norm(ratings_item2))
#                         similarity_matrix[i, j] = similarity
    
#     # Save matrix in a txt file for error checking
#     with open('similarityMatrix.txt', 'wb') as f:
#         for line in similarity_matrix:
#             np.savetxt(f, line, fmt='%.8f')
    
#     return similarity_matrix

# print("Test Rating 1:", user_item_matrix_train[0,10])
	# print("Test Rating 2:", user_item_matrix_train[430,581])
	# print("Test Rating 3:", user_item_matrix_train[430,580])
	# print("Test Rating 4:", user_item_matrix_train[430,602])
	# print("Test Rating 5:", user_item_matrix_train[650,1671])

	# knn_matrix_train = knn(user_item_matrix_train)

	# # print("Test Rating 1:", user_item_matrix_train[0,10])
	# # print("Test Rating 2:", user_item_matrix_train[430,581])
	# # print("Test Rating 3:", user_item_matrix_train[430,580])
	# # print("Test Rating 4:", user_item_matrix_train[430,602])

	# similarity_matrix_train = compute_similarity_matrix(knn_matrix_train, users_train, items_train, ratings_train)

def cosine_similarity2(a, b):
    # Get indices where both ratings are non-zero
    non_zero_indices = np.logical_and(a != 0, b != 0)
    
    # Calculate dot product only for non-zero ratings
    dot_product = np.sum(a[non_zero_indices] * b[non_zero_indices])
    
    # Calculate norms only for non-zero ratings
    norm_a = np.linalg.norm(a[non_zero_indices])
    norm_b = np.linalg.norm(b[non_zero_indices])
    
    # Handle the case when either norm is zero
    if norm_a == 0 or norm_b == 0:
        return 0
    
    # Calculate cosine similarity
    similarity = dot_product / (norm_a * norm_b)
    return similarity

num_items = user_item_matrix.shape[1]
item_similarity_matrix = np.zeros((num_items, num_items))

for i in range(num_items):
    for j in range(num_items):
        item_similarity_matrix[i, j] = cosine_similarity2(user_item_matrix[:, i], user_item_matrix[:, j])

print("Item-to-Item Similarity Matrix:")
print(item_similarity_matrix)

def adjusted_cosine_similarity_ignore_zeros(ratings_matrix):
    # Create a mask for non-zero values
    non_zero_mask = ratings_matrix != 0

    # Compute the mean of each row (user), ignoring zero values
    row_means = np.nanmean(np.where(non_zero_mask, ratings_matrix, np.nan), axis=1, keepdims=True)

    # Subtract the mean from each element in the matrix, ignoring zero values
    adjusted_ratings_matrix = np.where(non_zero_mask, ratings_matrix - row_means, 0)

    # Compute the cosine similarity between items, ignoring zero values
    similarity_matrix = np.dot(adjusted_ratings_matrix, adjusted_ratings_matrix.T)

    # Compute the norms for each row (user), ignoring zero values
    norms = np.sqrt(np.sum(adjusted_ratings_matrix ** 2, axis=1, keepdims=True))

    # Normalize the similarity matrix
    norms_product = np.dot(norms, norms.T)
    norms_product[norms_product == 0] = 1  # Avoid division by zero
    similarity_matrix /= norms_product

    return similarity_matrix

# Example usage:
ratings_matrix = np.array([[5, 3, 0, 0],
                           [4, 0, 0, 0],
                           [1, 1, 0, 0],
                           [2, 0, 0, 0],
                           [0, 0, 4, 5]])

similarity_matrix = adjusted_cosine_similarity_ignore_zeros(ratings_matrix)

print("\nItem-to-Item Similarity Matrix using adjusted cosine similarity:")
print(similarity_matrix)

#
# # infer function for cosine similarity recommender system algorithm
# #
# def predict_ratings(user_item_matrix_test, item_sim_matrix, user_average_rating_vector):
#     num_users, num_items = user_item_matrix_test.shape
#     prediction_matrix = np.zeros((num_users, num_items))

#     for user_idx in range(num_users):
#         for item_idx in range(num_items):
#             if user_item_matrix_test[user_idx, item_idx] == 0:  # Check if the rating is missing and needs to be predicted
#                 sim_sum = 0
#                 weighted_sum = 0

#                 for other_item_idx in range(num_items):
#                     similarity = item_sim_matrix[item_idx, other_item_idx]
#                     rating = user_item_matrix_test[user_idx, other_item_idx]
#                     if rating != 0:  # Only consider rated items for similarity calculation
#                         weighted_sum += similarity * rating
#                         sim_sum += abs(similarity)

#                 if sim_sum != 0:
#                     prediction = weighted_sum / sim_sum
#                 else:
#                     prediction = 0

#                 prediction_matrix[user_idx, item_idx] = prediction + user_average_rating_vector[user_idx]

#     np.savetxt("predictionMatrix.csv", prediction_matrix, delimiter=",", fmt="%.5f")
    
#     return prediction_matrix

# #
# # infer function for cosine similarity recommender system algorithm
# #
# def predict_ratings(user_item_matrix_test, item_sim_matrix, user_average_rating_vector):
#     num_users, num_items = user_item_matrix_test.shape
#     prediction_matrix = np.zeros((num_users, num_items))

#     for user_idx in range(num_users):
#         for item_idx in range(num_items):
#             if user_item_matrix_test[user_idx, item_idx] == 0:  # Check if the rating is missing and needs to be predicted
#                 sim_sum = 0
#                 weighted_sum = 0

#                 for other_item_idx in range(num_items):
#                     similarity = item_sim_matrix[item_idx, other_item_idx]
#                     rating = user_item_matrix_test[user_idx, other_item_idx]
#                     if rating != 0:  # Only consider rated items for similarity calculation
#                         weighted_sum += similarity * rating
#                         sim_sum += abs(similarity)

#                 if sim_sum != 0:
#                     prediction = weighted_sum / sim_sum
#                 else:
#                     prediction = 0

#                 prediction_matrix[user_idx, item_idx] = prediction + user_average_rating_vector[user_idx]

#     np.savetxt("predictionMatrix.csv", prediction_matrix, delimiter=",", fmt="%.5f")
    
#     return prediction_matrix

#############################################################################################################################

# def predict_ratings(user_item_matrix, item_similarity_matrix, user_avg_ratings ):
#     # Initialize a matrix to store the predicted ratings
#     predicted_ratings = np.zeros(user_item_matrix.shape)

#     # Iterate over each user-item pair
#     for user in range(user_item_matrix.shape[0]):
#         for item in range(user_item_matrix.shape[1]):
#             # Only predict ratings for user-item pairs that are missing a rating
#             if user_item_matrix[user, item] == 0:
#                 # Calculate the numerator and denominator of the formula
#                 numerator = np.sum(item_similarity_matrix[item, :] * (user_item_matrix[user, :] - user_avg_ratings[user]))
#                 denominator = np.sum(np.abs(item_similarity_matrix[item, :]))

#                 # Calculate the predicted rating
#                 if denominator != 0:
#                     p_rating = user_avg_ratings[user] + numerator / denominator
#                     if p_rating > 5:
#                         predicted_ratings[user, item] = 5
#                     elif p_rating < 0.5:
#                         predicted_ratings[user, item] = 0.5
#                     else:
#                         predicted_ratings[user, item] = p_rating
#                 else:
#                     predicted_ratings[user, item] = user_avg_ratings[user]

#     np.savetxt("predictionMatrix.csv", predicted_ratings, delimiter=",", fmt="%.5f")

#     return predicted_ratings

# #
# # training function for cosine similarity recommender system algorithm
# #
# def train_model(user_item_matrix) :

#     user_avg_ratings = np.nanmean(np.where(user_item_matrix != 0, user_item_matrix, np.nan), axis=1)

#     num_items = user_item_matrix.shape[1]
#     item_similarity_matrix = np.zeros((num_items, num_items))

#     for i in range(num_items):
#         for j in range(num_items):
#             if i != j:
#                 item_similarity_matrix[i, j] = cosine_similarity(user_item_matrix[:, i], user_item_matrix[:, j])
#             else:
#                 item_similarity_matrix[i, j] = 0

#     np.savetxt("similarityMatrix.csv", item_similarity_matrix, delimiter=",", fmt="%.5f")
# 	#
# 	# return the populated user average rating vector and the item to item simularity matrix
# 	#
#     return user_avg_ratings, item_similarity_matrix

# #
# # Calculate cosine similarity between two items used in item-to-item similarity matrix 
# #
# def cosine_similarity(a, b):
#     dot_product = np.dot(a, b)
#     norm_a = np.linalg.norm(a)
#     norm_b = np.linalg.norm(b)

# 	# Check for zero vectors
#     if norm_a == 0 or norm_b == 0:
#         return 0
	
#     similarity = dot_product / (norm_a * norm_b)
#     return similarity

# def adjusted_cosine_similarity(matrix, item1, item2):
#     # Find users who have rated both items
#     users_who_rated_both_items = np.where((matrix[:, item1] != 0) & (matrix[:, item2] != 0))[0]
    
#     # If no users have rated both items, return 0
#     if len(users_who_rated_both_items) == 0:
#         return 0
    
#     # Get the ratings given by the users who have rated both items
#     user_ratings = matrix[users_who_rated_both_items]
    
#     # Calculate the average rating for each user
#     user_averages = np.mean(user_ratings, axis=1)
    
#     # Calculate the numerator of the formula
#     numerator = np.sum((user_ratings[:, item1] - user_averages) * (user_ratings[:, item2] - user_averages))
    
#     # Calculate the denominator of the formula
#     denominator = np.sqrt(np.sum((user_ratings[:, item1] - user_averages)**2)) * \
#                   np.sqrt(np.sum((user_ratings[:, item2] - user_averages)**2))
    
#     # If the denominator is 0, return 0
#     if denominator == 0:
#         return 0
    
#     # Return the adjusted cosine similarity
#     return numerator / denominator\

#############################################################################################################################