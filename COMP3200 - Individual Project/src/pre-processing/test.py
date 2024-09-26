import json, time
from tensorflow.keras.preprocessing.text import Tokenizer # type: ignore

# Initialize counters
count_minus1 = 0
count_0 = 0
count_1 = 0
count_between_minus1_and_0 = 0
count_between_0_and_1 = 0

with open('training_set.jsonl', 'r') as f:
    # Use a list comprehension to create a list of lines
    lines = [line for line in f]


# Print the number of lines
print(f"The 'training_set.jsonl' file has {len(lines)} lines.")

# Open the file
with open('training_set.jsonl', 'r') as f:

    for line in f:
        paper = json.loads(line)
        cd_index = paper["cd_index"]

        if cd_index == -1:
            count_minus1 += 1
        elif cd_index == 0:
            count_0 += 1
        elif cd_index == 1:
            count_1 += 1
        elif -1 < cd_index < 0:
            count_between_minus1_and_0 += 1
        elif 0 < cd_index < 1:
            count_between_0_and_1 += 1


# Print the results
print(f"Number of cd_indices equal to -1: {count_minus1}")
print(f"Number of cd_indices equal to 0: {count_0}")
print(f"Number of cd_indices equal to 1: {count_1}")
print(f"Number of cd_indices between -1 and 0: {count_between_minus1_and_0}")
print(f"Number of cd_indices between 0 and 1: {count_between_0_and_1}")
print("Total number of indices:", count_minus1 + count_0 + count_1 + count_between_minus1_and_0 + count_between_0_and_1)

# Initialize an empty dictionary for word counts
word_counts = {}
abs_word_counts = {}

# Initialize variables for text size calculations
total_text_size = 0
num_texts = 0
min_text_size = float('inf')
max_text_size = 0
total_abstext_size = 0
abs_num_texts = 0
abs_min_text_size = float('inf')
abs_max_text_size = 0


# Initialize a list to store all texts
texts = []
abstracts = []

# Open your JSONL file
start_time = time.time()
with open('training_set.jsonl', 'r') as f:
    for line in f:
        paper = json.loads(line)

        # Split the text into words and count each word
        words = paper["text"].split()
        for word in words:
            if word in word_counts:
                word_counts[word] += 1
            else:
                word_counts[word] = 1

        # Calculate text size and update min, max, and total
        text_size = len(words)
        min_text_size = min(min_text_size, text_size)
        max_text_size = max(max_text_size, text_size)
        total_text_size += text_size
        num_texts += 1

        # Add the text to the list of texts
        texts.append(paper["text"])

end_time = time.time()

# Print the time elapsed for reading the file
print(f"Time elapsed for reading the file: {end_time - start_time} seconds")

# Open your JSONL file
start_time = time.time()
with open('training_set_abstracts.jsonl', 'r') as f:
    for line in f:
        paper = json.loads(line)

        # Split the text into words and count each word
        words = paper["abstract"].split()
        for word in words:
            if word in abs_word_counts:
                abs_word_counts[word] += 1
            else:
                abs_word_counts[word] = 1

        # Calculate text size and update min, max, and total
        abs_text_size = len(words)
        abs_min_text_size = min(abs_min_text_size, abs_text_size)
        abs_max_text_size = max(abs_max_text_size, abs_text_size)
        total_abstext_size += abs_text_size
        abs_num_texts += 1

        # Add the text to the list of texts
        abstracts.append(paper["abstract"])

end_time = time.time()

# Print the time elapsed for reading the file
print(f"Time elapsed for reading the abstracts file: {end_time - start_time} seconds")

# Convert texts to sequences of word indices
start_time = time.time()
tokenizer = Tokenizer(num_words=1000)
tokenizer.fit_on_texts(abstracts)
sequences = tokenizer.texts_to_sequences(abstracts)
end_time = time.time()
print(f"Time elapsed for converting texts to sequences: {end_time - start_time} seconds")

# Calculate and print the average sequence length
average_sequence_length = sum(len(sequence) for sequence in sequences) / len(sequences)
print(f"Average sequence length: {average_sequence_length}")

# Sort the dictionary by value in descending order and get the top words
sorted_word_counts = sorted(abs_word_counts.items(), key=lambda item: item[1], reverse=True)
top_words = sorted_word_counts[:20]  # Change this number to display more or fewer top words

# Print the top words with the highest count
for word, count in top_words:
    print(f"'{word}': {count}")

# Calculate and print the smallest, largest, and average text size
print(f"Smallest text size: {min_text_size}")
print(f"Largest text size: {max_text_size}")
print(f"Average text size: {total_text_size / num_texts}")

# Calculate and print the smallest, largest, and average text size
print(f"Smallest abstract size: {abs_min_text_size}")
print(f"Largest abstract size: {abs_max_text_size}")
print(f"Average abstract size: {total_abstext_size / abs_num_texts}")