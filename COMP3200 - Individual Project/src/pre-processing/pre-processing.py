import json
import re
from nltk.corpus import stopwords, words
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer

# Get the set of English stop words
stop_words = set(stopwords.words('english'))

# Get the set of English words
english_words = set(words.words())

# Initialize the WordNetLemmatizer
lemmatizer = WordNetLemmatizer()

# Method used to calculate the CD index of a paper
def calculate_cd_index(papers):
    cd_indices = {}  
    for paper_id, paper_data in papers.items():
        A = set(paper_data["references"])
        B = paper_data["cited_by"]
        x = 0
        #m = 0

        if A and len(B) >= 5:
            for paper in B:
                for a, b in papers[paper].items():
                    if a == "references":
                        A2 = set(b)

                        if A.intersection(A2):
                            x = x - 1 
                        else:
                            x = x + 1  
                            #m = m + 1 
            cd_index = (x) / len(B) 
            cd_indices[paper_id] = cd_index
    return cd_indices

# Load your JSON file
with open('SSNDataset\SSN\citation_relations.json', 'r') as f:
    data = json.load(f)

# The data is now a Python dictionary.
cd_indices = calculate_cd_index(data)

print("cd_indices have been calculated")

# Open your JSONL file and a new file to write the result
with open('SSNDataset\SSN\papers.SSN.jsonl', 'r') as f, open('training_set.jsonl', 'w') as outfile:
    for line in f:
        paper = json.loads(line)
        
        # Flatten each list of lists in "text" field into a single list of strings and join them
        text = paper["text"]
        if isinstance(text, list):
            text = ' '.join(sum(text, []))

        # Tokenize the text, converting each word to their base form
        word_tokens = word_tokenize(text)
        filtered_text = [lemmatizer.lemmatize(w) for w in word_tokens if not w in stop_words and w in english_words]
        filtered_text = ' '.join(filtered_text)
        filtered_text = re.sub(r'[^\w\s]', '', filtered_text)  # remove punctuation
        filtered_text = re.sub(r'\b\d+\b', '', filtered_text)  # remove standalone numbers

        # Replace the original "text" field with the filtered text
        paper["text"] = filtered_text

        # Add the CD index to the paper if it exists
        paper_id = paper["paper_id"]
        if paper_id in cd_indices:
            paper["cd_index"] = cd_indices[paper_id]
        
            # Check if the length of the filtered text is at least 1000 words
            if len(filtered_text.split()) >= 1000 and len(filtered_text.split()) <= 2500:
            # Write the paper with the new fields to the new file
                outfile.write(json.dumps(paper) + '\n')