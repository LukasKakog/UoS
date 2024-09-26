import json
import torch
import numpy as np
import time
from sklearn.feature_extraction.text import TfidfVectorizer
from torch_geometric.data import Data, DataLoader
from torch_geometric.nn import GCNConv

# Define the Graph Neural Network model
class GNN(torch.nn.Module):
    def __init__(self, num_features, num_classes):
        super(GNN, self).__init__()
        self.conv1 = GCNConv(num_features, 16)
        self.conv2 = GCNConv(16, num_classes)

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        x = self.conv1(x, edge_index)
        x = torch.relu(x)
        x = torch.dropout(x, training=self.training)
        x = self.conv2(x, edge_index)

        return torch.log_softmax(x, dim=1)

start_time = time.time()

# Load the dataset
with open('training_set.jsonl', 'r') as f:
    dataset = [json.loads(line) for line in f]

# Load the citation relations
with open('SSNDataset\\SSN\\citation_relations.json', 'r') as f:
    citation_relations = json.load(f)

# Prepare a mapping from paper_id to a continuous index
paper_id_to_index = {paper_id: i for i, paper_id in enumerate(paper_id for paper_id in citation_relations if paper_id in dataset)}

# Extract the text from the dataset and convert to TF-IDF features
texts = [data['text'] for data in dataset]
vectorizer = TfidfVectorizer(max_features=1000)  # Limit the number of features
tfidf_matrix = vectorizer.fit_transform(texts)
tfidf_coo = tfidf_matrix.tocoo()
features = torch.sparse_coo_tensor(torch.tensor([tfidf_coo.row.tolist(), tfidf_coo.col.tolist()]), tfidf_coo.data, tfidf_matrix.shape)

# Prepare the edge index and labels for PyTorch Geometric
edge_index = []
labels = []

for data in dataset:
    paper_id = data['paper_id']
    if paper_id in paper_id_to_index:
        paper_index = paper_id_to_index[paper_id]

        # Add label
        labels.append(data['cd_index'])

        # Add edges
        for referenced_paper_id in citation_relations[paper_id]['references']:
            if referenced_paper_id in paper_id_to_index:
                referenced_paper_index = paper_id_to_index[referenced_paper_id]
                edge_index.append([paper_index, referenced_paper_index])

# Convert to PyTorch tensors
edge_index = torch.tensor(edge_index, dtype=torch.long).t().contiguous()
labels = torch.tensor(labels, dtype=torch.long)

# Create a PyTorch Geometric Data object
data = Data(x=features, edge_index=edge_index, y=labels)

# Initialize the model and optimizer
model = GNN(data.num_features, len(set(labels.tolist())))
optimizer = torch.optim.Adam(model.parameters(), lr=0.01)

# Train the model
model.train()
for epoch in range(100):
    optimizer.zero_grad()
    out = model(data)
    loss = torch.nn.functional.nll_loss(out, data.y)
    loss.backward()
    optimizer.step()

    print(f"Epoch: {epoch}, Loss: {loss.item()}")

end_time = time.time()
print(f"Time elapsed for the entire process: {end_time - start_time} seconds")
