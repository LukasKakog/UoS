import plotly.express as px

# Data
cd_indices_labels = ['-1', '-1 to 0', '0', '0 to 1', '1']
cd_indices_counts = [3242, 7542, 602, 4340, 1119]

# Create a bar plot using Plotly Express
fig = px.bar(x=cd_indices_labels, y=cd_indices_counts, labels={'x': 'cd_indices', 'y': 'Count'},
             title='Distribution of cd_indices', template='seaborn')
fig.show()
