import networkx as nx
G = nx.DiGraph()

x = {"what":"start", "who":'rg', "time":0}
print("isdict?",isinstance(x, dict))

G.add_node(0, x)
G.nodes[0]["what"]


G.add_node('alpha', dob=1185, pob='usa')
print(G.nodes, G.nodes(), G.nodes['alpha']['dob']  )
print(G.nodes['alpha']['pob'])

G.nodes['alpha']['name'] = 'obb'
print(G.nodes['alpha']['name'])

G.nodes['alpha']["labels"]
