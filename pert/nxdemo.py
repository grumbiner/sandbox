import networkx as nx
G = nx.DiGraph()

#print("Hello", flush=True)

G.add_node(0)
G.add_node(1)
G.add_node(2)
G.add_node("end")
print("G.nodes = ",G.nodes)

G.add_edge(1,2,what="code drifter", who='rg', time=8)
print(G.adj[0])
print(G.adj[1])
print(G.adj[2])
print("edge data ", G.edges.data()) 
print("edge data ", G.edges[0].data()) 

#print("G.nodes = ",G.nodes)
#print("G.edges ",G.edges)
print("G.path 1 2",nx.is_path(G,(1,2) ) )
print("G.path 0 2",nx.is_path(G,(0,2) ) )


import matplotlib.pyplot as plt
#pos = nx.spring_layout(G)
nx.draw(G,with_labels=True)
plt.show()
