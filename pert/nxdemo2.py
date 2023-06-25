import networkx as nx
G = nx.DiGraph()
#G = nx.Graph()

#print("Hello", flush=True)
#print("G.nodes = ",G.nodes)
#print("G.edges ",G.edges)

G.add_node(0)
G.add_node(1)
G.add_node(2)
G.add_node(3)
G.add_node("end")
print("G.nodes = ",G.nodes)

G.add_edge(0,1,what="code drifter", who='rg', time=10)
G.add_edge(0,2,what="code retail verification", who='rg', time=4)
G.add_edge(1,3,what="apply to RTOFS", who='rg', time=8)
G.add_edge(2,3,what="apply to RTOFS", who='rg', time=4)
G.add_edge(3,"end",what="finish", who = 'rg', time = 2)

# adjacency -- who's next, and what are the properties of the edge
print("adj 0",G.adj[0])
print("adj 1", G.adj[1])
print("adj 2", G.adj[2])
print("adj 3", G.adj[3])
print("adj end", G.adj["end"])

# attributes of an edge
print("edge data ", G.edges.data()) 

#are points adjacent in the graph
print("G.path 1 2",nx.is_path(G,(1,2) ) )
print("G.path 0 2",nx.is_path(G,(0,2) ) )
print("G.path 0 3",nx.is_path(G,(0,3) ) )

#Is there a path from here to there?
print("G.path 0 end",nx.has_path(G,0,"end" ) )

#what is the shortest path?
k = nx.dijkstra_path(G, 0, "end")
print(k)


exit(0)

import matplotlib.pyplot as plt
#pos = nx.spring_layout(G)
nx.draw(G,with_labels=True)
plt.show()
