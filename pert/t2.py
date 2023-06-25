import networkx as nx
G = nx.DiGraph()

#G.set_attributes to cover all nodes, then add specific values from readin
G.add_node(0, what="start", who='rg', time=0)
G.add_node(1, what="code drifter", who={'rg','fred'}, time=4)
G.add_node(2, what="code retail verification", who='rg', time=1)
G.add_node(3, what="apply to RTOFS", who='rg', time=4)
G.add_node("end", time = 0, who='')

# Make the weight inverse of time (or other such function) so that
#    lowest cumulative weight = longest time path 
for i in G.nodes:
  if (G.nodes[i]['time'] != 0):
    G.nodes[i]['weight'] = 1./G.nodes[i]['time']
  else:
    G.nodes[i]['weight'] = 0

G.add_edge(0,1, weight=G.nodes[1]['weight'])
G.add_edge(0,2, weight=G.nodes[2]['weight'])
G.add_edge(1,3, weight=G.nodes[3]['weight'])
G.add_edge(2,"end", weight=G.nodes["end"]['weight'])
G.add_edge(3,"end", weight=G.nodes["end"]['weight'])

#Is there a path from here to there?
print("G.path 0 end",nx.has_path(G,0,"end" ) )

#what is the shortest path?
k = nx.dijkstra_path(G, 0, "end", weight='weight')

print(k, len(k), k[1], G.nodes[k[1]]['weight'])

sum = 0
for i in range(1,len(k)):
  tau = G.nodes[k[i]]['time']
  print("step:",i, "node:",k[i], "time:",G.nodes[k[i]]['time'], G.nodes[k[i]]['who'])
  sum += float(tau)
print("longest path time = ",sum)


import matplotlib.pyplot as plt
#pos = nx.spring_layout(G)
nx.draw(G,with_labels=True)
plt.show()

