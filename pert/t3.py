import networkx as nx
G = nx.DiGraph()

fin = open("input2","r")
numeric = ['time', 'number', 'cpu-intensity']

def path_time(path, G):
  sum = 0
  for i in range(1,len(path)-1):
    tau = G.nodes[path[i]]['time']
    sum += float(tau)
  return sum


G.add_node(0, time = 0, weight = 0)
G.add_node("end", time = 0, weight = 0)

k = 0
for lin in fin:
  line = lin.rstrip()
  if (k == 0):
    words = line.split(",")
    labels = line.split(",")
    for i in range(0,len(words)):
      labels[i] = words[i].strip()
    if (not('depends' in labels)):
      print("depends not in list of labels")
      exit(1)

  else:
    tmp = line.split(":")
    words = tmp
    for i in range(0,len(tmp)):
      words[i] = tmp[i].strip()
    #RG: Ensure words[0] != start or end
    if (words[0] == 'end'):
      dependencies = words[1].split(",")
      for i in range(0, len(dependencies)):
        G.add_edge(int(dependencies[i]), 'end')
    else: 
      G.add_node(int(words[0]) )
      if (len(words) != len(labels)):
        print("error, number of labels and values differ at line ",k)
        exit(1)
      for i in range(1,len(labels)):
        if (labels[i] in numeric):
          G.nodes[int(words[0])][labels[i]] = float(words[i])
        else:
          G.nodes[int(words[0])][labels[i]] = words[i]
  
      #RG: Check that input numbers are unique
      #add edges from list in 'depends' to this node
      dependencies = G.nodes[int(words[0])]['depends'].split(",")
      for i in range(0, len(dependencies)):
        G.add_edge(int(dependencies[i]), int(words[0]))
  
  k += 1

#A couple of checks on the character of the graph:
print("Is the graph acyclic?",    nx.is_directed_acyclic_graph(G) )
print("Is there a path from start to finish? ",nx.has_path(G,0,"end" ) )
if (not  (nx.is_directed_acyclic_graph(G) and nx.has_path(G,0,"end" ) ) ):
  print("Cannot continue")
  print(G.nodes)
  for i in G.nodes:
    print(G.nodes[i])
  exit(1) 


# Make the weight inverse of time (or other such function) so that
#    lowest cumulative weight = longest time path 
#print(G.nodes)
for i in G.nodes:
  if (G.nodes[i]['time'] != 0.0):
    G.nodes[i]['weight'] = 1./G.nodes[i]['time']
  else:
    G.nodes[i]['weight'] = 0

#Ensure that all labels are referenced in node values 
#RG: change 0 to start for the node references
for i in range(0, len(labels)):
  if (not (labels[i] == 'time' or labels[i] == 'weight') ):
    if (labels[i] in numeric ):
      G.nodes[0][labels[i]] = 0.0
      G.nodes["end"][labels[i]] = 0.0
    else:
      G.nodes[0][labels[i]] = ""
      G.nodes["end"][labels[i]] = ""


if (not nx.has_path(G,0,"end" ) ):
  print("There is no path from start to finish!")
#else:
#  #what is the shortest path?
#  #RG: This or something like it, with suitably defined weight function above
#  #    should work. But ... doesn't yet.
#  #path = nx.dijkstra_path(G, 0, "end", weight='weight')
#  print("dijkstra path:",path)


# Brute force: Find _all_ paths from start to finish and compute time
all_paths = nx.all_simple_paths(G, 0, "end")
k = 0
pathset = []
pathtime = []
for path in all_paths:
  pathset.append(path)
  pathtime.append(path_time(path, G))
  print(path, "path time = ",path_time(path, G) )
  k += 1
print("# paths",k)
print(pathtime)
print(max(pathtime), "max path number ",pathtime.index(max(pathtime)),pathset[pathtime.index(max(pathtime))] )
print(min(pathtime), "min path number ",pathtime.index(min(pathtime)),pathset[pathtime.index(min(pathtime))] )



exit(0)
import matplotlib.pyplot as plt
pos = nx.spring_layout(G)
nx.draw(G,with_labels=True)
plt.show()
