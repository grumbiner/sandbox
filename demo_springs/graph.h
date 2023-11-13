#include <stdio.h>


class node {
  public:
    int number;
    int linkcount;
    unsigned char color[3];

    node();   
    void operator=(node &);  
// name/label
// type
// color, linewidth, size, font, ...
};
node::node() {
  linkcount = 0;
  //printf("in ::node\n"); fflush(stdout);
}
void node::operator=(node &x) {
  //printf("equating a node\n");
  this->linkcount = x.linkcount;
  this->number    = x.number;
}

class arc {
  public:
    void makearc(node &, node &);

  public:
    node from;
    node to;
    int number;
    unsigned char color[3];
    arc();
// name/label
// type
// color, linewidth, size, font, ...
};
arc::arc() {
}
void arc::makearc(node &x, node &y) {
  //printf("making an arc\n"); fflush(stdout);

  this->from = x;
  this->to   = y;

  x.linkcount += 1;
  y.linkcount += 1;
  //printf("linkcounts %d %d\n",x.linkcount, y.linkcount); fflush(stdout);
  
};

class graph {
  public :
    node *nodes;
    arc  *arcs;
    int arccount;
    int nodecount;
    graph();
    graph(int );
    //addnode
    void addarc(int i, node &x, node &y);
    //rmnode
    //rmarc
};
graph::graph() {
  nodes = new node[200];
  arcs  = new arc[200];
  arccount = 0;
}
graph::graph(int x) {
  //printf("graph of int\n"); fflush(stdout);
  nodes = new node[x];
  arcs  = new arc[x];
  arccount = 0;
}
void graph::addarc(int i, node &x, node &y) {
  arcs[i].makearc(x, y);
  arccount += 1;
}
