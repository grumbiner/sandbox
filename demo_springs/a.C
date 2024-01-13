#include <stdio.h>
#include "graph.h"

int main(void) {
  graph z(200);

  int i;
  for (i = 0; i < 200; i++) {
    z.nodes[i].number = i;
  }
  for (i = 0; i < 199; i++) {
    //z.arcs[i].from.number = i;
    //z.arcs[i].to.number = i+1;

    //z.arcs[i].makearc(z.nodes[i], z.nodes[i+1]);

    z.addarc(i, z.nodes[i], z.nodes[i+1]);
  }

  i = 5;
  //z.arcs[i].makearc(z.nodes[5], z.nodes[72]);
  z.addarc(i, z.nodes[5], z.nodes[72+1]);
  //printf("%d %d arc5\n",z.arcs[i].from.number, z.arcs[i].to.number);

  //for (i = 0; i < 200; i++) {
  //  if (z.nodes[i].linkcount != 0) {
  //    printf("i %d count %d\n",i, z.nodes[i].linkcount);
  //  }
  //}
  //printf("past node check\n"); 

// export header for springy
  printf("<html><head>\
    <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"></head><body>\n\
    <script src=\"demo_files/jquery.js\"></script>\n\
    <script src=\"demo_files/springy.js\"></script>\n\
    <script src=\"demo_files/springyui.js\"></script>\n\
    <script>\n\
    var graph = new Springy.Graph();\
  \n");

  printf("var allbob = graph.newNode\(\{\
    label: 'allbob',\
    ondoubleclick: function() { console.log(\"Hello!\"); } }) \n" );

// print out nodes:
  for(i = 0; i < 200; i++) {
    if (z.nodes[i].linkcount != 0) {
      printf("var i%d = graph.newNode({label: 'i%d'}); \n",  i, i);
    }
  }

// print out arcs:
  for (i = 0; i < 200; i++) {
    printf("graph.newEdge(i%d, i%d, {color:'#000000'}); \n",
      z.arcs[i].from.number, z.arcs[i].to.number);
  }

// print out trailer
  printf("\
jQuery(function(){\
  var springy = window.springy = jQuery('#springydemo').springy({\
    graph: graph,\
    nodeSelected: function(node){\
      console.log('Node selected: ' + JSON.stringify(node.data));\
    }\
  });\
});\
</script>\
<canvas id=\"springydemo\" width=\"640\" height=\"480\">\
</canvas></body></html>\
\n");




  return 0;
}
