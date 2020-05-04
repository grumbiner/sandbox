
  // This JavaScript code can be freely redistributed
  // as long as this copyright notice is keept unchanged.
  // This code is used on AS-IS basis and
  // you use it on your own risk. Author of this code
  // is not responsible for any damage that this
  // code may make.
  //
  // If you use this JavaScript SNOW in your own web pages,
  // please sent a note to snow@altan.hr.
  //
  // JS Snow v0.1
  // finished on 11-10-1999 23:04 in Zagreb, Croatia.
  //
  // Copyright 1999 Altan d.o.o.
  // http://www.altan.hr/
  // http://www.altan.hr/snow/index.html
  // E-mail: snow@altan.hr
  
  var no = 10; // snow number

  var ns4up = (document.layers) ? 1 : 0;  // browser sniffer
  var ie4up = (document.all) ? 1 : 0;

  var dx, xp, yp;    // coordinate and position variables
  var am, stx, sty;  // amplitude and step variables
  var i, doc_width = 800, doc_height = 600;
  
  if (ns4up) {
    doc_width = self.innerWidth;
    doc_height = self.innerHeight;
  } else if (ie4up) {
    doc_width = document.body.clientWidth;
    doc_height = document.body.clientHeight;
  }

  dx = new Array();
  xp = new Array();
  yp = new Array();
  am = new Array();
  stx = new Array();
  sty = new Array();
  
  for (i = 0; i < no; ++ i) {  
    dx[i] = 0;                        // set coordinate variables
    xp[i] = Math.random()*(doc_width-50);  // set position variables
    yp[i] = Math.random()*doc_height;
    am[i] = Math.random()*20;         // set amplitude variables
    stx[i] = 0.02 + Math.random()/10; // set step variables
    sty[i] = 0.7 + Math.random();     // set step variables
    if (ns4up) {                      // set layers
      if (i == 0) {
        document.write("<layer name=\"dot"+ i +"\" left=\"15\" top=\"15\" visibility=\"show\"><a href=\"http://inet.hr/\"><img src=\"http://www.altan.hr/snow/dot.gif\" border=\"0\"></a></layer>");
      } else {
        document.write("<layer name=\"dot"+ i +"\" left=\"15\" top=\"15\" visibility=\"show\"><img src=\"http://www.altan.hr/snow/dot.gif\" border=\"0\"></layer>");
      }
    } else if (ie4up) {
      if (i == 0) {
        document.write("<div id=\"dot"+ i +"\" style=\"POSITION: absolute; Z-INDEX: "+ i +"; VISIBILITY: visible; TOP: 15px; LEFT: 15px;\"><a href=\"http://inet.hr/\"><img src=\"http://www.altan.hr/snow/dot.gif\" border=\"0\"></a></div>");
      } else {
        document.write("<div id=\"dot"+ i +"\" style=\"POSITION: absolute; Z-INDEX: "+ i +"; VISIBILITY: visible; TOP: 15px; LEFT: 15px;\"><img src=\"http://www.altan.hr/snow/dot.gif\" border=\"0\"></div>");
      }
    }
  }
  
  function snowNS() {  // Netscape main animation function
    for (i = 0; i < no; ++ i) {  // iterate for every dot
      yp[i] += sty[i];
      if (yp[i] > doc_height-50) {
        xp[i] = Math.random()*(doc_width-am[i]-30);
        yp[i] = 0;
        stx[i] = 0.02 + Math.random()/10;
        sty[i] = 0.7 + Math.random();
        doc_width = self.innerWidth;
        doc_height = self.innerHeight;
      }
      dx[i] += stx[i];
      document.layers["dot"+i].top = yp[i];
      document.layers["dot"+i].left = xp[i] + am[i]*Math.sin(dx[i]);
    }
    setTimeout("snowNS()", 10);
  }

  function snowIE() {  // IE main animation function
    for (i = 0; i < no; ++ i) {  // iterate for every dot
      yp[i] += sty[i];
      if (yp[i] > doc_height-50) {
        xp[i] = Math.random()*(doc_width-am[i]-30);
        yp[i] = 0;
        stx[i] = 0.02 + Math.random()/10;
        sty[i] = 0.7 + Math.random();
        doc_width = document.body.clientWidth;
        doc_height = document.body.clientHeight;
      }
      dx[i] += stx[i];
      document.all["dot"+i].style.pixelTop = yp[i];
      document.all["dot"+i].style.pixelLeft = xp[i] + am[i]*Math.sin(dx[i]);
    }
    setTimeout("snowIE()", 10);
  }

  if (ns4up) {
    snowNS();
  } else if (ie4up) {
    snowIE();
  }
