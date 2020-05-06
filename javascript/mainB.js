with (tags.h1) {
	textAlign: center;
	fontSize:30pt;
	fontColor: #006699;
  }
  
with (tags.h2)  {
	fontSize: 20pt;
	fontColor: #cc0000;
}

classes.bold.h3.fontWeight = 'bold';
classes.bold.h3.color = '#0066ff';
with (classes.abstract.h3) {
	fontSize : 15pt;
	font-family : Arial, Helvetica, San Serif;
}

with (tags.a:hover)   { color: #006666; 
			font-weight:600}
with (tags.a:link)    { color: #006699; }
with (tags.a:visited) { color: #000099; }
with (tags.a:active)  { color: #999900; }
with (tags.a)  {
        fontStyle: normal;
        font-weight:400;
   }


with (tags.ul)  {
        fontSize : 11pt ;
        font-weight : 600 ;
        color : #006699;
        font-family : Arial, Helvetica, San Serif ;
		fontStyle: normal;
}

with (tags.li) {
        fontSize : 9pt ;
        font-weight : 500 ;
        color : #660066 ;
        font-family : Arial, Helvetica, San Serif ;
		fontStyle: normal;
	    list-style-position : inside;
}

with (tags.td)  {
	fontSize : 9pt;
	font-family : Arial, Helvetica, San Serif;
	font-weight : 500;
}

with (classes.gray.all)  {
	fontSize : 8pt;
	color : #808080;
}

with (classes.normaltexttitle.all)  {
        fontSize : 12pt;
        text-decoration : none;
        color : #000066;
        font-weight : 600;
        font-family : Arial, Helvetica, San Serif ;
}

with (classes.normaltext.all)  {
        fontSize : 8pt;
        text-decoration : none;
        color : #000099;
        font-weight : 500;
        font-family : Serif ;
}

with (classes.datetext.all)  {
        fontSize : 9pt ;
        text-decoration : none ;
        color : #000000 ;
        font-weight : 600 ;
        font-family : Arial, Helvetica, San Serif ;
}
