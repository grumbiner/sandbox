  Using JavaScript you can control animations of the last 14 days of sea 
ice figures in this region.  This page is for the Alaska region
ice pack.  There will be a reference figure -- today's ice cover -- fixed 
on the right of the screen.  You will need JavaScript enabled (and a 
JavaScript-capable browser such as Netscape Navigator or MS Internet 
Explorer) and will need to load the images (if you've got 'automatically 
load images' set, which is the default both browsers are shipped with, 
you don't need to do anything).  You can also get a look at last year's 
ice cover on the same date by clicking 'last year'. <p>

  Please note that since these are small area images, their area of 
coverage will move from time to time. <p>

  Note from 4 January 2000: Due to user feedback from Alaska, we have 
reduced the span to 14 days from 30.  This will speed up the loading
and, according to the feedback, still provide a long enough look-back
for users to get a good sense of the changes in the area's ice pack.
Older images are available as always in 
<A HREF="ftp://polar.ncep.noaa.gov/pub/ice/alaska">
ftp://polar.ncep.noaa.gov/pub/ice/alaska</A>.  Further feedback is welcome!

<A HREF="mailto:Robert.Grumbine@noaa.gov">Robert.Grumbine@noaa.gov</a><BR>
Last Modified 17 June 2004
<HR>

<TABLE Border = 1>
<TR ALIGN=CENTER>
  <TD>Current Status</TD>
  <TD>Interactive Control</TD>
</TR>
<TR>
  <TD><IMG SRC="analysis/ak.gif" NAME=reference ALT="reference figure"></TD>
  <TD><IMG SRC="analysis/ak.gif" NAME=animation ALT="animation window"></TD>
</TR>
</TABLE>

<SCRIPT language="JavaScript" TYPE="text/javascript">
MAX_FRAMES=14;
area="analysis/alaska/ak.";

var frame = 0;
var timeout_id = null;
var images = new Array(MAX_FRAMES);
var today = new Date();
var figdate = new Date();
var secs = today.getTime() - 86400.*1000. - 10.*3600*1000; 
      // time in milliseconds for yesterday
for (var i = 1; i <= MAX_FRAMES; i++) {
  figdate.setTime(secs); 
  tag=area+figdate.getFullYear();
  if (figdate.getMonth() + 1 < 10 ) {
    tag += "0"+(figdate.getMonth()+1);
  }
  else {
    tag += (figdate.getMonth()+1);
  }
  if (figdate.getDate()  < 10 ) {
    tag += "0"+figdate.getDate() ;
  }
  else {
    tag += figdate.getDate() ;
  }
  tag += ".gif"
 
  images[MAX_FRAMES - i] = new Image();
  images[MAX_FRAMES - i].src = tag ;
  secs -= 86400.*1000.;
}
// Set up image for last year
  ly = new Image();
  year = today.getFullYear() - 1; 
  month = today.getMonth() + 1;
  day   = today.getDate() ;
  if (month < 10) { month = "0"+month; }
  if (day   < 10) { day   = "0"+day;   }
  ly.src = area+year+month+day+".gif";

// Now get to the functions
function animate() {
  document.animation.src = images[frame].src;
  frame = (frame + 1) % MAX_FRAMES;
  timeout_id = setTimeout("animate()", 125);
}
function step(size) {
  frame = (frame + size) % MAX_FRAMES;
  if (frame < 0) frame += MAX_FRAMES;
  document.animation.src = images[frame].src;
}
function lastyear() {
  document.animation.src = ly.src;
}
</SCRIPT>

<FORM ACTION="script">
  <INPUT TYPE=button VALUE="Animate" 
         onClick="if (timeout_id == null) animate()">
  <INPUT TYPE=button VALUE="Step Forward" onClick="step(1)">
  <INPUT TYPE=button VALUE="Step Backward" onClick="step(-1)">
  <INPUT TYPE=button VALUE="Stop" onClick="if (timeout_id) 
         clearTimeout(timeout_id); timeout_id = null; "> 
  <BR>
  <INPUT TYPE=button VALUE="Ice Cover Same Time Last Year" onClick="lastyear()">
</FORM>
<HR>
<A HREF="nh.html">Comparable page for the Northern Hemisphere ice pack</A><BR>
<A HREF="sh.html">Comparable page for the Southern Hemisphere ice pack</A><BR>
<A HREF="Analyses.shtml">Return to sea ice analysis page</A><BR>
<A HREF="/seaice/">Return to main sea ice page</A><BR>
<A HREF="/">Return to main MMAB page</A><BR>
<HR>
    <p>
      <a href="http://validator.w3.org/check?uri=referer"><img border="0"
          src="http://www.w3.org/Icons/valid-html401"
          alt="Valid HTML 4.01!" height="31" width="88"></a>
    </p>
