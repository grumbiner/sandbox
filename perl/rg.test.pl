#!perl

#Sample script trying to work with perl on a mac

use Mac::Files;
use Mac::QuickDraw;

#print FindFolder(kOnSystemDisk, kSystemFolderType), "\n";

$j = &MacPerl'Answer("message in box can be long, but note that the buttons are not being increased for\
the size of the text.  To further confuse the issues, the numberings are done last\
to first following this argument, so that the last one is 0", "default ", "optional1 ", "optional2");
print $j, " is the response number \n";

#Try standard file manager
use Mac::StandardFile;
$info = StandardGetFile(0,"TEXT");
print $info->sfType, "\n";
print $info->sfFile, "\n";
#Use the info from the standard file manager to interrogate further
$detinfo =  FSpGetFInfo($info->sfFile);
print $detinfo->fdType, "\n";
print $detinfo->fdCreator,"\n";
print $detinfo->fdFlags,"\n";
print $detinfo->fdLocation->h," and ", $detinfo->fdLocation->v,"\n";


#Now to pop a window -- Note that we've started using the object oriented
#  side of things
use Mac::Windows;
use Mac::Pane
$title="Testing box 1";
$bounds=new Rect(50,50,250,250);
$goaway=1;
$visible=1;
$proc=$documentProc;
$win = new MacColorWindow($bounds, $title, $visible, $proc, $goaway) ;
$wininfo = $win->window;
print "kind of window ",$wininfo->windowKind,"\n";
print "visible, go away",$wininfo->visible," ",$wininfo->goAwayFlag,"\n";
BringToFront($wininfo);

#Quickdraw stuff---------------------------------------
SetPort($wininfo);  
$initcolor = GetForeColor;
InitCursor;
ShowPen;
PenSize(5,5);
Move(50,50); #Relative Move
Line(200,200);

$pencol = new RGBColor(0, 0, 256*128) ;
print "Initial fgcolor ",$pencol->red, $pencol->green, $pencol->blue,
 " \n";
RGBForeColor($pencol);
$hearts = new Pattern q{
           .XX.XX..
			        X..X..X.
			        X.....X.
			        .X...X..
			        ..X.X...
			        ...X....
			        ........
			        ........
			    };
PenPat($hearts);
MoveTo(0,0); PenSize(8,8); Line(200,32);

use Mac::Fonts;
TextFont(GetFNum("times"));
MoveTo(10,10);
DrawString("Hello, here is some text in times\n");

#Try to insert a menu------------------------------
use Mac::Menus;
use Mac::Events;

$menu = new MacMenu 2048, "Demo-Menu from rg.pl", (
	["One", \&Handler],  #Note syntax: Menu element name, then name of subr
	["Two", \&Handler],
	["Three", \&Handler],
	[],                    #This is a separator line
	["Four", \&Handler],
);

$menu->insert;

WaitNextEvent until defined $Menu;

dispose $menu;

sub Handler {
	my($menu,$item) = @_;

 print "Selected menu ", $menu, " item ", $item, "\n";
	$Menu = $item;
}

#Insert a delay-------------------------------------------
$j=0;
while ($j < 1000000) {
  $j += 1;
}
#Note that having created a window, we need a way of getting rid of
#  it.
$win->dispose;

