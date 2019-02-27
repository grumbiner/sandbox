WindowRecord view_record;
WindowPtr    view_window;
RgnHandle    alpha;

startwindow()
    
{
    Rect         vrect;
    int fontnum, fontsize, pwid, pht;
    long fcolor, bcolor;
    
    vrect.top    =  40;
    vrect.bottom = 480;
    vrect.left   =   1;
    vrect.right  = 640;
    view_window = NewWindow(&view_record,&vrect,"\pView",true,0,-1L,true,0);
    SetPort(view_window);
    fontnum  = 4;
    fontsize = 9;
    pwid = 1;
    pht  = 1;
    TextFont(fontnum);
    TextSize(fontsize);
    fcolor = 409;
    bcolor = 69;
    ForeColor(fcolor);
    BackColor(bcolor);
    ShowPen();
    PenSize(pwid,pht);
    MoveTo(1,24);

}