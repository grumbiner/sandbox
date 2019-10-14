pro pickf
COMMON data, scan, window_id, window_id2

f=pickfile(path='c:\backup\cdback')
openr, unit, f, /get_lun
scan = BytArr(152,224)
ReadU, unit, scan
Free_LUN, unit

END

PRO ALPHA_Event, event

COMMON data, scan, window_id, window_id2

CASE event.value OF
     'Sobel'          : BEGIN
                          WSET, window_id2
                          TVScl, Sobel(scan)
                        END
     'Roberts'        : BEGIN
                          WSET, window_id2
                          TVScl, Roberts(scan)
                        END
     'Boxcar'         : BEGIN
                           WSET, window_id2
                           TVScl, Smooth(scan, 7)
                        END
     'Median'         : BEGIN
                          WSET, window_id2
                          TVScl, Median(scan, 7)
                        END
     'Original Image' : BEGIN
                          WSET, window_id
                          TVScl, scan
                        END
     'Done'           : Widget_Control, event.top, /Destroy
     'Select Color Table' : xloadct, group=event.top
     'Change Size'        : BEGIN
				print, 'What is new xsize? '
                                read, xs
                                print, 'What is new ysize? '
                                read, ys
                                scan=congrid(scan, xs, ys)
                                windraw, base, drawbase, drawbase2, xs, ys
                            END
     'Select a File'      : BEGIN
                              pickf
                              WSET, window_id
                              TVScl, scan
                            END
ELSE:
ENDCASE
END ; End ALPHA_Event Procedure *********************


;****************************************************
PRO Read_Scan_Data

COMMON data, scan, window_id, window_id2

Openr, unit, 'C:\backup\cdback\891201.n3c', /Get_LUN
scan = BytArr(152,224)
ReadU, unit, scan
Free_LUN, unit

END ; End Read_Scan_Data Procedure ******************

; Routine to draw the window
PRO windraw, base, drawbase, drawbase2, xs, ys
COMMON data, scan, window_id, window_id2

draw = Widget_Draw(drawbase, XSize=xs, YSize=ys)
draw2 = Widget_Draw(drawbase2, Xsize=xs, Ysize=ys)
Widget_Control, base, /Realize
Widget_Control, draw, Get_Value=window_id
Widget_Control, draw2, Get_Value=window_id2
WSet, window_id
LoadCT, 11
TvScl, scan

RETURN
END

; ***************************************************
PRO alpha3

COMMON data, scan, window_id, window_id2

Read_Scan_Data

junk = {PDMENU, flags:0, name:''}
menu = [{PDMENU, 1, 'Image Processing'}, $
                {PDMENU, 1, 'Edge Enhancement'}, $
                         {PDMENU, 0, 'Sobel'}, $
                         {PDMENU, 2, 'Roberts'}, $
                {PDMENU, 1, 'Smoothing'}, $
                         {PDMENU, 0, 'Boxcar'}, $
                         {PDMENU, 2, 'Median'}, $
                {PDMENU, 2, 'Original Image'}, $
        {PDMENU, 1, 'Controls'}, $
	    {PDMENU, 0, 'Change Size'}, $
            {PDMENU, 2, 'Select Color Table'}, $
        {PDMENU, 0, 'Select a File'}, $
        {PDMENU, 2, 'Done'}]

base = Widget_Base(Title='Pull-Down Menu Example', /Column)
menubase = Widget_Base(base)
child = Widget_Base(base, Title='Image Windows', /Row)
drawbase = Widget_Base(child)
drawbase2 = Widget_Base(child)
pdmenu = CW_PDMENU(menubase, menu, /Return_Name)
windraw, base, drawbase, drawbase2, 152, 224

XManager, 'ALPHA', base
END               

