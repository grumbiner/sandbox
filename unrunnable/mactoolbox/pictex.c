
pictex()
{
	int x[256], y[256], a[256], b[256];
	int i, j, npts;
	Rect	  drect, prect;
	PicHandle ph;

	prect.top = 130;
	prect.bottom = 300;
	prect.left = 1;
	prect.right = 640;
	npts = 40;

	ellipse(&x[0], &y[0], npts, 200, 210, 75, 0.0);
	ellipse(&a[0], &b[0], npts, 200, 210, 75, 0.8);
	HidePen();
	ph = OpenPicture(&prect);
	MoveTo(x[0], y[0]);
	for (i = 1; i< npts; i++)
	{
		LineTo(x[i], y[i]);
	}
	MoveTo(a[0], b[0]);
	for (i = 1; i< npts; i++)
	{
		LineTo(a[i], b[i]);
	}
	ClosePicture();

	ShowPen();
	prect.top = 130;
	prect.bottom = 300;
	prect.left = 1;
	prect.right = 640;
	DrawPicture(ph,&prect);

}
