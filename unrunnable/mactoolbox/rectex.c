
rectex()
{
	Rect trect;
	Pattern beta, *ptbeta;
	int j;

	beta[0] = 0xAA;
	beta[1] = 0xAA;
	beta[2] = 0xAA;
	beta[3] = 0xAA;
	beta[4] = 0xAA;
	beta[5] = 0xAA;
	beta[6] = 0xAA;
	beta[7] = 0xAA;

	trect.top = 160;
	trect.bottom = 320;
	trect.left = 213;
	trect.right = 426;

	FrameRect(&trect);
	for (j=1;j<16000;j++);
	PaintRect(&trect);
	for (j=1;j<16000;j++);
	InvertRect(&trect);
	for (j=1;j<16000;j++);
	EraseRect(&trect);
	for (j=1;j<16000;j++);

	FrameRect(&trect);
	for (j=1;j<16000;j++);
	FillRect(&trect,beta);

}
