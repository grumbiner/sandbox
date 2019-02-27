ovalex()
{
	Rect trect;
	Pattern beta;
	int j;

	beta[0] = 0x33;
	beta[1] = 0x33;
	beta[2] = 0x33;
	beta[3] = 0x33;
	beta[4] = 0x33;
	beta[5] = 0x33;
	beta[6] = 0x33;
	beta[7] = 0x33;

	trect.top = 160;
	trect.bottom = 320;
	trect.left = 213;
	trect.right = 426;

	FrameOval(&trect);
	for (j=1;j<16000;j++);
	PaintOval(&trect);
	for (j=1;j<16000;j++);
	InvertOval(&trect);
	for (j=1;j<16000;j++);
	EraseOval(&trect);
	for (j=1;j<16000;j++);

	FrameOval(&trect);
	for (j=1;j<16000;j++);
	FillOval(&trect,beta);

}
