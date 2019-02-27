polyex(px,py, npts)
int *px, *py, npts;

/* Polygon operations example */
/* ErasePoly does not work well at erasing the entire perimeter of a polygon,
    it does work ok at getting the interior */
/* beta is of type pattern */
{ 
    int i, j;
    PolyHandle   hubert;
    Pattern beta, *ptbeta;

    beta[0] = 0x01;
    beta[1] = 0x02;
    beta[2] = 0x04;
    beta[3] = 0x08;
    beta[4] = 0x0F;
    beta[5] = 0x10;
    beta[6] = 0x11;
    beta[7] = 0x12;
    hubert = OpenPoly();
      MoveTo(*px, *py);
      for (i = 1; i< npts; i++)
        { LineTo(*(px+i), *(py+i));}
    ClosePoly();
    ErasePoly (hubert);
    FillPoly(hubert,beta);
    InvertPoly(hubert);
    for (j=1;j<8192;j++);
    ErasePoly (hubert);
    for (j = 1; j< 11; j++)
    {
     for (i = 1; i< 11; i++)
      { ErasePoly (hubert);
        OffsetPoly(hubert, 10, 0);
        FillPoly(hubert,beta);
        FramePoly (hubert);}
     for (i = 1; i< 11; i++)
      { ErasePoly (hubert);
        OffsetPoly(hubert,-10, 0);
        FillPoly(hubert,beta);
        FramePoly (hubert);}
     }
     KillPoly(hubert);
}
