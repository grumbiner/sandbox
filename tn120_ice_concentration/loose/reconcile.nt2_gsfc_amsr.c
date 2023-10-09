#define n_atm 12	/*** Number of atmospheres ***/

/******************************************************
Calculate ice concentrations
******************************************************/
void nt2(short **icecon,short **v19,short **h19,short **v22,short **v37,
		short **v85,short **h85,
		float ***LUT19,float ***LUT19thin, float ***LUT85, float ***LUT85thin, 
		float ***LUTDGR,float ***LUTGR37,
		int xsize,int ysize,char *hemi,short **msk)
{

	double phi19, phi85;

	int x,y;
	int i,j,k;

	int ca,cc;
	double dmin,d;
	int imin,jmin,cai,ccj;
    int camina[n_atm],ccmina[n_atm],bestk;
    double dmina[n_atm];


	sinphi19=sin(phi19);
	sinphi85=sin(phi85);
	cosphi19=cos(phi19);
	cosphi85=cos(phi85);


	for(x=0;x<xsize;x++){
		for(y=0;y<ysize;y++){
		  if ((v19[y][x] > 500)&&(v85[y][x] > 500)&&(msk[y][x] == 0)){
					/*** If data are valid and no land ***/

		/*	if ((gr3719 < 0.05) && (gr2219 < 0.045)){  */
			if ((gr2219 < 0.045)){

				dmin=10000.;

				for (k=0;k<n_atm;k++){

				do {

					dmin=10000.;

					for (i=-1;i<=1;i++){
						for (j=-1;j<=1;j++){
							cai=ca+i;
							ccj=cc+j;
							if((cai < 101)&&(ccj < 101)&&(cai >=0)&&(ccj >=0)&&
								((cai+ccj) >=0)&&((cai+ccj) < 101)){

			
			
		d=w19*dpr19*dpr19+w85*dpr85*dpr85+wgr*ddgr*ddgr;
			
			                    if (d < dmin){
			                        dmin=d;
									imin=i;
									jmin=j;
			                    }
							} /*endif*/
						  } /*endfor*/
						}/*endfor*/
						ca=ca+imin;	
						cc=cc+jmin;
					} /*do */
					while ((imin !=0)||(jmin != 0));

					camina[k]=ca;
					ccmina[k]=cc;
					dmina[k]=dmin;

					} /*k*/
					

			
				icecon[y][x]=ccmina[bestk]+camina[bestk];

			} /*endif*/
			else icecon[y][x]=0;   /** Weather **/
		}/* endif*/
		else icecon[y][x]=117;	   /** Missing data or land **/
	} /*y*/
	} /*x*/
			
}

/******************************************************
M A I N 	MAIN
******************************************************/

main(int argc, char *argv[])
{

	/**************************
		 argv[1] = path
         argv[2] = cddate
         argv[3] = pole  (n or s)
		 argv[4] = hdridx ('hdf' or 'raid')
    *************************/

	char *path=argv[1];
	char *cddate=argv[2];
	char *pole=argv[3];
	char *hdr=argv[4];


	/**** TBs (2-D arrays) ****/
	int xsize, ysize;
	short int **v19,**h19,**v22,**v37,**v85,**h85;

		/*!!!! In our routine the TBs are stored as short integers in K*10 !!!!*/
		/*!!!! This may need to be changed !!!! */

	short int **msk;
	int j;

	char outfile[100];
	short int **icecon;


	/**** look-up tables (3-D arrays) ****/
	float ***LUT19,***LUT19thin,***LUT85,***LUT85thin,***LUTDGR, ***LUTGR37;

	FILE *file;




	/*-------------------------------------*/
	/*---- Begin program ------------------*/

	/* Here, the xsize and ysize correspond to the 12.5km polarstereo grid */
	if (*pole == 'n'){
		xsize=608;
		ysize=896;
	}
	else {
		xsize=632;
		ysize=664;
	}


	/**** Allocate memory ****/
		v19=imatrix(0,ysize-1,0,xsize-1);
		h19=imatrix(0,ysize-1,0,xsize-1);
		v22=imatrix(0,ysize-1,0,xsize-1);
		v37=imatrix(0,ysize-1,0,xsize-1);
		v85=imatrix(0,ysize-1,0,xsize-1); /* when reading the TBs, the 85 GHz are */
		h85=imatrix(0,ysize-1,0,xsize-1); /*   adjusted to the 19 and 37 GHz data */	

		msk=imatrix(0,ysize-1,0,xsize-1);

		icecon=imatrix(0,ysize-1,0,xsize-1);

		LUT19=f3dmatrix(0,100,0,100,0,n_atm-1);	
		LUT85=f3dmatrix(0,100,0,100,0,n_atm-1);
		LUT19thin=f3dmatrix(0,100,0,100,0,n_atm-1);	
		LUT85thin=f3dmatrix(0,100,0,100,0,n_atm-1);
		LUTDGR=f3dmatrix(0,100,0,100,0,n_atm-1);
		LUTGR37=f3dmatrix(0,100,0,100,0,n_atm-1);
	/*************************/

printf("Memory allocated\n");

	/**** Read land mask *****/
	(void) get_msk(msk,xsize,ysize,pole);

printf("Mask in\n");

	/**** Get look-up tables ****/
	(void) get_lut(LUT19,LUT19thin,LUT85,LUT85thin,LUTDGR,LUTGR37,pole);

printf("LUTs in\n");

	/**** Get brightness temperatures ****/
	get_tbs(v19,xsize,ysize,path,cddate,"19v",pole,hdr);
	get_tbs(h19,xsize,ysize,path,cddate,"19h",pole,hdr);
	get_tbs(v22,xsize,ysize,path,cddate,"22v",pole,hdr);
	get_tbs(v37,xsize,ysize,path,cddate,"37v",pole,hdr);
	get_tbs(v85,xsize,ysize,path,cddate,"85v",pole,hdr);
	get_tbs(h85,xsize,ysize,path,cddate,"85h",pole,hdr);

printf("TBs in\n");

	/***** Calculate ice concentration ****/
	(void) nt2(icecon,v19,h19,v22,v37,v85,h85,LUT19,LUT19thin,LUT85,LUT85thin,
			LUTDGR,LUTGR37,xsize,ysize,pole,msk);


	/***** Write ice concentration ****/
    sprintf(outfile,"%s%s.amsr.nt2",pole,cddate);


	file=fopen(outfile,"w");
	for (j=0;j<ysize;j++) fwrite(icecon[j],sizeof(short),xsize,file);
	


}
