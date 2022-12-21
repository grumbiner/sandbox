#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define n_atm 12	/*** Number of atmospheres ***/


/*******************************************
     Initialize short int (2byte) matrix
*******************************************/
short int **imatrix(int nrl,int nrh,int ncl,int nch)
{
        short int **m;
        int i;

        m=(short int **)calloc((nrh-nrl+1),sizeof(short int*));
        if (!m)  printf("Error in malloc 1");
        m -= nrl;

        for(i=nrl;i<=nrh;i++){
                m[i]=(short int *)calloc((nch-ncl+1),sizeof(short int));
                if (!m[i])  printf("Error in malloc 2");
                m[i]-=ncl;
        }
        return m;
}

/*******************************************
     Initialize float (4byte) matrix
*******************************************/
float **fmatrix(int nrl,int nrh,int ncl,int nch)
{
        float **m;
        int i;

        m=(float **)calloc((nrh-nrl+1),sizeof(float*));

        if (!m)  printf("Error in malloc 1");
        m -= nrl;

        for(i=nrl;i<=nrh;i++){
                m[i]=(float *)calloc((nch-ncl+1),sizeof(float));
                if (!m[i])  printf("Error in malloc 2");
                m[i]-=ncl;
        }
        return m;
}


/*******************************************
    Initialize float 3D matrix
*******************************************/
float ***f3dmatrix(int nrl,int nrh,int ncl,int nch,int nzl,int nzh)
{
        float ***m;
        int i,j;

        m=(float ***)calloc((nzh-nzl+1),sizeof(float**));
        if (!m)  printf("Error in malloc 1");
        m -= nzl;


        for(j=nzl;j<=nzh;j++){
                m[j]=(float **)calloc((nrh-nrl+1),sizeof(float*));
                if (!m[j])  printf("Error in malloc 2");
                for(i=nrl;i<=nrh;i++){
                        m[j][i]=(float *)calloc((nch-ncl+1),sizeof(float));
                        if (!m[j][i])  printf("Error in malloc 2");
                        m[j][i]-=ncl;
                }

                m[j]-=nrl;
        }
        return m;
}


/******************************************************
	Read land mask
******************************************************/
void get_msk(short **msk, int xs, int ys, char *hemi)
{
	int j;
	FILE *file;
	char hdr[300];

	if (*hemi == 'n') file = fopen("/Users/jmiller/disk6items/landmask_north_125", "rb");
	else file = fopen("/Users/jmiller/disk6items/landmask_south_125", "rb");

	fread(hdr, sizeof(char), 300, file);
	for(j=0;j<ys;j++)
                fread(msk[j],sizeof(short),xs,file);
        (void) fclose(file);

}

	

/******************************************************
	Read brightness temperatures
******************************************************/
void get_tbs(short **tb,
	int xsize,int ysize,char *path,char *cddate,char *chan,char *pole,char *hdr)
{
    char name[150];
	char hdr_hdf[294],hdr_raid[300];
	short int **data;
	int i,j;
	int tempxsize,tempysize;

	FILE *file;

	sprintf(name,"%s%s.%s",path,cddate,chan);
   if (*hdr == 'h') sprintf(name,"%s%s.%s",path,cddate,chan);
   if (*hdr == 'r'){
		 printf("in if\n");
		 sprintf(name,"%s%s%s",path,chan,cddate);
	}

	printf("%s\n",name);

   tempxsize=xsize;
   tempysize=ysize;



	data=imatrix(0,tempysize-1,0,tempxsize-1);

	file=fopen(name,"rb");

	if (*hdr == 'h') fread(hdr_hdf,sizeof(char),294,file);
	if (*hdr == 'r') fread(hdr_raid,sizeof(char),300,file);

	for (j=0;j<tempysize;j++)
		fread(data[j],sizeof(short),tempxsize,file);
	fclose(file);
	free(name);

	
	for(i=0;i<xsize;i++){
            for(j=0;j<ysize;j++){
                tb[j][i]=data[j][i];
            }
   }

	free(data);


}


/******************************************************
 Calculate look-up tables	
******************************************************/
void get_lut(float ***LUT19, float ***LUT19thin, float ***LUT85, float ***LUT85thin,
		float ***LUTDGR, float ***LUTGR37, char *hemi)
{
	double phi19, phi85;
    double tbmow[n_atm][7],tbmfy[n_atm][7],tbmcc[n_atm][7],tbmthin[n_atm][7];

	float tb19h,tb19v,tb37v,tb85h,tb85v;
	float tb19ht,tb19vt,tb37vt,tb85ht,tb85vt;
	double caf, cbf;
	int ca,cb;
	int i,f,k;


	FILE *file;

	/* ROTATION */
	if (*hemi == 'n'){
   		phi19=-0.18;
	    phi85=-0.06;
	}
	else {
	    phi19=-0.59;
	    phi85=-0.4;
	}



	/****** Read model data ****/
	/**** open water TBs ***/
	if (*hemi == 'n') /*file=fopen("/disk6/RTM/4icneu/TBowhig.tab","r");*/ file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBowark.tab.amsr_reg","r");
	else               file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBowant.tab.amsr_reg","r");
	for (i=0;i<n_atm;i++)
	    for (f=0;f<7;f++)
	        fscanf(file,"%lf",&tbmow[i][f]);
	fclose(file);

	
	/**** first-year TBs ***/
	if (*hemi == 'n') /*file=fopen("/disk6/RTM/4icneu/TBfyhig.tab","r");*/    file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBfyark.tab.amsr_reg","r");
	else               file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBfyant.tab.amsr_reg","r");
	for (i=0;i<n_atm;i++)
	    for (f=0;f<7;f++)
	        fscanf(file,"%lf",&tbmfy[i][f]);
	fclose(file);
	
	/**** ice type C TBs ***/
	if (*hemi == 'n') file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBccark.tab.amsr_reg","r");
	else               file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBccant.tab.amsr_reg","r");
	for (i=0;i<n_atm;i++)
	    for (f=0;f<7;f++)
	        fscanf(file,"%lf",&tbmcc[i][f]);
	fclose(file);

	/**** ice type thin TBs ***/
	if (*hemi == 'n') {
		file=fopen("/Users/jmiller/disk6items/RTM/4icneu/TBthark.tab.amsr_reg","r");
		for (i=0;i<n_atm;i++)
	    for (f=0;f<7;f++)
	        fscanf(file,"%lf",&tbmthin[i][f]);
		fclose(file);
	}


	/* Create Lookup Table */
	for (ca=0;ca<101;ca++) {
	  for (cb=0;cb<(101-ca);cb++) {
	        caf=ca/100.0;
	        cbf=cb/100.0;
	        for (k=0;k<n_atm;k++){
			
	            tb19h=(1.-caf-cbf)*tbmow[k][0]+caf*tbmfy[k][0]+cbf*tbmcc[k][0];
	            tb19v=(1.-caf-cbf)*tbmow[k][1]+caf*tbmfy[k][1]+cbf*tbmcc[k][1];
	            tb37v=(1.-caf-cbf)*tbmow[k][4]+caf*tbmfy[k][4]+cbf*tbmcc[k][4];
	            tb85h=(1.-caf-cbf)*tbmow[k][5]+caf*tbmfy[k][5]+cbf*tbmcc[k][5];
	            tb85v=(1.-caf-cbf)*tbmow[k][6]+caf*tbmfy[k][6]+cbf*tbmcc[k][6];

	            tb19ht=(1.-caf-cbf)*tbmow[k][0]+caf*tbmfy[k][0]+cbf*tbmthin[k][0];
	            tb19vt=(1.-caf-cbf)*tbmow[k][1]+caf*tbmfy[k][1]+cbf*tbmthin[k][1];
	            tb37vt=(1.-caf-cbf)*tbmow[k][4]+caf*tbmfy[k][4]+cbf*tbmthin[k][4];
	            tb85ht=(1.-caf-cbf)*tbmow[k][5]+caf*tbmfy[k][5]+cbf*tbmthin[k][5];
	            tb85vt=(1.-caf-cbf)*tbmow[k][6]+caf*tbmfy[k][6]+cbf*tbmthin[k][6];
	
	            LUT19[k][ca][cb]=-((tb37v-tb19v)/(tb37v+tb19v))*sin(phi19)+
	                           ((tb19v-tb19h)/(tb19v+tb19h))*cos(phi19);
	            LUT85[k][ca][cb]=-((tb37v-tb19v)/(tb37v+tb19v))*sin(phi85)+
	                            ((tb85v-tb85h)/(tb85v+tb85h))*cos(phi85);
	            LUT19thin[k][ca][cb]=-((tb37vt-tb19vt)/(tb37vt+tb19vt))*sin(phi19)+
	                           ((tb19vt-tb19ht)/(tb19vt+tb19ht))*cos(phi19);
	            LUT85thin[k][ca][cb]=-((tb37vt-tb19vt)/(tb37vt+tb19vt))*sin(phi85)+
	                            ((tb85vt-tb85ht)/(tb85vt+tb85ht))*cos(phi85);
	            LUTDGR[k][ca][cb]=(tb85h-tb19h)/(tb85h+tb19h)-(tb85v-tb19v)/(tb85v+tb19v);
				LUTGR37[k][ca][cb]=(tb37vt-tb19vt)/(tb37vt+tb19vt);
			

	        } /* k */
	   }/* cb */
	}/* ca */


}


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
	double sinphi19,sinphi85;
	double cosphi19,cosphi85;

	double v19i,h19i,v22i,v37i,v85i,h85i;
	double pr19,pr85,gr3719,gr2219,gr8519v,gr8519h;
	double pr19r,pr85r,dgr;
	double dpr19,dpr85,ddgr;

	/**** Weights (currently all set equal 1) ****/
	double w19=1.,w85=1.,wgr=1.;

	int x,y;
	int i,j,k;

	int ca,cc;
	double dmin,d;
	int imin,jmin,cai,ccj;
    int camina[n_atm],ccmina[n_atm],bestk;
    double dmina[n_atm];


    /* ROTATION */
    if (*hemi == 'n'){
        phi19=-0.18;
        phi85=-0.06;
    }
    else {
        phi19=-0.59;
        phi85=-0.4;
	}


	sinphi19=sin(phi19);
	sinphi85=sin(phi85);
	cosphi19=cos(phi19);
	cosphi85=cos(phi85);


	for(x=0;x<xsize;x++){
		for(y=0;y<ysize;y++){
		  if ((v19[y][x] > 500)&&(v85[y][x] > 500)&&(msk[y][x] == 0)){
					/*** If data are valid and no land ***/
			v19i=(double) v19[y][x]/10.;
			h19i=(double) h19[y][x]/10.;
			v22i=(double) v22[y][x]/10.;
			v37i=(double) v37[y][x]/10.;
			v85i=(double) v85[y][x]/10.;
			h85i=(double) h85[y][x]/10.;

			gr3719=(v37i-v19i)/(v37i+v19i);
			gr2219=(v22i-v19i)/(v22i+v19i);

		/*	if ((gr3719 < 0.05) && (gr2219 < 0.045)){  */
			if ((gr2219 < 0.045)){
					/*** if passed the weather filters ***/
				pr19=(v19i-h19i)/(v19i+h19i);
				pr85=(v85i-h85i)/(v85i+h85i);

				gr8519v=(v85i-v19i)/(v85i+v19i);
				gr8519h=(h85i-h19i)/(h85i+h19i);

				pr19r=-gr3719*sinphi19 + pr19*cosphi19;
				pr85r=-gr3719*sinphi85 + pr85*cosphi85;
				dgr=gr8519h-gr8519v; 

				dmin=10000.;

				for (k=0;k<n_atm;k++){
				imin=5;
				jmin=5;
				ca=45;	
				cc=45;

				do {

					dmin=10000.;

					for (i=-1;i<=1;i++){
						for (j=-1;j<=1;j++){
							cai=ca+i;
							ccj=cc+j;
							if((cai < 101)&&(ccj < 101)&&(cai >=0)&&(ccj >=0)&&
								((cai+ccj) >=0)&&((cai+ccj) < 101)){

			
								if((*hemi == 'n')&&(gr3719 > -0.01)){
							
			                    	dpr19=pr19r-LUT19thin[k][cai][ccj];
			                    	dpr85=pr85r-LUT85thin[k][cai][ccj];
									ddgr=gr3719-LUTGR37[k][cai][ccj];
								} else {
			                    	dpr19=pr19r-LUT19[k][cai][ccj];
			                    	dpr85=pr85r-LUT85[k][cai][ccj];
			                    	ddgr=dgr-LUTDGR[k][cai][ccj];
								}
								
			
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
					
					bestk=20;
					dmin=1000;
					for (k=0;k<n_atm;k++){
						if (dmina[k] < dmin){
							dmin=dmina[k];
							bestk=k;
						}
					}

			
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

