From Don Cavalieri before 2009 May 21

/***********************************
ice concentration and snow depth algorithms for sea_ice_nt
***********************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define n_atm 12	/*** Number of atmospheres ***/


/*******************************************
    Initialize float 3D matrix
*******************************************/
float ***f3dmatrix(int nrl,int nrh,int ncl,int nch,int nzl,int nzh, short int *status)
{
	float ***m;
	int i,j;
	*status=-1;

	m=(float ***)calloc((nzh-nzl+1),sizeof(float**));
	if (!m)  return m; /* Error in malloc */
	m -= nzl;

	for(j=nzl;j<=nzh;j++)
	{
		m[j]=(float **)calloc((nrh-nrl+1),sizeof(float*));
		if (!m[j])  return m; /* Error in malloc */
		for(i=nrl;i<=nrh;i++)
		{
			m[j][i]=(float *)calloc((nch-ncl+1),sizeof(float));
			if (!m[j][i])  return m; /* Error in malloc */
			m[j][i]-=ncl;
		}

		m[j]-=nrl;
	}
	*status=0;
	return m;
}

/******************************************************
 Calculate look-up tables	
******************************************************/
void get_lut(float LUT19[n_atm][100][100], float LUT19thin[n_atm][100][100], float LUT85[n_atm][100][100], float LUT85thin[n_atm][100][100], float LUTGR[n_atm][100][100], float LUTGR37[n_atm][100][100], char *pole)
{
	double phi19, phi85;
	double tbmow[n_atm][7],tbmfy[n_atm][7],tbmcc[n_atm][7], tbmth[n_atm][7];

	float tb19h,tb19v,tb37v,tb85h,tb85v;
	float tb19ht,tb19vt,tb37vt,tb85ht,tb85vt;
	double caf, cbf;
	int ca,cb;
	int k,m,n;

	double tbmow_N[][7] = 
	{ 
  99.4, 184.5, 196.7, 132.3, 211.9, 181.9, 248.8,
  96.0, 181.3, 192.1, 129.4, 208.7, 171.6, 243.6,
 103.8, 186.5, 199.2, 140.3, 214.7, 190.2, 249.3,
 100.9, 183.5, 194.5, 136.6, 211.0, 178.0, 243.9,
 100.8, 185.2, 197.7, 134.8, 212.9, 188.0, 250.1,
  97.2, 181.9, 192.9, 132.0, 209.7, 176.4, 244.5,
 103.6, 186.6, 199.6, 142.4, 216.1, 202.5, 253.1,
 101.1, 183.8, 195.4, 141.0, 213.2, 190.4, 246.8,
 106.0, 187.8, 201.2, 148.4, 218.5, 212.5, 255.2,
 104.3, 185.3, 197.4, 148.1, 216.0, 200.1, 248.4,
 115.1, 192.1, 206.7, 165.9, 225.0, 227.2, 255.6,
 113.2, 189.6, 202.8, 164.5, 222.2, 218.0, 250.5

	} ;

	double tbmow_S[][7] = 
	{ 
  99.4, 184.5, 196.7, 132.3, 211.9, 181.9, 248.8,
  96.0, 181.3, 192.1, 129.4, 208.7, 171.6, 243.6,
 103.8, 186.5, 199.2, 140.3, 214.7, 190.2, 249.3,
 100.9, 183.5, 194.5, 136.6, 211.0, 178.0, 243.9,
 100.8, 185.2, 197.7, 134.8, 212.9, 188.0, 250.1,
  97.2, 181.9, 192.9, 132.0, 209.7, 176.4, 244.5,
 103.6, 186.6, 199.6, 142.4, 216.1, 202.5, 253.1,
 101.1, 183.8, 195.4, 141.0, 213.2, 190.4, 246.8,
 106.0, 187.8, 201.2, 148.4, 218.5, 212.5, 255.2,
 104.3, 185.3, 197.4, 148.1, 216.0, 200.1, 248.4,
 115.1, 192.1, 206.7, 165.9, 225.0, 227.2, 255.6,
 113.2, 189.6, 202.8, 164.5, 222.2, 218.0, 250.5


	} ;
	
	double tbmfy_N[][7] = 
	{
      243.686,      257.860,      258.403,      242.772,      257.167,      230.090,      242.662,
      224.918,      238.760,      239.593,      224.328,      239.050,      209.777,      225.366,		      
      243.890,      257.660,      258.205,      243.514,      257.068,      232.976,      243.790,
      225.326,      238.660,      239.593,      225.282,      239.149,      212.441,      226.682,
      243.788,      257.960,      258.502,      243.302,      257.464,      232.754,      244.166,
      225.122,      238.860,      239.692,      224.858,      239.347,      212.108,      226.776,
      244.196,      258.060,      258.700,      244.786,      258.058,      239.303,      247.832,
      225.734,      239.060,      240.088,      226.872,      240.337,      219.212,      230.912,
      244.502,      258.160,      258.799,      245.846,      258.553,      243.854,      250.370,
      226.244,      239.260,      240.385,      228.462,      241.030,      224.096,      233.826,
      245.318,      258.160,      258.799,      248.178,      258.751,      248.849,      252.250,
      227.570,      239.860,      241.177,      231.960,      242.614,      233.198,      238.996

	} ;
	
	double tbmfy_S[][7] = 
	{
      243.686,      257.860,      258.403,      242.772,      257.167,      230.090,      242.662,
      224.918,      238.760,      239.593,      224.328,      239.050,      209.777,      225.366,		      
      243.890,      257.660,      258.205,      243.514,      257.068,      232.976,      243.790,
      225.326,      238.660,      239.593,      225.282,      239.149,      212.441,      226.682,
      243.788,      257.960,      258.502,      243.302,      257.464,      232.754,      244.166,
      225.122,      238.860,      239.692,      224.858,      239.347,      212.108,      226.776,
      244.196,      258.060,      258.700,      244.786,      258.058,      239.303,      247.832,
      225.734,      239.060,      240.088,      226.872,      240.337,      219.212,      230.912,
      244.502,      258.160,      258.799,      245.846,      258.553,      243.854,      250.370,
      226.244,      239.260,      240.385,      228.462,      241.030,      224.096,      233.826,
      245.318,      258.160,      258.799,      248.178,      258.751,      248.849,      252.250,
      227.570,      239.860,      241.177,      231.960,      242.614,      233.198,      238.996

	} ;
	
	double tbmcc_N[][7] = 
	{

      190.0,        236.2,        233.9,        190.1,        225.4,        195.5,        219.6,
      175.2,        218.5,        215.9,        176.4,        209.1,        176.4,        200.7,
      191.7,        236.7,        234.7,        194.3,        227.1,        202.0,        223.4,
      177.3,        219.2,        216.9,        180.4,        211.0,        181.9,        204.0,
      190.6,        236.4,        234.3,        191.5,        226.1,        200.4,        222.9,
      175.8,        218.7,        216.3,        178.0,        209.9,        180.6,        203.5,
      191.9,        237.0,        235.2,        195.8,        228.4,        212.4,        230.6,
      177.6,        219.5,        217.6,        183.3,        212.9,        192.9,        211.7,
      193.0,        237.4,        235.9,        199.2,        230.2,        220.7,        236.0,
      179.2,        220.2,        218.7,        187.4,        215.2,        201.5,        217.4,
      196.8,        238.6,        238.0,        208.4,        234.5,        232.2,        242.4,
      183.3,        221.9,        221.5,        197.1,        220.4,        217.5,        228.0

	} ;
	
	double tbmcc_S[][7] = 
	{
      187.202,      227.571,      231.572,      190.340,      221.940,      211.595,      227.555,
      171.902,      210.444,      214.901,      176.540,      205.940,      196.598,      210.170,		      
      189.140,      228.363,      232.396,      194.340,      223.940,      215.303,      230.500,
      174.146,      211.533,      216.000,      180.440,      207.940,      199.730,      212.830,
      187.916,      227.868,      232.030,      191.740,      222.740,      214.562,      230.120,
      172.514,      210.840,      215.359,      178.040,      206.840,      199.153,      212.450,
      189.344,      228.660,      232.854,      195.940,      225.240,      221.730,      236.485,
      174.554,      211.929,      216.550,      183.340,      210.040,      206.569,      219.195,
      190.568,      229.254,      233.496,      199.340,      227.140,      226.674,      240.855,
      176.186,      212.721,      217.466,      187.440,      212.440,      211.842,      223.850,
      194.750,      231.135,      235.602,      208.340,      231.940,      233.349,      245.985,
      180.776,      215.097,      220.305,      196.940,      218.040,      221.648,      232.590

	} ;
	
	double tbmth_S[][7] = 
	{	 
173.9, 239.2, 239.5, 192.1, 242.9, 209.1, 248.4,
160.1, 221.3, 221.3, 178.3, 225.3, 190.1, 229.7,
176.1, 239.6, 240.1, 196.1, 243.5, 214.1, 248.7,
162.8, 221.9, 222.1, 182.2, 226.1, 194.4, 230.4,
174.6, 239.4, 239.9, 193.5, 243.3, 213.1, 249.6,
160.8, 221.5, 221.7, 179.8, 225.8, 193.6, 230.8,
176.2, 239.9, 240.6, 197.7, 244.6, 222.7, 252.3,
163.1, 222.2, 222.8, 185.0, 227.6, 203.5, 234.1,
177.5, 240.2, 241.2, 201.0, 245.6, 229.3, 254.2,
164.9, 222.8, 223.6, 189.0, 229.0, 210.5, 236.5,
182.3, 241.3, 242.8, 209.9, 247.4, 238.0, 254.5,
169.9, 224.4, 226.0, 198.4, 232.0, 223.4, 240.4

	} ;

	double tbmth_N[][7] = 
	{	 

173.9, 239.2, 239.5, 192.1, 242.9, 209.1, 248.4,
160.1, 221.3, 221.3, 178.3, 225.3, 190.1, 229.7,
176.1, 239.6, 240.1, 196.1, 243.5, 214.1, 248.7,
162.8, 221.9, 222.1, 182.2, 226.1, 194.4, 230.4,
174.6, 239.4, 239.9, 193.5, 243.3, 213.1, 249.6,
160.8, 221.5, 221.7, 179.8, 225.8, 193.6, 230.8,
176.2, 239.9, 240.6, 197.7, 244.6, 222.7, 252.3,
163.1, 222.2, 222.8, 185.0, 227.6, 203.5, 234.1,
177.5, 240.2, 241.2, 201.0, 245.6, 229.3, 254.2,
164.9, 222.8, 223.6, 189.0, 229.0, 210.5, 236.5,
182.3, 241.3, 242.8, 209.9, 247.4, 238.0, 254.5,
169.9, 224.4, 226.0, 198.4, 232.0, 223.4, 240.4

	} ;
	
	
	/* ROTATION */
	if (*pole == 'n')
	{
   		phi19=-0.18;
	    phi85=-0.06;
	}
	else 
	{
	    phi19=-0.59;
	    phi85=-0.4;
	}

	if (*pole == 'n') 
	{
		for (m=0;m<n_atm;m++) 
		{ 
			for (n=0;n<7;n++) 
			{
				tbmow[m][n]=tbmow_N[m][n] ;
				tbmfy[m][n]=tbmfy_N[m][n] ;
				tbmcc[m][n]=tbmcc_N[m][n] ;
				tbmth[m][n]=tbmth_N[m][n] ;
			}
		}
	}
	else 
	{
		for (m=0;m<n_atm;m++) 
		{
			for (n=0;n<7;n++) 
			{
				tbmow[m][n]=tbmow_S[m][n] ;
				tbmfy[m][n]=tbmfy_S[m][n] ;
				tbmcc[m][n]=tbmcc_S[m][n] ;
				tbmth[m][n]=tbmth_S[m][n] ;
			}
		}
	}
		
	/* Create Lookup Table */

	for (ca=0;ca<101;ca++) 
	{
		for (cb=0;cb<(101-ca);cb++) 
		{
			caf=ca/100.0;
	        cbf=cb/100.0;
	        for (k=0;k<n_atm;k++)
			{
	            tb19h=(1.-caf-cbf)*tbmow[k][0]+caf*tbmfy[k][0]+cbf*tbmcc[k][0];
	            tb19v=(1.-caf-cbf)*tbmow[k][1]+caf*tbmfy[k][1]+cbf*tbmcc[k][1];
	            tb37v=(1.-caf-cbf)*tbmow[k][4]+caf*tbmfy[k][4]+cbf*tbmcc[k][4];
	            tb85h=(1.-caf-cbf)*tbmow[k][5]+caf*tbmfy[k][5]+cbf*tbmcc[k][5];
	            tb85v=(1.-caf-cbf)*tbmow[k][6]+caf*tbmfy[k][6]+cbf*tbmcc[k][6];
				
	            tb19ht=(1.-caf-cbf)*tbmow[k][0]+caf*tbmfy[k][0]+cbf*tbmth[k][0];
	            tb19vt=(1.-caf-cbf)*tbmow[k][1]+caf*tbmfy[k][1]+cbf*tbmth[k][1];
	            tb37vt=(1.-caf-cbf)*tbmow[k][4]+caf*tbmfy[k][4]+cbf*tbmth[k][4];
	            tb85ht=(1.-caf-cbf)*tbmow[k][5]+caf*tbmfy[k][5]+cbf*tbmth[k][5];
	            tb85vt=(1.-caf-cbf)*tbmow[k][6]+caf*tbmfy[k][6]+cbf*tbmth[k][6];				
	
	            LUT19[k][ca][cb]=-((tb37v-tb19v)/(tb37v+tb19v))*sin(phi19)+
	                              ((tb19v-tb19h)/(tb19v+tb19h))*cos(phi19);
	
	            LUT85[k][ca][cb]=-((tb37v-tb19v)/(tb37v+tb19v))*sin(phi85)+
	                              ((tb85v-tb85h)/(tb85v+tb85h))*cos(phi85);
								  
	            LUT19thin[k][ca][cb]=-((tb37vt-tb19vt)/(tb37vt+tb19vt))*sin(phi19)+
	                              ((tb19vt-tb19ht)/(tb19vt+tb19ht))*cos(phi19);
	
	            LUT85thin[k][ca][cb]=-((tb37vt-tb19vt)/(tb37vt+tb19vt))*sin(phi85)+
	                              ((tb85vt-tb85ht)/(tb85vt+tb85ht))*cos(phi85);								  
	
	            LUTGR[k][ca][cb]=(tb85h-tb19h)/(tb85h+tb19h) - (tb85v-tb19v)/(tb85v+tb19v);
				
				LUTGR37[k][ca][cb]=(tb37vt-tb19vt)/(tb37vt+tb19vt);
	        } /* k */
	   }/* cb */
	}/* ca */
}


/******************************************************
Calculate ice concentrations
******************************************************/
void icecon_nt2_ ( float *lat, float *lon, float *v19, float *h19, float *v22, float *v37,
	float *v85, float *h85, float *icecon, int *gdata, int *xsize, int *ysize, 
	float *invalid, short int *status)
{

	
	double phi19_N, phi85_N;
	double phi19_S, phi85_S;
	double sinphi19_N,sinphi85_N;
	double sinphi19_S,sinphi85_S;
	double cosphi19_N,cosphi85_N;
	double cosphi19_S,cosphi85_S;

	double v19i,h19i,v22i,v37i,v85i,h85i;
	double pr19,pr85,gr3719,gr2219,gr8519v,gr8519h;
	double pr19r,pr85r,dgr;
	double dpr19,dpr85,ddgr;
	/**** Weights (currently all set equal 1) ****/
	double w19=1.,w85=1.,wgr=1.;

	int x;
	int i,j,k;

	int ca,cc;
	double dmin,d;
	int imin,jmin,cai,ccj;
	int camina[n_atm],ccmina[n_atm],bestk;
	double dmina[n_atm];
	

	float LUT19_N[n_atm][100][100];
	float LUT19thin_N[n_atm][100][100];
	float LUT85_N[n_atm][100][100];
	float LUT85thin_N[n_atm][100][100];
	float LUTGR_N[n_atm][100][100];
	float LUTGR37_N[n_atm][100][100];
	float LUT19_S[n_atm][100][100];
	float LUT19thin_S[n_atm][100][100];
	float LUT85_S[n_atm][100][100];
	float LUT85thin_S[n_atm][100][100];
	float LUTGR_S[n_atm][100][100];
	float LUTGR37_S[n_atm][100][100];



	/***** Begin program *******************/


	
	/*************************/

	/**** Get look-up tables ****/
	(void) get_lut(LUT19_N,LUT19thin_N,LUT85_N,LUT85thin_N,LUTGR_N,LUTGR37_N,"n");
	(void) get_lut(LUT19_S,LUT19thin_S,LUT85_S,LUT85thin_S,LUTGR_S,LUTGR37_S,"s");

	/***** Calculate ice concentration ****/

	/* ROTATION */
	phi19_N=-0.18;
	phi85_N=-0.06;
	phi19_S=-0.59;
	phi85_S=-0.4;

	sinphi19_N=sin(phi19_N);
	sinphi85_N=sin(phi85_N);
	cosphi19_N=cos(phi19_N);
	cosphi85_N=cos(phi85_N);
	sinphi19_S=sin(phi19_S);
	sinphi85_S=sin(phi85_S);
	cosphi19_S=cos(phi19_S);
	cosphi85_S=cos(phi85_S);

	for(x=0;x<((*xsize)*(*ysize));x++)
	{
		if (gdata[x] != 0)	
		{
			icecon[x]=*invalid;
		}
		else
		{
			v19i=(double) v19[x];
			h19i=(double) h19[x];
			v22i=(double) v22[x];
			v37i=(double) v37[x];
			v85i=(double) v85[x];
			h85i=(double) h85[x];
	
			gr3719=(v37i-v19i)/(v37i+v19i);
			gr2219=(v22i-v19i)/(v22i+v19i);
	
			if ((gr3719 < 0.05) && (gr2219 < 0.045))
			{ 
				/*** if passed the weather filters ***/
				pr19=(v19i-h19i)/(v19i+h19i);
				pr85=(v85i-h85i)/(v85i+h85i);
	
				gr8519v=(v85i-v19i)/(v85i+v19i);
				gr8519h=(h85i-h19i)/(h85i+h19i);
	
				if (lat[x] >= 0) 
				{
					pr19r=-gr3719*sinphi19_N + pr19*cosphi19_N;
					pr85r=-gr3719*sinphi85_N + pr85*cosphi85_N;
				}
				else 
				{
					pr19r=-gr3719*sinphi19_S + pr19*cosphi19_S;
					pr85r=-gr3719*sinphi85_S + pr85*cosphi85_S;
				}
	
				dgr=gr8519h-gr8519v;
	
				for (k=0;k<n_atm;k++)
				{
					imin=0;
					jmin=0;
					ca=45;
					cc=45;
				
					do 
					{
						dmin=1000.;				
						for (i=-1;i<=1;i++)
						{
							for (j=-1;j<=1;j++)
							{
								cai=ca+i;
				                ccj=cc+j;

				                if ((cai < 101)&&(ccj < 101)&&(cai >= 0)&&(ccj >= 0)&&(cai+ccj >= 0)&&(cai+ccj < 101))
								{
									
									
/* 									if (lat[x] >= 0) 
									{
									  if ( (gr3719 > -0.01) )
										{ 
											dpr19=pr19r-LUT19thin_N[k][cai][ccj];
				                    		dpr85=pr85r-LUT85thin_N[k][cai][ccj];
				                    		ddgr=gr3719-LUTGR37_N[k][cai][ccj];
										}
										else
										{	
											dpr19=pr19r-LUT19_N[k][cai][ccj];
				                    		dpr85=pr85r-LUT85_N[k][cai][ccj];
				                    		ddgr=dgr-LUTGR_N[k][cai][ccj];
										} 
									}
									else 
									{					
										dpr19=pr19r-LUT19_S[k][cai][ccj];
										dpr85=pr85r-LUT85_S[k][cai][ccj];
										ddgr=dgr-LUTGR_S[k][cai][ccj];
									} */
				
									if (lat[x] >= 0) 
									{
									  if ( (gr3719 > -0.01) )
										{ 
								dpr19=pr19r-LUT19thin_N[k][cai][ccj];
				                    		dpr85=pr85r-LUT85thin_N[k][cai][ccj];
				                    		ddgr=gr3719-LUTGR37_N[k][cai][ccj];
										}
										else
										{	
								dpr19=pr19r-LUT19_N[k][cai][ccj];
				                    		dpr85=pr85r-LUT85_N[k][cai][ccj];
				                    		ddgr=dgr-LUTGR_N[k][cai][ccj];
										} 
									}
									else 
									{					
									  if ( (gr3719 > -0.01) )
										{ 
								dpr19=pr19r-LUT19thin_S[k][cai][ccj];
				                    		dpr85=pr85r-LUT85thin_S[k][cai][ccj];
				                    		ddgr=gr3719-LUTGR37_S[k][cai][ccj];
										}
										else
										{	
								dpr19=pr19r-LUT19_S[k][cai][ccj];
				                    		dpr85=pr85r-LUT85_S[k][cai][ccj];
				                    		ddgr=dgr-LUTGR_S[k][cai][ccj];
										} 
									}									
									
	
																	d=w19*dpr19*dpr19+w85*dpr85*dpr85+wgr*ddgr*ddgr;
				                    if (d < dmin)
									{	
				                        dmin=d;
				                        imin=i;
				                        jmin=j;
				                    }
								}
							}
						}
						ca=ca+imin;
						cc=cc+jmin;
					}
					while ((imin !=0)||(jmin != 0));
				
					camina[k]=ca;
					ccmina[k]=cc;
					dmina[k]=dmin;
				
				}
							
				bestk=20;
				dmin=1000.;
				for (k=0;k<n_atm;k++)
				{
					if (dmina[k] < dmin)
					{
						dmin=dmina[k];
						bestk=k;
					}
				}
				
				icecon[x]=(camina[bestk]+ccmina[bestk]);
				
				if (icecon[x] > 100.0) 
				{
					icecon[x]=*invalid;
				}
			} /*endif*/
			else icecon[x]=0;  /** Weather **/
		}/* endif*/
	} /*x*/			
}

 /* ============================================================================================= */

void snowdepth_nt2_ (float *lat, float *v19, float *v37,
                     float *icecon, float *snow, int *nc, int *nr, 
                     float *invalid, float *snowflag_val, float*multiyear_gr_swath)
{

int i,num;
double ice;
double Tow19v = 176.6;
double Tow37v = 200.5;
double A = 2.9;
double B = -782.0;
double T19, T37, g;

float v19s, v37s; /*tbs converted to ssmi equiv */

num=(*nc)*(*nr);

for (i=0;i<num;i++)
{
  if (icecon[i] == *invalid)
  {
    snow[i] = *invalid;
    multiyear_gr_swath[i] = *invalid;
  }
  else
  {
    if (lat[i] > 0.0)
    {

		  
	  v19s = v19[i] * 1.017076 - 2.65127;
	  v37s = v37[i] * 1.016822 - 5.22634;	  
	  
    }
    else
    {
	    
	  v19s = v19[i] * 1.033907 + 2.94;
	  v37s = v37[i] * 1.075178 - 19.1253;	    
    }	   
    
    ice = icecon[i]/100.0;
    if (ice > 0.2)
    {
      
      T19 = (v19s - (1.0 - ice) * Tow19v) /ice;
      T37 = (v37s - (1.0 - ice) * Tow37v) /ice;
      g = (T37 - T19)/ ( 1.0 * T37 + T19);
      multiyear_gr_swath[i] = g;
		      
      snow[i] = A + B * g;
      if (snow[i] < 0.0) snow[i] = 0.0;
      if (snow[i] > 60.0) snow[i] = 60.0;
    } 
    else 
    {
      snow[i] = *snowflag_val;
      multiyear_gr_swath[i] = *invalid;
    }
  }	
}
}
		


