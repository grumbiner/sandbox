#include <stdio.h>
#  #include <pascal.h>
#include <stdlib.h>
#include <math.h>

/* Note that for temporary convenience the cgs system is used for the rocket */
typedef struct { float x, y, z;                          } pt3;
typedef struct { float r, theta, phi;                    } sphpt;
typedef struct { float impulse, avnewt, delay, mass;     } engine;
typedef struct { float radius, length, mass;             } tube;
typedef struct { float shape, radius, length, mass;      } nose;
typedef struct { float shape, area, material, thickness, mass; } fins;
typedef struct { float shape, area, length;              } parachute;
float mag(pt3 vec);

class rocket  {
  private:
  	pt3     posit, vel, viewpt;
  	sphpt	disppt;
  	engine	e;
  	tube	t;
  	nose	n;
  	fins	*f;
  	short int nfins;
  	parachute p;
  	float mass;
  	CIconHandle icon;
  public:
  	void set(void);
  	void showPos(void);
  	void findsph(void);
/*  	void view(void); */
  	void view(float time);
  	void launch(float angle);
} ;

float mag(pt3 vec) {
  float magnitude;
  magnitude = vec.x*vec.x + vec.y*vec.y + vec.z*vec.z;
  magnitude = sqrt(magnitude);
  return magnitude;
}

void rocket::launch(float angle) {
	pt3 force;
	int i, nstep;
	float tburn, dt;
	float rhoa, cd;
	float t;
	
	tburn = e.impulse / e.avnewt;
	dt = tburn / 16.;
	cd = 5.0E-2;
	rhoa = 1.2E-3;
	
	t = 0.0;
	
	for (i = 0; i < 16; i++) {
	  t += dt;
	  force.x = - rhoa * cd * vel.x * mag(vel);
	  force.y = - rhoa * cd * vel.y * mag(vel)
	  			+ e.avnewt*cos(angle) * 1.E5;
	  force.z = -9.8*100.*mass 
	  		    - rhoa * cd * vel.z * mag(vel)
	  		    + e.avnewt*sin(angle) * 1.E5;
	  posit.x += vel.x * dt;
	  posit.y += vel.y * dt;
	  posit.z += vel.z * dt;
	  findsph();
	  vel.x += force.x * dt / mass;
	  vel.y += force.y * dt / mass;
	  vel.z += force.z * dt / mass;
	  view(t);
	}
	
/* Coast Segment of flight */
 	nstep = e.delay / dt;
	mass -= 20.;
	
	for (i = 0; i < nstep; i++) {
	  t += dt;
	  force.x = - rhoa * cd * vel.x * mag(vel);
	  force.y = - rhoa * cd * vel.y * mag(vel);
	  force.z = -9.8*100.*mass 
	  		    - rhoa * cd * vel.z * mag(vel);
	  posit.x += vel.x * dt;
	  posit.y += vel.y * dt;
	  posit.z += vel.z * dt;
	  findsph();
	  vel.x += force.x * dt / mass;
	  vel.y += force.y * dt / mass;
	  vel.z += force.z * dt / mass;
	  view(t); 
	}	
	
/* Return to Earth Section */
	cd = 1.0;
	
	while (posit.z > 0 ) {
	  t += dt;
	  force.x = - rhoa * cd * vel.x * mag(vel);
	  force.y = - rhoa * cd * vel.y * mag(vel);
	  force.z = -9.8*100.*mass 
	  		    - rhoa * cd * vel.z * mag(vel);
	  posit.x += vel.x * dt;
	  posit.y += vel.y * dt;
	  posit.z += vel.z * dt;
	  findsph();
	  vel.x += force.x * dt / mass;
	  vel.y += force.y * dt / mass;
	  vel.z += force.z * dt / mass;
	  view(t);
	}	
	  
 	return;
}

void rocket::showPos(void) {
	char buf[20];
	Point curloc;
	
	GetPen(&curloc);
	Move(-curloc.h, 10);
	sprintf(buf," %f",posit.x);
	DrawString(CtoPstr(buf));
	
	GetPen(&curloc);
/*	Move(-curloc.h, 10); */
	sprintf(buf," %f",posit.y);
	DrawString(CtoPstr(buf));
	
	GetPen(&curloc);
/*	Move(-curloc.h, 10); */
	sprintf(buf," %f",posit.z);
	DrawString(CtoPstr(buf));
	
	return;
}
	
void rocket::findsph(void) {
	float r2, x2, y2, z2;

	x2 = (viewpt.x - posit.x)*(viewpt.x - posit.x);
	y2 = (viewpt.y - posit.y)*(viewpt.y - posit.y);
	z2 = (viewpt.z - posit.z)*(viewpt.z - posit.z);
	disppt.r = sqrt( x2 + y2 + z2 );
	r2		 = sqrt( x2 + y2   );

	disppt.phi = atan( (posit.z - viewpt.z)/ r2 );
	disppt.theta = atan( -(posit.y - viewpt.y) / (posit.x - viewpt.x) );
	
	return;
}

void rocket::view(float t) {
  	char buf[20];
	Point curloc;
	Rect picturerect={368, 50, 400, 82};

	picturerect.left   += (int) t*12.;
    picturerect.top    -= (int) t*12.;
    picturerect.bottom -= (int) t*12.;
    picturerect.right  += (int) t*12.; 
    PlotCIcon(&picturerect, icon);

	return;

    GetPen(&curloc);
    Move(-curloc.h,0);
    sprintf(buf," %f",t);
    DrawString(CtoPstr(buf));
    sprintf(buf," %7.2f",disppt.r/100.);
    DrawString(CtoPstr(buf));
    sprintf(buf," %6.3f",disppt.phi);
    DrawString(CtoPstr(buf));
    sprintf(buf," %6.3f",disppt.theta);
    DrawString(CtoPstr(buf));
    
    return;
}
		
void rocket::set(void) {
  int i;
  float rho;

    icon = GetCIcon(128);
    if (icon == nil) { SysBeep(10); ExitToShell(); }

    rho = .20;
  
	posit.x = 0.;
	posit.y = 0.;
	posit.z = 0.;
	vel.x   = 0.;
	vel.y   = 0.;
	vel.z   = 0.;
	viewpt.x = -5.0*100.;
	viewpt.y = -5.0*100.;
	viewpt.z =  1.5*100.;
	findsph();
	e.impulse = 0.5;
	e.avnewt  = 8.0;
	e.delay   = 3.0;
	e.mass    = 50.0;
	t.radius  = 25.4;
	t.length  = 100.0;
	t.mass    = 10.0;
	n.shape   = 1.;
	n.radius  = t.radius;
	n.length  = 25.4;
	nfins = 4;
	f  = malloc( sizeof(fins)*nfins);
	for (i = 0; i < nfins; i++) {
		f[i].shape = 0.;
		f[i].area  = 25.;
		f[i].material = 1.;
		f[i].thickness = .2;
		f[i].mass      = rho*f[i].area*f[i].thickness;
	}
	p.shape = 1.;
	p.area = 100.;
	p.length = 10.;

	mass = e.mass + t.mass;
	for (i = 0; i < nfins; i++) {
	  mass += f[i].mass;
	}
			
	return;
}
 
/*
void rocket::view(void) {
  	char buf[20];
	Point curloc;
	RgnHandle tmpr;
	WindowPtr frwin;
	
	return;
	frwin=FrontWindow();
	tmpr=NewRgn();
	ScrollRect(&frwin->portRect, 0, -10, tmpr);
	DisposeRgn(tmpr);

    GetPen(&curloc);
    Move(-curloc.h,0);
    sprintf(buf," %f",disppt.r);
    DrawString(CtoPstr(buf));
    GetPen(&curloc);
    sprintf(buf," %f",disppt.phi);
    DrawString(CtoPstr(buf));
    GetPen(&curloc);
    sprintf(buf," %f",disppt.theta);
    DrawString(CtoPstr(buf));
    
    return;
}
*/
	
