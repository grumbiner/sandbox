template <class T>
class stlawrence : public llgrid<T> {
  public:
    stlawrence();
};
template <class T>
stlawrence<T>::stlawrence() {
  dlat =  1./12.;
  dlon =  1./12.;
  firstlon = 360.-70.-dlon/2.;
  firstlat = 44.0 - dlat/2.; //52.0 + dlat/2.;
  nx = (int) (0.5 + (-52+360 - firstlon) / dlon);
  ny = (int) (0.5 + (52.0+dlat/2.    - firstlat) / dlat) ;
//  printf("nx, ny = %d %d\n",nx, ny);
  cyclicx = (fabs(nx * dlon) >= 360.0);
  cyclicy = false;
  pds.set_gridid(255);

  grid = new T[nx*ny];
  if (grid == (T *) NULL) {cout << "failed to new a stlawrence\n"; }

}

template <class T>
class okhotsk : public llgrid<T> {
  public:
    okhotsk();
};
template <class T>
okhotsk<T>::okhotsk() {
  dlat =  1./12.;
  dlon =  1./12.;
  firstlon = 135.0 - dlon/2.;
  firstlat =  42.0 - dlat/2.; 
  nx = (int) (0.5 + (165.0 + dlon/2. - firstlon) / dlon);
  ny = (int) (0.5 + ( 63.0 + dlat/2. - firstlat) / dlat) ;
  printf("nx, ny = %d %d\n",nx, ny);
  cyclicx = (fabs(nx * dlon) >= 360.0);
  cyclicy = false;
  pds.set_gridid(255);

  grid = new T[nx*ny];
  if (grid == (T *) NULL) {cout << "failed to new a okhotsk\n"; }

}

template <class T>
class bigger : public llgrid<T> {
  public:
    bigger();
};
template <class T>
bigger<T>::bigger() {
  dlat =  1./4.;
  dlon =  1./4.;
  firstlon = 360.-90.-dlon/2.;
  firstlat = 40.0 - dlat/2.; //52.0 + dlat/2.;
  nx = (int) 600;
  ny = (int) 600;
//  printf("nx, ny = %d %d\n",nx, ny);
  cyclicx = (fabs(nx * dlon) >= 360.0);
  cyclicy = false;
  pds.set_gridid(255);

  grid = new T[nx*ny];
  if (grid == (T *) NULL) {cout << "failed to new a bigger\n"; }

}

