template <class T>
class stlawrence : public llgrid<T> {
  public:
    stlawrence();
};
template <class T>
stlawrence<T>::stlawrence() {
  this->dlat =  1./12.;
  this->dlon =  1./12.;
  this->firstlon = 360.-70.- this->dlon/2.;
  this->firstlat = 44.0 -  this->dlat/2.; //52.0 + dlat/2.;
  this->nx = (int) (0.5 + (-52+360 - this->firstlon) /  this->dlon);
  this->ny = (int) (0.5 + (52.0+ this->dlat/2.    - this->firstlat) /  this->dlat);
//  printf("nx, ny = %d %d\n",nx, ny);
  this->cyclicx = (fabs( this->nx *  this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a stlawrence\n"; }

}

template <class T>
class okhotsk : public llgrid<T> {
  public:
    okhotsk();
};
template <class T>
okhotsk<T>::okhotsk() {
  this->dlat =  1./12.;
  this->dlon =  1./12.;
  this->firstlon = 135.0 - this->dlon/2.;
  this->firstlat =  42.0 - this->dlat/2.; 
  this->nx = (int) (0.5 + (165.0 + this->dlon/2. - this->firstlon) / this->dlon);
  this->ny = (int) (0.5 + ( 63.0 + this->dlat/2. - this->firstlat) / this->dlat);
  //printf("nx, ny = %d %d\n",nx, ny);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a okhotsk\n"; }

}
