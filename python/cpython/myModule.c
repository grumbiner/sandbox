//  https://csl.name/post/c-functions-python/

#include <Python.h>

/*
 *  * Function to be called from Python
 *   */
static PyObject* py_myFunction(PyObject* self, PyObject* args)
{
  char *s = "Hello from C!";
  return Py_BuildValue("s", s);
}

/*
 *  * Another function to be called from Python
 *   */
static PyObject* py_myOtherFunction(PyObject* self, PyObject* args)
{
  double earth_radius, rpdg, ab, ac, bc, arg, dist = 0;
  double lat1, lon1, lat2, lon2;
  earth_radius = 6371.2;
  rpdg = M_PI/180.0;

  PyArg_ParseTuple(args, "dddd", &lat1, &lon1, &lat2, &lon2);
    ab = (90.-lat1)      * rpdg;
    ac = (90.-lat2)      * rpdg;
    bc = abs(lon1 - lon2)* rpdg;

    arg = cos(ab)*cos(ac)+sin(ab)*sin(ac)*cos(bc);
    if (arg > 1 ) arg = 1.;
    if (arg < -1) arg = -1.;

    dist = earth_radius * acos(arg);
  return Py_BuildValue("d",dist);

}

/*
 *  * Bind Python function names to our C functions
 *   */
static PyMethodDef myModule_methods[] = {
  {"myFunction", py_myFunction, METH_VARARGS},
  {"myOtherFunction", py_myOtherFunction, METH_VARARGS},
  {NULL, NULL}
};

/*
 *  * Python calls this to let us initialize our module
 *   */

void initmyModule()
{
  (void) Py_InitModule("myModule", myModule_methods);
}

