//------------------------------------------------------------------------------
//
// (C) QSS Corporation
//
//
// NAME: common_constants
//
// PURPOSE:
//    This is a module defining some common constant scientific parameters.
//
// CATEGORY:
//    Data storage
//
// CALLING SEQUENCE:
//    #include "common_constants.hpp"
//
// INPUTS:
//    None
//
// OUTPUTS:
//    None
//
// CALLS:
//    None
//
// MODULES:
//    #include "int_type_define.hpp"
//
// SIDE EFFECTS:
//    None known
//
// RESTRICTIONS:
//    None
//
// PROCEDURE:
//
// EXAMPLE:
//
// MODIFICATION HISTORY:
//       Written by: Thomas King  (03/19/2006)
//                   QSS Corporation
//                   Lanham, MD
//                   Thomas.S.King@noaa.gov
//
//  $Date:$
//  $Id:$
//  $Log:$
//
//------------------------------------------------------------------------------

//
//common_constants.hpp
//

#if not defined( common_constants_hpp )
#define common_constants_hpp

#include "int_type_define.hpp"

//
// These are the array dimensions for the full IASI record extracted
// from the binary.
//
//

const float PI = 3.1415926;   // The number pi
const float Re = 6371.004;    // diameter of the Earth (m)

#endif

