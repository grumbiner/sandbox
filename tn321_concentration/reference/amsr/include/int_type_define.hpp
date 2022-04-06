//------------------------------------------------------------------------------
//
// (C) QSS Corporation
//
//
// NAME: int_type_define
//
// PURPOSE:
//    This is a module defining integer byte sizes.  It is to be included in all
//    code.
//
// CATEGORY:
//    Data definition
//
// CALLING SEQUENCE:
//    #include "int_type_define.hpp"
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
//    #include <stdint.h>
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
//       Written by: Thomas King  (11/26/2004)
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
//int_type_define.hpp
//

#if not defined( int_type_define_hpp )
#define int_type_define_hpp
#include <stdint.h>

typedef int8_t SBYTE; 
typedef int16_t SSHORT;
typedef int32_t SLONG;
typedef int64_t SLONGLONG;  

typedef uint8_t UBYTE;
typedef uint16_t USHORT;
typedef uint32_t ULONG;
typedef uint64_t ULONGLONG;

#endif


