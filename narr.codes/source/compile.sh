#! /bin/sh
set -aeux

CLEAN=0
MACHINE=snow

cd bctend
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp bctend.x ../../exec32
fi

cd ../editbufr
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp editbufr.x ../../exec32
fi

cd ../etafcst
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp etafcst.x ../../exec32
fi

cd ../etapost
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp etapost.x ../../exec32
fi

cd ../gesprep
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp gesprep.x ../../exec32
fi

cd ../grdeta
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp grdeta.x ../../exec32
fi

cd ../gridtobs
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp gridtobs.x ../../exec32
fi

cd ../mkbnd
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp mkbnd.x ../../exec32
fi

cd ../monitoring
if [ $CLEAN -eq  1 ]; then
##make -f makefile_${MACHINE} clean
echo ""
else
##make -f makefile_${MACHINE}
compile_${MACHINE}.sh
cp convert4.x ../../exec32
cp readvsdb.x ../../exec32
cp readvsdblike.x ../../exec32
fi

cd ../post0
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp post0.x ../../exec32
fi

cd ../prdgen_nn
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp prdgen.x ../../exec32
fi

cd ../prepfits
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp prepfits.x ../../exec32
fi

cd ../r3dvar
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp r3dvar.x ../../exec32
fi

cd ../rrvents
if [ $CLEAN -eq  1 ]; then
##make -f makefile_${MACHINE} clean
echo ""
else
##make -f makefile_${MACHINE}
compile_${MACHINE}.sh
cp rrvents.x ../../exec32
fi

cd ../rstupt
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp rstupt.x ../../exec32
fi

cd ../sndp
if [ $CLEAN -eq  1 ]; then
make -f makefile_${MACHINE} clean
else
make -f makefile_${MACHINE}
cp sndp.x ../../exec32
fi
