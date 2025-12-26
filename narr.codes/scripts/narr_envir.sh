
echo "narr_envir.sh: NARR=${NARR:?"NARR is not set; fatal error"}"
echo "narr_envir.sh: STREAM=${STREAM:?"STREAM is not set; fatal error"}"

# assimilation of radiances?
export DORAD=YES

# assimilation of precipitation?
export DOPCP=YES

export DOOBS=YES

#
#
#

export TMPNARR=${NARR}/output/${STREAM}
export INPUTNARR=${NARR}/input
#readonly TMPNARR INPUTNARR

#
# NARR
#
export EXEC=${NARR}/exec32
#readonly EXEC

export FIX=${NARR}/fix
export FIX32=${NARR}/fix32
export UCLrad=${NARR}/r3dvar_rad
export UCLfits=${NARR}/ucl_prep
export UCL=${NARR}/ucl
#readonly FIX FIX32 UCLrad UCLfits UCL


#
# INPUTNARR
#

export DIR1B=${INPUTNARR}/tovs_1b_data
export SNOW32=${INPUTNARR}/snow32
export ICE32=${INPUTNARR}/ice32
export SST32=${INPUTNARR}/sst32
export ICE=${INPUTNARR}/snowicesst
export PCPDATA8=${INPUTNARR}/pcpdata_prod
export REINIT=${INPUTNARR}/reinit
export HOLDBUFR=${INPUTNARR}/prepbufr_r2
export HOLDIN=${INPUTNARR}
#readonly DIR1B SNOW32 ICE32 SST32 ICE PCPDATA8 REINIT HOLDBUFR HOLDIN

export HOLD32=${TMPNARR}/hold32
#readonly HOLD32

# UTIL directory with ndate executable
export UTIL=/nwprod/util/exec
#readonly UTIL

#
#export MP_LABELIO=YES
#export MP_STDOUTMODE=ORDERED
