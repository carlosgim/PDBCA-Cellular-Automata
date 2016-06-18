# bar library
SET(BAR_src ${SRCBAR}/mod_functions.f90
             ${SRCBAR}/mod_constants.f90
             ${SRCBAR}/mod_readinp.f90
)
ADD_LIBRARY(${BARLIB} STATIC ${BAR_src})
