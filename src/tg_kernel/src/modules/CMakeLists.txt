# bar library
SET(BAR_src ${SRCBAR}/tg_functions.f90
             ${SRCBAR}/constants.f90
)
ADD_LIBRARY(${BARLIB} STATIC ${BAR_src})
