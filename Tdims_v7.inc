C MAX_NTDH - maximum number of values for the Time Delay Histogram used by TOPMOD
C END_STEPS_MAX - Maximum number of time steps used in forecasting
C INI_ARRAY_SIZE - The initial size allocation to runoff routing arrays - these
C arrays are self expanding
C NSI - number of initial condition parameters
C NSP - number of basin parameters
C NRP - number of channel parameters
c NIP1 - the size of the IRR array used for topmodel output - DGT.
c dpm - the number of parameters that can be assigned by nlfit during calibration.
c       It should correspond to nlfit parameter nmx
c
c WARNING - make sure nrx in NLFIT.INC is at least the maximum mnumber of time steps
c in a simulation + 200

       INTEGER MAX_NTDH,END_STEPS_MAX
       PARAMETER (MAX_NTDH=450, END_STEPS_MAX=205)
       INTEGER INI_ARRAY_SIZE
       PARAMETER (INI_ARRAY_SIZE=100)
       INTEGER NSI,NSP,NRP,NIP1,dpm
       parameter(nip1 = 15)
       PARAMETER (NSI=3,NSP=39,NRP=4,dpm=17)
       integer nooksack,num_basinpars
       parameter (nooksack=1,num_basinpars=46)
       integer TRACE
       parameter (TRACE=51)
