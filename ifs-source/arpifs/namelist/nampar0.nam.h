
!     Initial determination of distributed versus shared memory configuration
!     Contains YOMMP0 and YOMGSTATS variables.
!     -----------------------------------------------------------------------
NAMELIST/NAMPAR0/ NPROC,NOUTPUT,&
                 &NPRGPNS,NPRGPEW,NPRTRW,NPRTRV,&
                 &NSPECRESMIN,&
                 &MP_TYPE,MBX_SIZE,&
                 &LMPOFF,LMPDIAG,&
                 &LOPT_SCALAR,LOPT_RS6K,NPRINTLEV,LSCMEC,&
                 &LSTATS,LSTATSCPU,LSYNCSTATS,LDETAILED_STATS,LXML_STATS,&
                 &LSTATS_MEM,NSTATS_MEM,LSTATS_ALLOC,LBARRIER_STATS,LBARRIER_STATS2,&
                 &NPRNT_STATS,NTRACE_STATS
!     ------------------------------------------------------------------

