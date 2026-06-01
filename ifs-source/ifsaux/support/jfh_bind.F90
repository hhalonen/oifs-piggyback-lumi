 SUBROUTINE JFH_BIND()
#ifndef RS6K
 WRITE(0,*) "JFH_BIND compiled without -DRS6K so not binding"
 RETURN
 END SUBROUTINE JFH_BIND
#else
!     PURPOSE.
!     --------
!       Binds task and threads to CPUs for LL submitted jobs 
!       (not for interactive jobs)

!     INTERFACE.
!     ----------
!       CALL JFH_BIND() 

!     METHOD.
!     -------
!       The first call to JFH_BIND routine binds CPUs according to 
!       the env vars 
!         JFH_BIND=map
!         JFH_BMAP="N 0 1 2 ..."  where N is number of entries
!       and prints out the binding
!         e.g. for 4 threads: CPUs = 0 32 1 33

!       Subsequent calls, and calls without env vars set, will not bind, 
!       but will print out the binding

!       JFH_BIND may be called before or after mpi_init

!       Note that IFS may also call EC_BIND, which binds differently to 
!       jfh_bind, but it will not be called (in CY37R3) if:
!         mpi_init is called before mpl_init
!         the job is not run on whole nodes

!       Serial (non MPI) version can be created by compiling with -DSERIAL

!     EXTERNALS.   
!     ----------   
!       SYSTEM             Get host name
!       MPI_INITIALIZED    Check if mpi iniitalised
!       MPI_COMM_RANK      Get rank of mpi tasK
!       GETENV             Get environment variable
!       OMP_GET_THREAD_NUM Get thread num
!       --- following in jfhc.c-----
!       AFF                Get cpu num & CPUs per node
!       SMTCTL             Get CPU ids for SMT id
!       IS_SMT_ON          Check for SMT 
!       JBIND              Bind thread to processor

!     AUTHOR.
!     -------
!        J.Hague,  IBM,  Aug 2011  
!     ------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER OMP_GET_MAX_THREADS
 INTEGER OMP_GET_THREAD_NUM
 INTEGER, ALLOCATABLE :: ITA(:), MAP(:)
 INTEGER, ALLOCATABLE :: JCPU(:), ID0(:), ID1(:)
 INTEGER IP,IT,NT,MP,IPROC,NMAP
 INTEGER ICPU,IER
 INTEGER I,II,MDUM
 INTEGER ISMT, IS_SMT_ON  
 INTEGER IFIRST, IPRT, IRA
 LOGICAL LFLG
 CHARACTER*120 C,C1,C2
 CHARACTER*3 CB, CC
 CHARACTER*1000 CBM
 SAVE        CB,CBM
 DATA IFIRST/0/
#ifndef SERIAL
#include "mpif.h"
#endif

 CALL SYSTEM("hostname")

#ifndef SERIAL
 CALL MPI_INITIALIZED(LFLG,IER)
 IF(LFLG) THEN
   CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IER)
 ELSE
#ifdef RS6K
  CALL GET_ENVIRONMENT_VARIABLE("MP_CHILD",VALUE=C)
#endif
  READ(C,*) IP
 ENDIF
!write(0,*) "LFLG,ip=",LFLG,ip
#else
 IP=0
#endif

 CALL AFF(MDUM,MP)
 ALLOCATE(MAP(MP+1))
 ALLOCATE(JCPU(MP))
 ALLOCATE(ID0(MP/2))
 ALLOCATE(ID1(MP/2))
 NT=OMP_GET_MAX_THREADS()
 ALLOCATE(ITA(NT))

 CALL GET_ENVIRONMENT_VARIABLE("JFH_BIND",CB)
 IF(IFIRST.EQ.0) THEN
   IFIRST=1
   IF(CB.EQ."yes".OR.CB.EQ."YES".OR.CB.EQ."map".OR.CB.EQ."MAP") THEN
     DO I=1,MP
       JCPU(I)=I-1
     ENDDO

!---------Must reset jcpu if SMT ON-----------------------------
     ISMT=IS_SMT_ON()
!    write(0,*) "ismt=",ismt
     IF(ISMT .NE. -99) THEN
       CALL SMTCTL(MP/2, ID0, ID1)
       DO I=1,MP/2
        JCPU(2*I-1)=ID0(I)
        JCPU(2*I  )=ID1(I)
       ENDDO
     ENDIF
!---------------------------------------------------------------
!    write(0,*) "jcpu=",jcpu

     MAP(1)=-1
     NMAP=MP
     IF(CB.EQ."map".OR.CB.EQ."MAP") THEN
        CALL GET_ENVIRONMENT_VARIABLE("JFH_BMAP",CBM)
      READ(CBM,*) NMAP,(MAP(I),I=1,MIN(MP,NMAP))
!       write(6,*) "NMAP,Map=",NMAP,(MAP(I),I=1,NMAP)
     ENDIF

     IRA=0
     CALL GET_ENVIRONMENT_VARIABLE("JFH_RA_DET",CC)
     IF(CC.EQ."yes".OR.CC.EQ."YES") ira=1
!    write(0,*) "CC,ira=",CC,ira
     IPRT=0
     CALL GET_ENVIRONMENT_VARIABLE("JFH_RA_PRT",CC)
     IF(CC.EQ."yes".OR.CC.EQ."YES") iprt=1

!$OMP PARALLEL 
     IF(IRA==1) THEN
!      write(0,*) "Calling ra_det"
       CALL RA_DET(2,IPRT)
     ENDIF
!$OMP END PARALLEL 

!$OMP PARALLEL PRIVATE(IT,IPROC)
     IT=OMP_GET_THREAD_NUM()
     IF(IRA==1) CALL RA_CHECK(2,IPRT)
     IPROC=NT*IP+IT
     IPROC=MOD(IPROC,NMAP)
!    write(0,*) "i,it,IPROC=",i,it,IPROC
!    write(0,*) "it,IPROC,MAP,jcpu,MAP",it,IPROC,MAP(IPROC+1),jcpu(MAP(IPROC+1)+1)
     IF(MAP(1).EQ.-1) CALL JBIND(JCPU(IPROC+1))
     IF(MAP(1).NE.-1) CALL JBIND(JCPU(MAP(IPROC+1)+1))
!$OMP END PARALLEL 

   ENDIF
 ENDIF

!$OMP PARALLEL PRIVATE(it,icpu,mdum)
   IT=OMP_GET_THREAD_NUM()
   CALL AFF(ICPU,MDUM)
   ITA(IT+1)=ICPU
!$OMP END PARALLEL
 WRITE(0,*) "CPUs = ",(ITA(I),I=1,NT)
 DEALLOCATE(ITA,MAP,JCPU,ID0,ID1)
 END SUBROUTINE JFH_BIND
#endif
