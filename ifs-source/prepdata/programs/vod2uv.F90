PROGRAM VOD2UV

!# Transform from VO/D in Spectral Space to #!
!# u/v in Gridpoint Space using TRANS package #!
!# 1 perturbation at a time #!
!# Massimo Bonavita, 2011 #!
!# Adapted for GRIB2: Tomas Wilhelmsson, Aug 2011 #!

USE PARKIND1  ,ONLY : JPIM     ,JPRB, JPRD

USE GRIB_API

USE MPL_MPIF, ONLY : MPI_COMM_WORLD
USE MPL_MODULE

IMPLICIT NONE

! Should be defined in GRIB_API module
INTEGER, PARAMETER :: GRIB_SECTION_PRODUCT =  1
INTEGER, PARAMETER :: GRIB_SECTION_GRID    =  2
INTEGER, PARAMETER :: GRIB_SECTION_LOCAL   =  4
INTEGER, PARAMETER :: GRIB_SECTION_DATA    =  8
INTEGER, PARAMETER :: GRIB_SECTION_BITMAP  = 16

CHARACTER OPTIONS*28
DATA OPTIONS/'q:s:S:G:t:N:p:P:h;l;f;V;B;'/
CHARACTER*1 CEDITION,CTYPEG,OPTLET
CHARACTER*127 ARG, COUTSF,COUTGPF,COUTGPF_U,COUTGPF_V,CINSF, CFIELD
CHARACTER*127 CRTABLE,CPROC,CNDGL,CGRIDTYPE,CSAMPLESFILE, CQ

INTEGER(KIND=JPIM),PARAMETER :: JPMLAT=2049
INTEGER(KIND=JPIM) :: NRGRI(JPMLAT)
INTEGER(KIND=JPIM) :: ISTACK,OPTVAL,getstackusage
INTEGER(KIND=JPIM) :: IGRIBIN,IGRIB,ISAMPLE,IGRIBOUT

INTEGER(KIND=JPIM) :: NERR,NOUT,NPROC,NLIN,INSF,NSMAX,NDGL,NDLON,NPROMA,NQ
INTEGER(KIND=JPIM) :: ITABLE,NSPEC2,NSPEC2G,NGPTOT,NGPTOTG,IMAXFLD,IFLD,ICODE,IOUTSF
INTEGER(KIND=JPIM) :: JROC,ITAG,IRET,NTYPE,IOUTGPF_u,IOUTGPF_v,JFLD
INTEGER(KIND=JPIM) :: GETOPT,MYPROC,JJ,IEDITION,IPROC,IPROC_OLD,ICOMM,ICOMM_OLD
REAL(KIND=JPRB)    :: ZT,TIMEF,RPI
CHARACTER*5,        ALLOCATABLE :: CNAME(:)   ! Parameter short name !
INTEGER(KIND=JPIM), ALLOCATABLE :: NLOEN(:),ITO(:),NPRCIDS(:),IPARAM(:),ILEVEL(:)
REAL(KIND=JPRB),    ALLOCATABLE :: ZSPECG_vo(:,:),ZSPEC_vo(:,:),ZSPECG_d(:,:),ZSPEC_d(:,:)
REAL(KIND=JPRB),    ALLOCATABLE :: ZFPDAT(:), ZG(:,:,:),ZGG(:,:)
REAL(KIND=JPRD),    ALLOCATABLE :: RMU(:)
LOGICAL :: LDONE,LSCAL,LVAR,LLFLT
NAMELIST/NAMRGRI/ NRGRI

!     ------------------------------------------------------------------

#include "setup_trans0.h"
#include "setup_trans.h"
#include "inv_trans.h"
#include "dist_spec.h"
#include "gath_grid.h"
#include "trans_inq.h"

! Initializations
NERR = 0
NOUT = 6
IMAXFLD = 600

! Set defaults for options

CINSF   = ' '
COUTSF  = ' '
COUTGPF = ' '
CFIELD  = ' '
CTYPEG  = 'r'
IPROC   = 1
LLFLT   = .false.
LSCAL   = .false.
LVAR    = .false.
NLIN    = 0
NDGL    = 0
CRTABLE = '/home/rdx/data/ifs'
NQ=0

! Crack options

DO
  OPTVAL = GETOPT(OPTIONS,ARG)
  IF(OPTVAL <= 0) EXIT
  OPTLET=CHAR(OPTVAL)
  IF(OPTVAL <= 0) EXIT
  IF(OPTLET /= 's'.AND.&
   &OPTLET  /= 'S'.AND.&
   &OPTLET  /= 't'.AND.&
   &OPTLET  /= 'q'.AND.&
   &OPTLET  /= 'l'.AND.&
   &OPTLET  /= 'f'.AND.&
   &OPTLET  /= 'p'.AND.&
   &OPTLET  /= 'P'.AND.&
   &OPTLET  /= 'h'.AND.&
   &OPTLET  /= 'N'.AND.&
   &OPTLET  /= 'V'.AND.&
   &OPTLET  /= 'B'.AND.&
   &OPTLET  /= 'G')  THEN
    CALL USE_VOD2UV
    CALL ABOR1('VOD2UV:ILLEGAL COMMAND OPTION')
  ENDIF
  IF(OPTLET == 'h') THEN
    CALL USE_VOD2UV
    STOP
  ELSEIF(OPTLET == 'l') THEN
    NLIN = 1
    WRITE(NOUT,*) 'GRID changed to linear grid'
  ELSEIF(OPTLET == 'q') THEN
    CQ = ARG
    READ(CQ,*) NQ
    WRITE(NOUT,*) 'GRID changed to cubic grid'
  ELSEIF(OPTLET == 'B') THEN
    LLFLT=.true.
    WRITE(NOUT,*)'FAST Legendre transform on'
  ELSEIF(OPTLET == 'V') THEN
    LVAR = .TRUE.
  ELSEIF(OPTLET == 's')  THEN
    CINSF = ARG
  ELSEIF(OPTLET == 'S') THEN
    COUTSF = ARG
  ELSEIF(OPTLET == 'G') THEN
    COUTGPF = ARG
  ELSEIF(OPTLET == 't') THEN
    CTYPEG = ARG
  ELSEIF(OPTLET == 'f') THEN
    CFIELD = ARG
  ELSEIF(OPTLET == 'p') THEN
    CRTABLE = ARG
  ELSEIF(OPTLET == 'P') THEN
    CPROC = ARG
    READ(CPROC,*) IPROC
  ELSEIF(OPTLET == 'N') THEN
    CNDGL = ARG
    READ(CNDGL,*) NDGL        !! READ IN N=128 => NDGL=256 !!
    NDGL=NDGL*2
  ENDIF
ENDDO

IF (IPROC == 1) LSCAL=.true.  !-- serial binary ---

! Message passing setup
! Participating processors limited by -P option

!--------------------------
 ZT=TIMEF()
 CALL MPL_INIT()

 NPROC=MPL_NPROC()
 MYPROC = MPL_MYRANK()
 IPROC_OLD=NPROC
 IF(IPROC_OLD > IPROC) THEN
   CALL MPL_LOCOMM_CREATE(IPROC,ICOMM)
   IF(MYPROC > IPROC) THEN
     CALL MPL_BARRIER()
     CALL MPL_END()
     STOP
   ENDIF
   CALL MPL_SETDFLT_COMM(ICOMM,ICOMM_OLD)
   NPROC=IPROC
 ENDIF
!-------------------------

ALLOCATE(NPRCIDS(NPROC))
DO JJ=1,NPROC
  NPRCIDS(JJ) = JJ
ENDDO
ITAG = 191919

IF(MYPROC == 1) THEN
  IF(CINSF == ' ') THEN
    PRINT *,' INPUT FILE NAME HAS NOT BEEN SUPPLIED'
    PRINT *,' FILE fort.11 WILL BE USED AS INPUT'
    CINSF = 'fort.11'
  ENDIF
  IF(COUTSF == ' ') THEN
    PRINT *,' OUTPUT SPECTRAL FILE NAME HAS NOT BEEN SUPPLIED'
    PRINT *,' FILE fort.10 WILL BE USED FOR SPECTRAL OUTPUT'
    COUTSF = 'fort.10'
  ENDIF
  IF(COUTGPF == ' ') THEN
    PRINT *,' OUTPUT GRID-POINT FILE NAME HAS NOT BEEN SUPPLIED'
    PRINT *,' FILE fort.12 WILL BE USED FOR GRID-POINT OUTPUT'
    COUTGPF = 'fort.12'
  ENDIF
ENDIF

IF(CTYPEG == 'r') THEN
  NTYPE = 1
ELSEIF(CTYPEG == 'f') THEN
  NTYPE = 0
ELSE
  WRITE(NERR,*) ' WRONG TYPE OF GRID: ',CTYPEG,'  It should be',' either f or r'
  CALL ABOR1('VOD2UV: WRONG TYPE OF GRID')
ENDIF
ICODE = 0
IF(CFIELD /= ' ') THEN
  READ(CFIELD,'(I4)') ICODE
ENDIF

! Find spectral resolution

CALL grib_open_file(INSF,CINSF,'R',IRET)
print*,'OPEN file in:', CINSF, IRET
IF(IRET /= 0) THEN
  WRITE(NERR,*) ' ERROR OPENING FILE INPUT SPECTRAL FILE',CINSF,IRET
  CALL ABOR1('VOD2UV: ERROR OPENING FILE INPUT SPECTRAL FILE')
ENDIF
CALL grib_new_from_file(INSF,IGRIBIN,IRET)
IF(IRET /= GRIB_SUCCESS) THEN
  WRITE(NERR,*)' ERROR IN FILE ',CINSF,' : GRIB_NEW_FROM_FILE RETURNS ',IRET
  CALL ABOR1('VOD2UV: GRIB_NEW_FROM_FILE ERROR')
ENDIF
CALL grib_close_file(INSF,IRET)

! GET INPUT GRIB INFO !
CALL grib_get(IGRIBIN,'edition',IEDITION)
CALL grib_get(IGRIBIN,'gridType',CGRIDTYPE)

IF(CGRIDTYPE /= 'sh') THEN
  WRITE(NERR,*)' INPUT DATA NOT IN SPECTRAL FORM'
  CALL ABOR1('VOD2UV:INPUT DATA NOT IN SPECTRAL FORM')
ENDIF
CALL grib_get(IGRIBIN,'pentagonalResolutionParameterJ',NSMAX)
!IF(NSMAX > 639) IMAXFLD=5
!IF(NSMAX > 800) IMAXFLD=1
print*,'Spectral Truncation: ',NSMAX


!Decide gridpoint resolution

IF(NDGL .eq. 0) THEN
  CALL SETNDGL
ELSE
 CALL CHECK_NDGL
ENDIF

print*,'Number of Meridional points: ',NDGL
IF(NDGL > JPMLAT) THEN
  WRITE(NERR,*) 'NDGL =',NDGL,' JPMLAT=',JPMLAT
  CALL ABOR1('VOD2UV: NUMBER OF GAUSSIAN LATITUDES HIGHER THAN MAX')
ENDIF

ALLOCATE(NLOEN(NDGL))

IF(NTYPE == 0) THEN
  NLOEN(:) = 2*NDGL
ELSEIF(NQ == 1.AND.NTYPE == 1) THEN
  ! cubic grid
  ITABLE = INDEX(CRTABLE,' ')
  IF(NSMAX < 1000) THEN
    WRITE(CRTABLE(ITABLE:ITABLE+11),'(A,I3.3)') '/rtable_3',NSMAX
  ELSE
    WRITE(CRTABLE(ITABLE:ITABLE+12),'(A,I4.4)') '/rtable_3',NSMAX
  ENDIF
ELSEIF(NQ == 2.AND.NTYPE == 1) THEN
  ! cubic grid + Collignon
  ITABLE = INDEX(CRTABLE,' ')
  IF(NSMAX < 1000) THEN
    WRITE(CRTABLE(ITABLE:ITABLE+11),'(A,I3.3)') '/rtable_4',NSMAX
  ELSE
    WRITE(CRTABLE(ITABLE:ITABLE+12),'(A,I4.4)') '/rtable_4',NSMAX
  ENDIF
ELSEIF(NLIN == 0.AND.NTYPE == 1) THEN
  ITABLE = INDEX(CRTABLE,' ')
  IF(NSMAX < 1000) THEN
    WRITE(CRTABLE(ITABLE:ITABLE+11),'(A,I3.3)') '/rtable_2',NSMAX
  ELSE
    WRITE(CRTABLE(ITABLE:ITABLE+12),'(A,I4.4)') '/rtable_2',NSMAX
  ENDIF
ELSEIF(NLIN == 1.AND.NTYPE == 1) THEN
  ITABLE = INDEX(CRTABLE,' ')
  IF(NSMAX < 1000) THEN
    WRITE(CRTABLE(ITABLE:ITABLE+12),'(A,I3.3)') '/rtablel_2',NSMAX
  ELSE
    WRITE(CRTABLE(ITABLE:ITABLE+13),'(A,I4.4)') '/rtablel_2',NSMAX
  ENDIF
ENDIF
IF(NTYPE == 1) THEN
  IF(MYPROC == 1) THEN
    PRINT *,' TABLE USED FOR REDUCED GRID:'
    PRINT *,CRTABLE
  ENDIF
  OPEN(15,FILE=CRTABLE,FORM='FORMATTED',ACTION='READ')
  READ(15,NAMRGRI)
  NLOEN(:) = NRGRI(1:NDGL)
  CLOSE(15)
ENDIF


! Prepare for transforms  !

ALLOCATE(RMU(NDGL))
CALL SETUP_TRANS0(KOUT=NOUT,KERR=NERR,KPRINTLEV=0,KMAX_RESOL=1,&
&                 KPRGPNS=NPROC,KPRGPEW=1,KPRTRW=NPROC,LDMPOFF=LSCAL)
!
CALL SETUP_TRANS(KSMAX=NSMAX,KDGL=NDGL,KLOEN=NLOEN,LDSPLIT=.FALSE.,&
&                LDUSEFFTW=.TRUE.,&
&                LDUSEFLT=LLFLT,LDUSERPNM=.TRUE.,LDKEEPRPNM=.FALSE.)
!
CALL TRANS_INQ(KSPEC2=NSPEC2,KSPEC2G=NSPEC2G,KGPTOT=NGPTOT,KGPTOTG=NGPTOTG)
CALL TRANS_INQ(PMU=RMU)

ALLOCATE(ZSPECG_vo(IMAXFLD,NSPEC2G))
ALLOCATE(ZSPECG_d(IMAXFLD,NSPEC2G))
ALLOCATE(ZSPEC_vo(IMAXFLD,NSPEC2))
ALLOCATE(ZSPEC_d(IMAXFLD,NSPEC2))
ALLOCATE(ZFPDAT(NSPEC2G))
ALLOCATE(ZG(1,IMAXFLD,NGPTOT))
ALLOCATE(ZGG(NGPTOTG,IMAXFLD))
ALLOCATE(ITO(IMAXFLD))

ALLOCATE(IPARAM(IMAXFLD))
ALLOCATE(ILEVEL(IMAXFLD))
ALLOCATE(CNAME(IMAXFLD))

! Open files

IF(MYPROC == 1) THEN
  CALL grib_open_file(INSF,CINSF,'R',IRET)
  print*,'OPEN file in:', CINSF, IRET
  IF(IRET /= 0) THEN
    WRITE(NERR,*) ' ERROR OPENING FILE INPUT SPECTRAL FILE',CINSF,IRET
    CALL ABOR1(' VOD2UV:ERROR OPENING FILE INPUT SPECTRAL FILE')
  ENDIF
  COUTGPF_U=TRIM(ADJUSTL(COUTGPF))//'_U'
  COUTGPF_V=TRIM(ADJUSTL(COUTGPF))//'_V'
  CALL grib_open_file(IOUTGPF_U,COUTGPF_U,'W',IRET)
  CALL grib_open_file(IOUTGPF_V,COUTGPF_V,'W',IRET)
  IF(ICODE /= 0) THEN
    CALL grib_open_file(IOUTSF,COUTSF,'W',IRET)
    IF(IRET /= 0) THEN
      WRITE(NERR,*) ' ERROR OPENING OUTPUT SPECTRAL FILE',COUTSF,IRET
      CALL ABOR1(' VOD2UV:ERROR OPENING OUTPUT SPECTRAL FILE')
    ENDIF
  ENDIF
ENDIF


! Spectral to gridpoint transform

ITO(:) = 1
IFLD = 0
LDONE = .FALSE.

! Inititialize GRIB_API handles to zero
IGRIB = 0
ISAMPLE = 0
IGRIBOUT = 0

DO
  IF(MYPROC == 1) THEN
    ! Read and decode spectral field
    CALL grib_new_from_file(INSF,IGRIB,IRET)
    IF(IRET == GRIB_END_OF_FILE) LDONE = .TRUE.
    IF(.NOT. LDONE) THEN
       CALL grib_get(IGRIB,'shortName',CNAME(IFLD+1))
       CALL grib_get(IGRIB,'paramId',IPARAM(IFLD+1))
       CALL grib_get(IGRIB,'level',ILEVEL(IFLD+1))
       CALL grib_get(IGRIB,'values',ZFPDAT(:))

      ! Write out "skipped field" to output spectral file
      IF(IPARAM(IFLD+1) /= ICODE.AND.ICODE /= 0) THEN
        PRINT *,' FIELD ',IPARAM(IFLD+1),' NOT TRANSFORMED'
        CALL grib_set(IGRIB,'values',ZFPDAT(:))
        CALL grib_write(IGRIB,IOUTSF)
        CALL grib_release(IGRIB)
        CYCLE
      ENDIF
      CALL grib_release(IGRIB)
      IFLD = IFLD+1
      IF (TRIM(ADJUSTL(CNAME(IFLD))) .eq. 'vo') THEN
        ZSPECG_VO(ILEVEL(IFLD),:)=ZFPDAT(:)
      ELSEIF (TRIM(ADJUSTL(CNAME(IFLD))) .eq. 'd') THEN
        ZSPECG_D(ILEVEL(IFLD),:)=ZFPDAT(:)
      ENDIF
    ENDIF
    IF(LDONE .OR. IFLD == IMAXFLD) THEN

      ! Send number of fields in this batch to other processors
      IF(NPROC > 1) THEN
        DO JROC=2,NPROC
          CALL MPL_SEND(IFLD,KDEST=NPRCIDS(JROC),KTAG=ITAG)
        ENDDO
      ENDIF
    ELSE
      CYCLE
    ENDIF
  ELSE
    ! Receive number of fields in this batch
    CALL MPL_RECV(IFLD,KSOURCE=NPRCIDS(1),KTAG=ITAG)
  ENDIF

  IFLD = IFLD / 2

  IF(NPROC > 1) THEN
    CALL MPL_BROADCAST(IPARAM(1:IFLD),KROOT=1,KTAG=ITAG, &
             &   CDSTRING='VOD2UV:')
  ENDIF
  IF(IFLD == 0 ) EXIT
  ! Syncronize processors
  IF(NPROC > 1) THEN
    CALL MPL_BARRIER()
  ENDIF
  ! Distribute spectral fields to processors
  CALL DIST_SPEC(PSPECG=ZSPECG_VO,KFDISTG=IFLD,KFROM=ITO,PSPEC=ZSPEC_VO)
  CALL DIST_SPEC(PSPECG=ZSPECG_D,KFDISTG=IFLD,KFROM=ITO,PSPEC=ZSPEC_D)
  ! Spectral transform
   nproma=1
  CALL INV_TRANS(PSPVOR=ZSPEC_VO(1:IFLD,:),PSPDIV=ZSPEC_D(1:IFLD,:),PGP=ZG,KPROMA=nproma)

  ! Limit fields
  IF(LVAR) THEN
    print*,'APPLYING MAX-MIN LIMIT VALUES'
    CALL MAXMINL
  ENDIF
  ! Gather gridpoint fields to processor 1
   CALL GATH_GRID(PGPG=ZGG,KPROMA=nproma,KFGATHG=IFLD*2,KTO=ITO,PGP=ZG)
  IF(MYPROC == 1) THEN
    ! Code and write out gridpoint fields
    WRITE(CEDITION,'(I1)') IEDITION
    IF (NTYPE == 0) THEN
      CSAMPLESFILE = 'regular_gg_ml_grib'//CEDITION
    ELSE
      CSAMPLESFILE = 'reduced_gg_ml_grib'//CEDITION
    ENDIF
    CALL grib_new_from_samples(ISAMPLE,CSAMPLESFILE)

    NDLON = 2*NDGL
    RPI = 2.0_JPRB*ASIN(1.0_JPRB)

    IF(NTYPE == 0) THEN
      CALL grib_set(ISAMPLE,'Ni',NDLON)
      CALL grib_set(ISAMPLE,'iDirectionIncrementInDegrees', &
                  & 360._JPRB/(NDLON+1))
    ELSE
      CALL grib_set(ISAMPLE,'pl',NLOEN)
    ENDIF
    CALL grib_set(ISAMPLE,'Nj',NDGL)
    CALL grib_set(ISAMPLE,'N',NDGL/2)
    CALL grib_set(ISAMPLE,'latitudeOfFirstGridPointInDegrees', &
                & 180._JPRB/RPI*ASIN(RMU(1)))
    CALL grib_set(ISAMPLE,'latitudeOfLastGridPointInDegrees', &
                & 180._JPRB/RPI*ASIN(RMU(NDGL)))
    CALL grib_set(ISAMPLE,'longitudeOfLastGridPointInDegrees', &
                & (360._JPRB*NDLON)/(NDLON+1))
    CALL grib_set(ISAMPLE,'longitudeOfFirstGridPoint',0)
    CALL grib_set(ISAMPLE,'resolutionAndComponentFlags',0)
    CALL grib_set(ISAMPLE,'bitsPerValue',24)

    DO JFLD=1,IFLD
      CALL grib_util_sections_copy(IGRIBIN,ISAMPLE, &
            & GRIB_SECTION_PRODUCT+GRIB_SECTION_LOCAL,IGRIBOUT)
      CALL grib_set(IGRIBOUT,'level',ILEVEL(JFLD))
      CALL grib_set(IGRIBOUT,'shortName','u') 
      CALL grib_set(IGRIBOUT,'values',ZGG(1:NGPTOTG,JFLD))
      CALL grib_write(IGRIBOUT,IOUTGPF_u)
      CALL grib_set(IGRIBOUT,'shortName','v') 
      CALL grib_set(IGRIBOUT,'values',ZGG(1:NGPTOTG,IFLD+JFLD))
      CALL grib_write(IGRIBOUT,IOUTGPF_v)
      CALL grib_release(IGRIBOUT)
    ENDDO
    CALL grib_release(ISAMPLE)
    CALL grib_release(IGRIBIN)
  ENDIF
  ! Syncronize processors
  IF(NPROC > 1) THEN
    CALL MPL_BARRIER()
  ENDIF
  IFLD = 0
ENDDO

!           gather stack usage statistics
ISTACK = GETSTACKUSAGE()
IF(MYPROC == 1) THEN
  PRINT 9000, istack
  9000 FORMAT("Stack Utilisation Information",/,&
       &"=============================",//,&
       &"Node           Size(Bytes)",/,&
       &"====           ===========",//,&
       &"   1",11x,I10)

  DO JROC=2,NPROC
    CALL MPL_RECV(ISTACK,KSOURCE=NPRCIDS(JROC),KTAG=JROC, &
         & CDSTRING='VOD2UV:')
    PRINT '(I4,11X,I10)', JROC,ISTACK
  ENDDO
ELSE
  CALL MPL_SEND(ISTACK,KDEST=NPRCIDS(1),KTAG=MYPROC, &
             &   CDSTRING='VOD2UV:')
ENDIF

!Close down message passing

!--------------------------
 IF(IPROC_OLD > IPROC) THEN
     MPL_COMM_OML(1)=MPI_COMM_WORLD
     MPL_NUMPROC=IPROC_OLD
     CALL MPL_SETDFLT_COMM(ICOMM_OLD,ICOMM)
 ENDIF

!write(0,*) "Calling barrier"
 CALL MPL_BARRIER()
 CALL MPL_END()
 ZT=(TIMEF()-ZT)/1000.0_JPRB
 IF(MYPROC == 1) write(0,'(a,I3,a,I4,a,F9.2,a)') &
    & "VOD2UV, on",NPROC," out of",IPROC_OLD," procs, took",ZT," sec"
!--------------------------

STOP


CONTAINS  

!     ------------------------------------------------------------------

SUBROUTINE USE_VOD2UV

!   WRITE MESSAGE TO INFORM ABOUT CORRECT USE OF PROGRAM

WRITE(NERR,*) ' CORRECT OPTIONS AND ARGUMENTS FOR VOD2UV ARE :'
WRITE(NERR,*) '   -s      input filename (spectral) (def: fort.11)'
WRITE(NERR,*) '   -S      output spectral  filename (def: fort.10)'
WRITE(NERR,*) '   -G      output grid-point filename (def: fort.12)'
WRITE(NERR,*) '   -t         type of grid (r: reduced or f: full)',&
                     &' default reduced' 
WRITE(NERR,*) '   -l      if linear grid (default quadratic)'
WRITE(NERR,*) '   -V      for MINVALUE=0. enforcedi (i.e., for Variance/StDev fields )'
WRITE(NERR,*) '   -N      Gaussian resolution (num. of lat. circles betw. Pole and Eq.'
WRITE(NERR,*) '   -p      path for resolution tables ',&
                       &'default: /home/rdx/data/ifs'
WRITE(NERR,*) '   -h      displays options and arguments'
WRITE(NERR,*) '   -P      number of processors to use (mandatory for parallel)'
WRITE(NERR,*) '   -f      field code to convert to GP (def: 0)'
WRITE(NERR,*) '           if -f 0, all fields will be converted'

END SUBROUTINE USE_VOD2UV

!     ------------------------------------------------------------------

SUBROUTINE SETNDGL

! Decide number of Gaussian latitudes given spectral truncation
! Only certain combinations of truncation/linear grid
! or quadratic grid are supported
 
IF(NLIN == 0.AND. (NQ == 1.OR.NQ == 2)) THEN
  IF(NSMAX == 79) THEN
    NDGL = 160
  ELSEIF(NSMAX == 95) THEN
    NDGL = 192
  ELSEIF(NSMAX == 127) THEN
    NDGL = 256
  ELSEIF(NSMAX == 159) THEN
    NDGL = 320
  ELSEIF(NSMAX == 199) THEN
    NDGL = 400
  ELSEIF(NSMAX == 255) THEN
    NDGL = 512
  ELSEIF(NSMAX == 319) THEN
    NDGL = 640
  ELSEIF(NSMAX == 399) THEN
    NDGL = 800
  ELSEIF(NSMAX == 511) THEN
    NDGL = 1024
  ELSEIF(NSMAX == 639) THEN
    NDGL = 1280
  ELSEIF(NSMAX == 799) THEN
    NDGL = 1600
  ELSEIF(NSMAX == 1023) THEN
    NDGL = 2048
  ELSEIF(NSMAX == 1279) THEN
    NDGL = 2560
  ELSEIF(NSMAX == 1599) THEN
    NDGL = 3200
  ELSEIF(NSMAX == 1999) THEN
    NDGL = 4000
  ELSEIF(NSMAX == 3999) THEN
    NDGL = 8000
  ELSEIF(NSMAX == 7999) THEN
    NDGL = 16000
  ELSE
    WRITE(NERR,*)' WRONG SPECTRAL RESOLUTION ',NSMAX,' CUBIC GRID'
    CALL ABOR1('SPTOGP:UNSUPPORTED SPECTRAL RESOLUTION - CUBIC GRID ')
  ENDIF
ELSEIF (NLIN == 0) THEN
  IF(NSMAX == 21) THEN
    NDGL = 32
  ELSEIF(NSMAX == 42) THEN
    NDGL = 64
  ELSEIF(NSMAX == 63) THEN
    NDGL = 96
  ELSEIF(NSMAX == 106) THEN
    NDGL = 160
  ELSEIF(NSMAX == 213) THEN
    NDGL = 320
  ELSEIF(NSMAX == 341) THEN
    NDGL = 512
  ELSEIF(NSMAX == 426) THEN
    NDGL = 640
  ELSEIF(NSMAX == 533) THEN
    NDGL = 800
  ELSEIF(NSMAX == 682) THEN
    NDGL = 1024
  ELSE
    WRITE(NERR,*)' WRONG SPECTRAL RESOLUTION ',NSMAX,' QUAD. GRID'
    CALL ABOR1('VOD2UV:UNSUPPORTED SPECTRAL RESOLUTION - QUAD. GRID ')
  ENDIF
ELSEIF(NLIN == 1) THEN
  IF(NSMAX == 63) THEN
    NDGL = 64
  ELSEIF(NSMAX == 95) THEN
    NDGL = 96
  ELSEIF(NSMAX == 127) THEN
    NDGL = 128
  ELSEIF(NSMAX == 159) THEN
    NDGL = 160
  ELSEIF(NSMAX == 191) THEN
    NDGL = 192
  ELSEIF(NSMAX == 199) THEN
    NDGL = 200
  ELSEIF(NSMAX == 255) THEN
    NDGL = 256
  ELSEIF(NSMAX == 319) THEN
    NDGL = 320
  ELSEIF(NSMAX == 399) THEN
    NDGL = 400
  ELSEIF(NSMAX == 511) THEN
    NDGL = 512
  ELSEIF(NSMAX == 639) THEN
    NDGL = 640
  ELSEIF(NSMAX == 799) THEN
    NDGL = 800
  ELSEIF(NSMAX == 1023) THEN
    NDGL = 1024
  ELSEIF(NSMAX == 1279) THEN
    NDGL = 1280
  ELSEIF(NSMAX == 1599) THEN
    NDGL = 1600
  ELSEIF(NSMAX == 2047) THEN
    NDGL = 2048
  ELSE
    WRITE(NERR,*)' WRONG SPECTRAL RESOLUTION ',NSMAX,' LIN. GRID'
    CALL ABOR1('VOD2UV:UNSUPPORTED SPECTRAL RESOLUTION - LIN. GRID')
  ENDIF
ELSE
  WRITE(NERR,*)' WRONG NLIN=',NLIN
  CALL ABOR1('VOD2UV:WRONG NLIN')
ENDIF

END SUBROUTINE SETNDGL

!     ------------------------------------------------------------------

SUBROUTINE CHECK_NDGL

! Decide number of Gaussian latitudes given spectral truncation
! Only certain combinations of truncation/linear grid
! or quadratic grid are supported
 
IF(NLIN == 0) THEN
  IF(NDGL .ne. 32 .and. NDGL .ne. 64 .and. NDGL .ne. 96 .and. &
     NDGL .ne. 160 .and. NDGL .ne. 320 .and. NDGL .ne. 512 .and. &
     NDGL .ne. 640 .and. NDGL .ne. 800 .and. NDGL .ne. 1024) THEN
    WRITE(NERR,*)' WRONG SPECTRAL RESOLUTION ',NSMAX,' QUAD. GRID'
    CALL ABOR1('VOD2UV:UNSUPPORTED SPECTRAL RESOLUTION - QUAD. GRID ')
  ENDIF
ELSEIF(NLIN == 1) THEN
  IF(NDGL .ne. 32 .and. NDGL .ne. 64 .and. NDGL .ne. 96 .and. NDGL .ne. 128 .and. NDGL .ne. 160 .and. &
     NDGL .ne. 256 .and. NDGL .ne. 320 .and. NDGL .ne. 400 .and. &
     NDGL .ne. 512 .and. NDGL .ne. 640 .and. NDGL .ne. 800 .and. NDGL .ne. 1024 .and. &
     NDGL .ne. 1280 .and. NDGL .ne. 1600 .and. NDGL .ne. 2048) THEN
    WRITE(NERR,*)' WRONG SPECTRAL RESOLUTION ',NSMAX,' LIN. GRID'
    CALL ABOR1('VOD2UV:UNSUPPORTED SPECTRAL RESOLUTION - LIN. GRID')
  ENDIF
ELSE
  WRITE(NERR,*)' WRONG NLIN=',NLIN
  CALL ABOR1('VOD2UV:WRONG NLIN')
ENDIF

END SUBROUTINE CHECK_NDGL

!     ------------------------------------------------------------------

SUBROUTINE MAXMINL

! Set MAX and MIN values for field depending on GRIB code

REAL(KIND=JPRB) :: ZMAX,ZMIN
INTEGER(KIND=JPIM) :: JFLD,JROF

DO JFLD=1,IFLD
  IF(IPARAM(JFLD) == 129) THEN
    ZMAX=60000._JPRB
    ZMIN=-6500._JPRB
  ELSEIF((IPARAM(JFLD) == 139.OR.IPARAM(JFLD) == 170 &
   &.OR.IPARAM(JFLD) == 183.OR.IPARAM(JFLD) == 235 &
   &.OR.IPARAM(JFLD) == 236)) THEN
    ZMAX=350._JPRB
    ZMIN=210._JPRB
  ELSEIF((IPARAM(JFLD) == 140.OR.IPARAM(JFLD) == 171 &
   &.OR.IPARAM(JFLD) == 184.OR.IPARAM(JFLD) == 237)) THEN
    ZMAX=0.033_JPRB
    ZMIN=2.E-6_JPRB
  ELSEIF(IPARAM(JFLD) == 246.OR.IPARAM(JFLD) == 247) THEN
    ZMAX=1.E-3_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 248) THEN
    ZMAX=1.0_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 141) THEN
    ZMAX=10._JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 133) THEN
    ZMAX=1.E-1_JPRB
    ZMIN=2.E-8_JPRB
  ELSEIF(IPARAM(JFLD) == 160) THEN
    ZMAX=1900._JPRB
    ZMIN=0.0_JPRB
  ELSEIF((IPARAM(JFLD) == 161.OR.IPARAM(JFLD) == 172 &
   &.OR.IPARAM(JFLD) == 199)) THEN
    ZMAX=1.0_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 162) THEN
    ZMAX=1.57_JPRB
    ZMIN=-1.57_JPRB
  ELSEIF(IPARAM(JFLD) == 163) THEN
    ZMAX=0.11_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 173) THEN
    ZMAX=100._JPRB
    ZMIN=0.001_JPRB
  ELSEIF(IPARAM(JFLD) == 174) THEN
    ZMAX=0.6_JPRB
    ZMIN=0.07_JPRB
  ELSEIF(IPARAM(JFLD) == 198) THEN
    ZMAX=8.E-4_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 200) THEN
    ZMAX=4.E6_JPRB
    ZMIN=0.0_JPRB
  ELSEIF(IPARAM(JFLD) == 233) THEN
    ZMAX=0.033_JPRB
    ZMIN=8.8E-6_JPRB
  ELSEIF(IPARAM(JFLD) == 234) THEN
    ZMAX=-1.38_JPRB
    ZMIN=-20._JPRB
  ELSEIF((IPARAM(JFLD) == 238 .OR. IPARAM(JFLD) ==  35 .OR. &
   &      IPARAM(JFLD) ==  36 .OR. IPARAM(JFLD) ==  37 .OR. &
   &      IPARAM(JFLD) ==  38)) THEN
    ZMAX=273.16_JPRB
    ZMIN=210._JPRB
  ELSEIF((IPARAM(JFLD) ==  27 .OR. IPARAM(JFLD) ==  28 .OR. &
   &      IPARAM(JFLD) ==  31 .OR. IPARAM(JFLD) ==  32)) THEN
    ZMAX=1.
    ZMIN=0.
  ELSEIF((IPARAM(JFLD) ==  29 .OR. IPARAM(JFLD) ==  30)) THEN
    ZMAX=20.
    ZMIN=1.
  ELSEIF(IPARAM(JFLD) ==  33) THEN
    ZMAX=500.
    ZMIN=50.
  ELSEIF(IPARAM(JFLD) ==  34) THEN
    ZMAX=350.
    ZMIN=210.
  ELSEIF((IPARAM(JFLD) ==  39 .OR. IPARAM(JFLD) ==  40 .OR. &
   &      IPARAM(JFLD) ==  41 .OR. IPARAM(JFLD) ==  42)) THEN
    ZMAX=0.472_JPRB
    ZMIN=0._JPRB
  ELSEIF(IPARAM(JFLD) ==  138) THEN
    ZMAX=1.E15_JPRB
    ZMIN=2.E-13_JPRB
  ELSE
    ZMAX=1.E30_JPRB
    ZMIN=-1.E30_JPRB
  ENDIF

  IF(LVAR .EQV. .TRUE.) THEN
    ZMAX=ZMAX*ZMAX
    ZMIN=ZMIN*ZMIN
    print*,'VOD2UV: APPLIED LVAR MAX-MIN VALUES LIMIT'
  ENDIF

  DO JROF=1,NGPTOT
    ZG(1,JFLD,JROF) = MAX(ZMIN,MIN(ZMAX,ZG(1,JFLD,JROF)))
  ENDDO
ENDDO

END SUBROUTINE MAXMINL

!     ------------------------------------------------------------------

END PROGRAM VOD2UV
