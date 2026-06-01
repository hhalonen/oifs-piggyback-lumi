      PROGRAM GETRES
      USE GRIB_API
C     ------------------------------------------------------------------

      IMPLICIT LOGICAL(L)
      INTEGER NRGRI(10000),IREDGRID(10000)
      NAMELIST/NAMRGRI/ NRGRI
      CHARACTER*16 CRTABLE
      CHARACTER*5 CGTYPE
      CHARACTER*128 CLARG
      INTEGER GETOPT 
      CHARACTER*4 CNSMAX
      CHARACTER*4 CNLEV
      CHARACTER CLOPT
      CHARACTER*128 CINSH,CINGP,COUT,CPATH,CLtypeOfGrid
      CHARACTER YINFO*80
      VCY=1.
C
      WRITE(YINFO,599) VCY
 599  FORMAT(' GETRES VERSION ',F4.1)
C
      print *,YINFO
      CINSH= ' '
      CINGP= ' '
      COUT= ' '
      CPATH= ' '
      IARG=0

      GET_LOOP:DO
        IOPTVAL=GETOPT('o:p:s:g:?',CLARG)
        CLOPT=CHAR(IOPTVAL)
        IF (IOPTVAL.EQ.-1) THEN
          IF(IARG.EQ.0) THEN
            CALL USAGE
            GOTO 999
          ELSE
            EXIT GET_LOOP
          ENDIF 
        ENDIF

C     GETS ARGUMENT VALUES FOR GIVEN OPTIONS
        IF    (CLOPT.EQ.'s')  THEN
          IARG=IARG+1
          CINSH=CLARG
        ELSEIF(CLOPT.EQ.'g') THEN
          IARG=IARG+1
          CINGP=CLARG
        ELSEIF(CLOPT.EQ.'p') THEN
          IARG=IARG+1
          CPATH=CLARG
        ELSEIF(CLOPT.EQ.'o') THEN
          IARG=IARG+1
          COUT=CLARG
        ELSEIF(CLOPT.EQ.'?') THEN
          CALL USAGE 
          GOTO 1000 
        ENDIF

      ENDDO GET_LOOP

      IF(CINSH.NE.' ') THEN
         CALL GRIB_OPEN_FILE(NINSH,CINSH,'R',KRET)
         IF(KRET.NE.GRIB_SUCCESS) THEN
           WRITE(0,*) ' ERROR OPENING FILE ',CINSH,' KRET=',KRET
           GOTO 999
         ENDIF
      ELSE
         WRITE(0,*)   ' INPUT SPECTRAL FILE NAME MUST BE SUPPLIED '
         CALL USAGE
         GOTO 999
      ENDIF
      IF(CINGP.NE.' ') THEN
         CALL GRIB_OPEN_FILE(NINGP,CINGP,'R',KRET)
         IF(KRET.NE.GRIB_SUCCESS) THEN
           WRITE(0,*) ' ERROR OPENING FILE ',CINGP,' KRET=',KRET
           GOTO 999   
         ENDIF
      ELSE
         WRITE(0,*) ' INPUT GRID-POINT FILE NAME MUST BE SUPPLIED '
         CALL USAGE
         GOTO 999
      ENDIF
      IF(CPATH.EQ.' ') THEN
        WRITE(0,*)  ' PATHNAME FOR RTABLE FILE MUST BE SUPPLIED '
        CALL USAGE
        GOTO 999
      ENDIF
      IF(COUT.EQ.' ') THEN
        WRITE(0,*)   ' OUTPUT FILENAME IS "./result" '
        COUT='result'
      ENDIF
      NULOUT=9
      OPEN(NULOUT,FILE=COUT,FORM='FORMATTED')  
      
      CALL GRIB_NEW_FROM_FILE(NINSH,IGRIB_SH,KRET)
      IF(KRET /= GRIB_SUCCESS) THEN
        WRITE(0,*) ' ERROR GRIB_NEW_FROM_FILE',KRET
        GOTO 999
      ENDIF
      CLtypeOfGrid=''
      call grib_get( igrib_sh, 'typeOfGrid', CLtypeOfGrid )
      IF(CLtypeOfGrid(1:2) /= 'sh' ) THEN
         WRITE(0,*) ' INPUT FIELD NOT IN SPECTRAL REPRESENTATION'
         GOTO 999
      ENDIF
      call grib_get( igrib_sh,'pentagonalResolutionParameterJ',NSMAX)
      call grib_get(igrib_sh,'numberOfVerticalCoordinateValues',inb_pv)
      NLEV=inb_pv/2-1
      CALL GRIB_NEW_FROM_FILE(NINGP,IGRIB_GG,KRET)
      IF(KRET /= GRIB_SUCCESS) THEN
        WRITE(0,*) ' ERROR GRIB_NEW_FROM_FILE GG ',KRET
        GOTO 999
      ENDIF
      call grib_get( igrib_gg, 'gridType', CLtypeOfGrid )
      IF(CLtypeOfGrid == 'sh') THEN
         WRITE(0,*) ' INPUT FIELD NOT IN GAUSSIAN GRID'
         GOTO 999
      ENDIF
      call grib_get(igrib_gg, 'Ny',nlat)
c      NLAT=ISEC2(3)
      IF(NLAT.LT.NSMAX*3/2-1) THEN
        LLGRI=.TRUE.
      ELSE
        LLGRI=.FALSE.
      ENDIF
      IF(CLtypeOfGrid == 'reduced_gg') THEN
         call grib_get( igrib_gg,'pl',IREDGRID)
         IF(IREDGRID(1).EQ.12) THEN
           LRGRI1=.TRUE.
           LRGRI2=.FALSE.
           CGTYPE='_12'
           IF(NSMAX < 1000) THEN
             WRITE(CRTABLE,'(A10,I3.3)') 
     1       '/rtable_12',NSMAX
           ELSE
             WRITE(CRTABLE,'(A10,I4.4)')
     1       '/rtable_12',NSMAX
           ENDIF
           IDUM=INDEX(CRTABLE,' ')-1
           ILEN=INDEX(CPATH,' ')-1
           CPATH=CPATH(1:ILEN)//CRTABLE(1:IDUM)
         ELSE
           LRGRI1=.FALSE.
           LRGRI2=.TRUE.
           IF( LLGRI ) THEN
             CGTYPE='l_2'   
             IF(NSMAX < 1000) THEN
               WRITE(CRTABLE,'(A10,I3.3)') 
     1       '/rtablel_2',NSMAX
             ELSE
               WRITE(CRTABLE,'(A10,I4.4)')
     1       '/rtablel_2',NSMAX
             ENDIF
           ELSEIF( 2*(NSMAX+1) == NLAT ) THEN
             IF( MAXVAL(IREDGRID(1:NLAT)) > 2*NLAT ) THEN
               CGTYPE='_4'
             ELSE
               CGTYPE='_3'
             ENDIF
             IF(NSMAX < 1000) THEN
               WRITE(CRTABLE,'(A7,A2,I3.3)') 
     1       '/rtable',CGTYPE,NSMAX
             ELSE
               WRITE(CRTABLE,'(A7,A2,I4.4)') 
     1       '/rtable',CGTYPE,NSMAX
             ENDIF
           ELSE
             CGTYPE='_2'
             IF(NSMAX < 1000) THEN
               WRITE(CRTABLE,'(A9,I3.3)') 
     1       '/rtable_2',NSMAX
             ELSE
               WRITE(CRTABLE,'(A9,I4.4)') 
     1       '/rtable_2',NSMAX
             ENDIF
           ENDIF
           IDUM=INDEX(CRTABLE,' ')-1
           ILEN=INDEX(CPATH,' ')-1
           CPATH=CPATH(1:ILEN)//CRTABLE(1:IDUM)
         ENDIF  
         print *,'PATH ',CPATH 
         OPEN(4,FILE=CPATH,FORM='FORMATTED')
         READ(4,NAMRGRI)
         CLOSE(4)
         DO JR=1,NLAT
           IF(NRGRI(JR).NE.IREDGRID(JR)) THEN
             WRITE(0,*) ' ERROR IN RESOLUTION'
             WRITE(0,*) ' ROW ',JR,' HAS ',IREDGRID(JR),' POINTS'
             WRITE(0,*) ' BUT IT SHOULD HAVE ',NRGRI(JR)
             GOTO 999
           ENDIF
         ENDDO
      ELSE
         LRGRI1=.FALSE.
         lRGRI2=.FALSE.
         CGTYPE='_full'
         CALL GRIB_GET(igrib_gg,'numberOfPointsAlongAParallel',ILONS)
         IF(ILONS.NE.2*NLAT) THEN
            WRITE(0,*) ' ERROR IN INPUT FULL GRID, NLON=',ILONS,
     1       ' NLAT=',NLAT       
         ENDIF   
      ENDIF
      IF( NLEV.LT.100 ) THEN
        WRITE(CNLEV(1:2),'(I2)') NLEV
        ILENV=2
      ELSE IF(NLEV.LT.1000) THEN
        WRITE(CNLEV(1:3),'(I3)') NLEV
        ILENV=3
      ELSE
        WRITE(CNLEV(1:4),'(I4)') NLEV
        ILENV=4
      ENDIF
      IF( NSMAX.LT.100 ) THEN
        WRITE(CNSMAX(1:2),'(I2)') NSMAX
        ILEN=2
      ELSE IF(NSMAX.LT.1000) THEN
        WRITE(CNSMAX(1:3),'(I3)') NSMAX
        ILEN=3
      ELSE
        WRITE(CNSMAX(1:4),'(I4)') NSMAX
        ILEN=4
      ENDIF
      WRITE(NULOUT,*) 'INPUT_RESOL=',CNSMAX(1:ILEN)
      PRINT *,' INPUT_RESOL=',NSMAX
      WRITE(NULOUT,*) 'export INPUT_RESOL'
      WRITE(NULOUT,*) 'INPUT_LEVELS=',CNLEV(1:ILENV)
      PRINT *,' INPUT_LEVELS=',NLEV
      WRITE(NULOUT,*) 'export INPUT_LEVELS'
      WRITE(NULOUT,*) 'INPUT_GTYPE=',CGTYPE
      PRINT *,' INPUT_GTYPE= ',CGTYPE
      WRITE(NULOUT,*) 'export INPUT_GTYPE'

      GOTO 1000

 999  CONTINUE
!mf      CALL SETRCD(1)
      CALL EXIT (1)

 1000 CONTINUE
      END
      SUBROUTINE USAGE
        WRITE(0,*) ' getres -s spfile -g gpfile -p path [-o out]'
        WRITE(0,*) ' delivers :'
        WRITE(0,*) ' INPUT_RESOL, INPUT_LEVELS, INPUT_GTYPE'
        WRITE(0,*) '    spfile = input spectral file (required)'
        WRITE(0,*) '    gpfile = input grid-point file (required)'
        WRITE(0,*) '    path   = dirpathname of resol-table file' 
        WRITE(0,*) '    out    = output filename (default:"result")'
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      INTEGER FUNCTION getopt(y_optstr, y_optarg)
      CHARACTER*(*)           y_optstr, y_optarg

      CHARACTER*512           y_arg
      CHARACTER*1             y_opt
      LOGICAL                 ll_endopts

      COMMON/gopt_com1/ initial  ,n_args  ,n_arg   ,ndx_arg ,ll_endopts
      COMMON/gopt_com2/ y_arg

C  Initialise on 1st. call
C  -----------------------

      IF (initial .NE. 123456) THEN
         initial    = 123456
         n_args     = iargc()
         n_arg      = 0
         ndx_arg    = 0
         ll_endopts = .FALSE.
      ENDIF

      y_optarg = ' '

C  Get length of "y_optstr"
C  ------------------------

      l_optstr = 0
      DO 10 i = len(y_optstr), 1, -1
      IF (y_optstr(i : i) .NE. ' ') THEN
         l_optstr = i
         GOTO 20
      ENDIF
   10 CONTINUE
   20 CONTINUE

C  If already at end of options, return
C  ------------------------------------

      IF ((n_arg .GT. n_args) .OR. ll_endopts) THEN
         ll_endopts = .TRUE.
         getopt = -1
         RETURN
      ENDIF

C  If we need to get the next argument, do so. Check for end of options
C  --------------------------------------------------------------------

      IF (ndx_arg .EQ. 0) THEN
         n_arg = n_arg + 1

         IF (n_arg .GT. n_args) THEN
            ll_endopts = .TRUE.
            getopt     = -1
            RETURN
         ENDIF

         y_arg   = ' '
         CALL getarg(n_arg, y_arg)
         ndx_arg = 1

         IF (y_arg(1 : 1) .NE. '-') THEN
            ll_endopts = .TRUE.
            getopt     = -1
            RETURN

         ELSE IF (y_arg .EQ. '--') THEN
            ll_endopts = .TRUE.
            n_arg      = n_arg + 1
            y_arg      = ' '
            IF (n_arg .LE. n_args) THEN
               CALL getarg(n_arg, y_arg)
            ENDIF
            getopt     = -1
            RETURN
         ENDIF
         ndx_arg = 2
      ENDIF

C  We have an option, now see if it is valid
C  -----------------------------------------

      y_opt   = y_arg(ndx_arg : ndx_arg)
      ndx_opt = index(y_optstr, y_opt)

      IF (ndx_opt .EQ. 0) THEN
         ndx_arg = ndx_arg + 1
         IF (y_arg(ndx_arg : ) .EQ. ' ') THEN
            ndx_arg = 0
         ENDIF
         getopt = ichar(y_opt)
         RETURN
      ENDIF

C  We have a valid option, see if it should have an argument
C  ---------------------------------------------------------

      ndx_arg = ndx_arg + 1
      IF (y_arg(ndx_arg :) .EQ. ' ') THEN
         ndx_arg = 0
      ENDIF

      IF (ndx_opt .EQ. l_optstr) THEN
         getopt = ichar(y_opt)
         RETURN
      ELSE IF (y_optstr(ndx_opt + 1 : ndx_opt + 1) .NE. ':') THEN
         getopt = ichar(y_opt)
         RETURN
      ENDIF


C  A valid option with an argument
C  -------------------------------

      IF (ndx_arg .EQ. 0) THEN
         IF (n_arg .EQ. n_args) THEN
            ll_endopts = .TRUE.
            getopt     = 0
            RETURN
         ENDIF

         n_arg = n_arg + 1
         y_arg = ' '
         call getarg(n_arg, y_arg)
         ndx_arg = 1
         ll_endopts = n_arg .eq. n_args
      ENDIF

      y_optarg = y_arg(ndx_arg : )
      ndx_arg  = 0
      getopt   = ichar(y_opt)

      RETURN
      END

      INTEGER FUNCTION getoarg(y_argbuf)
      CHARACTER*(*)            y_argbuf

      CHARACTER*512           y_arg
      LOGICAL                 ll_endopts

      COMMON/gopt_com1/ initial  ,n_args  ,n_arg   ,ndx_arg ,ll_endopts
      COMMON/gopt_com2/ y_arg

      y_argbuf = ' '

      IF (initial .NE. 123456) THEN
         getoarg = -1
         RETURN
      ENDIF

      IF (.NOT. ll_endopts) THEN
         getoarg = -1
         RETURN
      ENDIF

      IF (n_arg .GT. n_args) THEN
         getoarg = 0
         RETURN
      ENDIF

      CALL getarg(n_arg, y_arg)

      y_argbuf = y_arg
      n_arg    = n_arg + 1
      getoarg  = 1

      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
