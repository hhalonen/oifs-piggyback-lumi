MODULE SPLINES_MOD

!         PURPOSE
!         -------
!         Provide spline functions to avoid using NAG functions.
!

USE PARKIND1  ,ONLY : JPIM, JPRB, JPRD
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

PRIVATE
PUBLIC CUBIC_SPLINE_KNOTS, CUBIC_SPLINE_EVAL

CONTAINS

SUBROUTINE CUBIC_SPLINE_KNOTS(KM,KNCAP7,PX,PY,PW,PLAMDA,PC,PSIGMA,KFAIL)  
!        PURPOSE
!        ------
!        Calculate a cubic-spline approximation to input data. This is a
!        Fortran implementation of the procedure for fitting a cubic spline
!        to an arbitrary set of points from Appendix 3 of 'Curve Fitting: A Guide
!        and Suite of Algorithms for the Non-Specialist User', NPL Report NAC26, 
!        National Physical Laboratory. This is equivalent to the NAG function E02BAF.
!
!        ARGUMENTS
!        ---------
!           INPUT
!              KM      - Number of data points to be fitted
!              KNCAP7  - NCAP+7 where NCAP is the number of intervals in the spline
!              PX      - X values of the function to be fitted
!              PY      - Y values of the function to be fitted
!              PW      - Weights of each data point in the function
!
!           INPUT and OUTPUT
!              PLAMDA  - The values PLAMDA(5:NCAP+3) specify the values of the interior
!                        knots used in the spline. The routine defines additional exterior
!                        knots in PLAMDA(1:4) and PLAMDA(NCAP+4:NCAP7).
!
!           OUTPUT
!             PC       - The coefficients of the b-spline
!             PSIGMA   - Residual sum of squares
!             KFAIL    - Error flag

  IMPLICIT NONE

  INTEGER(KIND=JPIM), INTENT(IN)    :: KM,KNCAP7
  REAL(KIND=JPRB),    INTENT(IN)    :: PX(KM),PY(KM),PW(KM)
  REAL(KIND=JPRB),    INTENT(INOUT) :: PLAMDA(KNCAP7)
  REAL(KIND=JPRB),    INTENT(OUT)   :: PC(KNCAP7)
  REAL(KIND=JPRB),    INTENT(OUT)   :: PSIGMA
  INTEGER(KIND=JPIM), INTENT(OUT)   :: KFAIL

  INTEGER(KIND=JPIM) :: II,IJ
  INTEGER(KIND=JPIM) :: INCAP

  INTEGER(KIND=JPIM) :: IR,IJOLD,IL,IU
  REAL(KIND=JPRB)    :: ZB(KM)
  REAL(KIND=JPRB)    :: ZCOSINE,ZSINE
  REAL(KIND=JPRB)    :: ZDIAG(1:KNCAP7-4),ZA(1:KNCAP7-4,2:4)
  REAL(KIND=JPRB)    :: ZA_ROW, ZA_COL,ZBI,ZC_ROW,ZC_COL
  REAL(KIND=JPRB)    :: ZD,ZD4,ZD5,ZD6,ZD7,ZD8,ZD9,ZDPRIME
  REAL(KIND=JPRB)    :: ZE2,ZE3,ZE4,ZE5
  REAL(KIND=JPRB)    :: ZK0,ZK1,ZK2,ZK3,ZK4,ZK5,ZK6
  REAL(KIND=JPRB)    :: ZROW_ELEMENT,ZS,ZWI,ZXI
  REAL(KIND=JPRB)    :: ZROW(4),ZN1,ZN2,ZN3
  REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

  IF(LHOOK) CALL DR_HOOK('CUBIC_SPLINE_KNOTS',0,ZHOOK_HANDLE)


  INCAP=KNCAP7-7

  !Check input data
  KFAIL=0

  !Check knots are ordered
  DO IJ=5,INCAP+2
    IF(PLAMDA(IJ)>PLAMDA(IJ+1)) THEN
      KFAIL=1
      EXIT
    ENDIF
  ENDDO

  !Check the weights are positive
  IF(KFAIL==0) THEN
    DO IJ=1,KM
      IF(PW(IJ)<=0.0) THEN
        KFAIL=2
        EXIT
      ENDIF
    ENDDO
  ENDIF

  !Check the x values are ordered
  IF(KFAIL==0) THEN
    ZB(1)=PX(1)
    IJ=1
    DO II=2,KM
      IF(PX(II)<ZB(IJ)) THEN
        KFAIL=3
        EXIT
      ENDIF
      IF(PX(II)>ZB(IJ)) THEN
        IJ=IJ+1
        ZB(IJ)=PX(II)
      ENDIF
    ENDDO
    IR=IJ
  ENDIF

  !Check there are sufficient unique x points for the number of knots
  IF(KFAIL==0) THEN
    IF(IR<INCAP+3) THEN
      KFAIL=3
    ENDIF
  ENDIF

  !Check the Schoenberg-Whitney conditions
  IF(KFAIL==0) THEN
    KFAIL=5
    DO IJ=1,4
      IF(IJ>INCAP) THEN
        KFAIL=0
        EXIT
      ENDIF
      II=1-IJ
      IF(ZB(IJ)>=PLAMDA(IJ+4).OR.PLAMDA(INCAP+3+II)>=ZB(IR+II)) THEN 
        KFAIL=-5
        EXIT
      ENDIF
    ENDDO

    IF(KFAIL>0) THEN

      IR=IR-4
      II=4
      IJLOOP: DO IJ=5,INCAP-1
        ZK0=PLAMDA(IJ+4)
        ZK4=PLAMDA(IJ)
        INLOOP: DO
          II=II+1
          ZBI=ZB(II)
          IF(II>IR.OR.ZBI>=ZK0) THEN
            KFAIL=-5
            EXIT IJLOOP
          ENDIF
          IF(ZBI<=ZK4) THEN
            CYCLE INLOOP
          ENDIF
          EXIT INLOOP
        ENDDO INLOOP
      ENDDO IJLOOP
      IF(KFAIL>0) KFAIL=0
    ENDIF
    KFAIL=ABS(KFAIL)
  ENDIF
  !End of checks

  PLAMDA(1:4)=MINVAL(PX(:))
  PLAMDA(INCAP+4:KNCAP7)=MAXVAL(PX(:))

  IF(KFAIL==0) THEN

    ZDIAG(1:INCAP+3)=0.0
    ZA(1:INCAP+3,2:4)=0.0
    PC(1:KNCAP7)=0.0
    PSIGMA=0.0

    IJ=0
    IJOLD=0

    IILOOP: DO II=1,KM

      ZWI=PW(II)*PW(II)
      ZXI=PX(II)

      DO WHILE (ZXI>=PLAMDA(IJ+4).AND.IJ<=INCAP-1)
        IJ=IJ+1
      ENDDO

      IF(IJ/=IJOLD) THEN
        ZK1=PLAMDA(IJ+1)
        ZK2=PLAMDA(IJ+2)
        ZK3=PLAMDA(IJ+3)
        ZK4=PLAMDA(IJ+4)
        ZK5=PLAMDA(IJ+5)
        ZK6=PLAMDA(IJ+6)
        ZD4=1.0/(ZK4-ZK1)
        ZD5=1.0/(ZK5-ZK2)
        ZD6=1.0/(ZK6-ZK3)
        ZD7=1.0/(ZK4-ZK2)
        ZD8=1.0/(ZK5-ZK3)
        ZD9=1.0/(ZK4-ZK3)
        IJOLD=IJ
      ENDIF

      ZE5=ZK5-ZXI
      ZE4=ZK4-ZXI
      ZE3=ZXI-ZK3
      ZE2=ZXI-ZK2
      ZN2=ZE3*ZD8*ZD9
      ZN1=ZE4*ZD7*ZD9
      ZN3=ZE3*ZN2*ZD6
      ZN2=(ZE2*ZN1+ZE5*ZN2)*ZD5
      ZN1=ZE4*ZN1*ZD4
      ZROW(4)=ZE3*ZN3
      ZROW(3)=ZE2*ZN2+(ZK6-ZXI)*ZN3
      ZROW(2)=(ZXI-ZK1)*ZN1+ZE5*ZN2
      ZROW(1)=ZE4*ZN1

      ZC_ROW=PY(II)

      DO IL=0,3
        IF(ZWI==0.0) THEN
          CYCLE IILOOP
        ENDIF
        ZROW_ELEMENT=ZROW(IL+1)
        IF(ZROW_ELEMENT/=0) THEN
          ZD=ZDIAG(IJ+IL)
          ZS=ZWI*ZROW_ELEMENT
          ZDIAG(IJ+IL)=ZD+ZS*ZROW_ELEMENT
          ZDPRIME=ZD+ZS*ZROW_ELEMENT
          ZCOSINE=ZD/ZDPRIME
          ZSINE=ZS/ZDPRIME
          ZWI=ZCOSINE*ZWI
          DO IU=2,4-IL
            ZA_COL=ZA(IJ+IL,IU)
            ZA_ROW=ZROW(IL+IU)
            ZA(IJ+IL,IU)=ZCOSINE*ZA_COL+ZSINE*ZA_ROW
            ZROW(IL+IU)=ZA_ROW-ZROW_ELEMENT*ZA_COL
          ENDDO
          ZC_COL=PC(IJ+IL)
          PC(IJ+IL)=ZCOSINE*ZC_COL+ZSINE*ZC_ROW
          ZC_ROW=ZC_ROW-ZROW_ELEMENT*ZC_COL
        ENDIF
      ENDDO !IL

      PSIGMA=PSIGMA+ZWI*ZC_ROW*ZC_ROW
    ENDDO IILOOP

    DO IJ=INCAP+2,1,-1
      IF(IL<4) THEN

        IL=IL+1
      ENDIF
      IU=IJ-1
      ZS=PC(IJ)
      DO II=2,IL
        ZS=ZS-ZA(IJ,II)*PC(II+IU)
      ENDDO
      PC(IJ)=ZS
    ENDDO !IJ

  ENDIF

  IF(LHOOK) CALL DR_HOOK('CUBIC_SPLINE_KNOTS',1,ZHOOK_HANDLE)

END SUBROUTINE CUBIC_SPLINE_KNOTS

SUBROUTINE CUBIC_SPLINE_EVAL(KNCAP7,PK,PC,PX,PS,KFAIL)
!        PURPOSE
!        ------
!        Calculates a cubic-spline approximation for a given x coordinate. This is 
!        a Fortran implementation of the procedure for generating a cubic spline
!        from Appendix 5 of 'Curve Fitting: A Guide and Suite of Algorithms for 
!        the Non-Specialist User', NPL Report NAC26, National Physical Laboratory.
!        This is equivalent to the NAG function E02BBF.
!
!        ARGUMENTS
!        ---------
!           INPUT
!              KNCAP7  - NCAP+7 where NCAP is the number of intervals in the spline
!              PK      - The knots of the spline function 
!              PC      - The B-spline coefficients
!              PX      - The x value for which the value of the spline function is required
!
!           OUTPUT
!             PS       - The y value of the spline function
!             KFAIL    - Error flag

  IMPLICIT NONE

  INTEGER(KIND=JPIM), INTENT(IN)  :: KNCAP7
  REAL(KIND=JPRB),    INTENT(IN)  :: PK(KNCAP7)
  REAL(KIND=JPRB),    INTENT(IN)  :: PC(KNCAP7-4)
  REAL(KIND=JPRB),    INTENT(IN)  :: PX
  REAL(KIND=JPRB),    INTENT(OUT) :: PS
  INTEGER(KIND=JPIM), INTENT(OUT) :: KFAIL

  INTEGER(KIND=JPIM) :: IJ1, IJ, IL
  INTEGER(KIND=JPIM) :: INCAP
  REAL(KIND=JPRB)    :: ZE2,ZE3,ZE4,ZE5,ZC1,ZC2,ZC3
  REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

  IF(LHOOK) CALL DR_HOOK('CUBIC_SPLINE_EVAL',0,ZHOOK_HANDLE)

  INCAP=KNCAP7-7

  IF(PX<PK(4) .OR. PX>PK(INCAP+4)) THEN
    KFAIL=1
  ELSE

    IJ1=0
    IJ=INCAP
    DO WHILE (IJ-IJ1>1) 
      IL=(IJ1+IJ)/2
      IF(PX<PK(IL+4)) THEN
        IJ=IL
      ELSE
        IJ1=IL
      ENDIF
    ENDDO

    ZE2=PX-PK(IJ1+3)
    ZE3=PX-PK(IJ1+4)
    ZE4=PK(IJ+4)-PX
    ZE5=PK(IJ+5)-PX
    ZC2=PC(IJ+1)
    ZC3=PC(IJ+2)
    ZC1=((PX-PK(IJ1+2))*ZC2+ZE4*PC(IJ))/(PK(IJ+4)-PK(IJ1+2))
    ZC2=(ZE2*ZC3+ZE5*ZC2)/(PK(IJ+5)-PK(IJ1+3))
    ZC3=(ZE3*PC(IJ+3)+(PK(IJ+6)-PX)*ZC3)/(PK(IJ+6)-PK(IJ1+4))
    ZC1=(ZE2*ZC2+ZE4*ZC1)/(PK(IJ+4)-PK(IJ1+3))
    ZC2=(ZE3*ZC3+ZE5*ZC2)/(PK(IJ+5)-PK(IJ1+4))
    PS=(ZE3*ZC2+ZE4*ZC1)/(PK(IJ+4)-PK(IJ1+4))
    KFAIL=0
  ENDIF

  IF(LHOOK) CALL DR_HOOK('CUBIC_SPLINE_EVAL',1,ZHOOK_HANDLE)

END SUBROUTINE

END MODULE
