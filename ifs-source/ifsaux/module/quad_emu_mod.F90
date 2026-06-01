#if defined(NECSX) && defined(REALHUGE)

MODULE QUAD_EMU_MOD

!        NEC 2007. Emulation of vectorized quadruple precision

USE PARKIND2  ,ONLY : JPRH

IMPLICIT NONE

EXTERNAL QVMULQV, ISMULQV, QVADDQV, QVSUBQV
EXTERNAL R16VCPQV, QVCPR16V

TYPE :: QUADS
  REAL(8) :: H, L
END TYPE QUADS
TYPE :: QUADV
  REAL(8), POINTER, DIMENSION(:) :: H, L
END TYPE QUADV

CONTAINS

SUBROUTINE QXMY (V1,V2,N,V3)
USE PARKIND1  ,ONLY : JPIM
USE PARKIND2  ,ONLY : JPRH
  ! ***************************
  ! V3(1:N) = V1(1:N) * V2(1:N)
  ! ***************************
  INTEGER(KIND=JPIM), INTENT(IN) :: N
  REAL(KIND=JPRH), DIMENSION(N), INTENT(IN)  :: V1, V2
  REAL(KIND=JPRH), DIMENSION(N), INTENT(OUT) :: V3
  TYPE (QUADV),SAVE :: QV1, QV2, QV3
  LOGICAL FIRST
  DATA FIRST / .TRUE. /
  INTEGER, SAVE :: NMAX = -1

  IF (FIRST  .OR.  N>NMAX) THEN
    IF(.NOT.FIRST)DEALLOCATE (QV1%H,QV1%L,QV2%H,QV2%L,QV3%H,QV3%L)
    FIRST = .FALSE. 
    NMAX = N
    ALLOCATE ( QV1%H(NMAX) , QV1%L(NMAX) )
    ALLOCATE ( QV2%H(NMAX) , QV2%L(NMAX) )
    ALLOCATE ( QV3%H(NMAX) , QV3%L(NMAX) )
  ENDIF

  ! QV1(:) = V1(:)
  CALL R16VCPQV(V1,QV1%H,QV1%L,N)

  ! QV2(:) = V2(:)
  CALL R16VCPQV(V2,QV2%H,QV2%L,N)

  ! QV3(:) = QV1(:) * QV2(:)

  CALL QVMULQV(QV1%H(1),QV1%L(1),QV2%H(1),QV2%L(1),QV3%H(1),QV3%L(1),N)

  ! V3(:) = QV3(:)
  CALL QVCPR16V(QV3%H,QV3%L,V3,N)

RETURN
END SUBROUTINE QXMY

SUBROUTINE QAXPY (ALPHA,V1,V2,N)
USE PARKIND1  ,ONLY : JPIM
USE PARKIND2  ,ONLY : JPRH
  ! ***********************************
  ! V2(1:N) = ALPHA * V1(1:N) + V2(1:N)
  ! ***********************************
  INTEGER(KIND=JPIM), INTENT(IN) :: N
  REAL(KIND=JPRH),               INTENT(IN)    :: ALPHA
  REAL(KIND=JPRH), DIMENSION(N), INTENT(IN)    :: V1
  REAL(KIND=JPRH), DIMENSION(N), INTENT(INOUT) :: V2
  TYPE (QUADV),SAVE :: QALPHA
  TYPE (QUADV),SAVE :: QV1, QV2, QVTMP
  LOGICAL FIRST
  DATA FIRST / .TRUE. /
  INTEGER, SAVE :: NMAX = -1

  IF (FIRST  .OR.  N>NMAX) THEN
    IF(.NOT.FIRST)THEN
      DEALLOCATE (QALPHA%H,QALPHA%L,QVTMP%H,QVTMP%L,QV1%H,QV1%L,QV2%H,QV2%L)
    ENDIF
    FIRST = .FALSE.
    NMAX = N
    ALLOCATE ( QALPHA%H(NMAX) , QALPHA%L(NMAX) )
    ALLOCATE ( QV1%H(NMAX) , QV1%L(NMAX) )
    ALLOCATE ( QV2%H(NMAX) , QV2%L(NMAX) )
    ALLOCATE ( QVTMP%H(NMAX) , QVTMP%L(NMAX) )
  ENDIF

  ! QALPHA(:) = ALPHA
  CALL R16SCPQV(ALPHA,QALPHA%H,QALPHA%L,N)

  ! QV1(:) = V1(:)
  CALL R16VCPQV(V1,QV1%H,QV1%L,N)

  ! QV2(:) = V2(:)
  CALL R16VCPQV(V2,QV2%H,QV2%L,N)

  ! QVtmp(:) = ALPHA*QV1(:)
  CALL QVMULQV(QALPHA%H,QALPHA%L,QV1%H,QV1%L,QVTMP%H,QVTMP%L,N)

  ! QVtmp(:) = QVtmp(:) + QV2(:)
  CALL QVADDQV(QVTMP%H,QVTMP%L,QV2%H,QV2%L,QVTMP%H,QVTMP%L,N)

  ! V2(:) = QVtmp(:)
  CALL QVCPR16V(QVTMP%H,QVTMP%L,V2,N)

RETURN
END SUBROUTINE QAXPY

SUBROUTINE SQRTIVDV (I1, I2, N, X)
USE PARKIND1  ,ONLY : JPIM, JPIB
USE PARKIND2  ,ONLY : JPRH
  ! *********************************************
  ! X(1:N) = SQRT ( REAL(I1(1:N) / REAL(I2(1:N) )
  ! *********************************************
  INTEGER(KIND=JPIM), INTENT(IN) :: N
  INTEGER(KIND=JPIB), INTENT(IN), DIMENSION(N) :: I1
  INTEGER(KIND=JPIB), INTENT(IN), DIMENSION(N) :: I2
  REAL(KIND=JPRH), INTENT(INOUT), DIMENSION(N) :: X
  TYPE (QUADV),SAVE :: QV1, QV2, QV3, QVX
  LOGICAL FIRST
  DATA FIRST / .TRUE. /
  INTEGER, SAVE :: NMAX = -1

  IF (FIRST  .OR.  N>NMAX) THEN
    IF(.NOT.FIRST)THEN
      DEALLOCATE (QV1%H,QV1%L,QV2%H,QV2%L,QV3%H,QV3%L)
      DEALLOCATE (QVX%H,QVX%L)
    ENDIF
    FIRST = .FALSE.
    NMAX = N
    ALLOCATE ( QV1%H(NMAX) , QV1%L(NMAX) )
    ALLOCATE ( QV2%H(NMAX) , QV2%L(NMAX) )
    ALLOCATE ( QV3%H(NMAX) , QV3%L(NMAX) )
    ALLOCATE ( QVX%H(NMAX) , QVX%L(NMAX) )
  ENDIF
  ! QV1(:) = I1(:)
  CALL I4VCPQV(I1,QV1%H,QV1%L,N)
  ! QV2(:) = I2(:)
  CALL I4VCPQV(I2,QV2%H,QV2%L,N)
  ! QV3(:) = I1(:) / I2(:)
  CALL QVDIVQV(QV1%H,QV1%L,QV2%H,QV2%L,QV3%H,QV3%L,N)
  ! QVx(:) = sqrt ( QV3(:) )
  CALL QVSQRT (QV3%H,QV3%L,QVX%H,QVX%L,N)
  ! X(:) = QVx(:)
  CALL QVCPR16V(QVX%H,QVX%L,X,N)

RETURN
END SUBROUTINE SQRTIVDV

END MODULE QUAD_EMU_MOD



! New routines ------------------------------------------------
      SUBROUTINE I4VCPQV(I4,AHI,ALO,N)
! For SX vectorized version
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER(KIND=JPIM) :: N
      INTEGER(KIND=JPIB) :: I4(N)
      REAL(KIND=JPRB) :: AHI(N),ALO(N)
      REAL(KIND=JPRH) :: R16(N)

      R16(1:N) = I4(1:N)
      CALL R16VCPQV(R16,AHI,ALO,N)

      END SUBROUTINE I4VCPQV

      SUBROUTINE R16VCPQV(R16,AHI,ALO,N)
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
#ifdef SCALAR
! Fortran equivalent code
       INTEGER(KIND=JPIM) :: N
       REAL(KIND=JPRH) :: R16(N)
       REAL(KIND=JPRB) :: AHI(N),ALO(N)
       INTEGER I
       DO I=1,N
         AHI(I)=R16(I)
         ALO(I)=R16(I)-AHI(I)
       ENDDO
#else
! For SX vectorized version
      INTEGER(KIND=JPIM) :: N
      REAL(KIND=JPRB) :: R16(2,N)
      REAL(KIND=JPRB) :: AHI(N),ALO(N)

      REAL(KIND=JPRB) :: AA,BB
      INTEGER(KIND=JPIB) :: II,JJ,MSK1,MSK2,MSK3
      EQUIVALENCE(AA,II)
      EQUIVALENCE(BB,JJ)
      PARAMETER(MSK1=Z'7FF0000000000000')
      PARAMETER(MSK2=Z'FFF0000000000000')
      PARAMETER(MSK3=Z'0340000000000000')

      INTEGER I

      DO I=1,N
        AHI(I)=R16(1,I)
        BB=R16(1,I)
        JJ=IAND(JJ,MSK1)
        IF(JJ.LT.MSK3) THEN
          ALO(I)=0.D0
        ELSE
          AA=R16(2,I)
          II=IAND(II,MSK2)
          ALO(I)=R16(2,I)-AA
        ENDIF
      ENDDO
#endif
      END SUBROUTINE R16VCPQV

      SUBROUTINE QVCPR16V(AHI,ALO,R16,N)
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
#ifdef SCALAR
! Fortran equivalent code
       INTEGER N
       REAL(KIND=JPRB) :: AHI(N),ALO(N)
       REAL(KIND=JPRH) :: R16(N)
       INTEGER I
       DO I=1,N
         R16(I)=AHI(I)
         R16(I)=R16(I)+ALO(I)
       ENDDO
#else
! For SX vectorized version
      REAL(KIND=JPRB) :: AHI(N),ALO(N)
      REAL(KIND=JPRB) :: R16(2,N)
      INTEGER N
!
      REAL(KIND=JPRB) :: A1,A2,AA,ZHI,ZLO,AGOMI,R
      INTEGER(KIND=JPIB) :: II,MSK
      EQUIVALENCE(AA,II)
      PARAMETER(MSK=Z'FFF0000000000000')
      INTEGER I
!
      DO I=1,N
         A1=AHI(I)
         A2=ALO(I)
         AA=A1
         II=IAND(II,MSK)
         AGOMI=AA*2D0**(-52)
         ZHI=A1
         ZLO=A2+AGOMI
         R=A1*A2
         IF (R.LT.0) ZHI=A1-AGOMI
         R16(1,I)=ZHI
         R16(2,I)=(A1-ZHI)+ZLO
      ENDDO
#endif
      END SUBROUTINE QVCPR16V
! New routines ------------------------------------------------

      SUBROUTINE R16SCPQV(R16,AHI,ALO,N)
! ... Quadruple precision native to Quadruple emulation pair
!     QV(1:N) = R16
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: R16(2)
      REAL(KIND=JPRB) :: AHI(N),ALO(N)

      REAL(KIND=JPRB) :: AA
      INTEGER(KIND=JPIB) :: II,MSK
      EQUIVALENCE(AA,II)
      PARAMETER(MSK=Z'FFF0000000000000')

      INTEGER I

      DO I=1,N
        AHI(I)=R16(1)
        AA=R16(2)
        II=IAND(II,MSK)
        ALO(I)=R16(2)-AA
      ENDDO

      END SUBROUTINE R16SCPQV

      SUBROUTINE QVMULQV(AHI,ALO,CHI,CLO,ACHI,ACLO,N)
! ... multiplication of Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),CHI(N),CLO(N),ACHI(N),ACLO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: ZHI,ZLO,ZZ
      REAL(KIND=JPRB) :: A1,A2,C1,C2,T
      INTEGER I

!     longmul(a, c) RESULT(ac)
!     z = exactmul2(a%hi, c%hi)
!     zz = ((a%hi + a%lo) * c%lo + a%lo * c%hi) + z%lo
!     ac%hi = z%hi + zz
!     ac%lo = (z%hi - ac%hi) + zz

!     exactmul2(a, c) RESULT(ac)
!     t = constant * a
!     a1 = (a - t) + t
!     a2 = a - a1
!     t = constant * c
!     c1 = (c - t) + t
!     c2 = c - c1
!     ac%hi = a * c
!     ac%lo = (((a1 * c1 - ac%hi) + a1 * c2) + c1 * a2) + c2 * a2

      DO I=1,N
        T = CONSTANT * AHI(I)
        A1 = (AHI(I) - T) + T
        A2 = AHI(I) - A1
        T = CONSTANT * CHI(I)
        C1 = (CHI(I) - T) + T
        C2 = CHI(I) - C1
        ZHI = AHI(I) * CHI(I)
        ZLO = (((A1 * C1 - ZHI) + A1 * C2) + C1 * A2) + C2 * A2
        ZZ = ((AHI(I) + ALO(I)) * CLO(I) + ALO(I) * CHI(I)) + ZLO
        ACHI(I) = ZHI + ZZ
        ACLO(I) = (ZHI - ACHI(I)) + ZZ
      ENDDO

      END SUBROUTINE QVMULQV
      SUBROUTINE QVDIVQV(AHI,ALO,CHI,CLO,ACHI,ACLO,N)
! ... division of Quadruple emulation pair 
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),CHI(N),CLO(N),ACHI(N),ACLO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: Z,ZZ,QHI,QLO
      REAL(KIND=JPRB) :: A1,A2,C1,C2,T
      INTEGER I

!     longdiv(a, c) RESULT(ac)
!     z = a%hi / c%hi
!     q = exactmul2(c%hi, z)
!     zz = ((((a%hi - q%hi) - q%lo) + a%lo) - z*c%lo) / (c%hi + c%lo)
!     ac%hi = z + zz
!     ac%lo = (z - ac%hi) + zz

!     exactmul2(a, c) RESULT(ac)
!     t = constant * a
!     a1 = (a - t) + t
!     a2 = a - a1
!     t = constant * c
!     c1 = (c - t) + t
!     c2 = c - c1
!     ac%hi = a * c
!     ac%lo = (((a1 * c1 - ac%hi) + a1 * c2) + c1 * a2) + c2 * a2

      DO I=1,N
        Z = AHI(I)/CHI(I)
        T = CONSTANT * CHI(I)
        A1 = (CHI(I) - T) + T
        A2 = CHI(I) - A1
        T = CONSTANT * Z
        C1 = (Z - T) + T
        C2 = Z - C1
        QHI = CHI(I) * Z
        QLO = (((A1 * C1 - QHI) + A1 * C2) + C1 * A2) + C2 * A2
        ZZ = ((((AHI(I) - QHI) - QLO) + ALO(I)) - Z*CLO(I)) / (CHI(I) + CLO(I))
        ACHI(I) = Z + ZZ
        ACLO(I) = (Z - ACHI(I)) + ZZ
      ENDDO

      END SUBROUTINE QVDIVQV
      SUBROUTINE QVADDQV(AHI,ALO,CHI,CLO,ACHI,ACLO,N)
! ... addition of Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),CHI(N),CLO(N),ACHI(N),ACLO(N)

      REAL(KIND=JPRB) :: Z,ZZ,Q
      INTEGER I

!     longadd(a, c) RESULT(ac)
!     z = a%hi + c%hi
!     q = a%hi - z
!     zz = (((q + c%hi) + (a%hi - (q + z))) + a%lo) + c%lo
!     ac%hi = z + zz
!     ac%lo = (z - ac%hi) + zz

      DO I=1,N
        Z = AHI(I) + CHI(I)
        Q = AHI(I) - Z
        ZZ = (((Q + CHI(I)) + (AHI(I) - (Q + Z))) + ALO(I)) + CLO(I)
        ACHI(I) = Z + ZZ
        ACLO(I) = (Z - ACHI(I)) + ZZ
      ENDDO

      END SUBROUTINE QVADDQV
      SUBROUTINE QVSUBQV(AHI,ALO,CHI,CLO,ACHI,ACLO,N)
! ... subtraction of Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),CHI(N),CLO(N),ACHI(N),ACLO(N)

      REAL(KIND=JPRB) :: Z,ZZ,Q
      INTEGER I

!     longsub(a, c) RESULT(ac)
!     z = a%hi - c%hi
!     q = a%hi - z
!     zz = (((q - c%hi) + (a%hi - (q + z))) + a%lo) - c%lo
!     ac%hi = z + zz
!     ac%lo = (z - ac%hi) + zz

      DO I=1,N
        Z = AHI(I) - CHI(I)
        Q = AHI(I) - Z
        ZZ = (((Q - CHI(I)) + (AHI(I) - (Q + Z))) + ALO(I)) - CLO(I)
        ACHI(I) = Z + ZZ
        ACLO(I) = (Z - ACHI(I)) + ZZ
      ENDDO

      END SUBROUTINE QVSUBQV
      SUBROUTINE DVMULDV(A,C,ACHI,ACLO,N)
! ... multiplication of double precision to Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: A(N),C(N),ACHI(N),ACLO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: A1,A2,C1,C2,T
      INTEGER I

!     exactmul2(a, c) RESULT(ac)
!     t = constant * a
!     a1 = (a - t) + t
!     a2 = a - a1
!     t = constant * c
!     c1 = (c - t) + t
!     c2 = c - c1
!     ac%hi = a * c
!     ac%lo = (((a1 * c1 - ac%hi) + a1 * c2) + c1 * a2) + c2 * a2

      DO I=1,N
        T = CONSTANT * A(I)
        A1 = (A(I) - T) + T
        A2 = A(I) - A1
        T = CONSTANT * C(I)
        C1 = (C(I) - T) + T
        C2 = C(I) - C1
        ACHI(I) = A(I) * C(I)
        ACLO(I) = (((A1 * C1 - ACHI(I)) + A1 * C2) + C1 * A2) + C2 * A2
      ENDDO

      END SUBROUTINE DVMULDV
      SUBROUTINE ISMULQV(IS,QHI,QLO,QIHI,QILO,N)
! ... integer scale of Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER IS,N
      REAL(KIND=JPRB) :: QHI(N),QLO(N),QIHI(N),QILO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: A1,A2,Q1,Q2,T,ZHI,ZLO,ZZ
      INTEGER I

      T = CONSTANT * IS
      A1 = (DBLE(IS) - T) + T
      A2 = DBLE(IS) - A1
      DO I=1,N
        T = CONSTANT * QHI(I)
        Q1 = (QHI(I) - T) + T
        Q2 = QHI(I) - Q1
        ZHI = IS * QHI(I)
        ZLO = (((A1 * Q1 - ZHI) + A1 * Q2) + Q1 * A2) + Q2 * A2
        ZZ = IS * QLO(I) + ZLO
        QIHI(I) = ZHI + ZZ
        QILO(I) = (ZHI - QIHI(I)) + ZZ
      ENDDO

      END SUBROUTINE ISMULQV
      SUBROUTINE QVDIVIS(AHI,ALO,IS,AIHI,AILO,N)
! ... integer division of Quadruple emulation pair
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER IS,N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),AIHI(N),AILO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: Z,ZZ,QHI,QLO
      REAL(KIND=JPRB) :: A1,A2,C1,C2,T
      INTEGER I

!     div_quad_int(a, b) RESULT(c)
!     Divide a quadruple-precision number (a) by an integer (b)
!     z = a%hi / b
!     q = exactmul2(DBLE(b), z)
!     zz = (((a%hi - q%hi) - q%lo) + a%lo) / b
!     c%hi = z + zz
!     c%lo = (z - c%hi) + zz

!     exactmul2(a, c) RESULT(ac)
!     t = constant * a
!     a1 = (a - t) + t
!     a2 = a - a1
!     t = constant * c
!     c1 = (c - t) + t
!     c2 = c - c1
!     ac%hi = a * c
!     ac%lo = (((a1 * c1 - ac%hi) + a1 * c2) + c1 * a2) + c2 * a2

      T = CONSTANT * IS
      A1 = (DBLE(IS) - T) + T
      A2 = DBLE(IS) - A1
      DO I=1,N
        Z = AHI(I)/IS
        T = CONSTANT * Z
        C1 = (Z - T) + T
        C2 = Z - C1
        QHI = DBLE(IS) * Z
        QLO = (((A1 * C1 - QHI) + A1 * C2) + C1 * A2) + C2 * A2
        ZZ = (((AHI(I) - QHI) - QLO) + ALO(I)) / IS
        AIHI(I) = Z + ZZ
        AILO(I) = (Z - AIHI(I)) + ZZ
      ENDDO

      END SUBROUTINE QVDIVIS

      SUBROUTINE QVSQRT(AHI,ALO,BHI,BLO,N)
      USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
      USE PARKIND2  ,ONLY : JPRH
      INTEGER N
      REAL(KIND=JPRB) :: AHI(N),ALO(N),BHI(N),BLO(N)

      INTEGER, PARAMETER :: NBITS = DIGITS(1.0D0)
      REAL(KIND=JPRB), PARAMETER :: CONSTANT = 2.D0**(NBITS-NBITS/2)+1.D0

      REAL(KIND=JPRB) :: A1,A2,C1,C2,T
      REAL(KIND=JPRB) :: TTHI,TTLO,TT,RES
      INTEGER I

!     longsqrt(a) RESULT(b)
!     t = SQRT(a%hi)
!     tt = exactmul2(t, t)
!     res = (((a%hi - tt%hi) - tt%lo) + a%lo) * 0.5_dp / t
!     b%hi = t + res
!     b%lo = (t - b%hi) + res

!     exactmul2(a, c) RESULT(ac)
!     t = constant * a
!     a1 = (a - t) + t
!     a2 = a - a1
!     t = constant * c
!     c1 = (c - t) + t
!     c2 = c - c1
!     ac%hi = a * c
!     ac%lo = (((a1 * c1 - ac%hi) + a1 * c2) + c1 * a2) + c2 * a2

      DO I=1,N
        TT = SQRT(AHI(I))
        T = CONSTANT * TT
        A1 = (TT - T) + T
        A2 = TT - A1
        T = CONSTANT * TT
        C1 = (TT - T) + T
        C2 = TT - C1
        TTHI = TT * TT
        TTLO = (((A1 * C1 - TTHI) + A1 * C2) + C1 * A2) + C2 * A2
        RES = (((AHI(I) - TTHI) - TTLO) + ALO(I)) * 0.5D0 / TT
        BHI(I) = TT + RES
        BLO(I) = (TT - BHI(I)) + RES
      ENDDO

      END SUBROUTINE QVSQRT
#else
MODULE QUAD_EMU_MOD
END MODULE QUAD_EMU_MOD
#endif
