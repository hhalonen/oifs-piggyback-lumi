       SUBROUTINE MY_EXIT(K)
C
       IMPLICIT NONE
       INTEGER K
#ifdef __uxp__
C
C      VPP700 CALL TO SET EXIT NUMBER
       CALL SETRCD(K)
#endif
C
       CALL EXIT(K)
C
       RETURN
       END
