MODULE mo_xt_borrow_lite

  IMPLICIT NONE
  
  CONTAINS
  
  SUBROUTINE xt_borrow_lite(kproma, kbdim,  klev,  klevp1, ktrac,       &
                       papp1,  paphp1,                             &
                       pxtm1,  pxtte, pdtime, pdpg                               )

    ! *xt_borrow_lite* borrowing scheme to correct for negative
    !             tracer masses conserveing mass within the
    !             column
    ! Note by SF: actually, the scheme is also good for non-mass tracers, therefore it is
    ! now applied to all transported tracers.
    !
    ! Author:
    ! -------
    ! Philip Stier, MPI-Met,         06/2004
    !
    ! Edited for HAM-Lite:
    ! -------
    ! Hermanni Halonen, UEF,         04/2026
    !
    ! Method:
    ! -------
    ! If negative tracer values occur the scheme is iteratively
    ! borrowing tracer mixing ratios from the grid-box below.
    ! If the lowest layer can not compensate for accumulated remaining
    ! corrections, the procedure is repeated from bottom to top.
    !
    ! Restrictions:
    ! -------------
    ! Columns with a negative total integrated tracer
    ! content are set to zero despite the associated mass
    ! error.

    USE mo_physical_constants_lite, ONLY: grav
    USE mo_subm_trac_lite,          ONLY: trlist
    USE mo_subm_trac_para_lite,     ONLY: GAS, AEROSOLMASS, AEROSOLNUMBER
    USE mo_kind_lite,               ONLY: dp, wp
    
    ! -->hhermann: How to get these from HAM-Lite??
    USE mo_advection,               ONLY: no_advection !SF #246
    USE mo_submodel,                ONLY: lmoz  !csld #330
    ! <--hhermann

    IMPLICIT NONE

    INTEGER, INTENT(IN)    :: kproma, kbdim, klev, klevp1, ktrac
    REAL(dp),INTENT(IN)    :: pdtime
    REAL(wp),INTENT(IN)    :: papp1(kbdim,klev), paphp1(kbdim,klevp1)
    REAL(wp),INTENT(IN)    :: pxtm1(kbdim,klev,ktrac)
    REAL(wp),INTENT(INOUT) :: pxtte(kbdim,klev,ktrac)
    REAL(wp),INTENT(IN)    :: pdpg(kbdim,klev)
    

    !--- Local variables:

    INTEGER :: jl, jk, jt

    REAL(wp):: zxtp1, ztmst, zeps

    REAL(wp):: zxtbor(kbdim)

    REAL(wp):: zdpg(kbdim,klev)

    LOGICAL :: lborrtrac !csld #330 : determines if the tracer can be borrowed or not

    !--- 0) Initializations:
    ztmst=pdtime
    zeps=10._wp*EPSILON(1.0_wp)

    !--- 1) Calculate auxiliary variable dp/g :

    !--- Uppermost level:

   !  zdpg(1:kproma,1)=2._wp*(paphp1(1:kproma,2)-papp1(1:kproma,1))/grav

   !  !--- Other levels:

   !  DO jk=2, klev

   !     zdpg(1:kproma,jk)=(paphp1(1:kproma,jk+1)-paphp1(1:kproma,jk))/grav

   !  END DO

    zdpg(1:kproma,1:klev) = pdpg(1:kproma,1:klev)

    !--- 2) Borrowing scheme:
    
    DO jt=1, ktrac

       !>>csld #330
       ! use xt_borrow on all aerosols species
       lborrtrac = trlist%ti(jt)%nphase == AEROSOLMASS .OR. trlist%ti(jt)%nphase == AEROSOLNUMBER

       IF (.NOT. lmoz) THEN    ! in case of "ham only" runs, use of xt_borrow on gaz species as well
          lborrtrac = lborrtrac .OR. (trlist%ti(jt)%nphase == GAS)
       END IF
        
       !-->hhermann: I guess IF statement is not necessary
       !IF ( (trlist%ti(jt)%ntran /= no_advection)       .AND. &  !SF #246 exclude non-advected tracers
       !    lborrtrac) THEN
       !<--hhermann
       !<<csld #330

          !--- Start borrowing scheme:
    
          !--- Integrate from top to bottom:

          zxtbor(1:kproma)=0.0_wp

          DO jk=1, klev
             DO jl=1, kproma

                !--- Check if corrected mass, including the fix of the layer above,
                !    yields a negative mixing ratio:
                !    (Convert mass correction converted to [kg kg-1] in current layer)

                zxtp1=pxtm1(jl,jk,jt)+pxtte(jl,jk,jt)*ztmst + zxtbor(jl)/zdpg(jl,jk)

                IF ( zxtp1 > 0.0_wp ) THEN

                   !--- Subtract corrected tracer mass from current layer:
                   !    (zxtbor is negative)
       
                   pxtte(jl,jk,jt)=pxtte(jl,jk,jt)+zxtbor(jl)/(zdpg(jl,jk)*ztmst)
    
                   !--- Reset mass correction:
    
                   zxtbor(jl)=0.0_wp

                ELSE

                   !--- Adjust tendency to yield zero:

                   pxtte(jl,jk,jt)=-pxtm1(jl,jk,jt)/ztmst

                   !--- Add correcting mass mixing ratio and convert to [kg m-2]:
                   !    (implicit summation due to the inclusion
                   !     of zxtbor(jk-1) in zxtp1)

                   zxtbor(jl)=zxtp1*zdpg(jl,jk)

                END IF

             END DO
          END DO

          !--- If surface layer cannot compensate accumulated correction:
          !    Iterate from bottom to top

          DO jk=klev, 1, -1
             DO jl=1, kproma
                IF (zxtbor(jl) < -zeps) THEN

                   !--- Check if corrected mass, including the fix of the layer below,
                   !    yields a negative mixing ratio:
                   !    (Convert mass correction converted to [kg kg-1] in current layer)

                   zxtp1=pxtm1(jl,jk,jt)+pxtte(jl,jk,jt)*ztmst + zxtbor(jl)/zdpg(jl,jk)

                   IF ( zxtp1 > 0.0_wp ) THEN

                      !--- Subtract corrected tracer mass from current layer:
                      !    (zxtbor is negative)

                      pxtte(jl,jk,jt)=pxtte(jl,jk,jt)+zxtbor(jl)/(zdpg(jl,jk)*ztmst)

                      !--- Reset mass correction:

                      zxtbor(jl)=0.0_wp

                   ELSE

                      !--- Adjust tendency to yield zero:

                      pxtte(jl,jk,jt)=-pxtm1(jl,jk,jt)/ztmst
                   
                      !--- Add correcting mass mixing ratio and convert to [kg m-2]:
                      !    (implicit summation due to the inclusion
                      !     of zxtbor(jk-1) in zxtp1)
                   
                      zxtbor(jl)=zxtp1*zdpg(jl,jk) 
                   
                   END IF
                   
                END IF
             END DO 
          END DO
       
       !-->hhermann
       !END IF 
       !<--hhermann
    END DO
          
  END SUBROUTINE xt_borrow_lite
  
END MODULE mo_xt_borrow_lite