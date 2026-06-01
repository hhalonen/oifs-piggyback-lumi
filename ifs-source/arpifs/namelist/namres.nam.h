!     ------------------------------------------------------------------
!*    NAMELIST NAMRES




! NFRRES = RESTART FILE INTERVAL
! NRESTS = LIST OF RESTART TIMES
!       (1) = NR (positive),
!          THEN NRESTS(2..)*NR  DEFINES time-steps for RESTART FILE CREATION
!       (1) = NR (negative),
!          THEN NRESTS(2..)*NR  DEFINES hours      for RESTART FILE CREATION
! LSDHM = .TRUE. if time stamp is written as 'ddddhhmm' ; else time stamp
!         is controlled by LINC

!     ------------------------------------------------------------------
NAMELIST/NAMRES/NFRRES,NRESTS,LSDHM

!     ------------------------------------------------------------------


