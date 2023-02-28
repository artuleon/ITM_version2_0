        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHKDER__genmod
          INTERFACE 
            SUBROUTINE CHKDER(M,N,X,FVEC,FJAC,LDFJAC,XP,FVECP,MODE,ERR)
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(M)
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              REAL(KIND=8) :: XP(N)
              REAL(KIND=8) :: FVECP(M)
              INTEGER(KIND=4) :: MODE
              REAL(KIND=8) :: ERR(M)
            END SUBROUTINE CHKDER
          END INTERFACE 
        END MODULE CHKDER__genmod
