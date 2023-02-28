        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HYBRD__genmod
          INTERFACE 
            SUBROUTINE HYBRD(FCN,N,X,FVEC,XTOL,MAXFEV,ML,MU,EPSFCN,DIAG,&
     &MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,LDFJAC,R,LR,QTF)
              INTEGER(KIND=4) :: LR
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              REAL(KIND=8) :: XTOL
              INTEGER(KIND=4) :: MAXFEV
              INTEGER(KIND=4) :: ML
              INTEGER(KIND=4) :: MU
              REAL(KIND=8) :: EPSFCN
              REAL(KIND=8) :: DIAG(N)
              INTEGER(KIND=4) :: MODE
              REAL(KIND=8) :: FACTOR
              INTEGER(KIND=4) :: NPRINT
              INTEGER(KIND=4) :: INFO
              INTEGER(KIND=4) :: NFEV
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              REAL(KIND=8) :: R(LR)
              REAL(KIND=8) :: QTF(N)
            END SUBROUTINE HYBRD
          END INTERFACE 
        END MODULE HYBRD__genmod
