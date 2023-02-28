        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HYBRJ__genmod
          INTERFACE 
            SUBROUTINE HYBRJ(FCN,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,DIAG, &
     &MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,R,LR,QTF)
              INTEGER(KIND=4) :: LR
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              REAL(KIND=8) :: XTOL
              INTEGER(KIND=4) :: MAXFEV
              REAL(KIND=8) :: DIAG(N)
              INTEGER(KIND=4) :: MODE
              REAL(KIND=8) :: FACTOR
              INTEGER(KIND=4) :: NPRINT
              INTEGER(KIND=4) :: INFO
              INTEGER(KIND=4) :: NFEV
              INTEGER(KIND=4) :: NJEV
              REAL(KIND=8) :: R(LR)
              REAL(KIND=8) :: QTF(N)
            END SUBROUTINE HYBRJ
          END INTERFACE 
        END MODULE HYBRJ__genmod
