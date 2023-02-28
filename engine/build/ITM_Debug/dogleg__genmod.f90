        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DOGLEG__genmod
          INTERFACE 
            SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X)
              INTEGER(KIND=4) :: LR
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: R(LR)
              REAL(KIND=8) :: DIAG(N)
              REAL(KIND=8) :: QTB(N)
              REAL(KIND=8) :: DELTA
              REAL(KIND=8) :: X(N)
            END SUBROUTINE DOGLEG
          END INTERFACE 
        END MODULE DOGLEG__genmod
