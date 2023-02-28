        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QRSOLV__genmod
          INTERFACE 
            SUBROUTINE QRSOLV(N,R,LDR,IPVT,DIAG,QTB,X,SDIAG)
              INTEGER(KIND=4) :: LDR
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: R(LDR,N)
              INTEGER(KIND=4) :: IPVT(N)
              REAL(KIND=8) :: DIAG(N)
              REAL(KIND=8) :: QTB(N)
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: SDIAG(N)
            END SUBROUTINE QRSOLV
          END INTERFACE 
        END MODULE QRSOLV__genmod
