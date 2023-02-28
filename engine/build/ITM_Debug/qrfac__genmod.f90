        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QRFAC__genmod
          INTERFACE 
            SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,RDIAG,ACNORM)
              INTEGER(KIND=4) :: LIPVT
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: A(LDA,N)
              LOGICAL(KIND=4) :: PIVOT
              INTEGER(KIND=4) :: IPVT(LIPVT)
              REAL(KIND=8) :: RDIAG(N)
              REAL(KIND=8) :: ACNORM(N)
            END SUBROUTINE QRFAC
          END INTERFACE 
        END MODULE QRFAC__genmod
