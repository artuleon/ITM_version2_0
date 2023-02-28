        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LMDER1__genmod
          INTERFACE 
            SUBROUTINE LMDER1(FCN,M,N,X,FVEC,FJAC,LDFJAC,TOL,INFO)
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(M)
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              REAL(KIND=8) :: TOL
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE LMDER1
          END INTERFACE 
        END MODULE LMDER1__genmod
