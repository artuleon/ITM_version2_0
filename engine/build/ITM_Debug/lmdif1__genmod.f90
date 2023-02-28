        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LMDIF1__genmod
          INTERFACE 
            SUBROUTINE LMDIF1(FCN,M,N,X,FVEC,TOL,INFO)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(M)
              REAL(KIND=8) :: TOL
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE LMDIF1
          END INTERFACE 
        END MODULE LMDIF1__genmod
