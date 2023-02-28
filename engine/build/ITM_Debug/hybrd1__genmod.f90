        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HYBRD1__genmod
          INTERFACE 
            SUBROUTINE HYBRD1(FCN,N,X,FVEC,TOL,INFO)
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              REAL(KIND=8) :: TOL
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE HYBRD1
          END INTERFACE 
        END MODULE HYBRD1__genmod
