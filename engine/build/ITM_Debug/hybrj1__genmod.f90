        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HYBRJ1__genmod
          INTERFACE 
            SUBROUTINE HYBRJ1(FCN,N,X,FVEC,FJAC,LDFJAC,TOL,INFO)
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              REAL(KIND=8) :: TOL
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE HYBRJ1
          END INTERFACE 
        END MODULE HYBRJ1__genmod
