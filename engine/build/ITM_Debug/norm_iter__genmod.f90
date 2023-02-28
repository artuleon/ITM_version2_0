        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:48 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NORM_ITER__genmod
          INTERFACE 
            SUBROUTINE NORM_ITER(N,X,FVEC,IFLAG)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              INTEGER(KIND=4) :: IFLAG
            END SUBROUTINE NORM_ITER
          END INTERFACE 
        END MODULE NORM_ITER__genmod
