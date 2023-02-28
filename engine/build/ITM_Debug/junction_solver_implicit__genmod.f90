        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:51 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE JUNCTION_SOLVER_IMPLICIT__genmod
          INTERFACE 
            SUBROUTINE JUNCTION_SOLVER_IMPLICIT(N,X,FVEC,IFLAG)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(N)
              INTEGER(KIND=4) :: IFLAG
            END SUBROUTINE JUNCTION_SOLVER_IMPLICIT
          END INTERFACE 
        END MODULE JUNCTION_SOLVER_IMPLICIT__genmod
