        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QFORM__genmod
          INTERFACE 
            SUBROUTINE QFORM(M,N,Q,LDQ)
              INTEGER(KIND=4) :: LDQ
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: Q(LDQ,M)
            END SUBROUTINE QFORM
          END INTERFACE 
        END MODULE QFORM__genmod
