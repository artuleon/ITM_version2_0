        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE R1MPYQ__genmod
          INTERFACE 
            SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: A(LDA,N)
              REAL(KIND=8) :: V(N)
              REAL(KIND=8) :: W(N)
            END SUBROUTINE R1MPYQ
          END INTERFACE 
        END MODULE R1MPYQ__genmod
