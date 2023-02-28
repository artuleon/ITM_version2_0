        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE R1UPDT__genmod
          INTERFACE 
            SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
              INTEGER(KIND=4) :: LS
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: S(LS)
              REAL(KIND=8) :: U(M)
              REAL(KIND=8) :: V(N)
              REAL(KIND=8) :: W(M)
              LOGICAL(KIND=4) :: SING
            END SUBROUTINE R1UPDT
          END INTERFACE 
        END MODULE R1UPDT__genmod
