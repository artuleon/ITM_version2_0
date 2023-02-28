        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RWUPDT__genmod
          INTERFACE 
            SUBROUTINE RWUPDT(N,R,LDR,W,B,ALPHA,C,S)
              INTEGER(KIND=4) :: LDR
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: R(LDR,N)
              REAL(KIND=8) :: W(N)
              REAL(KIND=8) :: B(N)
              REAL(KIND=8) :: ALPHA
              REAL(KIND=8) :: C(N)
              REAL(KIND=8) :: S(N)
            END SUBROUTINE RWUPDT
          END INTERFACE 
        END MODULE RWUPDT__genmod
