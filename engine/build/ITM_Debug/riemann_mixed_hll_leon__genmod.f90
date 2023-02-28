        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:50 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RIEMANN_MIXED_HLL_LEON__genmod
          INTERFACE 
            SUBROUTINE RIEMANN_MIXED_HLL_LEON(J,SUMIDAROUND,IDL,IDR,HL, &
     &HR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,INTER_BOUND,QB)
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: SUMIDAROUND
              INTEGER(KIND=4) :: IDL
              INTEGER(KIND=4) :: IDR
              REAL(KIND=8) :: HL
              REAL(KIND=8) :: HR
              REAL(KIND=8) :: AL
              REAL(KIND=8) :: AR
              REAL(KIND=8) :: QL
              REAL(KIND=8) :: QR
              REAL(KIND=8) :: FF1L
              REAL(KIND=8) :: FF1R
              REAL(KIND=8) :: FF2L
              REAL(KIND=8) :: FF2R
              INTEGER(KIND=4) :: INTER_BOUND
              REAL(KIND=8) :: QB
            END SUBROUTINE RIEMANN_MIXED_HLL_LEON
          END INTERFACE 
        END MODULE RIEMANN_MIXED_HLL_LEON__genmod
