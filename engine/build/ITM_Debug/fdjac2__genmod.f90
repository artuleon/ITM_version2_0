        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 27 23:29:46 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FDJAC2__genmod
          INTERFACE 
            SUBROUTINE FDJAC2(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN)
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              EXTERNAL FCN
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: FVEC(M)
              REAL(KIND=8) :: FJAC(LDFJAC,N)
              INTEGER(KIND=4) :: IFLAG
              REAL(KIND=8) :: EPSFCN
            END SUBROUTINE FDJAC2
          END INTERFACE 
        END MODULE FDJAC2__genmod
