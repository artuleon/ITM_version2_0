  ©  Q   k820309              2021.6.0    ;ıc                                                                                                          
       D:\ITMGitHub\ITM_version2_0\engine\itm\itm_accessors.f90 ITM_ACCESSORS                                                 
                                                      
                     @               A                'T            #SIZE    #CAPACITY    #INDEX    #X    #Y                                                                                                            0                                                                                                       0                                                                                                      1                                                      
        &                                                                    0         
        &                                     @                           	     '(            #INIT_OPENING 
   #TARGET_OPENING    #OPENING_RATE    #CONTROL_TSERIES    #CONTROL_NODE    #CONTROL_CURVE                                            
            
                                                      
                                                      
                                                                                                                                                                                    @                                '            #TSERIES    #BASELINE    #SCALE_FACTOR                                                                                                              
                                                      
                                                                                  5                                                                (               40+                                              (           &                           +                                              (           &                                                                                      &                       #INFLOW_T         @ @                                       T            &                       #TABLE_T    %     @                                                    
   #TABLE    #TIME    #EXTEND          
                                      T       #TABLE_T          
                                      
        
                                                @                                          
        &                                                                                 &                       %     @                                                     
   #TABLE !         
                                  !     T      #TABLE_T           @                               "        T            &                       #TABLE_T    %     @                                #                    
   #TABLE $   #X_VALUE %         
                                  $     T      #TABLE_T          
                                 %     
  %     @                                &                    
   #TABLE '   #Y_VALUE (         
                                  '     T      #TABLE_T          
                                 (     
       @                                 )        (            &                       #GATE_T 	   #     @                                   *                    #WANT_LINK +   #INDEX ,   #ID -         
                                  +             
                                  ,             D                                -     (           %     @                                .                       #NODE_INDEX /         
                                  /       #     @                                   0                    #NODE_INDEX 1   #TIME 2   #FLOW 3         
                                  1             
  @                              2     
        D                                3     
   %     @                                 4                    
   #NODE_INDEX 5         
                                  5       #     @                                  6                    #NODE_INDEX 7   #X 8         
                                  7             D                                8     
   #     @                                   9                    #NODE_INDEX :   #H ;   #Q <         
                                  :             
  @                              ;     
        D                                <     
   #     @                                   =                    #NODE_INDEX >   #MAX_HEAD ?         
  @                               >             D @                              ?     
   #     @                                   @                    #NODE_INDEX A   #DEPTH B   #STORAGE C         
                                  A             
  @                              B     
        D                                C     
   #     @                                   D                    #NODE_INDEX E   #STORAGE F   #DEPTH G         
                                  E             
  @                              F     
        D                                G     
   %     @                                 H                    
   #NODE_INDEX I   #TIME J   #DT K   #OPENING L         
                                  I             
  @                              J     
        
                                 K     
        
                                 L     
  #     @                                   M                    #NODE_INDEX N   #OPENING O   #COEFF P         
                                  N             
  @                              O     
        D                                P     
          O      fn#fn    ï   <   J   COMMON_MODULE    +  <   J   ITM_TABLE "   g  u       TABLE_T+ITM_TABLE '   Ü  }   a   TABLE_T%SIZE+ITM_TABLE +   Y  }   a   TABLE_T%CAPACITY+ITM_TABLE (   Ö  }   a   TABLE_T%INDEX+ITM_TABLE $   S  l   a   TABLE_T%X+ITM_TABLE $   ¿  l   a   TABLE_T%Y+ITM_TABLE %   +  ¶       GATE_T+COMMON_MODULE 2   á  @   a   GATE_T%INIT_OPENING+COMMON_MODULE 4   !  @   a   GATE_T%TARGET_OPENING+COMMON_MODULE 2   a  @   a   GATE_T%OPENING_RATE+COMMON_MODULE 5   ¡  @   a   GATE_T%CONTROL_TSERIES+COMMON_MODULE 2   á  @   a   GATE_T%CONTROL_NODE+COMMON_MODULE 3   !  @   a   GATE_T%CONTROL_CURVE+COMMON_MODULE '   a  q       INFLOW_T+COMMON_MODULE /   Ò  @   a   INFLOW_T%TSERIES+COMMON_MODULE 0     @   a   INFLOW_T%BASELINE+COMMON_MODULE 4   R  @   a   INFLOW_T%SCALE_FACTOR+COMMON_MODULE &     ]       STORAGE+COMMON_MODULE $   ï  ^       IDLEN+COMMON_MODULE &   M  h       LINK_ID+COMMON_MODULE &   µ  h       NODE_ID+COMMON_MODULE %   	  r       INFLOW+COMMON_MODULE &   	  q       TSERIES+COMMON_MODULE /    
  i       TABLE_TSERIES_LOOKUP+ITM_TABLE 5   i
  I   a   TABLE_TSERIES_LOOKUP%TABLE+ITM_TABLE 4   ²
  8   a   TABLE_TSERIES_LOOKUP%TIME+ITM_TABLE 6   ê
  8   a   TABLE_TSERIES_LOOKUP%EXTEND+ITM_TABLE +   "  d       YRES_JUN_OLD+COMMON_MODULE )     d       NODE_CURVE+COMMON_MODULE *   ê  S       TABLE_GET_X_MAX+ITM_TABLE 0   =  I   a   TABLE_GET_X_MAX%TABLE+ITM_TABLE $     q       CURVE+COMMON_MODULE '   ÷  `       TABLE_LOOKUP+ITM_TABLE -   W  I   a   TABLE_LOOKUP%TABLE+ITM_TABLE /      8   a   TABLE_LOOKUP%X_VALUE+ITM_TABLE /   Ø  `       TABLE_REVERSE_LOOKUP+ITM_TABLE 5   8  I   a   TABLE_REVERSE_LOOKUP%TABLE+ITM_TABLE 7     8   a   TABLE_REVERSE_LOOKUP%Y_VALUE+ITM_TABLE (   ¹  p       GATE_DATA+COMMON_MODULE     )  f       ITM_GET_SWMM_ID *     8   a   ITM_GET_SWMM_ID%WANT_LINK &   Ç  8   a   ITM_GET_SWMM_ID%INDEX #   ÿ  @   a   ITM_GET_SWMM_ID%ID    ?  X       ITM_HAS_INFLOW *     8   a   ITM_HAS_INFLOW%NODE_INDEX    Ï  h       ITM_GET_INFLOW *   7  8   a   ITM_GET_INFLOW%NODE_INDEX $   o  8   a   ITM_GET_INFLOW%TIME $   §  8   a   ITM_GET_INFLOW%FLOW #   ß  X       ITM_GET_NODE_DEPTH .   7  8   a   ITM_GET_NODE_DEPTH%NODE_INDEX (   o  [       ITM_GET_MAX_CURVE_VAL_X 3   Ê  8   a   ITM_GET_MAX_CURVE_VAL_X%NODE_INDEX *     8   a   ITM_GET_MAX_CURVE_VAL_X%X )   :  b       ITM_GET_Q_FROM_RAT_CURVE 4     8   a   ITM_GET_Q_FROM_RAT_CURVE%NODE_INDEX +   Ô  8   a   ITM_GET_Q_FROM_RAT_CURVE%H +     8   a   ITM_GET_Q_FROM_RAT_CURVE%Q (   D  b       ITM_GET_MAX_RATING_HEAD 3   ¦  8   a   ITM_GET_MAX_RATING_HEAD%NODE_INDEX 1   Ş  8   a   ITM_GET_MAX_RATING_HEAD%MAX_HEAD       l       ITM_GET_STORAGE +     8   a   ITM_GET_STORAGE%NODE_INDEX &   º  8   a   ITM_GET_STORAGE%DEPTH (   ò  8   a   ITM_GET_STORAGE%STORAGE &   *  l       ITM_GET_STORAGE_DEPTH 1     8   a   ITM_GET_STORAGE_DEPTH%NODE_INDEX .   Î  8   a   ITM_GET_STORAGE_DEPTH%STORAGE ,     8   a   ITM_GET_STORAGE_DEPTH%DEPTH %   >  w       ITM_GET_GATE_OPENING 0   µ  8   a   ITM_GET_GATE_OPENING%NODE_INDEX *   í  8   a   ITM_GET_GATE_OPENING%TIME (   %  8   a   ITM_GET_GATE_OPENING%DT -   ]  8   a   ITM_GET_GATE_OPENING%OPENING (     l       ITM_GET_GATE_LOSS_COEFF 3     8   a   ITM_GET_GATE_LOSS_COEFF%NODE_INDEX 0   9  8   a   ITM_GET_GATE_LOSS_COEFF%OPENING .   q  8   a   ITM_GET_GATE_LOSS_COEFF%COEFF 