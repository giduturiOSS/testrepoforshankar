     fdualf1    if   e           k disk                                                             
     fdualf2    uf a e           k disk                                                             
     fDUALAD    cf   e             WORKSTN INFDS(INFDS)                                             
     f                                     SFILE(SUB01:RRN1)                                        
     f                                     SFILE(SUB02:RRN2)                                        
      * Program Info                                                                                
     d                SDS                                                                           
     d  @PGM                 001    010                                                             
     d  @JOB                 244    253                                                             
     d  @USER                254    263                                                             
                                                                                                    
      *  Field Definitions.                                                                         
     d EndScreen1      s               n                                                            
     d lastrrn1        s              4  0                                                          
     d lastrrn2        s              4  0                                                          
     d LenStr          s              4  0                                                          
     d messagecsc      s             10i 0                                                          
     d messagedata     s             80A                                                            
     d messagekey      s              4A                                                            
     d messagelen      s             10i 0                                                          
     d messagefile     s             20    inz('LBIMSG    *LIBL')                                   
     d messageid       s              7                                                             
     d Q               s              1    inz('''')                                                
     d Rcvar           s           5000A   Inz                                                      
     d RecordType      s              1    Inz('O')                                                 
     d ReloadSUB01     s               n                                                            
     d RRN1            s                   like(SCRRN1)                                             
     d RRN2            s                   like(SCRRN1)                                             
     d SavRrn1         s                   like(SCRRN1)                                             
     d SavRrn2         s                   like(SCRRN1)                                             
     d Screenerror1    s               n                                                            
     d worktitle       s             30                                                             
     d Varlen          s             10i 0 Inz(5000)                                                
     d whichOne        S              1  0                                                          
                                                                                                    
     d APIError        ds                  Qualified                                                
     d  BytesP                 1      4I 0 inz(%size(apiError))                                     
     d  BytesA                 5      8I 0 inz(0)                                                   
     d  Messageid              9     15                                                             
     d  Reserved              16     16                                                             
     d  messagedta            17    256                                                             
                                                                                                    
     d Infds           ds                                                       INFDS data structure
     d Choice                369    369                                                             
     d Currec                378    379I 0                                                          
                                                                                                    
      //  colors                                                                                    
     d Normal          c                   X'20'                                                    
     d White           c                   X'22'                                                    
                                                                                                    
      //  external called programs                                                                  
     d $clearmsg       pr                  extpgm('QMHRMVPM')                                       
     d   messageq                   276a   const                                                    
     d   CallStack                   10i 0 const                                                    
     d   Messagekey                   4a   const                                                    
     d   messagermv                  10a   const                                                    
     d   ErrorCode                  256                                                             
                                                                                                    
     d $sendmsg        PR                  ExtPgm('QMHSNDPM')                                       
     d   MessageID                    7A   Const                                                    
     d   QualMsgF                    20A   Const                                                    
     d   MsgData                    256A   Const                                                    
     d   MsgDtaLen                   10I 0 Const                                                    
     d   MsgType                     10A   Const                                                    
     d   CallStkEnt                  10A   Const                                                    
     d   CallStkCnt                  10I 0 Const                                                    
     d   Messagekey                   4A                                                            
     d   ErrorCode                  256A                                                            
                                                                                                    
      // Command Keys                                                                               
     d Cmd01           c                   const(x'31')                         Cmd-1               
     d Cmd02           c                   const(x'32')                         Cmd-2               
     d LeaveProgram    c                   const(x'33')                         Cmd-3               
     d Cmd04           c                   const(x'34')                         Cmd-4               
     d Cmd05           c                   const(x'35')                         Cmd-5               
     d Cmd06           c                   const(x'36')                         Cmd-6               
     d Cmd07           c                   const(x'37')                         Cmd-7               
     d Cmd08           c                   const(x'38')                         Cmd-8               
     d Cmd09           c                   const(x'39')                         Cmd-9               
     d Cmd10           c                   const(x'3A')                         Cmd-10              
     d Cmd11           c                   const(x'3B')                         Cmd-11              
     d Cmd12           c                   const(x'3C')                         Cmd-12              
     d Cmd13           c                   const(x'B1')                         Cmd-13              
     d Cmd14           c                   const(x'B2')                         Cmd-14              
     d TogWhichOne     c                   const(x'B3')                         Cmd-15              
     d SelectIt        c                   const(x'B4')                         Cmd-16              
     d Cmd17           c                   const(x'B5')                         Cmd-17              
     d Cmd18           c                   const(x'B6')                         Cmd-18              
     d Cmd19           c                   const(x'B7')                         Cmd-19              
     d Cmd20           c                   const(x'B8')                         Cmd-20              
     d Cmd21           c                   const(x'B9')                         Cmd-21              
     d Cmd22           c                   const(x'BA')                         Cmd-22              
     d Cmd23           c                   const(x'BB')                         Cmd-23              
     d Cmd24           c                   const(x'BC')                         Cmd-24              
     d EnterKey        c                   const(x'F1')                                             
     d RollUp          c                   const(x'F5')                         Roll Up             
     d RollDown        c                   const(x'F4')                         Roll Down           
                                                                                                    
      //  Procedures                                                                                
      /copy qpgmsrc,oen1ctrl                                                                        
      *                                                                                             
      /free                                                                                         
                                                                                                    
       //--------------------------------------------------------                                   
       // Main Program Processing                               -                                   
       //--------------------------------------------------------                                   
         exsr Hskpg;                                                                                
         exsr $Screen1;                                                                             
                                                                                                    
         *inlr = *on;                                                                               
                                                                                                    
        //----------------------------------------------                                            
        // $Screen1 - Set/Remove entries                                                            
        //----------------------------------------------                                            
        begsr $Screen1;                                                                             
                                                                                                    
         //Set the screen heading                                                                   
                                                                                                    
          workTitle = 'Maintain User Groups';                                                       
          LenStr =                                                                                  
          ((%len(workTitle) - %len(%trim(workTitle))) / 2) + 1;                                     
          %subst(C1TITLE:LenStr) = %trim(workTitle);                                                
                                                                                                    
         WhichOne = 1;                                                                              
         ExSr $ClearSFL1;                                                                           
         ExSr $LoadSfl1;                                                                            
                                                                                                    
         reset  EndScreen1;                                                                         
         dow  EndScreen1 = *off;                                                                    
                                                                                                    
          if ScreenError1 = *off;                                                                   
           $clearmsg('*' : *zero : *Blanks : '*ALL' : APIError);                                    
          endif;                                                                                    
                                                                                                    
          Write Header;                                                                             
          Write Footer;                                                                             
          If whichOne = 1;                                                                          
           Write Sub02Ctl;                                                                          
           ExFmt Sub01Ctl;                                                                          
           if Currec <> *Zeros;                                                                     
            RRN1 = Currec;                                                                          
            SCRRN1 = Currec;                                                                        
           endif;                                                                                   
          else;                                                                                     
           Write Sub01Ctl;                                                                          
           ExFmt Sub02Ctl;                                                                          
           if Currec <> *Zeros;                                                                     
            RRN2 = Currec;                                                                          
            SCRRN2 = Currec;                                                                        
           endif;                                                                                   
          endif;                                                                                    
          $clearmsg('*' : *zero : *Blanks : '*ALL' : APIError);                                     
          reset ScreenError1;                                                                       
                                                                                                    
          select;                                                                                   
           // F3 pressed end the program F3 = LeaveProgram                                          
           when  Choice = LeaveProgram;                                                             
            EndScreen1 = *on;                                                                       
                                                                                                    
           when Choice = TogWhichOne;                                                               
            If WhichOne = 1;                                                                        
             WhichOne = 2;                                                                          
            else;                                                                                   
             WhichOne = 1;                                                                          
            endif;                                                                                  
            exsr $clearsfl1;                                                                        
            exsr $loadsfl1;                                                                         
                                                                                                    
           when Choice = SelectIt;                                                                  
            If WhichOne = 1;                                                                        
             //Move from sub1 to sub2                                                               
             Chain where1 Sub01;                                                                    
             If %Found(DUALAD);                                                                     
              setll (@user : s1user) dualf2;                                                        
              if not%equal(dualf2);                                                                 
               //Write the record to the file                                                       
               dgroup = @user;                                                                      
               duser = s1user;                                                                      
               write dualf2r;                                                                       
              endif;                                                                                
             endif;                                                                                 
            else;                                                                                   
             //Remove                                                                               
             Chain where2 Sub02;                                                                    
             If %Found(DUALAD);                                                                     
              Chain (@user: s2user) dualf2;                                                         
              If %Found(dualf2);                                                                    
               delete dualf2r;                                                                      
              endif;                                                                                
             endif;                                                                                 
            endif;                                                                                  
                                                                                                    
            exsr $clearsfl1;                                                                        
            exsr $loadsfl1;                                                                         
                                                                                                    
           // Enter Key pressed                                                                     
           when  Choice = enterKey;                                                                 
                                                                                                    
            exsr $clearsfl1;                                                                        
            exsr $loadsfl1;                                                                         
                                                                                                    
           endsl;                                                                                   
          enddo;                                                                                    
                                                                                                    
        endsr;                                                                                      
                                                                                                    
        //----------------------------------------                                                  
        // $clearSfl1 - clear side by side subfiles                                                 
        //----------------------------------------                                                  
        begsr $clearSFL1;                                                                           
                                                                                                    
         // clear the subfile first                                                                 
                                                                                                    
          *in31 = *Off;                                                                             
          *in32 = *Off;                                                                             
          *in30 = *On;                                                                              
                                                                                                    
          *in35 = *Off;                                                                             
          *in36 = *Off;                                                                             
          *in34 = *On;                                                                              
                                                                                                    
          write SUB01CTL;                                                                           
          write SUB02CTL;                                                                           
                                                                                                    
          *in31 = *On;                                                                              
          *in32 = *On;                                                                              
          *in30 = *Off;                                                                             
                                                                                                    
          *in35 = *On;                                                                              
          *in36 = *On;                                                                              
          *in34 = *Off;                                                                             
                                                                                                    
          clear RRN1;                                                                               
          clear SCRRN1;                                                                             
          clear SavRrn1;                                                                            
                                                                                                    
          clear RRN2;                                                                               
          clear SCRRN2;                                                                             
          clear SavRrn2;                                                                            
                                                                                                    
        endsr;                                                                                      
                                                                                                    
        //--------------------------------------------------------                                  
        // $loadsfl1 - Load both side by side sfl's                                                 
        //--------------------------------------------------------                                  
        begsr $loadsfl1;                                                                            
                                                                                                    
         if  SavRrn1 > *zeros;                                                                      
          RRN1  =  SavRrn1;                                                                         
          SCRRN1=  SavRrn1;                                                                         
         endif;                                                                                     
                                                                                                    
         if  SavRrn2 > *zeros;                                                                      
          RRN2  =  SavRrn2;                                                                         
          SCRRN2=  SavRrn2;                                                                         
         endif;                                                                                     
                                                                                                    
         // load subfile# 1                                                                         
                                                                                                    
         setll *start  dualf1;                                                                      
         read dualf1;                                                                               
         dow not%eof(dualf1);                                                                       
          s1user = DUSER;                                                                           
          s1uname = DNAME;                                                                          
          RRN1 += 1;                                                                                
          SCRRN1 = RRN1;                                                                            
          write SUB01;                                                                              
          read dualf1;                                                                              
         enddo;                                                                                     
                                                                                                    
                                                                                                    
         // load subfile# 2                                                                         
                                                                                                    
         setll (@user) dualf2;                                                                      
         reade (@user) dualf2;                                                                      
         dow not %eof(dualf2);                                                                      
          //load sfl2                                                                               
          s2user = DUSER;                                                                           
          chain (s2user) dualf1;                                                                    
          if %found(dualf1);                                                                        
           s2uname = DNAME;                                                                         
          endif;                                                                                    
                                                                                                    
          RRN2 += 1;                                                                                
          SCRRN2 = RRN2;                                                                            
          write SUB02;                                                                              
          reade (@user) dualf2;                                                                     
         enddo;                                                                                     
                                                                                                    
         *in33 = *On;                                                                               
         *in37 = *On;                                                                               
         savrrn1 = SCRRN1;                                                                          
         savrrn2 = SCRRN2;                                                                          
                                                                                                    
         //  If no records in subfile then do not disply the subfile.                               
         if SavRrn1  = *zeros;                                                                      
          *in31 = *off;                                                                             
         else;                                                                                      
          //If we updated selected units make sure RRN is valid                                     
          If LastRRN1 > *zeros and LastRRN1 <= SavRRN1;                                             
           RRN1 = LastRRN1;                                                                         
           ScRRN1 = LastRRN1;                                                                       
          else;                                                                                     
           RRN1 = 1;                                                                                
           SCRRN1 = 1;                                                                              
          endif;                                                                                    
         endif;                                                                                     
                                                                                                    
         //  If no records in subfile then do not disply the subfile.                               
         if SavRrn2  = *zeros;                                                                      
          *in35 = *off;                                                                             
         else;                                                                                      
          //If we updated selected units make sure RRN is valid                                     
          If LastRRN2 > *zeros and LastRRN2 <= SavRRN2;                                             
           RRN2 = LastRRN2;                                                                         
           ScRRN2 = LastRRN2;                                                                       
          else;                                                                                     
           RRN2  = 1;                                                                               
           SCRRN2  = 1;                                                                             
          endif;                                                                                    
         endif;                                                                                     
                                                                                                    
        endsr;                                                                                      
                                                                                                    
        //----------------------------------------                                                  
        // $sendmessage - send the program message                                                  
        //----------------------------------------                                                  
        begsr $sendmessage;                                                                         
                                                                                                    
         $sendmsg(messageID   :                                                                     
                  messageFile :                                                                     
                  messagedata :                                                                     
                  messageLen  :                                                                     
                  '*DIAG'     :                                                                     
                  @PGM        :                                                                     
                  messagecsc  :                                                                     
                  messagekey  :                                                                     
                  APIError                                                                          
                              );                                                                    
                                                                                                    
        endsr;                                                                                      
                                                                                                    
        //--------------------------------------------------------                                  
        // Hskpg - one time run subroutine                                                          
        //--------------------------------------------------------                                  
        begsr Hskpg;                                                                                
                                                                                                    
                                                                                                    
        endsr;                                                                                      
                                                                                                    
      /End-Free                                                                                     
