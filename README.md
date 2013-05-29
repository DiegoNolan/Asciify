
Asciify is a library for making ascii art from images.

Some image types still cause problems.  There are 3 algrothims
implemented.  The Single algrothim takes the weighted mean of each rectangle
a character will cover and finds the appropriate character from that.  The
Quad method breaks the character in quandrants and tries to match both the
darkness and the shape of the character from the pixels.  The Novem method
breakes the character rectangle into 9 different regions and tries to match
a character.  There is still much improvement to be made.  Suggestion are
welcome.

You can test each algrothim in ghci by loading Asciify.hs and running

      testAllAlgorithms "path_to_file" 80

where 80 would be how many characters wide you want to results ascii art to
be.

![Goku](http://chainmailninja.com/wp-content/uploads/goku.png)

Novem Algorithm

                                   .                                               
                                    .                                              
                     _               .                                             
                     ?88cu.          '                                             
                      ?88888cu.       ]                                            
                       ?0Eb&88}>c_     "                                           
                        ?0EEEEbb88Gu    \                                          
                         ?2EEEEEEEb2Ec   [                                         
                          ?2EEEEEEEEEEb  "                                         
                           5GEEEEEEEEEEb  [                                        
                            0EEEEEEEEEEEE "                                        
                            "2EEEEEEEEEEEb L                                       
                _cw85558>cw_ ?GEEEEEEEEEEED;                                       
             _<8880GGEEEEBb&28GEEEEEEEEEEEEL                                       
           w88822bbbGEEEEEEEEEEbZEEEEEEEEEEEaaeL                                   
         u022bEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEe                                 
       _62bEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEL                               
      w2dEEEEEEEEEEEEEEEEEEEEEEE2GEEEEEEEEEEEEEEEEEEL__uuueeeen                    
     /dEEEEEEEEEEEEEEEEEEEEEEEE2GEEEEF7EEEEEEEEEEEEEEEEEEEEEELL_                   
    JMMMMEEEEEEEEEEEEEEEEEEEEEPGEEEF`/JEEEEF`NEEEEEEEEEEEEEEEEEEEEBbee__           
           '*3MEEEEEEEEEEEEEEE2EEE`   JEEEEeLJEEEEEEEEEEEEEEEEEEEEEEEEF5`          
                *3EEEEEEEEEEEE2EE`  ' JEEEMUP EEEEEEEEEEEEEEEEEEEEF5`              
                   *MEEEEEEEEEPEF   [ dEC_   J5EEE53EEEEEEEEEEER*                  
                 _edEEEEEE2EEC2@D    JP*  '.   3Eu[l2EEEEEEEE^.                    
              Lw22EEEEEEEE2EE&EEL    " [  L     '2C  EEEEEEEEEEF`                  
           .c2bEEEEEEEEEEE0EE}  ``       JE      '^  EEEEEEE5*                     
         udbEEEEEEEEEEEEEEEJEL .         _L      -} JEEEE5`                        
        44MMMMMMEEEEEEEEEEEEEB ' ve              _/ EEEEEF*                        
                   '**3MEEEEEEL   *.     `   !   luEEF5`                           
                         _2EEEE   'Lwr           P83L                              
                      udEEEEEEEB    '           8888[                              
                    =4MMMMEEEEEEL      f       ]  88l                              
                                '.           _6[  <82                              
                                  ' .       J8/    8}L                             
                                     ; _ _w88}     88L                             
                                     |  '.  } .    <80c                            
                                     ! ' ' f u     '#T<0uueeL                      
                                    "'    f  6        ubGEEEEu____                 
                               __.-"   ._wL_J8_    _<2GEEEEG558888}52wu            
                            ueEB` -.>-'        _LH82dEEEB5888882baEEEEEb           
                         _d&F*`  .r`    ._uedBE52bEEEB5888882bEEEEEEEEEEbL         
                     __c82E*    u[ _uedEEEEE5882EEEB58888882EEEEEEEEEEEEEEL        
                 _w688082ECL__udGGEEEEEEE588882EE5888888882EEEEEEEEEEEEEEEEL       
                 P888082EEEEEEEEEEEEEEEE888882B88888888882EEEEEEEEEEEEEEEEEE       
                /888882EEEEEEEEEEBEEEB88888888888888888882EEEEEEEEEEEMMMMEEEEL     
               &P888882EEEEEEEEEEEEE588888888888888888882EEEEEEEEEEP`' L '8EEEbL   
              &B088082EEEEEEEEEEEER888888888888888888888&EEEEEEE5* ,   ['.0EEEED   
             #EB082082EEEEEEEEEEB58888888888888888888882EEEEEEE8   'w /  uP<EEE[   
           uGEE888}882EEEEEEEEEM88888888008888882888888EEEEEE* ''   ?.  ]`  4EE    
          dEEEB8828882EEEEEEEE5888882bd5888888888G88880EEEEB`      '-  '     QF    
         JEEEEP8888880EEEEEEB08882bEEB888888888880EEEBbEEEE          ".      J0_   
          ?EEB88888888@EEEEE8882dEEB088888888888882EEEEEEEE           'L     ?[<L  
           3EP888288882EEEE8882GEE58888822bEEbb2880EEEEEEEE            ?     J  L  
            ?888880888}EEE8888GEB88882bGEEEEEEEEEBb2EEEEEEEL            Y       ba 
             08888288882E8888dB0882bGEEEEEEEEEEEEEEEEEEEEEEB             L     J3E 
             0888880888}P88285882bEEEEEEEEEEEEEEEEEEEEEEEEEEb            l     Y 3 
             '`''''*`'''''*`''''*****************************                  "   

Quad Algorithm

                                   ,                                               
                                   J,                                              
                    ._              J,                                             
                     7OO__,          J,                                            
                      7OOOOOh_        J,                                           
                       7OQjjOOOO_,     .                                           
                        7O@@@djjOOF_    \                                          
                         79@@@@@@@jQ#k   ,                                         
                          7]@@@@@@@@@\\  .                                         
                           79@@@@@@@@@kL  (                                        
                           JO@@@@@@@@@@@L j                                        
                            J9@@@@@@@@@@@L ,                                       
                __OOOOOOOh__ 79@@@@@@@@@@@k'                                       
             _jOOOOOOOjjQjjj\O9@@@@@@@@@@@@/                                       
           _OOOOOOOQjkj@@@@@@@kj?9@@@@@@@@@@j_d_                                   
         _OOOOOQjd@@@@@@9k@@@@@@@@@@@@@@@@@@@@@@kd                                 
       .jOOOQjj@@@@@@@@@@@@@@@@@@Y@@@@@@@@@@@@@@@@k_                               
      _OQj@@@@@@@@@@@@@@@@@@@J@@Ed@@@@@@@@@@@@@@@@@@____dddddd_                    
     jjd@@@@@@@@@@@@@@@@@@@@@@FOd@@@@"T@@@@@@@@@@@@@@@@k@j@@#h,                    
    jYYYY@@@@@@@@@@@@@@@@@@@@@FO@@@"'jjO@@@"'9@@@@@@@@@@@@@@@%OO@@jjdd_,           
           J'"*@@@@@@@@@@@@@@@O@@F` . j]@@kk,_@@@@@@@@@@@@@@@@@@@@@@@@""'          
                J"@@@@@@@@@@@@O@@`  , jd@#YF(.@@@@@@@@@@@@@@@@@@@@#"'              
                   `9@@@@@%@@#O@F   ( J@F_   _"@@#FY@@@@@@@@@@@"L                  
                 _dj@@@@@@O9@Lj9b    ,(/  J    79\(\7@@@@@@@#F                     
              _jOQ@@@@@@@@O@@d@9k    ' (  ,     JjO j@@@@@@@@@@"`                  
           .jOjd@@@@@@@@@@\9@9  ''       ]@      JO,.@@@@@@#"L                     
         _jj@@@@@@@@@@@@@@k?@<           .h J    jO ,@@@@"'                        
        `"""YYY9@@@@@@@@@@@@@@   ._              .T.@@@@@""                        
                   J'""*9@@@@@_   '      '       O_d@#"'                           
                         _d@@@k   '__,          .OO9'                              
                      __@@@@@@@L                jOOO(                              
                    jYYYY@@@@@@@,     .T'      j'JOOh                              
                                             .O(  jOQ                              
                                           .jOT   jOOb                             
                                     '   _jOOT     OO(                             
                                         _  T',    7O%_                            
                                    .'   ' j .'    j9'OO_dd__                      
                                          J  O     J' _\9FOOd____                  
                               _   '     j  jO_,   _jOOOOj?YFOOOOOOOOO_            
                            __@@L  ._J'        _jOOOOOQdYFOOOOOQjj@@@@kh           
                         _jd#"'  .j'     __djdYFOOOOj#FOOOOOQj@@@@@@@@@@L,         
                     .__OQ@L    j' __dj@@@@#FOOOOOQ9FOOOOOOQ@@@@@@@@@@@@@@_        
                 ._jOOOOQ@L___d_kd@@@@@@@YOOOOOOO9OOOOOOOOQ@@@@@@@@@@@@@@@@_       
                .FOOOOOQ@@@@@@@@@@@@@@@YOOOOOOOOOOOOOOOOOOd@@@@@@@@@@@@@@@@k       
                /OOOOOO@@@@@@@@@@@@@@#OOOOOOOOOOOOOOOOOOO]@@@@@@@@@@@@9Y9@@@L_     
               JFOOOOO]@@@@@@@@@@@@#FOOOOOOOOOOOOOOOOOOOQ@@@@@@@@@#"'' , 'O@@@_,   
              J#OOOOOO@@@@@@@@@@@@YOOOOOOOOOOOOOOOOOOOOOd@@@@@@@YL',   ' .9@@@@(   
            .J@kOOOOO]@@@@@@@@@@#FOOOOOOOOOOOOOOOOOOOOOQ@@@@@@#F'  J( j  jT7@@@(   
           _@@@%OOOOO]@@@@@@@@@#OOOOOOOOOOOOOOOOOOOOOOOd@@@@@L J' . 7. .j' J7@@    
          _@@@#OOOOOO]@@@@@@@@FOOOOOOOQOOOOOOOOOOEOOOOO@@@@#'      J7,      JJ(    
         ,@@@@FOOOOOOO@@@@@@@FOOOOOOQ9OOOOOOOOOOO9@OOOO@@@@'         ',      7h,   
          7@@%OOOOOOOO@@@@@#OOOOOOO\FOOOOOOOOOOOOO9kOOO]@@#           J,     jTO`  
           7@FOOOOOOOO]@@@#OOOOOOQYOOOOOOjjjkj\OOOOY%OOO@@k            \     j J(  
            _OOOOOOOOOO@@#OOOOOQ?OOOOQj@@@@@@@@@j\OO]jOO9@@_            h    j  _d 
             \OOOOOOOOO]#OOOOO\FOOOjd#YYOOOOOOOOYYkOO9YEjd@L            J,     .7@ 
             OOOOOO\OOOOFOOOOFOOQj#YOOOOOOOOOOOOOOOOOOOO]@@@L            \     J 7 
             ''''''`''''''`L''''"L''''''''''''''''''''''"""""            J       J 

Single Algorithm

                                   ,                                               
                                   ,,                                              
                    `-`             ,,                                             
                     +==+-,          ,,                                            
                      ~=====+-,       ,`                                           
                       ~iOTT====+-     -                                           
                        ~T0ZZ0OTiiT+`   ~                                          
                         +TZMMMMZ0OOO=  `,                                         
                          =OMMMMMMMZZOT` -                                         
                          `i0MMMMMMMMMZT` -                                        
                           ,iZMMMMMMMMMMO -                                        
                            -TMMMMMMMMMMMO`-                                       
               `~+==iii==+~-`=0MMMMMMMMMMMi-                                       
             ~==++=iTOOOOOTTTTTMMMMMMMMMMMM=                                       
          `+i+==iTTTOOOO000O000OOZMMMMMMMMMM0Oi~                                   
         ~i=iTOOOO000000OO00O00ZMMMMMMMMMMMMMMMMZ=`                                
       ,=iTOO00ZZZZZZZZZZ00O0ZMMM0ZZMMMMMMMMMMMMMMZ~                               
      +iT0ZMMMMMMMMMMMMMMMMMMMMMT0ZMMMMZ00MMMMMMMMMM+-~+++===i+`                   
     =OZMMMMMMMMMMMMMMMMMMMMMMMiOMMMMOi00MMMMMMMMMMMZ00O00ZMZ+,,`                  
    =OOO0ZMMMMMMMMMMMMMMMMMMMMiTZMMO++~OZMMT+ZMMMMMMMMZMMMMZ0OOOOOOTi+~-`          
           ,~=OMMMMMMMMMMMMMMM=0MM~~+,+OMM0i+iMMMMMMMMMMMMMMMMMMMMMMMMO=,          
                -iMMMMMMMMMZZMiZM=~-~`~ZMZ0T=+MMMMMMMMMMMMMMMMMMMM0i-              
                   +ZMMMMM00MZiMO~~,-,0Mi+~~~iTZMMTOMMMMMMMMMMMT+`                 
                `-=T0ZMMMMT0MiiZ=~-,, ++  ,~~~~iZ==+OMMMMMMMZi,`                   
             `-+iT00000MMMiZMZMZi~,``-`, `-`~~~~+i=-+MMMMMMMMMMO~                  
           ,+TT0ZZMMMMMMMMi0MT,,++,````- +M,~~~~~+i~~MMMMMMZT~`                    
         ~T0ZMMMMMMMMMMMMMMTMi,- `,````` ,+-~~~~~=i~iMMMMT-                        
        +iTTOO0ZMMMMMMMMMMMMMZ-- ,T````,,,,``~~~~+=+MMMMMO=`                       
                  `-~+iOZMMMMM~,` ~-````,~```-~~~==ZM0i-                           
                        `+0ZMMZ`,~-+++`,-````,~~+i=T-                              
                     `~TZZMMMMMi  `,---,``````~~====~                              
                   `=O00ZZZZMMMM- ````-=-````,~=+~===                              
                              ``,, `````````,+i+~~+=0                              
                                  ,,,``````-=i=~~~+==,                             
                                    `~---~=i=i~~~~~===                             
                                     -`,-+~~=~~~~~~+=T+-                           
                                    `~`-,~-+,++~~~~+T==i++==-                      
                                   `-,````~`-i~~~~~~+~=TOOOOO~---,``               
                               -,,--,``--~--+i+~~~~+=TTOOOOOTi====iiTi~            
                            -iZZ=,,-+--```````,+iiiiTOOOOi=====TO0MMMM0i           
                         -=ZO+-``-+-````-~=iO00TiTOOOOT=====i0MMZZZZZZMMO-         
                    `-~+iTZ=```,+~,~=iO0ZZM0T==iOOOOi======TMMZZ00ZZZZZMMM~        
                 ,~=i=i=TZO~~+=T00Z0ZMMMM0i===iOOTi=======iMMZ00ZZZZZZMMMMM-       
                `i=====TMZZ0000000ZZMMM0i====iTi==========ZMZZZ000000ZZZZZMZ`      
                =======MMZZZ00ZZZZZZZOi======i===========OMMMZZ000ZZM0Z00ZZZ0-     
              `Oii====0Z0ZZZZZZZZZZZi===================iMMZZ000ZZ0T+~~=~+iMMMO,   
             `O0i==i=iMZ00000ZZMMMO=====================ZMZZZ0ZZ0=~+` ,+-+TMMMM+   
            ,O00i==i=OM000Z0000Z0i=====================TMZMZZZZi~,`~+`~,~+=iMMM-   
           +0ZZO==i==0MZZZZZZZZ0========ii======ii=====ZMMZZZi--~~--=~-~=+~~ZMM    
         `TZ0Z0==ii==TMZZZ000ZT=====iTTT=========Oi===iMMMZZ+,```,-+=~~~-`,~~0~    
         -Z0ZMi==i===iMZ0ZZZZi===iTOOTi==========i0OTTTMMZM+,` ````,~++-` `,~=iM   
          iZ0O===i====ZZZMMZi===TOOOi=============O0OOO0MZ0-`    ```,~+-`  `-+=+,  
           =Zi===i====OMMMZ====TOOT=====iTO00OTii=iOOOOOZMZ,``    ```-~+`   `+-~i  
            +=====i===iMM0====TOTi===iO000000000OOTTOOOOOMM~``     ```-~+`  `~,,OT+
             T====i====0Z====TOi==iT00OOOOOOOOOOOOOOOOOOO0M0,``     ```~+-  `,`-iM,
            `T=====T====i==iTT==iO0OOOOOOOOOOOOOOOOOOOOOO000T```     ``,~+` ```~,T,
            ,ii====Oi=====OO====Z0OOOOOOOOOOOOOOOOOOOOOO0000Z-```     `,~=`````~`~,
