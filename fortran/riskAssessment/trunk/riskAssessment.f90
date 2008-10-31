program riskAssessment
    implicit none
    integer::natk,ndef,ndefStart,natkStart,natkd,ndefd,atk,def,nsims,atkWins,defWins,i,a,b
    integer,dimension(3)::atkRoll
    integer,dimension(2)::defRoll
    
    nsims = 10000
    atkWins = 0
    defWins = 0
    
    write(*,*)'How many attackers?'
    read(*,*)natkStart
    write(*,*)'How many defenders?'
    read(*,*)ndefStart

    do i = 1,nsims
        natk = natkStart
        ndef = ndefStart
        do
                !determine number of atack die
            if(natk >= 4)then
                natkd = 3
            elseif(natk == 3)then 
                natkd = 2
            elseif(natk == 2)then 
                natkd = 1
            endif
            
                !determine number of defense die
            if(ndef >= 2)then
                ndefd = 2
            elseif(ndef == 1)then
                ndefd = 1 
            end if 
                
                ! Ininitialze and roll the dice
            atkroll = 0
            defroll = 0
            do atk = 1,natkd
                atkRoll(atk) = int(rand(0)*6)+1
            end do 
            do def = 1,ndefd
                defRoll(def) = int(rand(0)*6)+1 
            end do
        
                !see who won the roll 
            if(ndefd == 2 .and. natkd >= 2) then
                    !highest two dice
                if(maxval(atkRoll) > maxval(defRoll)) then 
                    ndef = ndef - 1
                else 
                    natk = natk - 1
                end if 
                    !second highest two dice
                a = maxloc(atkRoll,1,atkRoll<maxval(atkRoll))
                b = maxloc(defRoll,1,defRoll<maxval(defRoll))
                if( atkRoll(a) > atkRoll(b) )then 
                    ndef = ndef - 1 
                else
                    natk = natk - 1
                end if
            else
                if(maxval(atkRoll) > maxval(defRoll)) then 
                    ndef = ndef - 1
                else
                    natk = natk - 1
                end if
            end if
                
                !has anyone lost the match?
            if(ndef == 0)then 
                atkWins = atkWins + 1
                exit
            elseif(natk == 1)then 
                defWins = defWins + 1
                exit
            end if
            
        end do 
    end do 

    write(*,'(a,1i2,a)')'The chance that the atacker suceeds is ',int(dble(atkWins)/dble(nsims)*100),'%'

end program riskAssessment



