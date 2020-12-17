program zz
    integer::a,b,c,d,z
    read(*,*)a
    do z=1,a
        read(*,*)b,c
        d=0
        if(b<=21)d=d+10
        if(b<=15)d=d+20
        if(b<=10)d=d+20
        if(b<=6)d=d+150
        if(b<=3)d=d+100
        if(b<=1)d=d+200
        if(b==0)d=d-500
        if(c<=31)d=d+32
        if(c<=15)d=d+32
        if(c<=7)d=d+64
        if(c<=3)d=d+128
        if(c<=1)d=d+256
        if(c==0)d=d-512
        write(*,'(i0)')d*10000
    end do
end program zz