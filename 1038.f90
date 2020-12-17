program zz
    integer(8)::a,b,c,d,e,f,g
    read(*,*)a
    b=0
    c=-1
    do
        d=1
        f=-1
        g=0
        do while((b/d)/=0)
            e=mod(b/d,10)
            if(e<=f)then
                g=1
                exit
            end if
            f=e
            d=d*10
        end do
        if(g==0)then
            c=c+1
        end if
        if(c==a)then
            write(*,'(i0)')b
            stop
        end if
        if(b>9876543210)then
            write(*,'(i0)')-1
            stop
        end if
        b=b+1
    end do
end program zz