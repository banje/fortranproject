program zz
    integer::a,d,e
    integer,dimension(10000)::b
    character(len=15)::c
    read(*,*)a
    d=1
    do while(a>0)
        read(*,'(a)')c
        if(c(1:4)=='push') then
            read(c(6:),'(i6)')e
            b(d)=e
            d=d+1
        else if(c=='pop') then
            if(d==1) then
                write(*,'(i0)')-1
            else
                d=d-1
                write(*,'(i0)')b(d)
            end if
        else if(c=='size') then
            write(*,'(i0)')d-1
        else if(c=='empty') then
            if(d==1) then
                write(*,'(i0)')1
            else
                write(*,'(i0)')0
            end if
        else
            if(d==1) then
                write(*,'(i0)')-1
            else
                write(*,'(i0)')b(d-1)
            end if
        end if
        a=a-1
    end do
end program zz