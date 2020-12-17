program zz
    integer(8)::a,c,e
    character::b,d
    character(len=65)::h
    integer::f,g,i,z
    read(*,*)g
    do while(g>0)
        read(*,'(a)')h
        i=len_trim(h)
        do z=1,i
            if(h(z:z)=='/')then
                h(z:z)='$'
            end if
        end do
        read(h,*)a,b,c,d,e
        f=0
        if(b=='+')then
            if(a+c==e)then
                f=1
            end if
        else if(b=='-')then
            if(a-c==e)then
                f=1
            end if
        else if(b=='*')then
            if(a*c==e)then
                f=1
            end if
        else
            if(a/c==e)then
                f=1
            end if
        end if
        if(f==1)then
            write(*,'(a)')'correct'
        else
            write(*,'(a)')'wrong answer'
        end if
        g=g-1
    end do
end program zz