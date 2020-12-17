program zz
    integer,dimension(6,6)::a
    integer::b,c,d,x,y,z
    do z=1,6
        read(*,*)(a(y,z),y=1,6)
    end do
    do z=1,6
        c=0
        do y=0,9
            do x=1,6
                a(x,z)=a(x,z)+1
                if(a(x,z)>=10)then
                    a(x,z)=a(x,z)-10
                end if
            end do
            b=0
            do x=1,6
                b=b+a(x,z)
            end do
            if(c<b)then
                c=b
                d=y
            end if
        end do
        do x=1,6
            a(x,z)=a(x,z)+d+1
            if(a(x,z)>=10)then
                a(x,z)=a(x,z)-10
            end if
        end do
    end do
    c=0
    do y=0,9
        do x=1,6
            a(x,x)=a(x,x)+1
            if(a(x,x)>=10)then
                a(x,x)=a(x,x)-10
            end if
        end do
        b=0
        do x=1,6
            b=b+a(x,x)
        end do
        if(c<b)then
            c=b
            d=y
        end if
    end do
    do x=1,6
        a(x,x)=a(x,x)+d+1
        if(a(x,x)>=10)then
            a(x,x)=a(x,x)-10
        end if
    end do
    do y=0,9
        do x=1,6
            a(x,7-x)=a(x,7-x)+1
            if(a(x,7-x)>=10)then
                a(x,7-x)=a(x,7-x)-10
            end if
        end do
        b=0
        do x=1,6
            b=b+a(x,7-x)
        end do
        if(c<b)then
            c=b
            d=y
        end if
    end do
    do x=1,6
        a(x,7-x)=a(x,7-x)+d+1
        if(a(x,7-x)>=10)then
            a(x,7-x)=a(x,7-x)-10
        end if
    end do
    write(*,*)a
    b=0
    do z=1,6
        do y=1,6
            b=b+a(y,z)
        end do
    end do
    write(*,*)b
end program zz