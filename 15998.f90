function aa(a,b)
    integer(8)::a,b,c,aa
    do while(b/=0)
        c=mod(a,b)
        a=b
        b=c
    end do
    aa=a
end function aa
program zz
    integer(8)::a,b,c,d,e,g,z,aa
    read(*,*)a
    d=0
    e=-2
    g=-1
    do z=1,a
        read(*,*)b,c
        if(b>0)then
            if(c/=b+d)then
                write(*,'(i0)')-1
                stop
            end if
            d=c
            cycle
        end if
        if(c-d-b==0)then
            d=c
            cycle
        end if
        if(c>e)e=c
        if(g==-1)then
            g=c-d-b
        else if(g<c-d-b)then
            g=aa(c-d-b,g)
        else
            g=aa(g,c-d-b)
        end if
        d=c
    end do
    if(g<=e)then
        write(*,'(i0)')-1
    else if(g==-1)then
        write(*,'(i0)')1
    else
        write(*,'(i0)')g
    end if
end program zz