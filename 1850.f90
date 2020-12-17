function aa(a,b)
    integer(16)::a,b,c,aa
    do while(b/=0)
        c=mod(a,b)
        a=b
        b=c
    end do
    aa=a
end function aa
program zz
    integer(16)::a,b,c,z,aa
    read(*,*)a,b
    if(a>b)then
        c=aa(a,b)
    else
        c=aa(b,a)
    end if
    write(*,'(a)')repeat('1',c)
end program zz