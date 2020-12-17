program zz
    integer::a,b,c,d,z
    read(*,*)a
    b=0
    c=1
    do z=1,a
        d=b+c
        b=c
        c=d
        c=mod(c,15746)
    end do
    write(*,'(i0)')c
end program zz