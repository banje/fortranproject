program zz
    integer::a,b,c,e,z
    integer(8)::f
    integer::d(35)
    read(*,*)a,b,c
    z=0
    do
        if(b==1)then
            exit
        end if
        z=z+1
        e=b/2
        d(z)=b-e*2
        b=e
    end do
    a=mod(a,c)
    f=a
    do while(z>0)
        f=mod(f*f,c)
        if(d(z)==1)then
            f=mod(f*a,c)
        end if
        z=z-1
    end do
    write(*,'(i0)')f
end program zz