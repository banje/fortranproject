program zz
    integer::a,b,x,y,z
    integer,dimension(:),allocatable::c
    real(8)::d,e,f,g
    read(*,*)a,b
    allocate(c(a))
    read(*,*)(c(z),z=1,a)
    f=10**7
    f=f*f
    do z=1,a-b+1
        do x=z+b-1,a
            d=0
            do y=z,x
                d=d+c(y)
            end do
            d=d/(x-z+1)
            e=0
            do y=z,x
                g=d-c(y)
                e=e+g**2
            end do
            e=e/(x-z+1)
            if(e<f)then
                f=e
            end if
        end do
    end do
    write(*,'(f0.9)')sqrt(f)
end program zz