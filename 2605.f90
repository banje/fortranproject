program zz
    integer::a,c,y,z
    integer,dimension(:),allocatable::b,d,e
    read(*,*)a
    allocate(b(a))
    allocate(d(a))
    allocate(e(a))
    read(*,*)(b(c),c=1,a)
    do z=1,a
        do y=1,b(z)
            e(y)=d(y)
        end do
        e(b(z)+1)=z
        do y=b(z)+2,z
            e(y)=d(y-1)
        end do
        do y=1,z
            d(y)=e(y)
        end do
    end do
    do z=a,1,-1
        write(*,'(i0X)',advance='no')d(z)
    end do
end program zz