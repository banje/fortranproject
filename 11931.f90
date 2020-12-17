program zz
    integer::a,z
    integer,dimension(2000001)::b,d
    read(*,*)a
    do z=1,2000001
        b(z)=0
    end do
    read(*,*)(d(z),z=1,a)
    do z=1,a
        b(1000001-d(z))=1
    end do
    do z=1,2000001
        if(b(z)==0)cycle
        write(*,'(i0)')1000001-z
    end do
end program zz