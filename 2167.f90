program zz
    integer::a,b,d,e,f,g,h,i,j,k,y,z
    integer,dimension(:,:),allocatable::c
    read(*,*)a,b
    allocate(c(a,b))
    do d=1,a
        read(*,*)(c(d,e),e=1,b)
    end do
    read(*,*)f
    do while(f>0)
        read(*,*)g,h,i,j
        k=0
        do z=g,i
            do y=h,j
                k=k+c(z,y)
            end do
        end do
        write(*,"(i0)")k
        f=f-1
    end do
end program zz