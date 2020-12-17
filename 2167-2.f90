program zz
    integer::a,b,d,e,f,g,h,i,y,z
    integer,dimension(:,:),allocatable::c
    read(*,*)a,b
    allocate(c(0:a+1,0:b+1))
    do z=0,a
        c(a,0)=0
    end do
    do z=0,b
        c(0,b)=0
    end do
    do z=1,a
        read(*,*)(c(z,y),y=1,b)
    end do
    do z=1,a
        do y=1,b
            c(z,y)=c(z,y)+c(z,y-1)+c(z-1,y)-c(z-1,y-1)
        end do
    end do
    read(*,*)e
    do while(e>0)
        read(*,*)f,g,h,i
        write(*,"(i0)") c(h,i)-c(h,g-1)-c(f-1,i)+c(f-1,g-1)
        e=e-1
    end do
end program zz