program zz
    integer::a,b,d,x,y,z
    character(len=50),dimension(50)::c
    read(*,*)a,b
    d=a
    if(b<a)d=b
    do z=1,a
        read(*,*)c(z)
    end do
    do z=d-1,0,-1
        do y=1,b-z
            do x=1,a-z
                if(c(x)(y:y)/=c(x)(y+z:y+z))cycle
                if(c(x+z)(y:y)/=c(x+z)(y+z:y+z))cycle
                if(c(x)(y:y)/=c(x+z)(y:y))cycle
                write(*,'(i0)')(z+1)**2
                stop
            end do
        end do
    end do
end program zz