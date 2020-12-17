program zz
    character(len=81)::a,b
    integer::c(81)
    integer::d,e,f,z
    read(*,*)a(:),b(:)
    d=len_trim(a)
    e=len_trim(b)
    a(82-d:)=a(1:d)
    b(82-e:)=b(1:e)
    do z=1,81-d
        a(z:z)='0'
    end do
    do z=1,81-e
        b(z:z)='0'
    end do
    do z=1,81
        c(z)=0
    end do
    do z=81,1,-1
        c(z)=c(z)+ichar(a(z:z))+ichar(b(z:z))-96
        if(c(z)>1)then
            c(z)=c(z)-2
            c(z-1)=c(z-1)+1
        end if
    end do
    do z=1,81
        if(c(z)==1)then
            exit
        end if
        if(z==81)then
            write(*,'(i0)')0
            stop
        end if
    end do
    do while(z<=81)
        write(*,'(i0)',advance='no')c(z)
        z=z+1
    end do
end program zz