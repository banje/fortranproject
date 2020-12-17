program zz
    integer::a,b,c,z
    read(*,*)a
    do z=1,a
        read(*,*)b
        c=1
        write(*,'(a,i0,a)',advance='no')'Pairs for ',b,': '
        if(b-c<=c)goto 1
        do
            write(*,'(i0,a,i0)',advance='no')c,' ',b-c
            c=c+1
            if(b-c<=c)exit
            write(*,'(a)',advance='no')', '
        end do
        1 write(*,*)
    end do
end program zz