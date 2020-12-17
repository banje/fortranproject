program zz
    integer::a,b
1   read(*,*)a,b
    if(a==0)then
        stop
    end if
    if(a>b)then
        write(*,'(a)')'Yes'
    else
        write(*,'(a)')'No'
    end if
    goto 1
end program zz