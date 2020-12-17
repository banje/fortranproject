program zz
    integer::a,c,y,z
    character(len=50)::b,d
    read(*,*)a
    read(*,*)b
    c=len_trim(b)
    d=b
    do z=2,a
        read(*,*)b
        do y=1,c
            if(d(y:y)/=b(y:y))then
                d(y:y)='?'
            end if
        end do
    end do
    write(*,'(a)')d
end program zz