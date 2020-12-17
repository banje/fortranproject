program zz
    integer::a,b,c,d,z
    read(*,*)a
    do z=1,a
        read(*,*)b,c
        d=mod(b,10)
        if(d==0)then
            write(*,'(i0)')10
        else if(d==1.or.d==5.or.d==6)then
            write(*,'(i0)')d
        else if(d==4.or.d==9)then
            write(*,'(i0)')mod((d**(mod(c,2)+2)),10)
        else
            write(*,'(i0)')mod((d**(mod(c,4)+4)),10)
        end if
    end do
end program zz