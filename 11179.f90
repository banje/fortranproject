program zz
    integer::a,b,c,d
    read(*,*)a
    c=0
    d=1
    do while(d<=a)
        d=d*2
    end do
    do
        d=d/2
        b=a/2
        if(a-2*b==1)then
            c=c+d
        end if
        a=b
        if(a==0)then
            exit
        end if
    end do
    write(*,'(i0)')c
end program zz