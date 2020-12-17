program zz
    integer(8)::a,c,e,f,g,x,y,z
    integer(8),dimension(6)::b
    character(len=21)::d
    read(*,*)a
    read(*,*)(b(z),z=1,6)
    c=b(1)+b(2)+b(3)
    d='124135145236246356456'
    do z=1,7
        y=b(ichar(d(3*z:3*z))-48)+b(ichar(d(3*z-1:3*z-1))-48)+b(ichar(d(3*z-2:3*z-2))-48)
        if(y<c)then
            c=y
        end if
    end do
    e=b(1)+b(2)
    do z=1,6
        do y=1,6
            if(y==z.or.z+y==7)cycle
            x=b(y)+b(z)
            if(x<e)then
                e=x
            end if
        end do
    end do
    f=b(1)
    g=b(1)
    do z=2,6
        if(b(z)>f)f=b(z)
        if(b(z)<g)g=b(z)
    end do
    if(a==1)then
        write(*,'(i0)')(b(1)+b(2)+b(3)+b(4)+b(5)+b(6)-f)
    else
        write(*,'(i0)')(c*4+e*((a-2)*8+4)+g*((a-2)*(a-2)*5+(a-2)*4))
    end if
end program zz