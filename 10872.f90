program zz
    integer :: a,b,c
    read (*,*) a
    if (a==0) then
        print "(i0)", 1
    else
        b=1
        c=1
        do while (c<=a)
            b=b*c
            c=c+1
        end do
        print "(i0)", b
    end if
end program zz