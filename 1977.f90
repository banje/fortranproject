program zz
    integer :: a,b,c,d,e,f
    read (*,*) a,b
    c=b
    e=0
    do while (c>=a)
        d=1
        do
            if (d*d>c) then
                exit
            else if (d*d==c) then
                e=e+c
                f=c
                exit
            else
                d=d+1
                cycle
            end if
        end do
        c=c-1
    end do
    if (e==0) then
        print "(i0)", -1
    else
        print "(i0)", e,f
    end if
end program zz