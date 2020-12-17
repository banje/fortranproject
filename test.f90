function aa(a,b)
    integer::a,b,c,aa
    do while(b/=0)
        c=mod(a,b)
        a=b
        b=c
    end do
    aa=a
end function aa
program zz
    integer(1)::a,b
    a=17
    b=17
    write(*,'(i0)')a+b
end program zz