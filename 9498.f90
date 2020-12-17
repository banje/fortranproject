program zz
    integer::a,b
    b=69
    read(*,*)a
    if(a>=90)b=b-1
    if(a>=80)b=b-1
    if(a>=70)b=b-1
    if(a>=60)b=b-1
    if(b==69)b=b+1
    write(*,'(a)')char(b)
end program zz