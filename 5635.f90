program zz
    integer::a,c,d,e,f,g,h
    character(len=15)::b,i,j
    read(*,*)a
    g=19891231
    h=20110101
    do while(a>0)
        read(*,*)b,c,d,e
        f=c+100*d+10000*e
        if(f>g) then
            i=b
            g=f
        end if
        if(f<h) then
            j=b
            h=f
        end if
        a=a-1
    end do
    write(*,"(A)")trim(i),trim(j)
end program zz