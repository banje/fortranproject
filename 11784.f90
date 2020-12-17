program zz
    integer::a,c,d,e,z
    character(len=250)::b,f
    do
        read(*,*,iostat=a)b
        if(a/=0)exit
        c=len_trim(b)
        do z=1,c,2
            e=ichar(b(z+1:z+1))
            if(e>64)then
                d=e-55
            else
                d=e-48
            end if
            e=ichar(b(z:z))
            if(e>64)then
                d=d+16*(e-55)
            else
                d=d+16*(e-48)
            end if
            write(*,'(a)',advance='no')char(d)
        end do
            write(*,*)
    end do
end program zz