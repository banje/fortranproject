program zz
    integer::a,b,c,e
    character(len=20)::d,f
    read (*,*) a
    do while (a>0)
        read (*,*) b
        e=0
        do while (b>0)
            read (*,*) c,d
            if (c>e) then
                f=d
                e=c
            end if
            b=b-1
        end do
        print "(a)", trim(f)
        a=a-1
    end do
end program zz