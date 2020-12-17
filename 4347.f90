program zz
    integer::a,c,d,e,f,x,y,z
    character,dimension(3,3)::b
    character(len=3)::g
    read(*,*)a
    do z=1,a
        if(z/=1)read(*,*)
        do y=1,3
            read(*,*)g
            do x=1,3
                b(y,x)=g(x:x)
            end do
        end do
        c=0
        d=0
        do y=1,3
            do x=1,3
                if(b(x,y)=='X')then
                    c=c+1
                else if(b(x,y)=='O')then
                    d=d+1
                end if
            end do
        end do
        if(c-d>1.or.c-d<0)then
            write(*,'(a)')'no'
            cycle
        end if
        e=0
        f=0
        do y=1,3
            if(b(y,1)=='X'.and.b(y,2)=='X'.and.b(y,3)=='X')then
                e=e+1
            end if
            if(b(y,1)=='O'.and.b(y,2)=='O'.and.b(y,3)=='O')then
                f=f+1
            end if
            if(b(1,y)=='X'.and.b(2,y)=='X'.and.b(3,y)=='X')then
                e=e+1
            end if
            if(b(1,y)=='O'.and.b(2,y)=='O'.and.b(3,y)=='O')then
                f=f+1
            end if
        end do
        if(b(1,1)=='X'.and.b(2,2)=='X'.and.b(3,3)=='X')then
            e=e+1
        end if
        if(b(1,1)=='O'.and.b(2,2)=='O'.and.b(3,3)=='O')then
            f=f+1
        end if
        if(b(3,1)=='X'.and.b(2,2)=='X'.and.b(1,3)=='X')then
            e=e+1
        end if
        if(b(3,1)=='O'.and.b(2,2)=='O'.and.b(1,3)=='O')then
            f=f+1
        end if
        if(e*f>1)then
            write(*,'(a)')'no'
            cycle
        end if
        if(e==1.and.c-d==0)then
            write(*,'(a)')'no'
            cycle
        end if
        if(f==1.and.c-d==1)then
            write(*,'(a)')'no'
            cycle
        end if
        write(*,'(a)')'yes'
    end do
end program zz