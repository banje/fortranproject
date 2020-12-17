program zz
    character(len=10),dimension(4)::a
    character(len=10),dimension(2)::b
    integer::x,y,z
    real,dimension(6,5)::c
    integer,dimension(4)::d,g
    real,dimension(4)::e
    integer,dimension(6)::w
    real::f
    read(*,*)(a(z),z=1,4)
    do z=1,6
        read(*,*)b(1),b(2),c(z,3),c(z,4),c(z,5)
        do y=1,2
            do x=1,4
                if(b(y)==a(x))c(z,y)=x
            end do
        end do
    end do
    do z=1,4
        e(z)=0
    end do
    do z=1,6
        w(z)=3
    end do
    do while(w(1)<6)
        do z=1,4
            d(z)=0
        end do
        do z=1,6
            if(w(z)==3)then
                d(c(z,1))=d(c(z,1))+3
            else if(w(z)==4)then
                d(c(z,1))=d(c(z,1))+1
                d(c(z,2))=d(c(z,2))+1
            else
                d(c(z,2))=d(c(z,2))+3
            end if
        end do
        f=1
        do z=1,6
            f=f*c(z,w(z))
        end do
        if(f/=0)then
            do z=1,4
                g(z)=z
            end do
            x=0
            do while(x==0)
                x=1
                do z=1,3
                    if(d(z)<d(z+1))then
                        y=d(z+1)
                        d(z+1)=d(z)
                        d(z)=y
                        y=g(z+1)
                        g(z+1)=g(z)
                        g(z)=y
                        x=0
                    end if
                end do
            end do
            if(d(2)/=d(3))then
                e(g(1))=e(g(1))+f
                e(g(2))=e(g(2))+f
            else if(d(1)==d(2))then
                if(d(3)==d(4))then
                    e(g(1))=e(g(1))+f/2
                    e(g(2))=e(g(2))+f/2
                    e(g(3))=e(g(3))+f/2
                    e(g(4))=e(g(4))+f/2
                else
                    e(g(1))=e(g(1))+f*2/3
                    e(g(2))=e(g(2))+f*2/3
                    e(g(3))=e(g(3))+f*2/3
                end if
            else if(d(3)==d(4))then
                e(g(1))=e(g(1))+f
                e(g(2))=e(g(2))+f/3
                e(g(3))=e(g(3))+f/3
                e(g(4))=e(g(4))+f/3
            else
                e(g(1))=e(g(1))+f
                e(g(2))=e(g(2))+f/2
                e(g(3))=e(g(3))+f/2
            end if
        end if
        w(6)=w(6)+1
        do z=6,2,-1
            if(w(z)>5)then
                w(z)=w(z)-3
                w(z-1)=w(z-1)+1
            end if
        end do
    end do
    do z=1,4
        write(*,'(f0.9)')e(z)
    end do
end program zz