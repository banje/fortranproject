program zz
    integer::a,b,c,d,e,f,g,z
    read(*,*)a
    do z=1,a
        read(*,*)b,c,d,e,f,g
        if(b==e.and.c==f.and.d==g)then
            write(*,'(i0)')-1
        else if((b-e)**2+(c-f)**2>(d+g)**2)then
            write(*,'(i0)')0
        else if((b-e)**2+(c-f)**2<(d-g)**2)then
            write(*,'(i0)')0
        else if((b-e)**2+(c-f)**2==(d+g)**2)then
            write(*,'(i0)')1
        else if((b-e)**2+(c-f)**2==(d-g)**2)then
            write(*,'(i0)')1
        else
            write(*,'(i0)')2
        end if
    end do
end program zz