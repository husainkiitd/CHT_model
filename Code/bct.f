csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine bct
c     temperature-boundary-conditions for the confining surfaces  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      if(ita.lt.iTrans)then
C
      do 100 j=jim1+1,jim2
      do 100 k=kim1+1,kim2
      t2(iim1+1,j,k) = t1(iim1+2,j,k) - xkr*((deltax(iim1+1))
     & /(deltax(iim1)))*(ts1(iim1,j,k) - ts1(iim1-1,j,k))  !East Wall
c
      t2(iim2,j,k) = t1(iim2-1,j,k) + xkr*((deltax(iim2))/
     & (deltax(iim2+1)))*(ts1(iim2+2,j,k)-ts1(iim2+1,j,k))  !West Wall
100   continue
c
      do 102 i=iim1,iim2+1
      do 102 k=kim1,kim2+1
      t2(i,jim1+1,k) = t1(i,jim1+2,k) - xkr*((deltay(jim1+1))
     & /(deltay(jim1)))*(ts1(i,jim1,k) - ts1(i,jim1-1,k))  !North Wall
c
      t2(i,jim2,k) = t1(i,jim2-1,k) + xkr*((deltay(jim2))/
     & (deltay(jim2+1)))*(ts1(i,jim2+2,k)-ts1(i,jim2+1,k))  !South Wall
102   continue
c
      do 104 i=iim1,iim2+1
      do 104 j=jim1,jim2+1
      t2(i,j,kim1) = 2.0*0.0 - t1(i,j,kim1+1)  !Floor
c
      t2(i,j,kim2) = t1(i,j,kim2-1) + xkr_Roof*((deltaz(kim2))/
     & (deltaz(kim2+1)))*(ts1(i,j,kim2+2)-ts1(i,j,kim2+1))  !Ceiling
104   continue
C
      else
C
      do 1001 j=jim1+1,jim2
      do 1001 k=kim1+1,kim2
      t2(iim1,j,k) = 2.0*(0.5*(ts2(iim1,j,k)+ts2(iim1+1,j,k))) -
     & t1(iim1+1,j,k)  !East Wall
c
      t2(iim2+1,j,k) = 2.0*(0.5*(ts2(iim2,j,k)+ts2(iim2+1,j,k))) -
     & t1(iim2,j,k)  !West Wall
1001   continue
c
      do 1021 i=iim1,iim2+1
      do 1021 k=kim1,kim2+1
      t2(i,jim1,k) = 2.0*(0.5*(ts2(i,jim1,k)+ts2(i,jim1+1,k))) -
     & t1(i,jim1+1,k)  !North Wall
c
      t2(i,jim2+1,k) = 2.0*(0.5*(ts2(i,jim2,k)+ts2(i,jim2+1,k))) -
     & t1(i,jim2,k)  !South Wall
1021   continue
c
      do 1041 i=iim1,iim2+1
      do 1041 j=jim1,jim2+1
      t2(i,j,kim1) = 2.0*0.0 - t1(i,j,kim1+1)  !Floor
c
      t2(i,j,kim2+1) = 2.0*(0.5*(ts2(i,j,kim2)+ts2(i,j,kim2+1))) -
     & t1(i,j,kim2)  !Ceiling
1041   continue
C
      endif
c
      return
      end
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
