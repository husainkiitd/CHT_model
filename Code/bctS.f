csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine bctS
c     temperature-boundary-conditions for the confining surfaces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      do 100 j=1,jim
      do 100 k=1,kim
      ts2(1,j,k) = 2.0*TheetaO_E - ts1(2,j,k)  !East Outside Wall
C      ts2(iim1+1,j,k) = 2.0*t1(iim1+1,j,k) - ts1(iim1,j,k) !East Inside Wall
      ts2(iim1+1,j,k) = t1(iim1+1,j,k)  !East Inside Wall
100   continue
c
      do 101 j=1,jim
      do 101 k=1,kim
      ts2(iim,j,k) = 2.0*TheetaO_W - ts1(iim-1,j,k)  !West Outside Wall
C      ts2(iim2,j,k) = 2.0*t1(iim2,j,k) - ts1(iim2+1,j,k)  !West Inside Wall
      ts2(iim2,j,k) = t1(iim2,j,k)  !West Inside Wall
101   continue
c
      do 102 i=1,iim
      do 102 k=1,kim
      ts2(i,1,k) = 2.0*TheetaO_N - ts1(i,2,k)  !North Outside Wall
c      ts2(i,jim1+1,k) = 2.0*t1(i,jim1+1,k) - ts1(i,jim1,k)  !North Inside Wall
      ts2(i,jim1+1,k) = t1(i,jim1+1,k)  !North Inside Wall
102   continue
c
      do 103 i=1,iim
      do 103 k=1,kim
      ts2(i,jim,k) = 2.0*TheetaO_S - ts1(i,jim-1,k)  !South Outside Wall
c      ts2(i,jim2,k) = 2.0*t1(i,jim2,k) - ts1(i,jim2+1,k)  !South Inside Wall
      ts2(i,jim2,k) = t1(i,jim2,k)  !South Inside Wall
103   continue
c
      do 104 i=1,iim
      do 104 j=1,jim
      ts2(i,j,1) = 2.0*0.0 - ts1(i,j,1+1)                 !Floor
104   continue
c
      do 105 i=1,iim
      do 105 j=1,jim
      ts2(i,j,kim) = 2.0*Theeta_Roof - ts1(i,j,kim-1)  !Roof Outside
C      ts2(i,j,kim2) = 2.0*t1(i,j,kim2) - ts2(i,j,kim2+1)  !Ceiling inside
      ts2(i,j,kim2) = t1(i,j,kim2)  !Ceiling inside
105   continue
c
      return
      end
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
