ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine restar
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      open (1,file='result1',form='formatted')
c
      read(1,*)pr,Ra,ita,itaa,zeit,zeit1,deltat,deltt,xkr_f,iTrans,ept,
     & eptS
      read(1,*)iim1,iim2,iim,jim1,jim2,jim,kim1,kim2,kim
      do 200 i=1,iim
      do 200 j=1,jim
      do 200 k=1,kim
      read(1,*)u2(i,j,k),v2(i,j,k),w2(i,j,k),t2(i,j,k),ts2(i,j,k),
     & u1(i,j,k),v1(i,j,k),w1(i,j,k),t1(i,j,k),ts1(i,j,k),p(i,j,k)
C      u1(i,j,k)=u2(i,j,k)
C      v1(i,j,k)=v2(i,j,k)
C      w1(i,j,k)=w2(i,j,k)
C      t1(i,j,k)=t2(i,j,k)
C      ts1(i,j,k)=ts2(i,j,k)
200   continue
      close (1)
c
      return
      end
