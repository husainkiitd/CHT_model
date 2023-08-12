cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine otre
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      open(1,file='result1',form='formatted')
      write(1,*)pr,Ra,ita,itaa,zeit,zeit1,deltat,deltt,xkr_f
      write(1,*)iim1,iim2,iim,jim1,jim2,jim,kim1,kim2,kim
      do 190 i=1,iim
      do 190 j=1,jim
      do 190 k=1,kim
      write(1,*)u2(i,j,k),v2(i,j,k),w2(i,j,k),t2(i,j,k),ts2(i,j,k),
     & TF_G2(i,j,k),p(i,j,k)
190   continue
      close(1)
c
      call tecplot
c
!      call nusselt
c
      stop
      end
