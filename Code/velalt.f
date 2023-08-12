cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine velalt
c
c       alternation of the velocity arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      do 80 i=iim1,iim2+1
      do 80 j=jim1,jim2+1
      do 80 k=kim1,kim2+1
      u1(i,j,k)=u2(i,j,k)
      v1(i,j,k)=v2(i,j,k)
      w1(i,j,k)=w2(i,j,k)
80    continue
      return
      end
