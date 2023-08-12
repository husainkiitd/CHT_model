csssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine temalt
c
c     alternation of the temperature-arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      do 10 i=iim1,iim2+1
      do 10 j=jim1,jim2+1
      do 10 k=kim1,kim2+1
      t1(i,j,k) = t2(i,j,k)
      TF_G1(i,j,k) = TF_G2(i,j,k)
10    continue
c     
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssss
