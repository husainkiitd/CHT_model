csssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine temaltS
c
c     alternation of the temperature-arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      do 10 i=1,iim
      do 10 j=1,jim
      do 10 k=1,kim
c
C      if((i .le. iim1 .or. i .gt. iim2) .or.
C     1  (j .le. jim1 .or. j .gt. jim2) .or.
C     1   (k .gt. kim2))then
      ts1(i,j,k) = ts2(i,j,k)
C      endif
10    continue
c     
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssss
