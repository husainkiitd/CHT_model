ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine bcc
c
c      boundary conditions for the mass continuity equation
c      conditions for the confining surfaces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      i=iim1
      do j=jim1,jim2+1
      do k=kim1,kim2+1
      u2(i,j,k)=0.0
      enddo
      enddo
c
      i=iim2+1
      do j=jim1,jim2+1
      do k=kim1,kim2+1
      u2(i-1,j,k)=0.0
      enddo
      enddo
c
      j=jim1
      do i=iim1,iim2+1
      do k=kim1,kim2+1
      v2(i,j,k)=0.0
      enddo
      enddo
c
      j=jim2+1
      do i=iim1,iim2+1
      do k=kim1,kim2+1
      v2(i,j-1,k)=0.0
      enddo
      enddo
c
      k=kim1
      do i=iim1,iim2+1
      do j=jim1,jim2+1
      w2(i,j,k)=0.0
      enddo
      enddo
c
      k=kim2+1
      do i=iim1,iim2+1
      do j=jim1,jim2+1
      w2(i,j,k-1)=0.0
      enddo
      enddo
c
      return
      end
