ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine bcns
c
c       boundary conditions for the navier stokes equations
c       conditions for the confining surfaces
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      i=iim1
      do 170 j=jim1,jim2+1
      do 170 k=kim1,kim2+1
      u2(i,j,k)=0.0
      v2(i,j,k)=-v2(i+1,j,k)
      w2(i,j,k)=-w2(i+1,j,k)
170   continue
c
      i=iim2+1
      do 171 j=jim1,jim2+1
      do 171 k=kim1,kim2+1
      u2(i-1,j,k)=0.0
      v2(i,j,k)=-v2(i-1,j,k)
      w2(i,j,k)=-w2(i-1,j,k)
171   continue
c
      j=jim1
      do 172 i=iim1,iim2+1
      do 172 k=kim1,kim2+1
      u2(i,j,k)=-u2(i,j+1,k)
      v2(i,j,k)=0.0
      w2(i,j,k)=-w2(i,j+1,k)
172   continue
c
      j=jim2+1
      do 173 i=iim1,iim2+1
      do 173 k=kim1,kim2+1
      u2(i,j,k)=-u2(i,j-1,k)
      v2(i,j-1,k)=0.0
      w2(i,j,k)=-w2(i,j-1,k)
173   continue
c
      k=kim1
      do 174 i=iim1,iim2+1
      do 174 j=jim1,jim2+1
      u2(i,j,k)=-u2(i,j,k+1)
      v2(i,j,k)=-v2(i,j,k+1)
      w2(i,j,k)=0.0
174   continue
c
      k=kim2+1
      do 175 i=iim1,iim2+1
      do 175 j=jim1,jim2+1
      u2(i,j,k)=-u2(i,j,k-1)
      v2(i,j,k)=-v2(i,j,k-1)
      w2(i,j,k-1)=0.0
175   continue
c
      return
      end
