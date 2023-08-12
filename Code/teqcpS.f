cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
      subroutine teqcpS
c
c     energy-equation for constant properties  in Solid Zone
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
      include 'header'
      double precision residtS_0(iim,jim,kim)
c
      do 10 i=2,ire
      do 10 j=2,jre
      do 10 k=2,kre
c
      if((i .le. iim1 .or. i .gt. iim2) .or.
     1  (j .le. jim1 .or. j .gt. jim2) .or.
     1   (k .ge. kim2))then
      dx=deltax(i)
      dxe=deltax(i+1)
C      if(i.eq.iim1) dxe=deltax(i)
      if(i.eq.iim1) dxe=1.01*deltax(i)
      dxw=deltax(i-1)
C      if(i.eq.iim2+1) dxw=deltax(i)
      if(i.eq.iim2+1) dxw=1.01*deltax(i)
      dxxe=0.5*(dx+dxe)
      dxxw=0.5*(dx+dxw)
      dx2xe=(dx+dxe)
      dx2xw=(dx+dxw)
c
      dy=deltay(j)
      dyn=deltay(j+1)
C      if(j.eq.jim1) dyn=deltay(j)
      if(j.eq.jim1) dyn=1.01*deltay(j)
      dys=deltay(j-1)
C      if(j.eq.jim2+1) dys=deltay(j)
      if(j.eq.jim2+1) dys=1.01*deltay(j)
      dyyn=0.5*(dy+dyn)
      dyys=0.5*(dy+dys)
      dy2yn=(dy+dyn)
      dy2ys=(dy+dys)
c
      dz=deltaz(k)
      dzt=deltaz(k+1)
C      if(k.eq.kim1) dzt=deltaz(k)
      dzb=deltaz(k-1)
C      if(k.eq.kim2+1) dzb=deltaz(k)
      if(k.eq.kim2+1) dzb=1.01*deltaz(k)
      dzzt=0.5*(dz+dzt)
      dzzb=0.5*(dz+dzb)
      dz2zt=(dz+dzt)
      dz2zb=(dz+dzb)
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      t_difxa = ts1(i+1,j,k)-ts1(i,j,k)
      t_difxb = ts1(i,j,k)-ts1(i-1,j,k)
      t_difya = ts1(i,j+1,k)-ts1(i,j,k)
      t_difyb = ts1(i,j,k)-ts1(i,j-1,k)
      t_difza = ts1(i,j,k+1)-ts1(i,j,k)
      t_difzb = ts1(i,j,k)-ts1(i,j,k-1)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      d2tdx2=(1.0/dx)*((t_difxa/dxxe)-(t_difxb/dxxw))
      d2tdy2=(1.0/dy)*((t_difya/dyyn)-(t_difyb/dyys))
      d2tdz2=(1.0/dz)*((t_difza/dzzt)-(t_difzb/dzzb))
c
      if(k.le.kim2)then
      residt = (xAlpha_s/x_Nu)*(d2tdx2+d2tdy2+d2tdz2) !Walls region
      else
      residt = (xAlpha_s_Roof/x_Nu)*(d2tdx2+d2tdy2+d2tdz2) !Roof region
      endif
c
C      ts2(i,j,k) = ts1(i,j,k)+delttS*residt
cccc
c                                               
C      write(*,*)ita,ittS,irest
C      if(ita.eq.1 .or. irest.eq.1)then
      ts2(i,j,k) = ts1(i,j,k)+10.0*delttS*residt !Multiplied by 10, just to accelerate the solution
C      else
C      ts2(i,j,k) = ts1(i,j,k)+delttS*(0.5*(3.0*residt-residtS_0(i,j,k)))
C      endif
Cc
C      residtS_0(i,j,k)=residt
c
      endif
10  	continue
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      dalt = 0.
      do 15 i=2,ire
      do 15 j=2,jre
      do 15 k=2,kre
c
      if((i .le. iim1 .or. i .gt. iim2) .or.
     1  (j .le. jim1 .or. j .gt. jim2) .or.
     1   (k .gt. kim2))then
C      dab = dabs(ts2(i,j,k)-ts1(i,j,k))
      dab = dabs(ts2(i,j,k)-ts1(i,j,k))/delttS
      if(dab.gt.dalt) then
      dtS = dab
      ittmaxS = i
      jttmaxS = j
      kttmaxS = k
      end if
      dalt=dtS
      endif
15    continue
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
