cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine nseqcp
c
c     navier-stokes equations for constant properties with Boussinesq Approximation
c                                              15.03.2023
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c      double precision residw_0(kf,if),residu_0(kf,if)
c
      zeit=zeit+deltat
c
      do 150 i=iim1+1,iim2
      do 150 j=jim1+1,jim2
      do 150 k=kim1+1,kim2
      dx=deltax(i)
      dxe=deltax(i+1)
      if(i.eq.iim2) dxe=deltax(i)
      dxw=deltax(i-1)
      if(i.eq.iim1+1) dxw=deltax(i)
      dxxe=0.5*(dx+dxe)
      dxxw=0.5*(dx+dxw)
      dx2xe=(dx+dxe)
      dx2xw=(dx+dxw)
c
      dy=deltay(j)
      dyn=deltay(j+1)
      if(j.eq.jim2) dyn=deltay(j)
      dys=deltay(j-1)
      if(j.eq.jim1+1) dys=deltay(j)
      dyyn=0.5*(dy+dyn)
      dyys=0.5*(dy+dys)
      dy2yn=(dy+dyn)
      dy2ys=(dy+dys)
c
      dz=deltaz(k)
      dzt=deltaz(k+1)
      if(k.eq.kim2) dzt=deltaz(k)
      dzb=deltaz(k-1)
      if(k.eq.kim1+1) dzb=deltaz(k)
      dzzt=0.5*(dz+dzt)
      dzzb=0.5*(dz+dzb)
      dz2zt=(dz+dzt)
      dz2zb=(dz+dzb)
c
cccccccccccccccccccccc
c
      dpdx = (p(i+1,j,k)-p(i,j,k))/dxxe
      dpdy = (p(i,j+1,k)-p(i,j,k))/dyyn
      dpdz = (p(i,j,k+1)-p(i,j,k))/dzzt
c
c ********************** U - Momentum ****************************
c
      u_e=0.5*(u1(i,j,k)+u1(i+1,j,k))
      u_w=0.5*(u1(i,j,k)+u1(i-1,j,k))
      u_n=u1(i,j+1,k)+(dyn/(dy2yn))*(u1(i,j,k)-u1(i,j+1,k))
      u_s=u1(i,j-1,k)+(dys/(dy2ys))*(u1(i,j,k)-u1(i,j-1,k))
      u_t=u1(i,j,k+1)+(dzt/(dz2zt))*(u1(i,j,k)-u1(i,j,k+1))
      u_b=u1(i,j,k-1)+(dzb/(dz2zb))*(u1(i,j,k)-u1(i,j,k-1))
      v_n=v1(i+1,j,k)+(dxe/(dx2xe))*(v1(i,j,k)-v1(i+1,j,k))
      v_s=v1(i+1,j-1,k)+(dxe/(dx2xe))*(v1(i,j-1,k)-v1(i+1,j-1,k))
      w_t=w1(i+1,j,k)+(dxe/(dx2xe))*(w1(i,j,k)-w1(i+1,j,k))
      w_b=w1(i+1,j,k-1)+(dxe/(dx2xe))*(w1(i,j,k-1)-w1(i+1,j,k-1))
c
      duudx=(u_e**2.0-u_w**2.0)/dxxe
      dvudy=(v_n*u_n-v_s*u_s)/dy
      dwudz=(w_t*u_t-w_b*u_b)/dz
c
      d2udx2=(1.0/dxxe)*(((u1(i+1,j,k)-u1(i,j,k))/dxe)
     & -((u1(i,j,k)-u1(i-1,j,k))/dx))
      d2udy2=(1.0/dy)*(((u1(i,j+1,k)-u1(i,j,k))/dyyn)
     & -((u1(i,j,k)-u1(i,j-1,k))/dyys))
      d2udz2=(1.0/dz)*(((u1(i,j,k+1)-u1(i,j,k))/dzzt)
     & -((u1(i,j,k)-u1(i,j,k-1))/dzzb))
c
      residu=(-duudx-dvudy-dwudz+d2udx2+d2udy2+d2udz2)
c
      u2(i,j,k)=u1(i,j,k)+deltat*(residu-dpdx)
c
c ********************** V - Momentum ****************************
c
      u_e=u1(i,j+1,k)+(dyn/(dy2yn))*(u1(i,j,k)-u1(i,j+1,k))
      u_w=u1(i-1,j+1,k)+(dyn/(dy2yn))*(u1(i-1,j,k)-u1(i-1,j+1,k))
      v_e=v1(i+1,j,k)+(dxe/(dx2xe))*(v1(i,j,k)-v1(i+1,j,k))
      v_w=v1(i-1,j,k)+(dxw/(dx2xw))*(v1(i,j,k)-v1(i-1,j,k))
      v_n=0.5*(v1(i,j,k)+v1(i,j+1,k))
      v_s=0.5*(v1(i,j,k)+v1(i,j-1,k))
      v_t=v1(i,j,k+1)+(dzt/(dz2zt))*(v1(i,j,k)-v1(i,j,k+1))
      v_b=v1(i,j,k-1)+(dzb/(dz2zb))*(v1(i,j,k)-v1(i,j,k-1))
      w_t=w1(i,j+1,k)+(dyn/(dy2yn))*(w1(i,j,k)-w1(i,j+1,k))
      w_b=w1(i,j+1,k-1)+(dyn/(dy2yn))*(w1(i,j,k-1)-w1(i,j+1,k-1))
c
      duvdx=(u_e*v_e-u_w*v_w)/dx
      dvvdy=(v_n**2.0-v_s**2.0)/dyyn
      dwvdz=(w_t*v_t-w_b*v_b)/dz
c
      d2vdx2=(1.0/dx)*(((v1(i+1,j,k)-v1(i,j,k))/dxxe)
     & -((v1(i,j,k)-v1(i-1,j,k))/dxxw))
	  d2vdy2=(1.0/dyyn)*(((v1(i,j+1,k)-v1(i,j,k))/dyn)
     & -((v1(i,j,k)-v1(i,j-1,k))/dy))
	  d2vdz2=(1.0/dz)*(((v1(i,j,k+1)-v1(i,j,k))/dzzt)
     & -((v1(i,j,k)-v1(i,j,k-1))/dzzb))
c
      residv =(-duvdx-dvvdy-dwvdz+d2vdx2+d2vdy2+d2vdz2)
c
      v2(i,j,k)=v1(i,j,k)+deltat*(residv-dpdy)
c
c ********************** W - Momentum ****************************
c
      u_e=u1(i,j,k+1)+(dzt/(dz2zt))*(u1(i,j,k)-u1(i,j,k+1))
      u_w=u1(i-1,j,k+1)+(dzt/(dz2zt))*(u1(i-1,j,k)-u1(i-1,j,k+1))
      w_e=w1(i+1,j,k)+(dxe/(dx2xe))*(w1(i,j,k)-w1(i+1,j,k))
      w_w=w1(i-1,j,k)+(dxw/(dx2xw))*(w1(i,j,k)-w1(i-1,j,k))
      w_n=w1(i,j+1,k)+(dyn/(dy2yn))*(w1(i,j,k)-w1(i,j+1,k))
      w_s=w1(i,j-1,k)+(dys/(dy2ys))*(w1(i,j,k)-w1(i,j-1,k))
      w_t=0.5*(w1(i,j,k)+w1(i,j,k+1))
      w_b=0.5*(w1(i,j,k)+w1(i,j,k-1))
      v_n=v1(i,j,k+1)+(dzt/(dz2zt))*(v1(i,j,k)-v1(i,j,k+1))
      v_s=v1(i,j-1,k+1)+(dzt/(dz2zt))*(v1(i,j-1,k)-v1(i,j-1,k+1))
c
      duwdx=(u_e*w_e-u_w*w_w)/dx
      dvwdy=(v_n*w_n-v_s*w_s)/dy
      dwwdz=(w_t**2.0-w_b**2.0)/dzzt
c
      d2wdx2=(1.0/dx)*(((w1(i+1,j,k)-w1(i,j,k))/dxxe)
     & -((w1(i,j,k)-w1(i-1,j,k))/dxxw))
      d2wdy2=(1.0/dy)*(((w1(i,j+1,k)-w1(i,j,k))/dyyn)
     & -((w1(i,j,k)-w1(i,j-1,k))/dyys))
      d2wdz2=(1.0/dzzt)*(((w1(i,j,k+1)-w1(i,j,k))/dzt)
     & -((w1(i,j,k)-w1(i,j,k-1))/dz))
c
      t_w=t2(i,j,k+1)+(dzt/(dz2zt))*(t2(i,j,k)-t2(i,j,k+1))
C      if(t_w.gt.1.0) t_w=1.0
C      if(t_w.lt.1e-5) t_w=1e-5
      Source=(Ra/pr)*t_w	!Boussinesq Source Term
C      Source=(1.0/pr)*t_w	!Boussinesq Source Term
c
      residw =(-duwdx-dvwdy-dwwdz+d2wdx2+d2wdy2+d2wdz2+Source)
c
      W2(i,j,k)=W1(i,j,k)+deltat*(residw-dpdz)
c
c      if((i .le. iim1+1 .or. i .ge. iim2) .or.
c     1  (j .le. jim1+1 .or. j .ge. jim2) .or.
c     1   (k .ge. kim2))then
c      u2(i,j,k)=0.
c      v2(i,j,k)=0.
c      W2(i,j,k)=0.
c      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 150   continue
c
      return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssss
