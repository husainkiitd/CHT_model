cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
      subroutine teqcp
c
c     energy-equation for constant properties
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C      use omp_lib
c
      include 'header'
      double precision residt_0(iim,jim,kim)
c
C!$omp parallel private(rank)
C!$omp do
c
      if(ita.lt.iTrans)then
      do 10 i=iim1+2,iim2-1
      do 10 j=jim1+2,jim2-1
      do 10 k=kim1+1,kim2-1
      dx=deltax(i)
      dxe=deltax(i+1)
C      if(i.eq.iim2) dxe=deltax(i)
      dxw=deltax(i-1)
C      if(i.eq.iim1+1) dxw=deltax(i)
      dxxe=0.5*(dx+dxe)
      dxxw=0.5*(dx+dxw)
      dx2xe=(dx+dxe)
      dx2xw=(dx+dxw)
c
      dy=deltay(j)
      dyn=deltay(j+1)
C      if(j.eq.jim2) dyn=deltay(j)
      dys=deltay(j-1)
C      if(j.eq.jim1+1) dys=deltay(j)
      dyyn=0.5*(dy+dyn)
      dyys=0.5*(dy+dys)
      dy2yn=(dy+dyn)
      dy2ys=(dy+dys)
c
      dz=deltaz(k)
      dzt=deltaz(k+1)
C      if(k.eq.kim2) dzt=deltaz(k)
      dzb=deltaz(k-1)
C      if(k.eq.kim1+1) dzb=deltaz(k)
      dzzt=0.5*(dz+dzt)
      dzzb=0.5*(dz+dzb)
      dz2zt=(dz+dzt)
      dz2zb=(dz+dzb)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      t_e = t1(i+1,j,k)+(dxe/dx2xe)*(t1(i,j,k)-t1(i+1,j,k))
C      if(i.eq.iim2) t_e = TF_G2(iim2,j,k)
      t_w = t1(i-1,j,k)+(dxw/dx2xw)*(t1(i,j,k)-t1(i-1,j,k))
C      if(i.eq.iim1+1) t_w = TF_G2(iim1,j,k)
      t_n = t1(i,j+1,k)+(dyn/dy2yn)*(t1(i,j,k)-t1(i,j+1,k))
C      if(j.eq.jim2) t_n = TF_G2(i,jim2,k)
      t_s = t1(i,j-1,k)+(dys/dy2ys)*(t1(i,j,k)-t1(i,j-1,k))
C      if(j.eq.jim1+1) t_s = TF_G2(i,jim1,k)
      t_t = t1(i,j,k+1)+(dzt/dz2zt)*(t1(i,j,k)-t1(i,j,k+1))
C      if(k.eq.kim2) t_t = TF_G2(i,j,kim2)
      t_b = t1(i,j,k-1)+(dzt/dz2zt)*(t1(i,j,k)-t1(i,j,k-1))
c
      t_difxa = t1(i+1,j,k)-t1(i,j,k)
C      if(i.eq.iim2) t_difxa = TF_G2(iim2,j,k)-t1(i,j,k)
      t_difxb = t1(i,j,k)-t1(i-1,j,k)
C      if(i.eq.iim1+1) t_difxb = t1(i,j,k)-TF_G2(iim1,j,k)
      t_difya = t1(i,j+1,k)-t1(i,j,k)
C      if(j.eq.jim2) t_difya = TF_G2(i,jim2,k)-t1(i,j,k)
      t_difyb = t1(i,j,k)-t1(i,j-1,k)
C      if(j.eq.jim1+1) t_difyb = t1(i,j,k)-TF_G2(i,jim1,k)
      t_difza = t1(i,j,k+1)-t1(i,j,k)
C      if(k.eq.kim2) t_difza = TF_G2(i,j,kim2)-t1(i,j,k)
      t_difzb = t1(i,j,k)-t1(i,j,k-1)
c
      dutdx1 = (u2(i,j,k)*t_e-u2(i-1,j,k)*t_w)/dx
      dvtdy1 = (v2(i,j,k)*t_n-v2(i,j-1,k)*t_s)/dy
      dwtdz1 = (w2(i,j,k)*t_t-w2(i,j,k-1)*t_b)/dz
c
      dutdx2=(u2(i,j,k)+u2(i-1,j,k))*(t1(i,j,k)-t1(i-1,j,k))/dx2xw
C      if(i.eq.iim1+1) dutdx2=(u2(i,j,k)+u2(i-1,j,k))*(t1(i,j,k)-
C     & TF_G2(iim1,j,k))/dxxw
      dvtdy2=(v2(i,j,k)+v2(i,j-1,k))*(t1(i,j,k)-t1(i,j-1,k))/dy2ys
C      if(j.eq.iimj+1) dvtdy2=(v2(i,j,k)+v2(i,j-1,k))*(t1(i,j,k)-
C     & TF_G2(i,jim1,k))/dyys
      dwtdz2=(w2(i,j,k)+w2(i,j,k-1))*(t1(i,j,k)-t1(i,j,k-1))/dz2zb
c
      dutdx = (1.0-alphat)*dutdx1 + alphat*dutdx2
      dvtdy = (1.0-alphat)*dvtdy1 + alphat*dvtdy2
      dwtdz = (1.0-alphat)*dwtdz1 + alphat*dwtdz2
c
      d2tdx2=(1.0/dx)*((t_difxa/dxxe)-(t_difxb/dxxw))
C      if(i.eq.iim2) d2tdx2=(1.0/dx)*((t_difxa/(0.5*dx))-(t_difxb/dxxw))
C      if(i.eq.iim1+1)d2tdx2=(1.0/dx)*((t_difxa/dxxe)-(t_difxb/(0.5*dx)))
      d2tdy2=(1.0/dy)*((t_difya/dyyn)-(t_difyb/dyys))
C      if(i.eq.iim2) d2tdy2=(1.0/dy)*((t_difya/(0.5*dy))-(t_difyb/dyys))
C      if(i.eq.iim1+1)d2tdy2=(1.0/dy)*((t_difya/dyyn)-(t_difyb/(0.5*dy)))
      d2tdz2=(1.0/dz)*((t_difza/dzzt)-(t_difzb/dzzb))
C      if(k.eq.kim2) d2tdz2=(1.0/dz)*((t_difza/(0.5*dz))-(t_difzb/dzzb))
c
      residtF(i,j,k) = (-dutdx-dvtdy-dwtdz+
     & (1.0/pr)*(d2tdx2+d2tdy2+d2tdz2))
c
C      write(*,*)ita,itt,irest
C      if(ita.eq.1 .or. irest.eq.1)then
      t2(i,j,k) = t1(i,j,k)+deltt*residtF(i,j,k)
C      else
C      t2(i,j,k) = t1(i,j,k)+
C     & deltt*(0.5*(3.0*residtF(i,j,k)-residt_0(i,j,k)))
C      endif
Cc
C      residt_0(i,j,k)=residtF(i,j,k)
c
10    continue
c
C!$omp end parallel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      dalt = 0.
      do 15 i=iim1+1,iim2
      do 15 j=jim1+1,jim2
      do 15 k=kim1+1,kim2
C	  if(i.eq.iim1+1 .and. j.eq.22 .and. k.eq.15)then
C	  write(*,*)'Unlimited T',t2(i,j,k)
C	  endif
C      if(t2(i,j,k).gt.1.0) t2(i,j,k)=1.0
C      if(t2(i,j,k).lt.0.0) t2(i,j,k)=0.0
C	  if(i.eq.iim1+1 .and. j.eq.22 .and. k.eq.15)then
C	  write(*,*)'Limited T',t2(i,j,k)
C	  endif
      dab = dabs(t2(i,j,k)-t1(i,j,k))/deltt
      if(dab.gt.dalt) then
      dt = dab
      ittmax = i
      jttmax = j
      kttmax = k
      end if
      dalt=dt
15    continue
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      else
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do 101 i=iim1+1,iim2
      do 101 j=jim1+1,jim2
      do 101 k=kim1+1,kim2
      dx=deltax(i)
      dxe=deltax(i+1)
C      if(i.eq.iim2) dxe=deltax(i)
      dxw=deltax(i-1)
C      if(i.eq.iim1+1) dxw=deltax(i)
      dxxe=0.5*(dx+dxe)
      dxxw=0.5*(dx+dxw)
      dx2xe=(dx+dxe)
      dx2xw=(dx+dxw)
c
      dy=deltay(j)
      dyn=deltay(j+1)
C      if(j.eq.jim2) dyn=deltay(j)
      dys=deltay(j-1)
C      if(j.eq.jim1+1) dys=deltay(j)
      dyyn=0.5*(dy+dyn)
      dyys=0.5*(dy+dys)
      dy2yn=(dy+dyn)
      dy2ys=(dy+dys)
c
      dz=deltaz(k)
      dzt=deltaz(k+1)
C      if(k.eq.kim2) dzt=deltaz(k)
      dzb=deltaz(k-1)
C      if(k.eq.kim1+1) dzb=deltaz(k)
      dzzt=0.5*(dz+dzt)
      dzzb=0.5*(dz+dzb)
      dz2zt=(dz+dzt)
      dz2zb=(dz+dzb)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      t_e = t1(i+1,j,k)+(dxe/dx2xe)*(t1(i,j,k)-t1(i+1,j,k))
C      if(i.eq.iim2) t_e = TF_G2(iim2,j,k)
      t_w = t1(i-1,j,k)+(dxw/dx2xw)*(t1(i,j,k)-t1(i-1,j,k))
C      if(i.eq.iim1+1) t_w = TF_G2(iim1,j,k)
      t_n = t1(i,j+1,k)+(dyn/dy2yn)*(t1(i,j,k)-t1(i,j+1,k))
C      if(j.eq.jim2) t_n = TF_G2(i,jim2,k)
      t_s = t1(i,j-1,k)+(dys/dy2ys)*(t1(i,j,k)-t1(i,j-1,k))
C      if(j.eq.jim1+1) t_s = TF_G2(i,jim1,k)
      t_t = t1(i,j,k+1)+(dzt/dz2zt)*(t1(i,j,k)-t1(i,j,k+1))
C      if(k.eq.kim2) t_t = TF_G2(i,j,kim2)
      t_b = t1(i,j,k-1)+(dzt/dz2zt)*(t1(i,j,k)-t1(i,j,k-1))
c
      t_difxa = t1(i+1,j,k)-t1(i,j,k)
C      if(i.eq.iim2) t_difxa = TF_G2(iim2,j,k)-t1(i,j,k)
      t_difxb = t1(i,j,k)-t1(i-1,j,k)
C      if(i.eq.iim1+1) t_difxb = t1(i,j,k)-TF_G2(iim1,j,k)
      t_difya = t1(i,j+1,k)-t1(i,j,k)
C      if(j.eq.jim2) t_difya = TF_G2(i,jim2,k)-t1(i,j,k)
      t_difyb = t1(i,j,k)-t1(i,j-1,k)
C      if(j.eq.jim1+1) t_difyb = t1(i,j,k)-TF_G2(i,jim1,k)
      t_difza = t1(i,j,k+1)-t1(i,j,k)
C      if(k.eq.kim2) t_difza = TF_G2(i,j,kim2)-t1(i,j,k)
      t_difzb = t1(i,j,k)-t1(i,j,k-1)
c
      dutdx1 = (u2(i,j,k)*t_e-u2(i-1,j,k)*t_w)/dx
      dvtdy1 = (v2(i,j,k)*t_n-v2(i,j-1,k)*t_s)/dy
      dwtdz1 = (w2(i,j,k)*t_t-w2(i,j,k-1)*t_b)/dz
c
      dutdx2=(u2(i,j,k)+u2(i-1,j,k))*(t1(i,j,k)-t1(i-1,j,k))/dx2xw
C      if(i.eq.iim1+1) dutdx2=(u2(i,j,k)+u2(i-1,j,k))*(t1(i,j,k)-
C     & TF_G2(iim1,j,k))/dxxw
      dvtdy2=(v2(i,j,k)+v2(i,j-1,k))*(t1(i,j,k)-t1(i,j-1,k))/dy2ys
C      if(j.eq.iimj+1) dvtdy2=(v2(i,j,k)+v2(i,j-1,k))*(t1(i,j,k)-
C     & TF_G2(i,jim1,k))/dyys
      dwtdz2=(w2(i,j,k)+w2(i,j,k-1))*(t1(i,j,k)-t1(i,j,k-1))/dz2zb
c
      dutdx = (1.0-alphat)*dutdx1 + alphat*dutdx2
      dvtdy = (1.0-alphat)*dvtdy1 + alphat*dvtdy2
      dwtdz = (1.0-alphat)*dwtdz1 + alphat*dwtdz2
c
      d2tdx2=(1.0/dx)*((t_difxa/dxxe)-(t_difxb/dxxw))
C      if(i.eq.iim2) d2tdx2=(1.0/dx)*((t_difxa/(0.5*dx))-(t_difxb/dxxw))
C      if(i.eq.iim1+1)d2tdx2=(1.0/dx)*((t_difxa/dxxe)-(t_difxb/(0.5*dx)))
      d2tdy2=(1.0/dy)*((t_difya/dyyn)-(t_difyb/dyys))
C      if(i.eq.iim2) d2tdy2=(1.0/dy)*((t_difya/(0.5*dy))-(t_difyb/dyys))
C      if(i.eq.iim1+1)d2tdy2=(1.0/dy)*((t_difya/dyyn)-(t_difyb/(0.5*dy)))
      d2tdz2=(1.0/dz)*((t_difza/dzzt)-(t_difzb/dzzb))
C      if(k.eq.kim2) d2tdz2=(1.0/dz)*((t_difza/(0.5*dz))-(t_difzb/dzzb))
c
      residtF(i,j,k) = (-dutdx-dvtdy-dwtdz+
     & (1.0/pr)*(d2tdx2+d2tdy2+d2tdz2))
c
C      write(*,*)ita,itt,irest
C      if(ita.eq.1 .or. irest.eq.1)then
      t2(i,j,k) = t1(i,j,k)+deltt*residtF(i,j,k)
C      else
C      t2(i,j,k) = t1(i,j,k)+
C     & deltt*(0.5*(3.0*residtF(i,j,k)-residt_0(i,j,k)))
C      endif
Cc
C      residt_0(i,j,k)=residtF(i,j,k)
c
101    continue
c
C!$omp end parallel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      dalt = 0
      do 151 i=iim1+1,iim2
      do 151 j=jim1+1,jim2
      do 151 k=kim1+1,kim2
C	  if(i.eq.iim1+1 .and. j.eq.22 .and. k.eq.15)then
C	  write(*,*)'Unlimited T',t2(i,j,k)
C	  endif
C      if(t2(i,j,k).gt.1.0) t2(i,j,k)=1.0
C      if(t2(i,j,k).lt.0.0) t2(i,j,k)=0.0
C	  if(i.eq.iim1+1 .and. j.eq.22 .and. k.eq.15)then
C	  write(*,*)'Limited T',t2(i,j,k)
C	  endif
      dab = dabs(t2(i,j,k)-t1(i,j,k))/deltt
      if(dab.gt.dalt) then
      dt = dab
      ittmax = i
      jttmax = j
      kttmax = k
      end if
      dalt=dt
151   continue
C
      endif
c
      stab_t= 1.0E-01/(dabs(t2(ittmax,jttmax,kttmax)
     & -t1(ittmax,jttmax,kttmax)))
!      write(*,*)'T1 = ',t1(ittmax,jttmax,kttmax),
!     & 'T2 = ',t2(ittmax,jttmax,kttmax)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
