ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine ceqcp
c
c      mass continuity equation for  constant properties
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      divmax=0.
      dalt=0.
      imax=0
      jmax=0
      kmax=0
c
      do 70 i=iim1+1,iim2
      do 70 j=jim1+1,jim2
      do 70 k=kim1+1,kim2
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
      beta_den1=(1./dx)*((1./dxxe)+(1./dxxw))
      beta_den2=(1./dy)*((1./dyyn)+(1./dyys))
      beta_den3=(1./dz)*((1./dzzt)+(1./dzzb))
      beta_den=deltat*(beta_den1+beta_den2+beta_den3)
      beta=beta0/beta_den
c
      div=(u2(i,j,k)-u2(i-1,j,k))/dx + (v2(i,j,k)-v2(i,j-1,k))/dy
     $+ (w2(i,j,k)-w2(i,j,k-1))/dz
c
      deltap=-beta*div                      
      a=deltat*deltap
c
      p(i,j,k)=p(i,j,k)+deltap
c
      u2(i,j,k)=u2(i,j,k)+a/dxxe
      u2(i-1,j,k)=u2(i-1,j,k)-a/dxxw
      v2(i,j,k)=v2(i,j,k)+a/dyyn
      v2(i,j-1,k)=v2(i,j-1,k)-a/dyys
      w2(i,j,k)=w2(i,j,k)+a/dzzt
      w2(i,j,k-1)=w2(i,j,k-1)-a/dzzb
c
      dab=dabs(div)
      if(dab.gt.dalt) then
      divmax=dab
      imax=i
      jmax=j
      kmax=k
      end if
      dalt=divmax
c
70    continue
c
      iti=iti+1
      isum=isum+1
c
c      if(iti.eq.1 .or. iti.eq.(iti/1000*1000)) then
      if(ita.eq.(ita/100*100) .and. iti.eq.1) then
      write(*,71)ita,iti,isum,divmax,imax,jmax,kmax
71    format(2x,2i7,2x,i12,4x,e15.8,3i5,2x,'From ceqcp')
      end if
      return
      end
