cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      subroutine tecplot
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
      include 'header'
c
!      open (16,file='tecplot.plt',form='formatted')
c
cccccccccccccccccccccccccccccccc
c
      call bcc
c
      call bcns
c
      call bct
c
      call bctS
c
ccccccccccccccccccccccccccccccc
      do 201 i=1,iim
      do 201 j=1,jim
      do 201 k=1,kim
      if((i .le. iim1 .or. i .gt. iim2) .or.
     1  (j .le. jim1 .or. j .gt. jim2) .or. (k .gt. kim2))then
      t3(i,j,k) = ts2(i,j,k)
      else
      t3(i,j,k) = t2(i,j,k)
      endif
201   continue
c
ccccccccc Converting all quantities in dimensional form cccccccccccccccccc
c
      do 300 i=1,iim
      do 300 j=1,jim
      do 300 k=1,kim
      u2(i,j,k)=(x_Nu/xL_ref)*u2(i,j,k)
      v2(i,j,k)=(x_Nu/xL_ref)*v2(i,j,k)
      w2(i,j,k)=(x_Nu/xL_ref)*w2(i,j,k)
      t3(i,j,k)=T_C+(T_H-T_C)*t3(i,j,k)
300   continue
c
cccccccccccccccccccccccccccccccc
      do 70 i=2,ire
      do 70 j=2,jre
      do 70 k=2,kre
      dx=deltax(i)
      dxe=deltax(i+1)
      dxw=deltax(i-1)
      dxxe=0.5*(dx+dxe)
      dxxw=0.5*(dx+dxw)
      dx2xe=(dx+dxe)
      dx2xw=(dx+dxw)
c
      dy=deltay(j)
      dyn=deltay(j+1)
      dys=deltay(j-1)
      dyyn=0.5*(dy+dyn)
      dyys=0.5*(dy+dys)
      dy2yn=(dy+dyn)
      dy2ys=(dy+dys)
c
      dz=deltaz(k)
      dzt=deltaz(k+1)
      dzb=deltaz(k-1)
      dzzt=0.5*(dz+dzt)
      dzzb=0.5*(dz+dzb)
      dz2zt=(dz+dzt)
      dz2zb=(dz+dzb)
c
      u2_b=u2(i,j+1,k)+(dyn/dy2yn)*(u2(i,j,k)-u2(i,j+1,k))
      u2_t=u2(i,j+1,k+1)+(dyn/dy2yn)*(u2(i,j,k+1)-u2(i,j+1,k+1))
      uu(i,j,k)=u2_t+(dzt/dz2zt)*(u2_b-u2_t)
c
      v2_b=v2(i+1,j,k)+(dxe/dx2xe)*(v2(i,j,k)-v2(i+1,j,k))
      v2_t=v2(i+1,j,k+1)+(dxe/dx2xe)*(v2(i,j,k+1)-v2(i+1,j,k+1))
      vv(i,j,k)=v2_t+(dzt/dz2zt)*(v2_b-v2_t)
c
      w2_s=w2(i+1,j,k)+(dxe/dx2xe)*(w2(i,j,k)-w2(i+1,j,k))
      w2_n=w2(i+1,j+1,k)+(dxe/dx2xe)*(w2(i,j+1,k)-w2(i+1,j+1,k))
      ww(i,j,k)=w2_n+(dyn/dy2yn)*(w2_s-w2_n)
c
      p_s_b=p(i+1,j,k)+(dxe/dx2xe)*(p(i,j,k)-p(i+1,j,k))
      p_n_b=p(i+1,j+1,k)+(dxe/dx2xe)*(p(i,j+1,k)-p(i+1,j+1,k))
      p_b=p_n_b+(dyn/dy2yn)*(p_s_b-p_n_b)
      p_s_t=p(i+1,j,k+1)+(dxe/dx2xe)*(p(i,j,k+1)-p(i+1,j,k+1))
      p_n_t=p(i+1,j+1,k+1)+(dxe/dx2xe)*(p(i,j+1,k+1)-p(i+1,j+1,k+1))
      p_t=p_n_t+(dyn/dy2yn)*(p_s_t-p_n_t)
      pp(i,j,k)=p_t+(dzt/dz2zt)*(p_b-p_t)
c
      t_s_b=t3(i+1,j,k)+(dxe/dx2xe)*(t3(i,j,k)-t3(i+1,j,k))
      t_n_b=t3(i+1,j+1,k)+(dxe/dx2xe)*(t3(i,j+1,k)-t1(i+1,j+1,k))
      t_b=t_n_b+(dyn/dy2yn)*(t_s_b-t_n_b)
      t_s_t=t3(i+1,j,k+1)+(dxe/dx2xe)*(t3(i,j,k+1)-t3(i+1,j,k+1))
      t_n_t=t3(i+1,j+1,k+1)+(dxe/dx2xe)*(t3(i,j+1,k+1)-t3(i+1,j+1,k+1))
      t_t=t_n_t+(dyn/dy2yn)*(t_s_t-t_n_t)
      tt(i,j,k)=t_t+(dzt/dz2zt)*(t_b-t_t)
c
70    continue
c
!      write(16,*)'VARIABLES = "X", "Y", "Z",  "U", "V", "W", "T", "P"'
!      write(16,*)'ZONE I=',(iim-1), 'J=',(jim-1),'K=',(kim-1), 'F=BLOCK'
!      write(16,22)(((xx(i)),i=1,(iim-1)),k=1,kim-1)
!      write(16,22)(((zz(k)),i=1,(iim-1)),k=1,kim-1)
!      write(16,22)((ww(k,i),i=1,(iim-1)),k=1,kim-1)
!      write(16,22)((uu(k,i),i=1,(iim-1)),k=1,kim-1)
!      write(16,22)((tt(k,i),i=1,(iim-1)),k=1,kim-1)
!      write(16,23)((pp(k,i),i=1,(iim-1)),k=1,kim-1)
!22	  format(6(f20.14))
!23	  format(6(f24.14))
c
!      close(16)
c
ccccccccccccccc X-Axis profiles cccccccccccccccccc
c
      j1=jim/2
      k1=kim2/2
c
      open(10,file='profiles-XX.dat',form='formatted')
c
      do i=1,iim-1
      write(10,111)xL_ref*xx(i),uu(i,j1,k1),vv(i,j1,k1),ww(i,j1,k1),
     & tt(i,j1,k1)
      end do
111   format(5f12.6)
c
      close(10)
c
      open(14,file='profiles-X.dat',form='formatted')
c
      do i=1,iim
      write(14,111)xL_ref*x(i),u2(i,j1,k1),v2(i,j1,k1),w2(i,j1,k1),
     & t2(i,j1,k1)
      end do
c
      close(14)
c
ccccccccccccccc Y-Axis profiles cccccccccccccccccc
c
      i1=iim/2
      k1=kim2/2
c
      open(11,file='profiles-YY.dat',form='formatted')
c
      do j=1,jim-1
      write(11,111)xL_ref*yy(j),uu(i1,j,k1),vv(i1,j,k1),ww(i1,j,k1),
     & tt(i1,j,k1)
      end do
c
      close(11)
c
      open(15,file='profiles-Y.dat',form='formatted')
c
      do j=1,jim
      write(15,111)xL_ref*y(j),u2(i1,j,k1),v2(i1,j,k1),w2(i1,j,k1),
     & t2(i1,j,k1)
      end do
c
      close(15)
c
ccccccccccccccc Z-Axis profiles cccccccccccccccccc
c
      i1=iim/2
      j1=jim/2
c
      open(12,file='profiles-ZZ.dat',form='formatted')
c
      do k=1,kim-1
      write(12,111)xL_ref*zz(k),uu(i1,j1,k),vv(i1,j1,k),ww(i1,j1,k),
     & tt(i1,j1,k)
      end do
c
      close(12)
c
      open(16,file='profiles-Z.dat',form='formatted')
c
      do k=1,kim
      write(16,111)xL_ref*z(k),u2(i1,j1,k),v2(i1,j1,k),w2(i1,j1,k),
     & t2(i1,j1,k)
      end do
c
      close(16)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end
c**************************************************************
