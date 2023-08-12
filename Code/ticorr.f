ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine ticorr
c
c       calculation of time-increment using both CFL and Fourier nos.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      utop=0.
      vtop=0.
      wtop=0.
c
      do 90 i=iim1+1,iim2
      do 90 j=jim1+1,jim2
      do 90 k=kim1+1,kim2
      uchek=0.5*(u2(i,j,k)+u2(i-1,j,k))
      vchek=0.5*(v2(i,j,k)+v2(i,j-1,k))
      wchek=0.5*(w2(i,j,k)+w2(i,j,k-1))
      utop=dmax1(utop,abs(uchek))
      vtop=dmax1(vtop,abs(vchek))
      wtop=dmax1(wtop,abs(wchek))
90    continue
c
      if(wtop.gt.0)then
c
      umax=dxmin/utop
      vmax=dymin/vtop
      wmax=dzmin/wtop
c
C      deltat=dmin1(umax,vmax,wmax)
      deltat=5.0e-2*dmin1(umax,vmax,wmax)
c
      endif
c
      deltch=0.5*(dxmin**2.0*dymin**2.0*dzmin**2.0
     & /(dxmin**2.0+dymin**2.0+dzmin**2.0))
C	  write(*,*)'Sanjeev Test',deltat,deltch
c
      deltat=dmin1(deltat,deltch)
c
      deltat=stab*deltat
c
      return
      end

