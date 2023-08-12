cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
      program main
c
c******************************************************************
c*       program main for computing flow field (with CHT energy) in a
c*       3D rectangular cavity (natural convection- Boussinesq) March 2023
c******************************************************************
c
c
      include 'header'
c
      open(68,file='pro',form='formatted')
c
      call init
      Ra_F=1e-3*Ra
c
      if (irest.eq.0) call start
      if (irest.eq.1) call restar
C
      if(ita.ge.70000)Ra=1.0*Ra_F
      if(ita.lt.70000 .and. ita.ge.60000)Ra=5e-1*Ra_F
      if(ita.lt.60000 .and. ita.ge.50000)Ra=1e-1*Ra_F
      if(ita.lt.50000 .and. ita.ge.40000)Ra=5e-2*Ra_F
      if(ita.lt.40000 .and. ita.ge.30000)Ra=1e-2*Ra_F
      if(ita.lt.30000)Ra=1e-3*Ra_F
C
      if(ita.ge.120000) epsi2=5e0
      if(ita.lt.120000 .and. ita.ge.110000)epsi2=6e0
      if(ita.lt.110000 .and. ita.ge.105000)epsi2=8e0
      if(ita.lt.105000 .and. ita.ge.100000)epsi2=1e1
      if(ita.lt.100000 .and. ita.ge.90000)epsi2=3e1
      if(ita.lt.90000 .and. ita.ge.80000)epsi2=5e1
      if(ita.lt.80000 .and. ita.ge.70000)epsi2=7e1
      if(ita.lt.70000 .and. ita.ge.60000)epsi2=9e1
      if(ita.lt.60000 .and. ita.ge.50000)epsi2=1.1e2
      if(ita.lt.50000 .and. ita.ge.45000)epsi2=1.3e2
      if(ita.lt.45000 .and. ita.ge.40000)epsi2=1.5e2
      if(ita.lt.40000 .and. ita.ge.30000)epsi2=2e2
      if(ita.lt.30000 .and. ita.ge.20000)epsi2=4e2
      if(ita.lt.20000 .and. ita.ge.10000)epsi2=6e2
      if(ita.lt.10000 .and. ita.ge.5000)epsi2=8e2
      if(ita.lt.5000 .and. ita.ge.2000)epsi2=1e3
      if(ita.lt.2000 .and. ita.ge.500)epsi2=2e3
      if(ita.lt.500)epsi2=3e3
c
11    if(ita.lt.iTrans .or. ita .eq. 0)then
      if(ita.eq.ita/1000*1000)write(*,*)'Energy Solid is ON!',ept,iTrans
      do ix=1,5
      call energySolid
      enddo
      endif
C
      if(ita .gt. 10) then
      if(ept .lt. 5.0 .and. eptS .lt. 1.0e-5 .and. iTrans .gt. ita)then
      iTrans=ita
      endif
      endif
c
10    call conti
c
      open(3,file='iwrite',form='formatted')
      read(3,*)iwrite
      read(3,*)itime
      read(3,*)istop
      read(3,*)iwrite2
      close(3)
c
      if (istop.eq.1) then
      call bcns
      call otre
      end if
c
      call energy
      call velalt
      call ticorr
      call otpt
      call nseqcp
      call bcns
      call tigrad
c
c	*******************************
c
      if(ita.ge.70000)Ra=1.0*Ra_F
      if(ita.lt.70000 .and. ita.ge.60000)Ra=5e-1*Ra_F
      if(ita.lt.60000 .and. ita.ge.50000)Ra=1e-1*Ra_F
      if(ita.lt.50000 .and. ita.ge.40000)Ra=5e-2*Ra_F
      if(ita.lt.40000 .and. ita.ge.30000)Ra=1e-2*Ra_F
      if(ita.lt.30000)Ra=1e-3*Ra_F
c
c	*******************************
c
      if(ita.ge.1400) epsi=0.00001
      if(ita.lt.1400 .and. ita.ge.1200)epsi=0.000025
      if(ita.lt.1200 .and. ita.ge.1000)epsi=0.00005
      if(ita.lt.1000 .and. ita.ge.900)epsi=0.0001
      if(ita.lt.900 .and. ita.ge.800)epsi=0.00025
      if(ita.lt.800 .and. ita.ge.700)epsi=0.0005
      if(ita.lt.700 .and. ita.ge.600)epsi=0.001
      if(ita.lt.600 .and. ita.ge.500)epsi=0.005
      if(ita.lt.500 .and. ita.ge.400)epsi=0.01
      if(ita.lt.400 .and. ita.ge.300)epsi=0.025
      if(ita.lt.300 .and. ita.ge.200)epsi=0.05
      if(ita.lt.200 .and. ita.ge.100)epsi=0.075
      if(ita.lt.100 .and. ita.ge.50)epsi=0.1
      if(ita.lt.50)epsi=0.5
c
c	*******************************
c
      if(ita.ge.120000) epsi2=5e0
      if(ita.lt.120000 .and. ita.ge.110000)epsi2=6e0
      if(ita.lt.110000 .and. ita.ge.105000)epsi2=8e0
      if(ita.lt.105000 .and. ita.ge.100000)epsi2=1e1
      if(ita.lt.100000 .and. ita.ge.90000)epsi2=3e1
      if(ita.lt.90000 .and. ita.ge.80000)epsi2=5e1
      if(ita.lt.80000 .and. ita.ge.70000)epsi2=7e1
      if(ita.lt.70000 .and. ita.ge.60000)epsi2=9e1
      if(ita.lt.60000 .and. ita.ge.50000)epsi2=1.1e2
      if(ita.lt.50000 .and. ita.ge.45000)epsi2=1.3e2
      if(ita.lt.45000 .and. ita.ge.40000)epsi2=1.5e2
      if(ita.lt.40000 .and. ita.ge.30000)epsi2=2e2
      if(ita.lt.30000 .and. ita.ge.20000)epsi2=4e2
      if(ita.lt.20000 .and. ita.ge.10000)epsi2=6e2
      if(ita.lt.10000 .and. ita.ge.5000)epsi2=8e2
      if(ita.lt.5000 .and. ita.ge.2000)epsi2=1e3
      if(ita.lt.2000 .and. ita.ge.500)epsi2=2e3
      if(ita.lt.500)epsi2=3e3
c
      if(irest.eq.1)irest=0
c
      if(ita.lt.iTrans) goto 11
c
      goto 10
c	*******************************
      end
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

