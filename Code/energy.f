csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine energy
c
c     solution of the energy-equation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
C      deltt = stab_temp*deltat
C      if(deltt .gt. (0.5*deltat))then
C      deltt = 0.5*deltat
C      endif
      deltt = 10.0*deltat
C      deltt = 1.0e-8
c
11    itt=1
10    call bct
c
      call teqcp
c
      call temalt
c
      zeit1 = zeit1 + deltt
c
      itt = itt+1
      itaa = itaa+1
c
      ept=dabs(dt)
c      if(itt.eq.2 .or. itt.eq.itt/50*50)then
c      if(ita.eq.(ita/50*50) .and. itt.eq.2) then
      if(itt.eq.(itt/20*20)) then
      write(*,110)itt,ittmax,jttmax,kttmax,zeit1,deltt,deltat,ept
110   format(2x,'From Energy ',2x,4i5,4e13.3)
      endif
c
      ii=1/stab_temp
C      if(itt.le.ii)goto 10
      if(ept.ge.epsi2)then
      if(itt.lt.40)goto 10
C      if(ita.ge.220000 .and. itt.lt.25000)goto 10
C      if(ita.lt.220000 .and. ita.ge.190000 .and. itt.lt.20000)goto 10
C      if(ita.lt.190000 .and. ita.ge.150000 .and. itt.lt.15000)goto 10
C      if(ita.lt.130000 .and. ita.ge.110000 .and. itt.lt.10000)goto 10
C      if(ita.lt.110000 .and. ita.ge.90000 .and. itt.lt.5000)goto 10
C      if(ita.lt.90000 .and. itt.lt.3000)goto 10
CC      if(ept.ge.epsi2 .and. itt.lt.30000)goto 10
      endif
c
      stab_temp=stab_t
      zeit=zeit+deltat
c
c     	if(ept.ge.5.0) go to 11
c
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
