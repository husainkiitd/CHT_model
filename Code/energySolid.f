csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine energySolid
c
c     solution of the energy-equation for the solid zone
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      include 'header'
c
      if(ita.lt.1)then
      delttS = 1.0e-5/f_wall
      else
C      delttS = deltt
C      delttS = 1.0e-8
C      delttS = 0.05*deltat
      delttS = 1.0e-5/f_wall
      endif
c
      ittS=1
10    call bctS
c
      call teqcpS
c
      call temaltS
c
      ittS = ittS+1
c
      eptS=dabs(dtS)
      iiS=1/stab_temp
C      if(ita.eq.(ita/20*20) .and. ittS.eq.2) then
      if(ittS.eq.(ittS/200*200)) then
      write(*,110)ittS,iiS,ittmaxS,jttmaxS,kttmaxS,delttS,eptS
110   format(2x,'From EnergySolid ',2x,5i10,2e13.3)
      endif
c
c      if(eptS.ge.epsi)goto 10
c      if(eptS.ge.epsi .and. ittS.lt.500)goto 10
c
      if(ita.lt.1)then
      if(eptS.ge.1.0e-3 .and. ittS.lt.2000)goto 10
      else
      if(eptS.ge.1.0e-5 .and. ittS.lt.600)goto 10
      endif
c
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
