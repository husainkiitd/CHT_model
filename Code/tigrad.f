cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine tigrad
c
c       maximum of the velocity-alternation during a zeit increment
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      dtmax=0.
      dumax=0.
      dvmax=0.
      dwmax=0.
c
      do 180 i=iim1+1,iim2
      do 180 j=jim1+1,jim2
      do 180 k=kim1+1,kim2
c
      dudt=dabs(u1(i,j,k)-u2(i,j,k))/deltat
      dvdt=dabs(v1(i,j,k)-v2(i,j,k))/deltat
      dwdt=dabs(w1(i,j,k)-w2(i,j,k))/deltat
c
      dact=dtmax
      dtmax=dmax1(dtmax,dudt,dvdt,dwdt)
      if(dtmax.le.dact) goto 180
      idtm=i
      jdtm=j
      kdtm=k
180   continue
c
c ****** constant properties ********
      open(27,file='prolt',form='formatted')
      write(27,101)ita,iti,isum,dtmax,deltat
101   format(2x,3i7,2x,2e13.5)
      close(27)
c
      if(ita.eq.(ita/500*500)) then
      write(6,182)idtm,jdtm,kdtm,dtmax,deltat,ept
	  endif
      write(68,999)ita,iti,isum,dtmax,idtm,jdtm,kdtm,deltat,ept,eptS
999   format(2x,2i8,i15,e13.5,2x,3i5,2x,3e13.5)
182   format(2x,'Maximum change in velocity ',2x,3i5,3e13.5)
      if (dtmax*.1 .le. epsi) epsi = dtmax*.1
      return
      end
