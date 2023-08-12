ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine otpt
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
cccccccccccccccc
c
      call bct
      call bctS
      if(ita.eq.ita/50*50)goto 303
303   open(144,file='Test.dat',form='formatted')
c
      do k=1,kim
      T_dim1=T_C+(T_H-T_C)*t2(iim2/2,jim/2,k)
      T_dim2=T_C+(T_H-T_C)*ts2(iim2/2,jim/2,k)
      write(144,89)xL_ref*z(k),T_dim1,T_dim2
      end do
89    format(3f12.6)
c
      close(144)
c      stop
c
      if(ita.lt.2000)then
      if(ita.eq.ita/50*50.or.iwrite.eq.1)goto 777
	  else
      if(ita.eq.ita/2000*2000.or.iwrite.eq.1)goto 777
	  endif
c      if(ita.eq.ita/1*1.or.iwrite.eq.1)goto 777
      if(ept.le.stat .or. ita.ge.itamax)goto 777
      goto 888
777   open (1, file='result1',form='formatted')
      write(1,*)pr,Ra,ita,itaa,zeit,zeit1,deltat,deltt,xkr_f,iTrans,ept,
     & eptS
      write(1,*)iim1,iim2,iim,jim1,jim2,jim,kim1,kim2,kim
      do 197 i=1,iim
      do 197 j=1,jim
      do 197 k=1,kim
      write(1,*)u2(i,j,k),v2(i,j,k),w2(i,j,k),t2(i,j,k),ts2(i,j,k),
     & u1(i,j,k),v1(i,j,k),w1(i,j,k),t1(i,j,k),ts1(i,j,k),p(i,j,k)
197   continue
      close(1)
c
cccccccccccccccccccccccccccccccccccccccc
c      if(iwrite2.eq.1)then
c      call tecplot
c!      call nusselt
c      endif
cccccccccccccccccccccccccccccccccccccccc
c
c888  if(dtmax.le.stat .or. ita.ge.itamax) then
888   if(ept.le.stat .or. ita.ge.itamax) then
c
      write(*,*)'ept = ',ept,'ita = ',ita, 'zeit = ',zeit
      write(*,*)'Convergence Reached.'
c
      stop
      endif
      return
      end
