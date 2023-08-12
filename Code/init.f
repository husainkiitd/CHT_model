ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine init 
c
c       initiation of computing data
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      open(7,file='grid.out')
c
      read(7,*)iim1,iim2,iim,jim1,jim2,jim,kim1,kim2,kim
c
      read(7,*)(deltax(i),i=1,iim)
c
      read(7,*)(deltay(j),j=1,jim)
c
      read(7,*)(deltaz(k),k=1,kim)
c
      close(7)
c
      x(1)=-(deltax(1)/2.0)
      do i=2,iim
      x(i)=x(i-1)+0.5*(deltax(i)+deltax(i-1))
      enddo
c
      do i=1,iim
      xx(i)=x(i)+(deltax(i)/2.0)
      enddo
c
      y(1)=-(deltay(1)/2.0)
      do j=2,jim
      y(j)=y(j-1)+0.5*(deltay(j)+deltay(j-1))
      enddo
c
      do j=1,jim
      yy(j)=y(j)+(deltay(j)/2.0)
      enddo
c
      z(1)=-(deltaz(1)/2.0)
      do k=2,kim
      z(k)=z(k-1)+0.5*(deltaz(k)+deltaz(k-1))
      enddo
c
      do k=1,kim
      zz(k)=z(k)+(deltaz(k)/2.0)
      enddo
c
      open(8,file='daten')
c
      read(8,*)irest,stab,itamax,epsi,stat,beta0,pr,alphat,stab_temp,
     & xL_ref,x_Nu,xks,T_floor,T_Roof,T_North,T_South,
     & T_East,T_West,T_Ceiling,f_wall,f_roof,iTrans
C
      f_roof_original = f_roof
c
      write(*,*)' irest =',irest,' stab =',stab,' kim =',kim,' iim =',
     & iim
      write(*,*)' inim =',inim,' itamax =',itamax,' epsi =',epsi,
     &	' stat =',stat
      write(*,*)' beta0 =',beta0,' pr =',pr,'alphat =',alphat,
     & 'stab_temp =',stab_temp,'L_ref =',xL_ref,
     & 'nu =',x_Nu ,'xks =',xks
c
      f_roof=0.000457*f_roof !A correction factor has been multiplied
C
C      T_C = T_East       !T_floor
C      T_H = T_West       !T_Ceiling
      if(T_Ceiling .gt. T_floor) then
      T_C = T_floor
      T_H = T_Ceiling
      elseif(T_Ceiling .lt. T_floor)then
      T_C = T_Ceiling
      T_H = T_floor
      else
      T_C = T_East
      T_H = T_West
      endif
      Ra = 9.81*0.0032452*(T_H-T_C)*(xL_ref**3.)/(x_Nu*2.3154e-5) !Ra=g*beta*dT*H^3/(nu*alpha)
      write(*,*)'Rayleigh Number =',Ra
      xkr = f_wall*xks/0.02671 !Relative thermal conductivity = K_S/K_air
      xAlpha_s = f_wall*xks/(2288.0*880.0)  !Thermal diffusivity of solid material, alpha=K\(rho*CP)
      xkr_Roof = f_roof*xkr !Relative thermal conductivity = K_S/K_air
      xAlpha_s_Roof = f_roof*xAlpha_s  !Thermal diffusivity of solid material, alpha=K\(rho*CP)
c
      TheetaO_E = (T_East-T_C)/(T_H-T_C) !Non-dimensional Temp., East outside wall
      TheetaO_W = (T_West-T_C)/(T_H-T_C) !Non-dimensional Temp., West outside wall
      TheetaO_N = (T_North-T_C)/(T_H-T_C) !Non-dimensional Temp., North outside wall
      TheetaO_S = (T_South-T_C)/(T_H-T_C) !Non-dimensional Temp., South outside wall
      Theeta_Roof = (T_Roof-T_C)/(T_H-T_C) !Non-dimensional Temp., Roof outside
c
      ire=iim-1
      jre=jim-1
      kre=kim-1
      ita=0
      isum=0
c    
      dtmax=1.
c      
      dxmin=1.0
      dymin=1.0
      dzmin=1.0
c
      do i=iim1+1,iim2
      if(deltax(i).le.dxmin)dxmin=deltax(i)
      enddo
c
      do j=jim1+1,jim2
      if(deltay(j).le.dymin)dymin=deltay(j)
      enddo
c
      do k=kim1+1,kim2
      if(deltaz(k).le.dzmin)dzmin=deltaz(k)
      enddo
c
      write(*,*)'dxmin',dxmin,'dymin',dymin,'dzmin',dzmin
      deltat=stab*dmin1(dxmin,dymin,dzmin)
c
      return
      end
