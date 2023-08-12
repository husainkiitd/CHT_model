ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine start
c
c       start conditions
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      zeit=0.
      zeit1 =0.
c
      do 31 i=1,iim
      do 31 j=1,jim
      do 31 k=1,kim
      u1(i,j,k)=1e-6
      u2(i,j,k)=1e-6
      v1(i,j,k)=1e-6
      v2(i,j,k)=1e-6
      w1(i,j,k)=1e-6
      w2(i,j,k)=1e-6
      p(i,j,k)=1.0
      f_factor = 1.+ (xks*f_roof_original - 0.07391) + 0.2*(xks - 0.721)
      t1(i,j,k)= ((f_factor*T_Ceiling-T_C)/(T_H-T_C)) !Initialize with Ceiling Temperature
C	  write(*,*)T_C+(T_H-T_C)*t1(i,j,k)
C	  STOP
      t2(i,j,k)= t1(i,j,k)
      ts1(i,j,k)= t1(i,j,k)
      ts2(i,j,k)= t1(i,j,k)
31    continue
c
      return
      end

