cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine conti
c
c       pressure iteration
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
      iti=0
      ita=ita+1
c
40    call bcc
c
      call ceqcp
c
      if(divmax.ge.epsi) goto 40
c
      return
      end
