program lev2grads128
  parameter(nr=2000,sigtop=-1.e-6,sigbot=1+1.e-4)
  real r(nr),d(nr),e(nr)
  frho(asig)=(asig-sigtop)/(sigbot-asig)
  fsig(arho)=(sigtop+arho*sigbot)/(1+arho)
  rho1=frho(1.)
  rho0=frho(0.)
  f=log(rho0/rho1)/nr
  do k=1,nr
    rho=rho1*exp(f*(k-1))
    r(k)=fsig(rho)
  enddo
  r=r*1000.e2
  call baopenwt(61,'wcoss.lev2grads128.dat',iret)
  call hybdel('wcoss.ttakbkgen.hyb64',1000.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb64',750.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb64',500.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ec137.txt',1000.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ec137.txt',750.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ec137.txt',500.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128A',1000.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128A',750.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128A',500.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128B',1000.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128B',750.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128B',500.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128C',1000.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128C',750.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call hybdel('wcoss.ttakbkgen.hyb128C',500.e2,nr,r,d,e)
  call wryte(61,4*nr,d)
  call wryte(61,4*nr,e)
  call baclose(61,iret)
  open(62,file='wcoss.lev2grads128.ctl')
  write(62,'("dset wcoss.lev2grads128.dat")')
  write(62,'("undef -9999.")')
  write(62,'("title wcoss.lev2grads128")')
  write(62,'("xdef 1 linear 0 1")')
  write(62,'("ydef 1 linear 0 1")')
  write(62,'("zdef",i6," levels")') nr
  write(62,'(5f12.3)') r
  write(62,'("tdef 1 linear 00Z01jan2001 1yr")')
  write(62,'("vars",i6)') 30
  write(62,'("n064dp1000  ",i6," 99 n064dp1000 ")') nr
  write(62,'("n064lp1000  ",i6," 99 n064lp1000 ")') nr
  write(62,'("n064dp750   ",i6," 99 n064dp750  ")') nr
  write(62,'("n064lp750   ",i6," 99 n064lp750  ")') nr
  write(62,'("n064dp500   ",i6," 99 n064dp500  ")') nr
  write(62,'("n064lp500   ",i6," 99 n064lp500  ")') nr
  write(62,'("e137dp1000  ",i6," 99 e137dp1000")') nr
  write(62,'("e137lp1000  ",i6," 99 e137lp1000")') nr
  write(62,'("e137dp750   ",i6," 99 e137dp750")') nr
  write(62,'("e137lp750   ",i6," 99 e137lp750")') nr
  write(62,'("e137dp500   ",i6," 99 e137dp500 ")') nr
  write(62,'("e137lp500   ",i6," 99 e137lp500 ")') nr
  write(62,'("A128dp1000  ",i6," 99 A128dp1000")') nr
  write(62,'("A128lp1000  ",i6," 99 A128lp1000")') nr
  write(62,'("A128dp750   ",i6," 99 A128dp750 ")') nr
  write(62,'("A128lp750   ",i6," 99 A128lp750 ")') nr
  write(62,'("A128dp500   ",i6," 99 A128dp500 ")') nr
  write(62,'("A128lp500   ",i6," 99 A128lp500 ")') nr
  write(62,'("B128dp1000  ",i6," 99 B128dp1000")') nr
  write(62,'("B128lp1000  ",i6," 99 B128lp1000")') nr
  write(62,'("B128dp750   ",i6," 99 B128dp750 ")') nr
  write(62,'("B128lp750   ",i6," 99 B128lp750 ")') nr
  write(62,'("B128dp500   ",i6," 99 B128dp500 ")') nr
  write(62,'("B128lp500   ",i6," 99 B128lp500 ")') nr
  write(62,'("C128dp1000  ",i6," 99 C128dp1000")') nr
  write(62,'("C128lp1000  ",i6," 99 C128lp1000")') nr
  write(62,'("C128dp750   ",i6," 99 C128dp750 ")') nr
  write(62,'("C128lp750   ",i6," 99 C128lp750 ")') nr
  write(62,'("C128dp500   ",i6," 99 C128dp500 ")') nr
  write(62,'("C128lp500   ",i6," 99 C128lp500 ")') nr
  write(62,'("endvars")')
  close(62)
end program
subroutine hybdel(chyb,psfc,nr,r,d,e)
  implicit none
  integer,intent(in):: nr
  character(*),intent(in):: chyb
  real,intent(in):: psfc,r(0:nr)
  real,intent(out):: d(0:nr),e(0:nr)
  integer ios,it,nhyb,k,n
  real ak(0:500),bk(0:500)
  real pd,pu,p(500),dp(500),ep(500)
  real w
  open(11,file=chyb)
  read(11,*) it,nhyb,(ak(k),bk(k),k=0,nhyb)
  close(11)
  do k=1,nhyb
    pd=psfc*bk(k-1)+ak(k-1)
    pu=psfc*bk(k)+ak(k)
    p(k)=(pd+pu)/2
    dp(k)=pd-pu
    if(k.lt.nhyb) ep(k)=log(pd/pu)
  enddo
   print *,'nhyb,psfc=',nhyb,psfc
   print *,'p=',p(1:nhyb)
   print *,'dp=',dp(1:nhyb)
  ep(nhyb)=3*ep(nhyb-1)-2*ep(nhyb-2)
  ep(nhyb)=1
  do n=1,nr
    d(n)=-9999.
    e(n)=-9999.
    do k=1,nhyb-1
      w=(p(k)-r(n))/(p(k)-p(k+1))
      if(w.ge.0.and.w.le.1) then
        d(n)=dp(k)+w*(dp(k+1)-dp(k))
        e(n)=ep(k)+w*(ep(k+1)-ep(k))
      endif
    enddo
  enddo
end subroutine
