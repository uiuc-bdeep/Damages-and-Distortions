c
c Bootstrap 200 sets of first-stage probit estimates that are then
c used to generate 200 predicted probabilities of a response for members
c of each race group in each tract.  Those bootstrapped probabilities are
c used in the second-stage consideration set estimation.  We use nX 
c experiments from each city for the first-stage estimation.  Predications 
c are made for each of nchoiceX tracts in each city X.
c
c Variables used in probit prediction include school quality, murders,
c cafes, and log(RSEI), race and race^2 (i.e., same variables as used
c in estimation).  We also include a dummy variable for each city, and 
c latitude and longitude also enter differently for each city.  There 
c are 21 total parameters estimated for each race group.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PROGRAM first_stage_probit
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Setting Model Parameters                             
c 1 = Atlanta
c 2 = Houston
c 3 = Philadelphia
c 4 = Cleveland
c 5 = San Jose
c nX = number of experiments used for first-stage bootstrap
c      estimation in city X
c nchoiceX = number of tracts in choice set for city X
c            used in second stage estimation
c npp = number of parameters to be estimated in first-
c       stage bootstrap
c nb = number of bootstrap draws
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER (nchoice1=129,nchoice2=322,nchoice3=138)
      PARAMETER (nchoice4=92,nchoice5=184)
      PARAMETER (n1=871,n2=1505,n3=318,n4=138,n5=324)
      PARAMETER (nb=200,npp=21,ntot=n1+n2+n3+n4+n5,nb5=5*nb)
      INTEGER j,k,m,iseed1,iseed1a(nb5),iseed1b,iseed2(nb),
     &   r1(n1),r2(n2),r3(n3),r4(n4),r5(n5),iparam(7),
     &   maxfcn,ibtype,seedcount
      DOUBLE PRECISION z1(47,nchoice1),z2(47,nchoice2),
     &   z3(47,nchoice3),z4(47,nchoice4),z5(47,nchoice5),
     &   pblack1(nchoice1),pwhite1(nchoice1),phisp1(nchoice1),
     &   pblack2(nchoice2),pwhite2(nchoice2),phisp2(nchoice2),
     &   pblack3(nchoice3),pwhite3(nchoice3),phisp3(nchoice3),
     &   pblack4(nchoice4),pwhite4(nchoice4),phisp4(nchoice4),
     &   pblack5(nchoice5),pwhite5(nchoice5),phisp5(nchoice5),
     &   lat1(nchoice1),lon1(nchoice1),lat2(nchoice2),
     &   lon2(nchoice2),lat3(nchoice3),lon3(nchoice3),lat4(nchoice4),
     &   lon4(nchoice4),lat5(nchoice5),lon5(nchoice5),
     &   x1(9,nchoice1),x2(9,nchoice2),x3(9,nchoice3),
     &   x4(9,nchoice4),x5(9,nchoice5),xb1(3,nchoice1),
     &   xb2(3,nchoice2),xb3(3,nchoice3),xb4(3,nchoice4),
     &   xb5(3,nchoice5),bt(3,npp),b(3,9),p1(3,nchoice1),
     &   p2(3,nchoice2),p3(3,nchoice3),p4(3,nchoice4),
     &   p5(3,nchoice5),aschool1(nchoice1),aschool2(nchoice2),
     &   aschool3(nchoice3),aschool4(nchoice4),aschool5(nchoice5),
     &   bs(npp,ntot),pstart(npp),pscale(npp),pguess(npp),
     &   pub(npp),plb(npp),rparam(7),fscale,ftol,par(npp),fvalue,
     &   brent1(n1),trent1(n1),bschool1(n1),tschool1(n1),
     &   bcafe1(n1),tcafe1(n1),brsei1(n1),trsei1(n1),bpwhite1(n1),
     &   tpwhite1(n1),blat1(n1),blon1(n1),bres1(3,n1),
     &   brent2(n2),trent2(n2),bschool2(n2),tschool2(n2),
     &   bcafe2(n2),tcafe2(n2),brsei2(n2),trsei2(n2),bpwhite2(n2),
     &   tpwhite2(n2),blat2(n2),blon2(n2),bres2(3,n2),
     &   brent3(n3),trent3(n3),bschool3(n3),tschool3(n3),
     &   bcafe3(n3),tcafe3(n3),brsei3(n3),trsei3(n3),bpwhite3(n3),
     &   tpwhite3(n3),blat3(n3),blon3(n3),bres3(3,n3),
     &   brent4(n4),trent4(n4),bschool4(n4),tschool4(n4),
     &   bcafe4(n4),tcafe4(n4),brsei4(n4),trsei4(n4),bpwhite4(n4),
     &   tpwhite4(n4),blat4(n4),blon4(n4),bres4(3,n4),
     &   brent5(n5),trent5(n5),bschool5(n5),tschool5(n5),
     &   bcafe5(n5),tcafe5(n5),brsei5(n5),trsei5(n5),bpwhite5(n5),
     &   tpwhite5(n5),blat5(n5),blon5(n5),bres5(3,n5),
     &   tw1(n1),tw2(n2),tw3(n3),tw4(n4),tw5(n5),
     &   tb1(n1),tb2(n2),tb3(n3),tb4(n4),tb5(n5),
     &   th1(n1),th2(n2),th3(n3),th4(n4),th5(n5),
     &   tlat1(n1),tlon1(n1),tlat2(n2),tlon2(n2),
     &   tlat3(n3),tlon3(n3),tlat4(n4),tlon4(n4),
     &   tlat5(n5),tlon5(n5),y(ntot),bsfit(ntot),
     &   bmurders1(n1),tmurders1(n1),bmurders2(n2),tmurders2(n2),
     &   bmurders3(n3),tmurders3(n3),bmurders4(n4),tmurders4(n4),
     &   bmurders5(n5),tmurders5(n5),race1(n1),race2(n2),
     &   race3(n3),race4(n4),race5(n5),
     &   bpblack1(n1),tpblack1(n1),bpblack2(n2),tpblack2(n2),
     &   bpblack3(n3),tpblack3(n3),bpblack4(n4),tpblack4(n4),
     &   bpblack5(n5),tpblack5(n5),
     &   bphisp1(n1),tphisp1(n1),bphisp2(n2),tphisp2(n2),
     &   bphisp3(n3),tphisp3(n3),bphisp4(n4),tphisp4(n4),
     &   bphisp5(n5),tphisp5(n5),f1,f2,f3
c
      common /starter/ pstart
      common /bounds/ plb,pub
      common /data3/ y,bs
c
      external fcn
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c CityX Tract-Level Data
c ----------------------
c Read in data used to make predictions of response probabilties for 
c testers from each race group for use in second-stage estimation.
c pblackX, pwhiteX, phispX = percent tract in race group (0 to 100).
c See data dictionary for other variables
c
      OPEN(20,file='../cleaned_data/tracts_ATL.txt')
      REWIND(20)
      DO j=1,nchoice1
         READ(20,*) z1(1,j),z1(2,j),z1(3,j),z1(4,j),
     &      z1(5,j),z1(6,j),z1(7,j),z1(8,j),z1(9,j),
     &      z1(10,j),z1(11,j),z1(12,j),z1(13,j),z1(14,j),
     &      z1(15,j),z1(16,j),z1(17,j),z1(18,j),z1(19,j),
     &      z1(20,j),z1(21,j),z1(22,j),z1(23,j),z1(24,j),
     &      z1(25,j),z1(26,j),z1(27,j),z1(28,j),z1(29,j),
     &      z1(30,j),z1(31,j),z1(32,j),z1(33,j),z1(34,j),
     &      z1(35,j),z1(36,j),z1(37,j),z1(38,j),z1(39,j),
     &      z1(40,j),z1(41,j),z1(42,j),z1(43,j),z1(44,j),
     &      z1(45,j),z1(46,j),z1(47,j)  
         pblack1(j) = z1(3,j)*100.d0
         pwhite1(j) = z1(2,j)*100.d0
         phisp1(j) = z1(4,j)*100.d0
      enddo
c
      OPEN(21,file='../cleaned_data/tracts_HOU.txt')
      REWIND(21)
      DO j=1,nchoice2
         READ(21,*) z2(1,j),z2(2,j),z2(3,j),z2(4,j),
     &      z2(5,j),z2(6,j),z2(7,j),z2(8,j),z2(9,j),
     &      z2(10,j),z2(11,j),z2(12,j),z2(13,j),z2(14,j),
     &      z2(15,j),z2(16,j),z2(17,j),z2(18,j),z2(19,j),
     &      z2(20,j),z2(21,j),z2(22,j),z2(23,j),z2(24,j),
     &      z2(25,j),z2(26,j),z2(27,j),z2(28,j),z2(29,j),
     &      z2(30,j),z2(31,j),z2(32,j),z2(33,j),z2(34,j),
     &      z2(35,j),z2(36,j),z2(37,j),z2(38,j),z2(39,j),
     &      z2(40,j),z2(41,j),z2(42,j),z2(43,j),z2(44,j),
     &      z2(45,j),z2(46,j),z2(47,j)
         pblack2(j) = z2(3,j)*100.d0
         pwhite2(j) = z2(2,j)*100.d0
         phisp2(j) = z2(4,j)*100.d0   
      enddo
c
      OPEN(22,file='../cleaned_data/tracts_PHL.txt')
      REWIND(22)
      DO j=1,nchoice3
         READ(22,*) z3(1,j),z3(2,j),z3(3,j),z3(4,j),
     &      z3(5,j),z3(6,j),z3(7,j),z3(8,j),z3(9,j),
     &      z3(10,j),z3(11,j),z3(12,j),z3(13,j),z3(14,j),
     &      z3(15,j),z3(16,j),z3(17,j),z3(18,j),z3(19,j),
     &      z3(20,j),z3(21,j),z3(22,j),z3(23,j),z3(24,j),
     &      z3(25,j),z3(26,j),z3(27,j),z3(28,j),z3(29,j),
     &      z3(30,j),z3(31,j),z3(32,j),z3(33,j),z3(34,j),
     &      z3(35,j),z3(36,j),z3(37,j),z3(38,j),z3(39,j),
     &      z3(40,j),z3(41,j),z3(42,j),z3(43,j),z3(44,j),
     &      z3(45,j),z3(46,j),z3(47,j)
         pblack3(j) = z3(3,j)*100.d0
         pwhite3(j) = z3(2,j)*100.d0
         phisp3(j) = z3(4,j)*100.d0
      enddo
c
      OPEN(23,file='../cleaned_data/tracts_CLE.txt')
      REWIND(23)
      DO j=1,nchoice4
         READ(23,*) z4(1,j),z4(2,j),z4(3,j),z4(4,j),
     &      z4(5,j),z4(6,j),z4(7,j),z4(8,j),z4(9,j),
     &      z4(10,j),z4(11,j),z4(12,j),z4(13,j),z4(14,j),
     &      z4(15,j),z4(16,j),z4(17,j),z4(18,j),z4(19,j),
     &      z4(20,j),z4(21,j),z4(22,j),z4(23,j),z4(24,j),
     &      z4(25,j),z4(26,j),z4(27,j),z4(28,j),z4(29,j),
     &      z4(30,j),z4(31,j),z4(32,j),z4(33,j),z4(34,j),
     &      z4(35,j),z4(36,j),z4(37,j),z4(38,j),z4(39,j),
     &      z4(40,j),z4(41,j),z4(42,j),z4(43,j),z4(44,j),
     &      z4(45,j),z4(46,j),z4(47,j)
         pblack4(j) = z4(3,j)*100.d0
         pwhite4(j) = z4(2,j)*100.d0
         phisp4(j) = z4(4,j)*100.d0  
      enddo
c
      OPEN(24,file='../cleaned_data/tracts_SJC.txt')
      REWIND(24)
      DO j=1,nchoice5
         READ(24,*) z5(1,j),z5(2,j),z5(3,j),z5(4,j),
     &      z5(5,j),z5(6,j),z5(7,j),z5(8,j),z5(9,j),
     &      z5(10,j),z5(11,j),z5(12,j),z5(13,j),z5(14,j),
     &      z5(15,j),z5(16,j),z5(17,j),z5(18,j),z5(19,j),
     &      z5(20,j),z5(21,j),z5(22,j),z5(23,j),z5(24,j),
     &      z5(25,j),z5(26,j),z5(27,j),z5(28,j),z5(29,j),
     &      z5(30,j),z5(31,j),z5(32,j),z5(33,j),z5(34,j),
     &      z5(35,j),z5(36,j),z5(37,j),z5(38,j),z5(39,j),
     &      z5(40,j),z5(41,j),z5(42,j),z5(43,j),z5(44,j),
     &      z5(45,j),z5(46,j),z5(47,j)
         pblack5(j) = z5(3,j)*100.d0
         pwhite5(j) = z5(2,j)*100.d0
         phisp5(j) = z5(4,j)*100.d0       
      enddo
c
c
c Read in Tract-Level Latitudes and Longitudes
c

      open(25,file='../cleaned_data/latlon_atl.txt')
      rewind(25)
      do j = 1,nchoice1
         read(25,*) lon1(j),lat1(j)
      enddo
c
      open(26,file='../cleaned_data/latlon_hou.txt')
      rewind(26)
      do j = 1,nchoice2
         read(26,*) lon2(j),lat2(j)
      enddo
c
      open(27,file='../cleaned_data/latlon_phl.txt')
      rewind(27)
      do j = 1,nchoice3
         read(27,*) lon3(j),lat3(j)
      enddo
c
      open(28,file='../cleaned_data/latlon_cle.txt')
      rewind(28)
      do j = 1,nchoice4
         read(28,*) lon4(j),lat4(j)
      enddo
c
      open(29,file='../cleaned_data/latlon_sjc.txt')
      rewind(29)
      do j = 1,nchoice5
         read(29,*) lon5(j),lat5(j)
      enddo
c
c
c Read in experimental outcomes and tract attribute data for estimation of 
c bootstrap probits.  nX experiments are available in each city X. Variable 
c names are preceded with "t" to indicate temporary status.  These variables 
c will be used below to create bootstrap samples of experimental results by 
c taking random draws
c with replacement.
c
      open(31,file='probdata1.txt')
      rewind(31)
      do j = 1,n1
         read(31,*) trent1(j),tschool1(j),tcafe1(j),tmurders1(j),
     &      trsei1(j),tpwhite1(j),tpblack1(j),tphisp1(j),tlat1(j),
     &      tlon1(j),tw1(j),tb1(j),th1(j)
      enddo
      open(32,file='probdata2.txt')
      rewind(32)
      do j = 1,n2
         read(32,*) trent2(j),tschool2(j),tcafe2(j),tmurders2(j),
     &      trsei2(j),tpwhite2(j),tpblack2(j),tphisp2(j),tlat2(j),
     &      tlon2(j),tw2(j),tb2(j),th2(j)
      enddo
      open(33,file='probdata3.txt')
      rewind(33)
      do j = 1,n3
         read(33,*) trent3(j),tschool3(j),tcafe3(j),tmurders3(j),
     &      trsei3(j),tpwhite3(j),tpblack3(j),tphisp3(j),tlat3(j),
     &      tlon3(j),tw3(j),tb3(j),th3(j)
      enddo
      open(34,file='probdata4.txt')
      rewind(34)
      do j = 1,n4
         read(34,*) trent4(j),tschool4(j),tcafe4(j),tmurders4(j),
     &      trsei4(j),tpwhite4(j),tpblack4(j),tphisp4(j),tlat4(j),
     &      tlon4(j),tw4(j),tb4(j),th4(j)
      enddo
      open(35,file='probdata5.txt')
      rewind(35)
      do j = 1,n5
         read(35,*) trent5(j),tschool5(j),tcafe5(j),tmurders5(j),
     &      trsei5(j),tpwhite5(j),tpblack5(j),tphisp5(j),tlat5(j),
     &      tlon5(j),tw5(j),tb5(j),th5(j)
      enddo
c
c Open files where output will be stored (200 bootstrap probabilities
c for each tract in each city X)
c.
      open(100,file='fitprob1_200.txt')
      rewind(100)
      open(110,file='fitprob2_200.txt')
      rewind(110)
      open(120,file='fitprob3_200.txt')
      rewind(120)
      open(130,file='fitprob4_200.txt')
      rewind(130)
      open(140,file='fitprob5_200.txt')
      rewind(140)
 150  format(2i8,3f12.5)
c
c Open file where bootstrap parameter estimates will be stored.
c
      open(160,file='prob_estimates_200a.txt')
      rewind(160)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c BOOTSTRAP PROBIT ESTIMATION
c
c Generate vector of 5 x number bootstrap draws random seeds
c
      iseed1 = 9199297285
      call rnset(iseed1)
      call rnund(nb5,999999999,iseed1a)
c
c For each of nb bootstrap draws, for each city generate random vector of 
c indexes of size nX with replacement and use to populate bootstrap data sets.
c Setting rX(j) = j will return the actual data set.
c 
      seedcount = 0
      do k = 1,nb
         write(*,*) 'bootiter =',k
         seedcount = seedcount+1
         iseed1b = iseed1a(seedcount)
         call rnset(iseed1b)
         call rnund(n1,n1,r1)
         do j = 1,n1
c            r1(j) = j
            brent1(j) = trent1(r1(j))
            bschool1(j) = tschool1(r1(j))
            bcafe1(j) = tcafe1(r1(j))
            bmurders1(j) = tmurders1(r1(j))
            brsei1(j) = trsei1(r1(j))
            bpwhite1(j) = tpwhite1(r1(j))
            bpblack1(j) = tpblack1(r1(j))
            bphisp1(j) = tphisp1(r1(j))
            blat1(j) = tlat1(r1(j))
            blon1(j) = tlon1(r1(j))
            bres1(1,j) = tw1(r1(j))
            bres1(2,j) = tb1(r1(j))
            bres1(3,j) = th1(r1(j))
         enddo
         seedcount = seedcount+1
         iseed1b = iseed1a(seedcount)
         call rnset(iseed1b)
         call rnund(n2,n2,r2)
         do j = 1,n2
c            r2(j) = j
            brent2(j) = trent2(r2(j))
            bschool2(j) = tschool2(r2(j))
            bcafe2(j) = tcafe2(r2(j))
            bmurders2(j) = tmurders2(r2(j))
            brsei2(j) = trsei2(r2(j))
            bpwhite2(j) = tpwhite2(r2(j))
            bpblack2(j) = tpblack2(r2(j))
            bphisp2(j) = tphisp2(r2(j))
            blat2(j) = tlat2(r2(j))
            blon2(j) = tlon2(r2(j))
            bres2(1,j) = tw2(r2(j))
            bres2(2,j) = tb2(r2(j))
            bres2(3,j) = th2(r2(j))
         enddo
         seedcount = seedcount+1
         iseed1b = iseed1a(seedcount)
         call rnset(iseed1b)
         call rnund(n3,n3,r3)
         do j = 1,n3
c            r3(j) = j
            brent3(j) = trent3(r3(j))
            bschool3(j) = tschool3(r3(j))
            bcafe3(j) = tcafe3(r3(j))
            bmurders3(j) = tmurders3(r3(j))
            brsei3(j) = trsei3(r3(j))
            bpwhite3(j) = tpwhite3(r3(j))
            bpblack3(j) = tpblack3(r3(j))
            bphisp3(j) = tphisp3(r3(j))
            blat3(j) = tlat3(r3(j))
            blon3(j) = tlon3(r3(j))
            bres3(1,j) = tw3(r3(j))
            bres3(2,j) = tb3(r3(j))
            bres3(3,j) = th3(r3(j))
         enddo
         seedcount = seedcount+1
         iseed1b = iseed1a(seedcount)
         call rnset(iseed1b)
         call rnund(n4,n4,r4)
         do j = 1,n4
c            r4(j) = j
            brent4(j) = trent4(r4(j))
            bschool4(j) = tschool4(r4(j))
            bcafe4(j) = tcafe4(r4(j))
            bmurders4(j) = tmurders4(r4(j))
            brsei4(j) = trsei4(r4(j))
            bpwhite4(j) = tpwhite4(r4(j))
            bpblack4(j) = tpblack4(r4(j))
            bphisp4(j) = tphisp4(r4(j))
            blat4(j) = tlat4(r4(j))
            blon4(j) = tlon4(r4(j))
            bres4(1,j) = tw4(r4(j))
            bres4(2,j) = tb4(r4(j))
            bres4(3,j) = th4(r4(j))
         enddo
         seedcount = seedcount+1
         iseed1b = iseed1a(seedcount)
         call rnset(iseed1b)
         call rnund(n5,n5,r5)
         do j = 1,n5
c            r5(j) = j
            brent5(j) = trent5(r5(j))
            bschool5(j) = tschool5(r5(j))
            bcafe5(j) = tcafe5(r5(j))
            bmurders5(j) = tmurders5(r5(j))
            brsei5(j) = trsei5(r5(j))
            bpwhite5(j) = tpwhite5(r5(j))
            bpblack5(j) = tpblack5(r5(j))
            bphisp5(j) = tphisp5(r5(j))
            blat5(j) = tlat5(r5(j))
            blon5(j) = tlon5(r5(j))
            bres5(1,j) = tw5(r5(j))
            bres5(2,j) = tb5(r5(j))
            bres5(3,j) = th5(r5(j))
         enddo
c
c Run separate code for each race group m1 = 1,2,3
c
         do m1 = 1,3
            if (m1.eq.1) then
               do j = 1,n1
                  race1(j) = bpwhite1(j)
               enddo
               do j = 1,n2
                  race2(j) = bpwhite2(j)
               enddo
               do j = 1,n3
                  race3(j) = bpwhite3(j)
               enddo
               do j = 1,n4
                  race4(j) = bpwhite4(j)
               enddo
               do j = 1,n5
                  race5(j) = bpwhite5(j)
               enddo
            endif
            if (m1.eq.2) then
               do j = 1,n1
                  race1(j) = bpblack1(j)
               enddo
               do j = 1,n2
                  race2(j) = bpblack2(j)
               enddo
               do j = 1,n3
                  race3(j) = bpblack3(j)
               enddo
               do j = 1,n4
                  race4(j) = bpblack4(j)
               enddo
               do j = 1,n5
                  race5(j) = bpblack5(j)
               enddo
            endif
            if (m1.eq.3) then
               do j = 1,n1
                  race1(j) = bphisp1(j)
               enddo
               do j = 1,n2
                  race2(j) = bphisp2(j)
               enddo
               do j = 1,n3
                  race3(j) = bphisp3(j)
               enddo
               do j = 1,n4
                  race4(j) = bphisp4(j)
               enddo
               do j = 1,n5
                  race5(j) = bphisp5(j)
               enddo
            endif
c
c Build data set for bootstrap estimation.  y(j) = 1 if tester of race
c m1 in experiment j for city X received a postive response from landlord 
c
            do j = 1,n1
               y(j) = bres1(m1,j)
               bs(1,j) = bschool1(j)
               bs(2,j) = bcafe1(j)
               bs(3,j) = bmurders1(j)
               bs(4,j) = dlog(brsei1(j))
               bs(5,j) = race1(j)
               bs(6,j) = race1(j)*race1(j)
               bs(7,j) = 1.d0
               bs(8,j) = 0.d0
               bs(9,j) = 0.d0
               bs(10,j) = 0.d0
               bs(11,j) = 0.d0
               bs(12,j) = blat1(j) 
               bs(13,j) = 0.d0
               bs(14,j) = 0.d0
               bs(15,j) = 0.d0
               bs(16,j) = 0.d0
               bs(17,j) = blon1(j)
               bs(18,j) = 0.d0
               bs(19,j) = 0.d0
               bs(20,j) = 0.d0
               bs(21,j) = 0.d0
            enddo
            do j = 1,n2
               y(n1+j) = bres2(m1,j)
               bs(1,n1+j) = bschool2(j)
               bs(2,n1+j) = bcafe2(j)
               bs(3,n1+j) = bmurders2(j)
               bs(4,n1+j) = dlog(brsei2(j))
               bs(5,n1+j) = race2(j)
               bs(6,n1+j) = race2(j)*race2(j)
               bs(7,n1+j) = 1.d0
               bs(8,n1+j) = 1.d0
               bs(9,n1+j) = 0.d0
               bs(10,n1+j) = 0.d0
               bs(11,n1+j) = 0.d0
               bs(12,n1+j) = 0.d0
               bs(13,n1+j) = blat2(j) 
               bs(14,n1+j) = 0.d0
               bs(15,n1+j) = 0.d0
               bs(16,n1+j) = 0.d0
               bs(17,n1+j) = 0.d0
               bs(18,n1+j) = blon2(j)
               bs(19,n1+j) = 0.d0
               bs(20,n1+j) = 0.d0
               bs(21,n1+j) = 0.d0
            enddo
            do j = 1,n3
               y(n1+n2+j) = bres3(m1,j)
               bs(1,n1+n2+j) = bschool3(j)
               bs(2,n1+n2+j) = bcafe3(j)
               bs(3,n1+n2+j) = bmurders3(j)
               bs(4,n1+n2+j) = dlog(brsei3(j))
               bs(5,n1+n2+j) = race3(j)
               bs(6,n1+n2+j) = race3(j)*race3(j)
               bs(7,n1+n2+j) = 1.d0
               bs(8,n1+n2+j) = 0.d0
               bs(9,n1+n2+j) = 1.d0
               bs(10,n1+n2+j) = 0.d0
               bs(11,n1+n2+j) = 0.d0
               bs(12,n1+n2+j) = 0.d0
               bs(13,n1+n2+j) = 0.d0
               bs(14,n1+n2+j) = blat3(j) 
               bs(15,n1+n2+j) = 0.d0
               bs(16,n1+n2+j) = 0.d0
               bs(17,n1+n2+j) = 0.d0
               bs(18,n1+n2+j) = 0.d0
               bs(19,n1+n2+j) = blon3(j)
               bs(20,n1+n2+j) = 0.d0
               bs(21,n1+n2+j) = 0.d0
            enddo
            do j = 1,n4
               y(n1+n2+n3+j) = bres4(m1,j)
               bs(1,n1+n2+n3+j) = bschool4(j)
               bs(2,n1+n2+n3+j) = bcafe4(j)
               bs(3,n1+n2+n3+j) = bmurders4(j)
               bs(4,n1+n2+n3+j) = dlog(brsei4(j))
               bs(5,n1+n2+n3+j) = race4(j)
               bs(6,n1+n2+n3+j) = race4(j)*race4(j)
               bs(7,n1+n2+n3+j) = 1.d0
               bs(8,n1+n2+n3+j) = 0.d0
               bs(9,n1+n2+n3+j) = 0.d0
               bs(10,n1+n2+n3+j) = 1.d0
               bs(11,n1+n2+n3+j) = 0.d0
               bs(12,n1+n2+n3+j) = 0.d0
               bs(13,n1+n2+n3+j) = 0.d0
               bs(14,n1+n2+n3+j) = 0.d0 
               bs(15,n1+n2+n3+j) = blat4(j)
               bs(16,n1+n2+n3+j) = 0.d0
               bs(17,n1+n2+n3+j) = 0.d0
               bs(18,n1+n2+n3+j) = 0.d0
               bs(19,n1+n2+n3+j) = 0.d0
               bs(20,n1+n2+n3+j) = blon4(j)
               bs(21,n1+n2+n3+j) = 0.d0
            enddo
            do j = 1,n5
               y(n1+n2+n3+n4+j) = bres5(m1,j)
               bs(1,n1+n2+n3+n4+j) = bschool5(j)
               bs(2,n1+n2+n3+n4+j) = bcafe5(j)
               bs(3,n1+n2+n3+n4+j) = bmurders5(j)
               bs(4,n1+n2+n3+n4+j) = dlog(brsei5(j))
               bs(5,n1+n2+n3+n4+j) = race5(j)
               bs(6,n1+n2+n3+n4+j) = race5(j)*race5(j)
               bs(7,n1+n2+n3+n4+j) = 1.d0
               bs(8,n1+n2+n3+n4+j) = 0.d0
               bs(9,n1+n2+n3+n4+j) = 0.d0
               bs(10,n1+n2+n3+n4+j) = 0.d0
               bs(11,n1+n2+n3+n4+j) = 1.d0
               bs(12,n1+n2+n3+n4+j) = 0.d0
               bs(13,n1+n2+n3+n4+j) = 0.d0
               bs(14,n1+n2+n3+n4+j) = 0.d0 
               bs(15,n1+n2+n3+n4+j) = 0.d0
               bs(16,n1+n2+n3+n4+j) = blat5(j)
               bs(17,n1+n2+n3+n4+j) = 0.d0
               bs(18,n1+n2+n3+n4+j) = 0.d0
               bs(19,n1+n2+n3+n4+j) = 0.d0
               bs(20,n1+n2+n3+n4+j) = 0.d0
               bs(21,n1+n2+n3+n4+j) = blon5(j)
            enddo
c
c Set starting parameter values (these are the optimized values
c using actual data).
c
            if (m1.eq.1) then
               pstart(1) =   .0072152d0
               pstart(2) =   .0036576d0
               pstart(3) =   .0000139d0
               pstart(4) =   .1418094d0
               pstart(5) =   .2866165d0
               pstart(6) =   -.219227d0
               pstart(7) =   -53.0164d0
               pstart(8) =   24.97375d0
               pstart(9) =   15.47625d0
               pstart(10) =  -115.7315d0
               pstart(11) =   126.1653d0
               pstart(12) =  -.2441249d0
               pstart(13) =  -.3440281d0
               pstart(14) =   -.200579d0
               pstart(15) =   2.186052d0
               pstart(16) =   .5760891d0
               pstart(17) =  -.7087807d0
               pstart(18) =   -.375509d0
               pstart(19) =  -.5870988d0
               pstart(20) =  -.9391676d0
               pstart(21) =   .7769735d0
            endif
            if(m1.eq.2) then
               pstart(1) =   -.004631d0
               pstart(2) =   .0018763d0
               pstart(3) =   .0001974d0
               pstart(4) =   .1678978d0
               pstart(5) =   .1218061d0
               pstart(6) =  -.5497726d0
               pstart(7) =  -42.51271d0
               pstart(8) =   .9133692d0
               pstart(9) =   82.65388d0
               pstart(10) =   56.04355d0
               pstart(11) =   136.1588d0
               pstart(12) =  -.3241021d0
               pstart(13) =  -.4311636d0
               pstart(14) =   .1457474d0
               pstart(15) =   .1884512d0
               pstart(16) =   .9592426d0
               pstart(17) =   -.615478d0
               pstart(18) =  -.5432785d0
               pstart(19) =   .6335175d0
               pstart(20) =   .2799984d0
               pstart(21) =   1.064389d0
            endif
            if(m1.eq.3) then  
               pstart(1) =  -.0076934d0
               pstart(2) =    .001546d0
               pstart(3) =   -.000175d0
               pstart(4) =   .1931302d0
               pstart(5) =  -.5527991d0
               pstart(6) =   .8065145d0
               pstart(7) =  -81.51162d0
               pstart(8) =   19.77554d0
               pstart(9) =   71.13838d0
               pstart(10) =  -31.51688d0
               pstart(11) =   63.92919d0
               pstart(12) =   .0110577d0
               pstart(13) =  -.3600295d0
               pstart(14) =   1.531672d0
               pstart(15) =   1.926601d0
               pstart(16) =  -.2290794d0
               pstart(17) =  -.9415066d0
               pstart(18) =  -.7301241d0
               pstart(19) =   .6972624d0
               pstart(20) =  -.3816439d0
               pstart(21) =  -.2135958d0
            endif
c
c "pstart" is a scaling parameter.  Fortran estimates a vector of y's (size = nparam)
c We guess that y=1 to start with, and scale it's vale to 1.  We restrict fortran to
c look at y's between -10 and 10.  The actual parameter value is b = pstart*y.  I.e., 
c if y were estimated to be 1, then it would just give us pstart from above.
c
            do i = 1,npp
               pscale(i) = 1.d0
               pguess(i) = 1.d0
               plb(i) = -10.d0
               pub(i) = 10.d0
            enddo
            do i = 7,11
               plb(i) = -3.d0
               pub(i) = 3.d0
            enddo
            fscale = 1.d0
            CALL du4inf(iparam,rparam)
c            iparam(1) = 0
            rparam(1) = 0.0001d0
            rparam(2) = 0.0001d0
            iparam(3) = 10000
            maxfcn = 10000000
            ftol = 0.001d0
            ibtype = 0
c            print*,10
c            call dbcpol(fcn,npp,pguess,ibtype,plb,pub,ftol,
c     &         maxfcn,par,fvalue)
            CALL dbconf(fcn,npp,pguess,ibtype,plb,pub,pscale,fscale,
     &          iparam,rparam,par,fvalue)
c
c Set bootstrapped parameter estimates associated with parameters i = 1,...,npp 
c for race group m1
c
            do i = 1,npp
               bt(m1,i) = pstart(i)*par(i)
            enddo
c
c Generate fitted probability for all tracts across all cities j = 1,...,ntot
c
            do j = 1,ntot
               bsfit(j) = 0.d0
               do i = 1,npp
                  bsfit(j) = bsfit(j)+bt(m1,i)*bs(i,j)
               enddo
            enddo
         enddo
c
c Output all npp estimated parameters for each race group m1 = 1,2,3
         do i = 1,npp
            write(160,161) k,i,bt(1,i)
         enddo
         do i = 1,npp
            write(160,161) k,i,bt(2,i)
         enddo
         do i = 1,npp
            write(160,161) k,i,bt(3,i)
         enddo
 161     format(2i10,f20.10)
c
c Use estimates to predict response probabilities in each census tract used in
c second-stage estimation.  Assign tract attributes corresponding to attributes
c used in probit estimations for each city.
c
         do m1 = 1,3
            do j = 1,nchoice1
               x1(1,j) = (z1(13,j)*z1(14,j)*z1(15,j))**(1.d0/3.d0)
               x1(2,j) = z1(19,j)
               x1(3,j) = z1(42,j)
               x1(4,j) = dlog(z1(46,j))
               if (m1.eq.1) then
                  x1(5,j) = pwhite1(j)/100.d0
                  x1(6,j) = (pwhite1(j)/100.d0)**2.d0
               endif
               if (m1.eq.2) then
                  x1(5,j) = pblack1(j)/100.d0
                  x1(6,j) = (pblack1(j)/100.d0)**2.d0
               endif
               if (m1.eq.3) then
                  x1(5,j) = phisp1(j)/100.d0
                  x1(6,j) = (phisp1(j)/100.d0)**2.d0
               endif
               x1(7,j) = 1.d0
               x1(8,j) = lat1(j)
               x1(9,j) = lon1(j)
            enddo
            do j = 1,nchoice2
                x2(1,j) = (z2(13,j)*z2(14,j)*z2(15,j))**(1.d0/3.d0)
                x2(2,j) = z2(19,j)
                x2(3,j) = z2(42,j)
                x2(4,j) = dlog(z2(46,j))
                if (m1.eq.1) then
                    x2(5,j) = pwhite2(j)/100.d0
                    x2(6,j) = (pwhite2(j)/100.d0)**2.d0
                endif
                if (m1.eq.2) then
                    x2(5,j) = pblack2(j)/100.d0
                    x2(6,j) = (pblack2(j)/100.d0)**2.d0
                endif
                if (m1.eq.3) then
                    x2(5,j) = phisp2(j)/100.d0
                    x2(6,j) = (phisp2(j)/100.d0)**2.d0
                endif
                x2(7,j) = 1.d0
                x2(8,j) = lat2(j)
                x2(9,j) = lon2(j)
            enddo
            do j = 1,nchoice3
                x3(1,j) = (z3(13,j)*z3(14,j)*z3(15,j))**(1.d0/3.d0)
                x3(2,j) = z3(19,j)
                x3(3,j) = z3(42,j)
                x3(4,j) = dlog(z3(46,j))
                if (m1.eq.1) then
                    x3(5,j) = pwhite3(j)/100.d0
                    x3(6,j) = (pwhite3(j)/100.d0)**2.d0
                endif
                if (m1.eq.2) then
                    x3(5,j) = pblack3(j)/100.d0
                    x3(6,j) = (pblack3(j)/100.d0)**2.d0
                endif
                if (m1.eq.3) then
                    x3(5,j) = phisp3(j)/100.d0
                    x3(6,j) = (phisp3(j)/100.d0)**2.d0
                endif
                x3(7,j) = 1.d0
                x3(8,j) = lat3(j)
                x3(9,j) = lon3(j)
            enddo
            do j = 1,nchoice4
                x4(1,j) = (z4(13,j)*z4(14,j)*z4(15,j))**(1.d0/3.d0)
                x4(2,j) = z4(19,j)
                x4(3,j) = z4(42,j)
                x4(4,j) = dlog(z4(46,j))
                if (m1.eq.1) then
                    x4(5,j) = pwhite4(j)/100.d0
                    x4(6,j) = (pwhite4(j)/100.d0)**2.d0
                endif
                if (m1.eq.2) then
                    x4(5,j) = pblack4(j)/100.d0
                    x4(6,j) = (pblack4(j)/100.d0)**2.d0
                endif
                if (m1.eq.3) then
                    x4(5,j) = phisp4(j)/100.d0
                    x4(6,j) = (phisp4(j)/100.d0)**2.d0
                endif
                x4(7,j) = 1.d0
                x4(8,j) = lat4(j)
                x4(9,j) = lon4(j)
            enddo
            do j = 1,nchoice5
                x5(1,j) = (z5(13,j)*z5(14,j)*z5(15,j))**(1.d0/3.d0)
                x5(2,j) = z5(19,j)
                x5(3,j) = z5(42,j)
                x5(4,j) = dlog(z5(46,j))
                if (m1.eq.1) then
                    x5(5,j) = pwhite5(j)/100.d0
                    x5(6,j) = (pwhite5(j)/100.d0)**2.d0
                endif
                if (m1.eq.2) then
                    x5(5,j) = pblack5(j)/100.d0
                    x5(6,j) = (pblack5(j)/100.d0)**2.d0
                endif
                if (m1.eq.3) then
                    x5(5,j) = phisp5(j)/100.d0
                    x5(6,j) = (phisp5(j)/100.d0)**2.d0
                endif
                x5(7,j) = 1.d0
                x5(8,j) = lat5(j)
                x5(9,j) = lon5(j)
            enddo
c 
c Assign parameter values derived from bootstrap run and use to calculate 
c probit probability
c
            do i = 1,7
               b(m1,i) = bt(m1,i)
            enddo
            do j = 1,nchoice1
               b(m1,8) = bt(m1,12)
               b(m1,9) = bt(m1,17)
               xb1(m1,j) = 0.d0
               do i = 1,9
                  xb1(m1,j) = xb1(m1,j)+b(m1,i)*x1(i,j)
               enddo
c               write(*,*) m1,j,xb1(m1,j)
               p1(m1,j) = dnordf(xb1(m1,j))           
            enddo
            do j = 1,nchoice2
               b(m1,8) = bt(m1,13)
               b(m1,9) = bt(m1,18)
               xb2(m1,j) = bt(m1,8)
               do i = 1,9
                  xb2(m1,j) = xb2(m1,j)+b(m1,i)*x2(i,j)
               enddo
               p2(m1,j) = dnordf(xb2(m1,j))    
            enddo
            do j = 1,nchoice3
               b(m1,8) = bt(m1,14)
               b(m1,9) = bt(m1,19)
               xb3(m1,j) = bt(m1,9)
               do i = 1,9
                  xb3(m1,j) = xb3(m1,j)+b(m1,i)*x3(i,j)
               enddo
               p3(m1,j) = dnordf(xb3(m1,j))    
            enddo
            do j = 1,nchoice4
               b(m1,8) = bt(m1,15)
               b(m1,9) = bt(m1,20)
               xb4(m1,j) = bt(m1,10)
               do i = 1,9
                  xb4(m1,j) = xb4(m1,j)+b(m1,i)*x4(i,j)
               enddo
               p4(m1,j) = dnordf(xb4(m1,j))    
            enddo
            do j = 1,nchoice5
               b(m1,8) = bt(m1,16)
               b(m1,9) = bt(m1,21)
               xb5(m1,j) = bt(m1,11)
               do i = 1,9
                  xb5(m1,j) = xb5(m1,j)+b(m1,i)*x5(i,j)
               enddo
               p5(m1,j) = dnordf(xb5(m1,j))    
            enddo
         enddo
c
c Output fitted probabilities for tract j in city X for bootstrap run k.
c
         do j = 1,nchoice1
            write(100,150) k,j,p1(1,j),p1(2,j),p1(3,j)
         enddo
         do j = 1,nchoice2
            write(110,150) k,j,p2(1,j),p2(2,j),p2(3,j)
         enddo
         do j = 1,nchoice3
            write(120,150) k,j,p3(1,j),p3(2,j),p3(3,j)
         enddo
         do j = 1,nchoice4
            write(130,150) k,j,p4(1,j),p4(2,j),p4(3,j)
         enddo
         do j = 1,nchoice5 
            write(140,150) k,j,p5(1,j),p5(2,j),p5(3,j)
         enddo
      enddo
c
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This subroutine calculates the probit likelihood function.
c We weight the observations from the different cities so that
c each city enters the likelihood function equally (controls for
c the fact that different cities had different numbers of experiments)
c
      SUBROUTINE fcn(n,par,f)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
      PARAMETER(n1=871,n2=1505,n3=318,n4=138,n5=324)
      PARAMETER(npp=21,ntot=n1+n2+n3+n4+n5)
      INTEGER n
      DOUBLE PRECISION bf(npp),f,y(ntot),
     &   pstart(npp),bs(npp,ntot),par(npp),
     &   plb(npp),pub(npp),wt(ntot),bsfit(ntot),
     &   nbsfit(ntot)
c
      common /starter/ pstart
      common /bounds/ plb,pub
      common /data3/ y,bs
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do k = 1,npp
         bf(k) = par(k)*pstart(k)
      enddo
      do j = 1,n1
         wt(j) = 1.d0/(1.d0*n1)
      enddo
      do j = 1,n2
         wt(n1+j) = 1.d0/(1.d0*n2)
      enddo
      do j = 1,n3
         wt(n1+n2+j) = 1.d0/(1.d0*n3)
      enddo
      do j = 1,n4
         wt(n1+n2+n3+j) = 1.d0/(1.d0*n4)
      enddo
      do j = 1,n5
         wt(n1+n2+n3+n4+j) = 1.d0/(1.d0*n5)
      enddo
c
      do j = 1,ntot
         bsfit(j) = 0.d0
         do k = 1,npp
            bsfit(j) = bsfit(j)+bf(k)*bs(k,j)
         enddo
         nbsfit(j) = dnordf(bsfit(j))
      enddo
c
c Aggregate over likelihood values for each experimental observation to get
c total likelihood value.  Use bounds to catch bad parameter values that produce
c under- or over-flow and assign bad likelihood value to those parameters (keeps
c bootstrap procedure from crashing and allows algorithm to continue to search in
c parameter space even after trying a bad parameter vector).  f is reported back
c to the nonlinear search algorithm with the value of the likelihood function for
c this paramater guess.
c
      f = 0.d0
      do j = 1,ntot
         if (((y(j).eq.1.d0).and.(nbsfit(j).lt.0.0000000000001d0)).or.
     &      ((y(j).eq.0.d0).and.(nbsfit(j).gt.0.9999999999999d0))) then
               f = 999999999999999.d9
               go to 900
         endif
         f = f - (y(j)*(dlog(nbsfit(j))) + (1.d0-y(j))*
     &       (dlog(1.d0-nbsfit(j))))*wt(j)
      enddo
c
c This loop flags if any of the parameters hit the bounds set in the nonlinear
c search algorithm.  Bounds can be set very wide, given the catch above for
c parameter values tha cause under or overflow.
c
      do m = 1,npp
         if ((y(m).eq.plb(m)).or.(y(m).eq.pub(m))) then
            write(*,*) 'Constrained Parameter #',m
         endif
      enddo
 900  continue
c      write(*,*) 'Aggregate Objective Function =',f
c
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

