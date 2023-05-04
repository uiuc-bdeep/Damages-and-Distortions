ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This estimation repeats the procedure in the main bootstrap
c estimation code, but allows for preference parameters to be 
c different by race (white versus renter of color) for after-rent 
c income and all neighborhood characteristics including lat/lon.
c Preferences for own-race neighbors already differ by race.
c
c By commenting out lines 448-477 of the code and un-commenting
c lines 485-509, this code can be altered to turn-off consideration
c sets (i.e., all options appear in every individual's choice
c set).  We use that option to estimate the "no consideration set"
c model in the paper.

      PROGRAM est
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
c
      PARAMETER (nchoice1=129,nchoice2=322,nchoice3=138)
      PARAMETER (nchoice4=92,nchoice5=184)
      PARAMETER (nparam=36,nx=5,nsim=50,nbs=1,nbs5=5*nbs)
      PARAMETER (np1=10000,np2=10000,np3=10000,np4=10000,np5=10000)
      PARAMETER (ncnb1=nbs*nchoice1,ncnb2=nbs*nchoice2)
      PARAMETER (ncnb3=nbs*nchoice3,ncnb4=nbs*nchoice4)
      PARAMETER (ncnb5=nbs*nchoice5)
      INTEGER choice1(np1),choice2(np2),choice3(np3),maxfcn,
     &   ibtype,iseed2a(np1),iseed2b(np2),iseed2c(np3),iseed0d,
     &   iparam(7),itcount,iseed0a,iseed0b,iseed0c,choice4(np4),
     &   iseed1a(nsim),iseed1b(nsim),iseed1c(nsim),choice5(np5),
     &   iseed0e,iseed1d(nsim),iseed2d(np4),iseed1e(nsim),
     &   iseed2e(np5),idf1(ncnb1),idf2(ncnb2),idf3(ncnb3),
     &   idf4(ncnb4),idf5(ncnb5),rb1(np1),rb2(np2),rb3(np3),
     &   rb4(np4),rb5(np5),iseed3,iseed3a(nbs5),seedcount,iseed3b,
     &   idnb1(ncnb1),idnb2(ncnb2),idnb3(ncnb3),idnb4(ncnb4),
     &   idnb5(ncnb5),bidnc,tchoice1(np1), tchoice2(np2),
     &   tchoice3(np3),tchoice4(np4),tchoice5(np5),iter,
     &   bootcount
      DOUBLE PRECISION pstart(nparam),yguess(nparam),
     &   yscale(nparam),ylb(nparam),yub(nparam),ftol,
     &   rparam(7),y(nparam),fvalue,fscale,s1(np1),s2(np2),
     &   phisp1(nchoice1),rwhite1(nchoice1),s3(np3),white5(np5),
     &   cut1(np1,nchoice1),r1a(nchoice1),rhisp1(nchoice1),
     &   good1(np1,nchoice1,nsim),wt1a(np1,nsim),hisp4(np4),
     &   z1(47,nchoice1),rblack1(nchoice1),wt2a(np1,nsim),
     &   black1(np1),white1(np1),hisp1(np1),inc1(np1),inc5(np5),
     &   rent1(np1),temp1,med1(7),med2(7),med3(7),black4(np4),
     &   x1(nx,nchoice1),gschool1(nchoice1),rblack5(nchoice5),
     &   pblack1(nchoice1),pwhite1(nchoice1),rblack4(nchoice4),
     &   phisp2(nchoice2),rwhite2(nchoice2),rwhite5(nchoice5),
     &   cut2(np2,nchoice2),r1b(nchoice2),rhisp2(nchoice2),
     &   good2(np2,nchoice2,nsim),wt1b(np2,nsim),rhisp5(nchoice5),
     &   z2(47,nchoice2),rblack2(nchoice2),wt2b(np2,nsim),
     &   black2(np2),white2(np2),hisp2(np2),inc2(np2),white4(np4),
     &   rent2(np2),temp2,x2(nx,nchoice2),gschool2(nchoice2),
     &   pblack2(nchoice2),pwhite2(nchoice2),rwhite4(nchoice4),
     &   phisp3(nchoice3),rwhite3(nchoice3),z5(47,nchoice5),
     &   cut3(np3,nchoice3),r1c(nchoice3),rhisp3(nchoice3),
     &   good3(np3,nchoice3,nsim),wt1c(np3,nsim),rhisp(nchoice4),
     &   z3(47,nchoice3),rblack3(nchoice3),wt2c(np3,nsim),
     &   black3(np3),white3(np3),hisp3(np3),inc3(np3),inc4(np4),
     &   rent3(np3),temp3,dinc1(4,np1),z4(47,nchoice4),
     &   x3(nx,nchoice3),gschool3(nchoice3),par(nbs,nparam),
     &   pblack3(nchoice3),pwhite3(nchoice3),
     &   black5(np5),hisp5(np5),x4(nx,nchoice4),x5(nx,nchoice5),
     &   cut4(np4,nchoice4),cut5(np5,nchoice5),r1d(nchoice4),
     &   r1e(nchoice4),good4(np4,nchoice4,nsim),wt1d(np4,nsim),
     &   good5(np5,nchoice5,nsim),wt1e(np5,nsim),temp4,temp5,
     &   wt2d(np4,nsim),wt2e(np5,nsim),rhisp4(nchoice4),
     &   pwhite5(nchoice5),pblack5(nchoice5),phisp5(nchoice5),
     &   pwhite4(nchoice4),pblack4(nchoice4),phisp4(nchoice4),
     &   aschool1(nchoice1),aschool2(nchoice2),aschool3(nchoice3),
     &   aschool4(nchoice4),aschool5(nchoice5),emed(5),bmed(5),
     &   amed(5),avg32,count32,tri1(nchoice1),tri2(nchoice2),
     &   tri3(nchoice3),tri4(nchoice4),tri5(nchoice5),
     &   dtri1(nchoice1),dtri2(nchoice2),dtri3(nchoice3),
     &   dtri4(nchoice4),dtri5(nchoice5),p1(3,ncnb1),
     &   p2(3,ncnb2),p3(3,ncnb3),p4(3,ncnb4),
     &   p5(3,ncnb5),thisp5(np5),
     &   tinc1(np1),twhite1(np1),tblack1(np1),
     &   thisp1(np1),thisp2(np2),thisp3(np3),thisp4(np4),
     &   tinc2(np2),twhite2(np2),tblack2(np2),
     &   tinc3(np3),twhite3(np3),tblack3(np3),
     &   tinc4(np4),twhite4(np4),tblack4(np4),
     &   tinc5(np5),twhite5(np5),tblack5(np5),
     &   lat1(nchoice1),lon1(nchoice1),lat2(nchoice2),
     &   lon2(nchoice2),lat3(nchoice3),lon3(nchoice3),
     &   lat4(nchoice4),lon4(nchoice4),lat5(nchoice5),
     &   lon5(nchoice5)
c
      common /data1a/ pblack1,pwhite1,phisp1
      common /data2a/ choice1,inc1,black1,
     &   white1,hisp1
      common /simulate1a/ good1
      common /simulate2a/ wt2a
      common /data1b/ pblack2,pwhite2,phisp2
      common /data2b/ choice2,inc2,black2,
     &   white2,hisp2
      common /simulate1b/ good2
      common /simulate2b/ wt2b
      common /data1c/ pblack3,pwhite3,phisp3
      common /data2c/ choice3,inc3,black3,
     &   white3,hisp3
      common /simulate1c/ good3
      common /simulate2c/ wt2c
      common /data1d/ pblack4,pwhite4,phisp4
      common /data2d/ choice4,inc4,black4,
     &   white4,hisp4
      common /simulate1d/ good4
      common /simulate2d/ wt2d
      common /data1e/ pblack5,pwhite5,phisp5
      common /data2e/ choice5,inc5,black5,
     &   white5,hisp5
      common /simulate1e/ good5
      common /simulate2e/ wt2e
      common /starter/ pstart
      common /bounds/ ylb,yub
      common /data3/ x1,x2,x3,x4,x5
      common /counts/ bootcount,iter
      common /latlon1/ lat1,lon1
      common /latlon2/ lat2,lon2
      common /latlon3/ lat3,lon3
      common /latlon4/ lat4,lon4
      common /latlon5/ lat5,lon5
c
      EXTERNAL fcn,fcn1,fcn2,fcn3,fcn4,fcn5
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      OPEN(10,file='../cleaned_data/renters_atl_10.txt')
      REWIND(10)
      DO j=1,np1
         READ(10,*) tchoice1(j),tinc1(j),twhite1(j),tblack1(j),
     &        thisp1(j)
      enddo
      OPEN(11,file='../cleaned_data/renters_hou_10.txt')
      REWIND(11)
      DO j=1,np2
         READ(11,*) tchoice2(j),tinc2(j),twhite2(j),tblack2(j),
     &        thisp2(j)
      enddo
      OPEN(12,file='../cleaned_data/renters_phl_10.txt')
      REWIND(12)
      DO j=1,np3
         READ(12,*) tchoice3(j),tinc3(j),twhite3(j),tblack3(j),
     &        thisp3(j)
      enddo
c
      OPEN(13,file='../cleaned_data/renters_cle_10.txt')
      REWIND(13)
      DO j=1,np4
         READ(13,*) tchoice4(j),tinc4(j),twhite4(j),tblack4(j),
     &        thisp4(j)
      enddo
c
      OPEN(14,file='../cleaned_data/renters_sjc_10.txt')
      REWIND(14)
      DO j=1,np5
         READ(14,*) tchoice5(j),tinc5(j),twhite5(j),tblack5(j),
     &        thisp5(j)
      enddo
c
      OPEN(15,file='../cleaned_data/Atlanta_tri.txt')
      REWIND(15)
      DO j=1,nchoice1
         READ(15,*) tri1(j)
      enddo
      OPEN(16,file='../cleaned_data/Houston_tri.txt')
      REWIND(16)
      DO j=1,nchoice2
         READ(16,*) tri2(j)
      enddo
      OPEN(17,file='../cleaned_data/Philadelphia_tri.txt')
      REWIND(17)
      DO j=1,nchoice3
         READ(17,*) tri3(j)
      enddo
      OPEN(18,file='../cleaned_data/Cleveland_tri.txt')
      REWIND(18)
      DO j=1,nchoice4
         READ(18,*) tri4(j)
      enddo
      OPEN(19,file='../cleaned_data/SanJose_tri.txt')
      REWIND(19)
      DO j=1,nchoice5
         READ(19,*) tri5(j)
      enddo
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
      open(30,file='../probabilities_2/fitprob1.txt')
      rewind(30)
      do j=1,ncnb1
         read(30,*)  idnb1(j),idf1(j),p1(1,j),p1(2,j),p1(3,j)
      enddo
c
      open(31,file='../probabilities_2/fitprob2.txt')
      rewind(31)
      do j=1,ncnb2
         read(31,*) idnb2(j),idf2(j),p2(1,j),p2(2,j),p2(3,j)
      enddo
c
      open(32,file='../probabilities_2/fitprob3.txt')
      rewind(32)
      do j=1,ncnb3
         read(32,*) idnb3(j),idf3(j),p3(1,j),p3(2,j),p3(3,j)
      enddo
c
      open(33,file='../probabilities_2/fitprob4.txt')
      rewind(33)
      do j=1,ncnb4
         read(33,*) idnb4(j),idf4(j),p4(1,j),p4(2,j),p4(3,j)
      enddo
c
      open(34,file='../probabilities_2/fitprob5.txt')
      rewind(34)
      do j=1,ncnb5
         read(34,*) idnb5(j),idf5(j),p5(1,j),p5(2,j),p5(3,j)
      enddo     
c
      open(900,file='stage2_allhet3_out_cs.txt')
      rewind(900)
 901  format(i10,i10,f15.7)
c
      iseed3 = 9193570473
      call rnset(iseed3)
      call rnund(nbs5,999999999,iseed3a)
c
      seedcount = 0
      do j2 = 1,1
         write(*,*) j2
         bootcount = j2
         iter = 0
         seedcount = seedcount+1
         iseed3b = iseed3a(seedcount)
         call rnset(iseed3b)
         call rnund(np1,np1,rb1)
         do j = 1,np1
            rb1(j) = j
            choice1(j) = tchoice1(rb1(j))
            inc1(j) = tinc1(rb1(j))
            white1(j) = twhite1(rb1(j))
            black1(j) = tblack1(rb1(j))
            hisp1(j) = thisp1(rb1(j))
         enddo
         seedcount = seedcount+1
         iseed3b = iseed3a(seedcount)
         call rnset(iseed3b)
         call rnund(np2,np2,rb2)
         do j = 1,np2
            rb2(j) = j
            choice2(j) = tchoice2(rb2(j))
            inc2(j) = tinc2(rb2(j))
            white2(j) = twhite2(rb2(j))
            black2(j) = tblack2(rb2(j))
            hisp2(j) = thisp2(rb2(j))
         enddo
         seedcount = seedcount+1
         iseed3b = iseed3a(seedcount)
         call rnset(iseed3b)
         call rnund(np3,np3,rb3)
         do j = 1,np3
            rb3(j) = j
            choice3(j) = tchoice3(rb3(j))
            inc3(j) = tinc3(rb3(j))
            white3(j) = twhite3(rb3(j))
            black3(j) = tblack3(rb3(j))
            hisp3(j) = thisp3(rb3(j))
         enddo
         seedcount = seedcount+1
         iseed3b = iseed3a(seedcount)
         call rnset(iseed3b)
         call rnund(np4,np4,rb4)
         do j = 1,np4
            rb4(j) = j
            choice4(j) = tchoice4(rb4(j))
            inc4(j) = tinc4(rb4(j))
            white4(j) = twhite4(rb4(j))
            black4(j) = tblack4(rb4(j))
            hisp4(j) = thisp4(rb4(j))
         enddo
         seedcount = seedcount+1
         iseed3b = iseed3a(seedcount)
         call rnset(iseed3b)
         call rnund(np5,np5,rb5)
         do j = 1,np5
            rb5(j) = j
            choice5(j) = tchoice5(rb5(j))
            inc5(j) = tinc5(rb5(j))
            white5(j) = twhite5(rb5(j))
            black5(j) = tblack5(rb5(j))
            hisp5(j) = thisp5(rb5(j))
         enddo
c
         do j = 1,nchoice1
            bidnc = (j2-1)*nchoice1+j
            rwhite1(j) = p1(1,bidnc)
            rblack1(j) = p1(2,bidnc)
            rhisp1(j) = p1(3,bidnc)
         enddo
c         do j = 1,nchoice1
c            write(*,*) rwhite1(j),rblack1(j),rhisp1(j)
c         enddo
         do j = 1,nchoice2
            bidnc = (j2-1)*nchoice2+j
            rwhite2(j) = p2(1,bidnc)
            rblack2(j) = p2(2,bidnc)
            rhisp2(j) = p2(3,bidnc)
         enddo
         do j = 1,nchoice3
            bidnc = (j2-1)*nchoice3+j
            rwhite3(j) = p3(1,bidnc)
            rblack3(j) = p3(2,bidnc)
            rhisp3(j) = p3(3,bidnc)
         enddo
         do j = 1,nchoice4
            bidnc = (j2-1)*nchoice4+j
            rwhite4(j) = p4(1,bidnc)
            rblack4(j) = p4(2,bidnc)
            rhisp4(j) = p4(3,bidnc)
         enddo
         do j = 1,nchoice5
            bidnc = (j2-1)*nchoice5+j
            rwhite5(j) = p5(1,bidnc)
            rblack5(j) = p5(2,bidnc)
            rhisp5(j) = p5(3,bidnc)
         enddo
         do j = 1,np1
            do k = 1,nchoice1
               cut1(j,k) = black1(j)*rblack1(k)+white1(j)*
     &                 rwhite1(k)+hisp1(j)*rhisp1(k)
            enddo
         enddo
         do j = 1,np2
            do k = 1,nchoice2
              cut2(j,k) = black2(j)*rblack2(k)+white2(j)*
     &                rwhite2(k)+hisp2(j)*rhisp2(k)
            enddo
         enddo
         do j = 1,np3
            do k = 1,nchoice3
               cut3(j,k) = black3(j)*rblack3(k)+white3(j)*
     &                 rwhite3(k)+hisp3(j)*rhisp3(k)
            enddo
         enddo
         do j = 1,np4
            do k = 1,nchoice4
               cut4(j,k) = black4(j)*rblack4(k)+white4(j)*
     &                rwhite4(k)+hisp4(j)*rhisp4(k)
            enddo
         enddo
         do j = 1,np5
            do k = 1,nchoice5
               cut5(j,k) = black5(j)*rblack5(k)+white5(j)*
     &                 rwhite5(k)+hisp5(j)*rhisp5(k)
            enddo
         enddo
c
c Switch to these cutoff values to eliminate any choice constraints
c (also reduce consideration set simulations to nsim = 1 since no
c randomness is needed).  These estimates correspond to the "No
c Consideration Set" case.
c
c
c         do j = 1,np1
c            do k = 1,nchoice1
c               cut1(j,k) = 1.d0
c            enddo
c         enddo
c         do j = 1,np2
c            do k = 1,nchoice2
c               cut2(j,k) = 1.d0
c            enddo
c         enddo
c         do j = 1,np3
c            do k = 1,nchoice3
c               cut3(j,k) = 1.d0
c            enddo
c         enddo
c         do j = 1,np4
c            do k = 1,nchoice4
c               cut4(j,k) = 1.d0
c            enddo
c         enddo
c         do j = 1,np5
c            do k = 1,nchoice5
c               cut5(j,k) = 1.d0
c            enddo
c         enddo
c
c
         do j = 1,nchoice1
            if (tri1(j).gt.0.d0) then
               dtri1(j) = 1.d0
            else
               dtri1(j) = 0.d0
            endif
         enddo
         do j = 1,nchoice2
            if (tri2(j).gt.0.d0) then
               dtri2(j) = 1.d0
            else
               dtri2(j) = 0.d0
            endif
         enddo
         do j = 1,nchoice3
            if (tri3(j).gt.0.d0) then
               dtri3(j) = 1.d0
            else
               dtri3(j) = 0.d0
            endif
         enddo
         do j = 1,nchoice4
            if (tri4(j).gt.0.d0) then
               dtri4(j) = 1.d0
            else
            dtri4(j) = 0.d0
            endif
         enddo
         do j = 1,nchoice5
            if (tri5(j).gt.0.d0) then
               dtri5(j) = 1.d0
            else
               dtri5(j) = 0.d0
            endif
         enddo
c
         do j = 1,nchoice1
            aschool1(j) = (z1(13,j)*z1(14,j)*z1(15,j))**(1.d0/3.d0)
            do k = 1,5
               x1(k,j) = 0.d0
            enddo
            x1(1,j) = z1(5,j)/1000.d0
            x1(2,j) = (aschool1(j))
            x1(3,j) = (z1(19,j))
            x1(4,j) = (z1(42,j))
            x1(5,j) = dlog(z1(46,j))
         enddo
         do j = 1,nchoice2
            aschool2(j) = (z2(13,j)*z2(14,j)*z2(15,j))**(1.d0/3.d0)
            do k = 1,5
               x2(k,j) = 0.d0
            enddo
            x2(1,j) = z2(5,j)/1000.d0
            x2(2,j) = (aschool2(j))
            x2(3,j) = (z2(19,j))
            x2(4,j) = (z2(42,j))
            x2(5,j) = dlog(z2(46,j))
         enddo
         do j = 1,nchoice3
            aschool3(j) = (z3(13,j)*z3(14,j)*z3(15,j))**(1.d0/3.d0)
            do k = 1,5
               x3(k,j) = 0.d0
            enddo
            x3(1,j) = z3(5,j)/1000.d0
            x3(2,j) = (aschool3(j))
            x3(3,j) = (z3(19,j))
            x3(4,j) = (z3(42,j))
            x3(5,j) = dlog(z3(46,j))
         enddo
         do j = 1,nchoice4
            aschool4(j) = (z4(13,j)*z4(14,j)*z4(15,j))**(1.d0/3.d0)
            do k = 1,5
               x4(k,j) = 0.d0
            enddo
            x4(1,j) = z4(5,j)/1000.d0
            x4(2,j) = (aschool4(j))
            x4(3,j) = (z4(19,j))
            x4(4,j) = (z4(42,j))
            x4(5,j) = dlog(z4(46,j))
         enddo
         do j = 1,nchoice5
            aschool5(j) = (z5(13,j)*z5(14,j)*z5(15,j))**(1.d0/3.d0)
            do k = 1,5
               x5(k,j) = 0.d0
            enddo
            x5(1,j) = z5(5,j)/1000.d0
            x5(2,j) = (aschool5(j))
            x5(3,j) = (z5(19,j))
            x5(4,j) = (z5(42,j))
            x5(5,j) = dlog(z5(46,j))
         enddo
c
         iseed0a = 3570473
         call rnset(iseed0a)
         call rnund(nsim,9999999,iseed1a)
         do m = 1,nsim
            call rnset(iseed1a(m))
            call rnund(np1,9999999,iseed2a)
            do j = 1,np1
               call rnset(iseed2a(j))
               CALL DRNUN (nchoice1,r1a)
               do k = 1,nchoice1
                  if(r1a(k).lt.cut1(j,k)) then
                     good1(j,k,m) = 1.d0
                  else
                     good1(j,k,m) = 0.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np1
            do k = 1,nchoice1
               do m = 1,nsim
                  if (choice1(j).eq.k) then
                     good1(j,k,m) = 1.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np1
            do m1 = 1,nsim
               do m2 = 1,nsim
                  wt1a(j,m2) = 1.d0
                  do k = 1,nchoice1
                     wt1a(j,m2) = wt1a(j,m2)*((((cut1(j,k))**
     &                    good1(j,k,m2))*((1.d0-cut1(j,k))**
     &                    (1.d0-good1(j,k,m2))))/(((cut1(j,k))
     &                    **good1(j,k,m1))*((1.d0-cut1(j,k))
     &                    **(1.d0-good1(j,k,m1)))))
                  enddo
               enddo
               temp1 = 0.d0
               do m2 = 1,nsim
                  temp1 = temp1 + wt1a(j,m2)
               enddo
               wt2a(j,m1) = 1.d0/temp1
c               write(*,*) j,m1,wt2a(j,m1)
            enddo
         enddo
c
         iseed0b = 5370347
         call rnset(iseed0b)
         call rnund(nsim,9999999,iseed1b)
         do m = 1,nsim
            call rnset(iseed1b(m))
            call rnund(np2,9999999,iseed2b)
            do j = 1,np2
               call rnset(iseed2b(j))
               CALL DRNUN (nchoice2,r1b)
               do k = 1,nchoice2
                  if(r1b(k).lt.cut2(j,k)) then
                     good2(j,k,m) = 1.d0
                  else
                     good2(j,k,m) = 0.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np2
            do k = 1,nchoice2
               do m = 1,nsim
                  if (choice2(j).eq.k) then
                     good2(j,k,m) = 1.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np2
            do m1 = 1,nsim
               do m2 = 1,nsim
                  wt1b(j,m2) = 1.d0
                  do k = 1,nchoice2
                     wt1b(j,m2) = wt1b(j,m2)*((((cut2(j,k))**
     &                    good2(j,k,m2))*((1.d0-cut2(j,k))**
     &                    (1.d0-good2(j,k,m2))))/(((cut2(j,k))
     &                    **good2(j,k,m1))*((1.d0-cut2(j,k))
     &                    **(1.d0-good2(j,k,m1)))))
                  enddo
               enddo
               temp2 = 0.d0
               do m2 = 1,nsim
                  temp2 = temp2 + wt1b(j,m2)
               enddo
               wt2b(j,m1) = 1.d0/temp2
            enddo
         enddo
c
         iseed0c = 7530473
         call rnset(iseed0c)
         call rnund(nsim,9999999,iseed1c)
         do m = 1,nsim
            call rnset(iseed1c(m))
            call rnund(np3,9999999,iseed2c)
            do j = 1,np3
               call rnset(iseed2c(j))
               CALL DRNUN (nchoice3,r1c)
               do k = 1,nchoice3
                  if(r1c(k).lt.cut3(j,k)) then
                     good3(j,k,m) = 1.d0
                  else
                     good3(j,k,m) = 0.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np3
            do k = 1,nchoice3
               do m = 1,nsim
                  if (choice3(j).eq.k) then
                     good3(j,k,m) = 1.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np3
            do m1 = 1,nsim
               do m2 = 1,nsim
                  wt1c(j,m2) = 1.d0
                  do k = 1,nchoice3
                     wt1c(j,m2) = wt1c(j,m2)*((((cut3(j,k))**
     &                    good3(j,k,m2))*((1.d0-cut3(j,k))**
     &                    (1.d0-good3(j,k,m2))))/(((cut3(j,k))
     &                    **good3(j,k,m1))*((1.d0-cut3(j,k))
     &                    **(1.d0-good3(j,k,m1)))))
                  enddo
               enddo
               temp3 = 0.d0
               do m2 = 1,nsim
                  temp3 = temp3 + wt1c(j,m2)
               enddo
               wt2c(j,m1) = 1.d0/temp3
            enddo
         enddo
c
         iseed0d = 7540373
         call rnset(iseed0d)
         call rnund(nsim,9999999,iseed1d)
         do m = 1,nsim
            call rnset(iseed1d(m))
            call rnund(np4,9999999,iseed2d)
            do j = 1,np4
               call rnset(iseed2d(j))
               CALL DRNUN (nchoice4,r1d)
               do k = 1,nchoice4
                  if(r1d(k).lt.cut4(j,k)) then
                     good4(j,k,m) = 1.d0
                  else
                     good4(j,k,m) = 0.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np4
            do k = 1,nchoice4
               do m = 1,nsim
                  if (choice4(j).eq.k) then
                     good4(j,k,m) = 1.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np4
            do m1 = 1,nsim
               do m2 = 1,nsim
                  wt1d(j,m2) = 1.d0
                  do k = 1,nchoice4
                     wt1d(j,m2) = wt1d(j,m2)*((((cut4(j,k))**
     &                    good4(j,k,m2))*((1.d0-cut4(j,k))**
     &                    (1.d0-good4(j,k,m2))))/(((cut4(j,k))
     &                    **good4(j,k,m1))*((1.d0-cut4(j,k))
     &                    **(1.d0-good4(j,k,m1)))))
                  enddo
               enddo
               temp4 = 0.d0
               do m2 = 1,nsim
                  temp4 = temp4 + wt1d(j,m2)
               enddo
               wt2d(j,m1) = 1.d0/temp4
            enddo
         enddo      
c
         iseed0e = 5740733
         call rnset(iseed0e)
         call rnund(nsim,9999999,iseed1e)
         do m = 1,nsim
            call rnset(iseed1e(m))
            call rnund(np5,9999999,iseed2e)
            do j = 1,np5
               call rnset(iseed2e(j))
               CALL DRNUN (nchoice5,r1e)
               do k = 1,nchoice5
                  if(r1e(k).lt.cut5(j,k)) then
                     good5(j,k,m) = 1.d0
                  else
                     good5(j,k,m) = 0.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np5
            do k = 1,nchoice5
               do m = 1,nsim
                  if (choice5(j).eq.k) then
                     good5(j,k,m) = 1.d0
                  endif
               enddo
            enddo
         enddo
         do j = 1,np5
            do m1 = 1,nsim
               do m2 = 1,nsim
                  wt1e(j,m2) = 1.d0
                  do k = 1,nchoice5
                     wt1e(j,m2) = wt1e(j,m2)*((((cut5(j,k))**
     &                    good5(j,k,m2))*((1.d0-cut5(j,k))**
     &                    (1.d0-good5(j,k,m2))))/(((cut5(j,k))
     &                    **good5(j,k,m1))*((1.d0-cut5(j,k))
     &                    **(1.d0-good5(j,k,m1)))))
                  enddo
               enddo
               temp5 = 0.d0
               do m2 = 1,nsim
                  temp5 = temp5 + wt1e(j,m2)
               enddo
               wt2e(j,m1) = 1.d0/temp5
            enddo
         enddo      
c         do j = 1,np1
c            do k = 1,nchoice1
c               write(*,*) j,k,good1(j,k,m)
c            enddo
c         enddo
c
cccccccccccccccccccccccccccccccccccccccccc
c                                        c
c Set-up parameter optimization problem  c
c                                        c
cccccccccccccccccccccccccccccccccccccccccc
c
c Generic Starting Values
c  
c
      pstart(1) = 1.d0
      pstart(2) = 1.d0
      pstart(3) =   0.1d0
      pstart(4) =   0.1d0  
      pstart(5) =   -0.1d0
      pstart(6) =   -0.1d0   
      pstart(7) =   0.1d0
      pstart(8) =   0.1d0
      pstart(9) =   0.1d0  
      pstart(10) =  0.1d0
      pstart(11) =  0.05d0
      pstart(12) = -0.0005d0
      pstart(13) =  0.05d0
      pstart(14) = -0.0005d0
      pstart(15) =  0.05d0
      pstart(16) = -0.0005d0
      pstart(17) = 1.d0
      pstart(18) = 1.d0
      pstart(19) = 1.d0
      pstart(20) = 1.d0
      pstart(21) = 1.d0
      pstart(22) = 1.d0
      pstart(23) = 1.d0
      pstart(24) = 1.d0
      pstart(25) = 1.d0
      pstart(26) = 1.d0
      pstart(27) = 1.d0
      pstart(28) = 1.d0
      pstart(29) = 1.d0
      pstart(30) = 1.d0
      pstart(31) = 1.d0
      pstart(32) = 1.d0
      pstart(33) = 1.d0
      pstart(34) = 1.d0
      pstart(35) = 1.d0
      pstart(36) = 1.d0
c      
c Starting values associated with no consideration set case
c
c      pstart(       1 ) =     1.2783992d0
c      pstart(       2 ) =     0.0856275d0
c      pstart(       3 ) =     0.0575409d0
c      pstart(       4 ) =     0.0140668d0
c      pstart(       5 ) =    -0.0007921d0
c      pstart(       6 ) =    -0.1020601d0
c      pstart(       7 ) =    -0.0278965d0
c      pstart(       8 ) =    -0.0039676d0
c      pstart(       9 ) =    -0.0008091d0
c      pstart(       10 ) =    -0.0004480d0
c      pstart(       11 ) =     0.0917866d0
c      pstart(       12 ) =    -0.0007314d0
c      pstart(       13 ) =     0.1079631d0
c      pstart(       14 ) =    -0.0007434d0
c      pstart(       15 ) =     0.0789694d0
c      pstart(       16 ) =    -0.0007679d0
c      pstart(       17 ) =     4.0132164d0
c      pstart(       18 ) =    -3.3800917d0
c      pstart(       19 ) =     0.7543532d0
c      pstart(       20 ) =     0.2788294d0
c      pstart(       21 ) =     0.5564049d0
c      pstart(       22 ) =    -1.2543850d0
c      pstart(       23 ) =     0.5704692d0
c      pstart(       24 ) =    -0.2738572d0
c      pstart(       25 ) =     3.9671480d0
c      pstart(       26 ) =     0.8900729d0
c      pstart(       27 ) =    -3.6380422d0
c      pstart(       28 ) =    -2.6032121d0
c      pstart(       29 ) =    -0.0597393d0
c      pstart(       30 ) =    -0.6336089d0
c      pstart(       31 ) =     0.1319058d0
c      pstart(       32 ) =     1.6491432d0
c      pstart(       33 ) =    -0.8234581d0
c      pstart(       34 ) =     1.1140190d0
c      pstart(       35 ) =    -5.2159179d0
c      pstart(       36 ) =    -3.9601391d0
c      
      do m = 1,nparam
c         pstart(m) = 0.1d0
         yscale(m) = 1.d0
         yguess(m) = 1.d0
         ylb(m) = -100.d0
         yub(m) = 100.d0
      enddo
      do m = 3,6
         yguess(m) = 1.d0
         ylb(m) = -100.d0
         yub(m) = 100.d0
      enddo
      do m = 7,10
         yguess(m) = 0.d0
         ylb(m) = -100.d0
         yub(m) = 100.d0
      enddo
      do m = 17,36
         yguess(m) = 0.d0
         ylb(m) = -100.d0
         yub(m) = 100.d0
      enddo
      do m = 11,16
         yguess(m) = 1.d0
         ylb(m) = -100.d0
         yub(m) = 100.d0
      enddo
      yguess(1) = 1.d0
      ylb(1) = -100.d0
      yub(1) = 100.d0
      yguess(2) = 0.d0
      ylb(2) = -100.d0
      yub(2) = 100.d0
c
         do i = 1,nparam
            pstart(i) = pstart(i)*100.d0
            yscale(i) = yscale(i)/100.d0
            yguess(i) = yguess(i)/100.d0
            yub(i) = yub(i)/100.d0
            ylb(i) = ylb(i)/100.d0
         enddo
c
         fscale = 1.d0
c
         CALL du4inf(iparam,rparam)
         iparam(1) = 0
c
         maxfcn = 10000000
         ftol = 0.001d0
         ibtype = 0
c         print*,10
c         call dbcpol(fcn,nparam,yguess,ibtype,ylb,yub,ftol,
c     &      maxfcn,y,fvalue)
         CALL dbconf(fcn,nparam,yguess,ibtype,ylb,yub,yscale,fscale,
     &               iparam,rparam,y,fvalue)
c
         do k = 1,nparam
            write(900,901) j2,k,y(k)*pstart(k)
         enddo
         write(*,*) 'Objective Function =',fvalue
      enddo
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn(n,y,f)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
      PARAMETER (nchoice1=129,nchoice2=322,nchoice3=138)
      PARAMETER (nchoice4=92,nchoice5=184)
      PARAMETER (np1=10000,np2=10000,np3=10000,np4=10000,np5=10000)
      PARAMETER (nparam=36,nx=5)
      INTEGER n
      DOUBLE PRECISION b(nparam),f,y(nparam),
     &   pstart(nparam),f1,f2,f3,x1(nx,nchoice1),
     &   ylb(nparam),yub(nparam),x2(nx,nchoice2),
     &   x3(nx,nchoice3),f4,f5,x4(nx,nchoice4),
     &   x5(nx,nchoice5),ft
c
      common /starter/ pstart
      common /bounds/ ylb,yub
      common /data3/ x1,x2,x3,x4,x5
c
      external fcn1,fcn2,fcn3,fcn4,fcn5
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do k = 1,nparam
         b(k) = y(k)*pstart(k)
      enddo
c
      write(*,206) b(1),b(2)
      write(*,207) b(3),b(4),b(5),b(6)
      write(*,207) b(7),b(8),b(9),b(10)
      write(*,208) b(11),b(12),b(13),b(14),b(15),b(16)
      write(*,209) b(17),b(18),b(19),b(20),b(21)
      write(*,209) b(22),b(23),b(24),b(25),b(26)
      write(*,209) b(27),b(28),b(29),b(30),b(31)
      write(*,209) b(32),b(33),b(34),b(35),b(36)
 206  format(2f16.10)
 207  format(4f16.10)
 208  format(6f16.10)
 209  format(5f16.10)
c
      call fcn1(b,x1,f1)
      call fcn2(b,x2,f2)
      call fcn3(b,x3,f3)
      call fcn4(b,x4,f4)
      call fcn5(b,x5,f5)
      do m = 1,nparam
         if ((y(m).eq.ylb(m)).or.(y(m).eq.yub(m))) then
            write(*,*) 'Constrained Parameter #',m
         endif
      enddo
c
      ft = (f1+f2+f3+f4+f5)/5.d0
c
      if (((ft.lt.999999.d0).and.(ft.gt.-999999.d0)).and.
     &   (b(1).gt.0.01d0).and.((b(1)+b(2)).gt.0.01d0)) then
        f = ft
      else
        f = 999999.d0
      endif 
      write(*,*) 'Aggregate Objective Function =',f
c
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn1(b,x1,f1)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
      PARAMETER (nchoice1=129,nparam1=16,nparam=36,nx=5,nsim=50)
      PARAMETER (np1=10000)
      INTEGER n,choice1(np1)
      DOUBLE PRECISION b(nparam),f1,denom,
     &   x1(nx,nchoice1),ratio1(np1),
     &   wt2a(np1,nsim),price1(nchoice1),
     &   util(nchoice1),prob(np1,nsim),p(np1),
     &   good1(np1,nchoice1,nsim),black1(np1),white1(np1),
     &   hisp1(np1),pblack1(nchoice1),pwhite1(nchoice1),
     &   phisp1(nchoice1),inc1(np1),
     &   lon1(nchoice1),lat1(nchoice1)
c
      common /data1a/ pblack1,pwhite1,phisp1
      common /data2a/ choice1,inc1,black1,
     &   white1,hisp1
      common /simulate1a/ good1
      common /simulate2a/ wt2a
      common /latlon1/ lat1,lon1
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do m = 1,nsim
         do j = 1,np1
            do k = 1,nchoice1
               if ((inc1(j)-x1(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 501
               else
                  util(k) = b(nparam1-5)*white1(j)*pwhite1(k)+
     &                b(nparam1-4)*white1(j)*(pwhite1(k)**2.d0)+
     &                b(nparam1-3)*black1(j)*pblack1(k)+
     &                b(nparam1-2)*black1(j)*(pblack1(k)**2.d0)+
     &                b(nparam1-1)*hisp1(j)*phisp1(k)+
     &                b(nparam1)*hisp1(j)*(phisp1(k)**2.d0)+
     &                b(1)*dlog(inc1(j)-x1(1,k)*12.d0)+
     &                b(2)*dlog(inc1(j)-x1(1,k)*12.d0)*(black1(j)+
     &                hisp1(j))     
               endif
               do i = 2,nx
                  util(k) = util(k)+b(i+1)*x1(i,k)
               enddo
               do i = 2,nx
                  util(k) = util(k)+b(i+5)*x1(i,k)*(black1(j)+hisp1(j))
               enddo
               util(k) = util(k)+b(nparam1+1)*lat1(k)+
     &                   b(nparam1+2)*lon1(k)+(b(nparam1+11)*lat1(k)+
     &                   b(nparam1+12)*lon1(k))*(black1(j)+hisp1(j))
 501           continue
            enddo
            denom = 0.d0
            do k = 1,nchoice1
               denom = denom+good1(j,k,m)*dexp(util(k))
            enddo
            prob(j,m) = dexp(util(choice1(j)))/denom
         enddo
      enddo
      do j = 1,np1
         p(j) = 0.d0
         do m = 1,nsim
            p(j) = p(j)+prob(j,m)*wt2a(j,m)
         enddo
      enddo
      f1 = 0.d0
      do j = 1,np1
         f1 = f1 - dlog(p(j))
      ENDDO
      write(*,*) 'objective function #1=',f1
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn2(b,x2,f2)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
      PARAMETER (nchoice2=322,nparam1=16,nparam=36,nx=5,nsim=50)
      PARAMETER (np2=10000)
      INTEGER n,choice2(np2)
      DOUBLE PRECISION b(nparam),f2,denom,
     &   x2(nx,nchoice2),wt2b(np2,nsim),
     &   util(nchoice2),prob(np2,nsim),p(np2),
     &   good2(np2,nchoice2,nsim),black2(np2),white2(np2),
     &   hisp2(np2),pblack2(nchoice2),pwhite2(nchoice2),
     &   phisp2(nchoice2),inc2(np2),
     &   lon2(nchoice2),lat2(nchoice2)
c
      common /data1b/ pblack2,pwhite2,phisp2
      common /data2b/ choice2,inc2,black2,
     &   white2,hisp2
      common /simulate1b/ good2
      common /simulate2b/ wt2b
      common /latlon2/ lat2,lon2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do m = 1,nsim
         do j = 1,np2
            do k = 1,nchoice2
               if ((inc2(j)-x2(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 502
               else
                  util(k) = b(nparam1-5)*white2(j)*pwhite2(k)+
     &                b(nparam1-4)*white2(j)*(pwhite2(k)**2.d0)+
     &                b(nparam1-3)*black2(j)*pblack2(k)+
     &                b(nparam1-2)*black2(j)*(pblack2(k)**2.d0)+
     &                b(nparam1-1)*hisp2(j)*phisp2(k)+
     &                b(nparam1)*hisp2(j)*(phisp2(k)**2.d0)+
     &                b(1)*dlog(inc2(j)-x2(1,k)*12.d0)+
     &                b(2)*dlog(inc2(j)-x2(1,k)*12.d0)*(black2(j)+
     &                hisp2(j))
               endif
               do i = 2,nx
                  util(k) = util(k)+b(i+1)*x2(i,k)
               enddo
               do i = 2,nx
                  util(k) = util(k)+b(i+5)*x2(i,k)*(black2(j)+hisp2(j))
               enddo
               util(k) = util(k)+b(nparam1+3)*lat2(k)+
     &                   b(nparam1+4)*lon2(k)+(b(nparam1+13)*lat2(k)+
     &                   b(nparam1+14)*lon2(k))*(black2(j)+hisp2(j))
 502           continue
            enddo
            denom = 0.d0
            do k = 1,nchoice2
               denom = denom+good2(j,k,m)*dexp(util(k))
            enddo
            prob(j,m) = dexp(util(choice2(j)))/denom
         enddo
      enddo
      do j = 1,np2
         p(j) = 0.d0
         do m = 1,nsim
            p(j) = p(j)+prob(j,m)*wt2b(j,m)
         enddo
      enddo
      f2 = 0.d0
      do j = 1,np2
         f2 = f2 - dlog(p(j))
      ENDDO
      write(*,*) 'objective function #2=',f2
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn3(b,x3,f3)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c  
      PARAMETER (nchoice3=138,nparam1=16,nparam=36,nx=5,nsim=50)
      PARAMETER (np3=10000)
      INTEGER n,choice3(np3)
      DOUBLE PRECISION b(nparam),f3,denom,
     &   x3(nx,nchoice3),wt2c(np3,nsim),
     &   util(nchoice3),prob(np3,nsim),p(np3),
     &   good3(np3,nchoice3,nsim),black3(np3),white3(np3),
     &   hisp3(np3),pblack3(nchoice3),pwhite3(nchoice3),
     &   phisp3(nchoice3),inc3(np3),
     &   lon3(nchoice3),lat3(nchoice3)
c
      common /data1c/ pblack3,pwhite3,phisp3
      common /data2c/ choice3,inc3,black3,
     &   white3,hisp3
      common /simulate1c/ good3
      common /simulate2c/ wt2c
      common /latlon3/ lat3,lon3
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do m = 1,nsim
         do j = 1,np3
            do k = 1,nchoice3
               if ((inc3(j)-x3(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 503
               else
                  util(k) = b(nparam1-5)*white3(j)*pwhite3(k)+
     &                b(nparam1-4)*white3(j)*(pwhite3(k)**2.d0)+
     &                b(nparam1-3)*black3(j)*pblack3(k)+
     &                b(nparam1-2)*black3(j)*(pblack3(k)**2.d0)+
     &                b(nparam1-1)*hisp3(j)*phisp3(k)+
     &                b(nparam1)*hisp3(j)*(phisp3(k)**2.d0)+
     &                b(1)*dlog(inc3(j)-x3(1,k)*12.d0)+
     &                b(2)*dlog(inc3(j)-x3(1,k)*12.d0)*(black3(j)+
     &                hisp3(j))
               endif
               do i = 2,nx
                  util(k) = util(k)+b(i+1)*x3(i,k)
               enddo
               do i = 2,nx
                  util(k) = util(k)+b(i+5)*x3(i,k)*(black3(j)+hisp3(j))
               enddo
               util(k) = util(k)+b(nparam1+5)*lat3(k)+
     &                   b(nparam1+6)*lon3(k)+(b(nparam1+15)*lat3(k)+
     &                   b(nparam1+16)*lon3(k))*(black3(j)+hisp3(j))
 503           continue
            enddo
            denom = 0.d0
            do k = 1,nchoice3
               denom = denom+good3(j,k,m)*dexp(util(k))
            enddo
            prob(j,m) = dexp(util(choice3(j)))/denom
         enddo
      enddo
      do j = 1,np3
         p(j) = 0.d0
         do m = 1,nsim
            p(j) = p(j)+prob(j,m)*wt2c(j,m)
         enddo
      enddo
      f3 = 0.d0
      do j = 1,np3
         f3 = f3 - dlog(p(j))
      ENDDO
      write(*,*) 'objective function #3=',f3
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn4(b,x4,f4)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c  
      PARAMETER (nchoice4=92,nparam1=16,nparam=36,nx=5,nsim=50)
      PARAMETER (np4=10000)
      INTEGER n,choice4(np4)
      DOUBLE PRECISION b(nparam),f4,denom,
     &   x4(nx,nchoice4),wt2d(np4,nsim),
     &   util(nchoice4),prob(np4,nsim),p(np4),
     &   good4(np4,nchoice4,nsim),black4(np4),white4(np4),
     &   hisp4(np4),pblack4(nchoice4),pwhite4(nchoice4),
     &   phisp4(nchoice4),inc4(np4),
     &   lon4(nchoice4),lat4(nchoice4)
c
      common /data1d/ pblack4,pwhite4,phisp4
      common /data2d/ choice4,inc4,black4,
     &   white4,hisp4
      common /simulate1d/ good4
      common /simulate2d/ wt2d
      common /latlon4/ lat4,lon4
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do m = 1,nsim
         do j = 1,np4
            do k = 1,nchoice4
               if ((inc4(j)-x4(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 504
               else
                  util(k) = b(nparam1-5)*white4(j)*pwhite4(k)+
     &                b(nparam1-4)*white4(j)*(pwhite4(k)**2.d0)+
     &                b(nparam1-3)*black4(j)*pblack4(k)+
     &                b(nparam1-2)*black4(j)*(pblack4(k)**2.d0)+
     &                b(nparam1-1)*hisp4(j)*phisp4(k)+
     &                b(nparam1)*hisp4(j)*(phisp4(k)**2.d0)+
     &                b(1)*dlog(inc4(j)-x4(1,k)*12.d0)+
     &                b(2)*dlog(inc4(j)-x4(1,k)*12.d0)*(black4(j)+
     &                hisp4(j))
               endif
               do i = 2,nx
                  util(k) = util(k)+b(i+1)*x4(i,k)
               enddo
               do i = 2,nx
                  util(k) = util(k)+b(i+5)*x4(i,k)*(black4(j)+hisp4(j))
               enddo
               util(k) = util(k)+b(nparam1+7)*lat4(k)+
     &                   b(nparam1+8)*lon4(k)+(b(nparam1+17)*lat4(k)+
     &                   b(nparam1+18)*lon4(k))*(black4(j)+hisp4(j))
 504           continue
            enddo
            denom = 0.d0
            do k = 1,nchoice4
               denom = denom+good4(j,k,m)*dexp(util(k))
            enddo
            prob(j,m) = dexp(util(choice4(j)))/denom
         enddo
      enddo
      do j = 1,np4
         p(j) = 0.d0
         do m = 1,nsim
            p(j) = p(j)+prob(j,m)*wt2d(j,m)
         enddo
      enddo
      f4 = 0.d0
      do j = 1,np4
         f4 = f4 - dlog(p(j))
      ENDDO
      write(*,*) 'objective function #4=',f4
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE fcn5(b,x5,f5)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
      PARAMETER (nchoice5=184,nparam1=16,nparam=36,nx=5,nsim=50)
      PARAMETER (np5=10000)
      INTEGER n,choice5(np5)
      DOUBLE PRECISION b(nparam),f5,denom,
     &   x5(nx,nchoice5),wt2e(np5,nsim),
     &   util(nchoice5),prob(np5,nsim),p(np5),
     &   good5(np5,nchoice5,nsim),black5(np5),white5(np5),
     &   hisp5(np5),pblack5(nchoice5),pwhite5(nchoice5),
     &   phisp5(nchoice5),inc5(np5),
     &   lon5(nchoice5),lat5(nchoice5)
c
      common /data1e/ pblack5,pwhite5,phisp5
      common /data2e/ choice5,inc5,black5,
     &   white5,hisp5
      common /simulate1e/ good5
      common /simulate2e/ wt2e
      common /latlon5/ lat5,lon5
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do m = 1,nsim
         do j = 1,np5
            do k = 1,nchoice5
               if ((inc5(j)-x5(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 505
               else
                  util(k) = b(nparam1-5)*white5(j)*pwhite5(k)+
     &                b(nparam1-4)*white5(j)*(pwhite5(k)**2.d0)+
     &                b(nparam1-3)*black5(j)*pblack5(k)+
     &                b(nparam1-2)*black5(j)*(pblack5(k)**2.d0)+
     &                b(nparam1-1)*hisp5(j)*phisp5(k)+
     &                b(nparam1)*hisp5(j)*(phisp5(k)**2.d0)+
     &                b(1)*dlog(inc5(j)-x5(1,k)*12.d0)+
     &                b(2)*dlog(inc5(j)-x5(1,k)*12.d0)*(black5(j)+
     &                hisp5(j))
               endif
               do i = 2,nx
                  util(k) = util(k)+b(i+1)*x5(i,k)
               enddo
               do i = 2,nx
                  util(k) = util(k)+b(i+5)*x5(i,k)*(black5(j)+hisp5(j))
               enddo
               util(k) = util(k)+b(nparam1+9)*lat5(k)+
     &                   b(nparam1+10)*lon5(k)+(b(nparam1+19)*lat5(k)+
     &                   b(nparam1+20)*lon5(k))*(black5(j)+hisp5(j))
 505           continue
            enddo
            denom = 0.d0
            do k = 1,nchoice5
               denom = denom+good5(j,k,m)*dexp(util(k))
            enddo
            prob(j,m) = dexp(util(choice5(j)))/denom
         enddo
      enddo
      do j = 1,np5
         p(j) = 0.d0
         do m = 1,nsim
            p(j) = p(j)+prob(j,m)*wt2e(j,m)
         enddo
      enddo
      f5 = 0.d0
      do j = 1,np5
         f5 = f5 - dlog(p(j))
      ENDDO
      write(*,*) 'objective function #5=',f5
c
      END
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc