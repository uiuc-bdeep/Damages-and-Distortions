ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PROGRAM choicesetsim
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE numerical_libraries
c
      PARAMETER (nparam1=11,nparam=21,nsim=25,nx=5,maxit=50000)
      PARAMETER (np1=10000,np=30000,nchoice=129,np3=5*np)
      PARAMETER (hinc=109.d0)
      INTEGER id(nchoice),iseed0,iseed1,
     &   iseed3,iseed4(np1),iseed5,iseed6(nsim),
     &   iseed2(np3),iseed7(np1),sc,id1(nchoice),
     &   id2(nchoice)
      DOUBLE PRECISION price(nchoice),pwhite(nchoice),
     &   phisp(nchoice),pblack(nchoice),dinc(4,np),
     &   rwhite(nchoice),rblack(nchoice),rhisp(nchoice),
     &   cut(np,nchoice),rincome(5),inc(np),mudiff(maxit),
     &   r1(nparam),pran(nparam,np),r2(nchoice),
     &   good(np,nchoice,nsim),wt1(np,nsim),temp,
     &   wt2(np,nsim),acount(np),b_t(nparam),x(nx,nchoice),
     &   b(nparam,np),rr(np),util(nchoice),denom,aeuc(np),
     &   euc(nsim),eu(np),ainc,sinc,aschool(nchoice),
     &   white(np),black(np),hisp(np),probc(np,nchoice,nsim),
     &   prob(np,nchoice),rrace(np),amed(5),bmed(5),
     &   z1(47,nchoice),bwdutil(np1),hwdutil(np1),bwdex(np1,nx),
     &   hwdex(np1,nx),bwcount(np1),hwcount(np1),hassault(nchoice),
     &   wcount(np1),hcount(np1),bcount(np1),ceuc(nsim),
     &   caeuc(np),cv(np),muguess,p1(3,nchoice),lat(nchoice),
     &   lon(nchoice)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      OPEN(20,file='../cleaned_data/tracts_ATL.txt')
      REWIND(20)
      DO j=1,nchoice
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
         pblack(j) = z1(3,j)*100.d0
         pwhite(j) = z1(2,j)*100.d0
         phisp(j) = z1(4,j)*100.d0
      enddo
      open(25,file='../cleaned_data/latlon_atl.txt')
      rewind(25)
      do j = 1,nchoice
         read(25,*) lon(j),lat(j)
      enddo
      open(30,file='../probabilities_2/fitprob1.txt')
      rewind(30)
      do j=1,nchoice
         read(30,*)  id1(j),id2(j),p1(1,j),p1(2,j),p1(3,j)
         rwhite(j) = p1(1,j)
         rblack(j) = p1(2,j)
         rhisp(j) = p1(3,j)
      enddo
c

cccccccccccccccccccccccccccccccccc
c Income Distributions
cccccccccccccccccccccccccccccccccc
c
c PHL (129) -- (15,140)
c all		3.859689    .6044559	
c white	 	4.000417    .5525852 
c black	 	3.43297    .5556193
c hisp  	3.678148    .6114176
c
c HOU (132) -- (15,133)
c all 		3.664489    .5364071	
c white 	3.788308    .5337688	
c black 	3.480775    .4935966	
c hisp 		3.548357    .5040794
c
c SJC (337) -- (28,221)
c all		4.312315    .5575128	
c white		4.355837    .5566913 	
c black 	4.215069    .5618713	
c hisp  	4.164949    .5337081
c
c ATL (108) -- (17,109)
c all 		3.716946    .4538693
c white 	3.797645    .4404112
c black 	3.502367    .4232063
c hisp 		3.707266    .4351284
c
c CLE (200)	-- (13,87)
c all 		3.402393     .486671	
c white 	3.468512    .4853827	
c black 	3.238787     .455082	
c hisp 		3.266122    .4344927
c
c
ccccccccccccccccccccccccccccccccccccccccccc
c Common Incomes (Average)
c
      iseed1 = 9297285
      call rnset(iseed1)
      call rnund(np3,9999999,iseed2)
c
      ainc = 3.707266d0       
      sinc = .4351284d0 
c
      sc = 0
      do j = 1,np1
 200     continue
         sc = sc+1
         call rnset(iseed2(sc))
         CALL DRNLNL(5,ainc,sinc,rincome)
         if ((rincome(1).gt.hinc).or.
     &      (rincome(1).lt.10.d0)) then
            go to 200
         else
            inc(j) = rincome(1)
            inc(j+np1) = rincome(1)
            inc(j+2*np1) = rincome(1)
         endif
         write(*,*) j,inc(j),inc(j+np1),inc(j+2*np1)
      enddo
      write(*,*) ' '
      write(*,*) 'seed count =',sc
c
      do j = 1,np1
         white(j) = 1.d0
         black(j) = 0.d0
         hisp(j) = 0.d0
      enddo
      do j = np1+1,2*np1
         white(j) = 0.d0
         black(j) = 1.d0
         hisp(j) = 0.d0
      enddo
      do j = 2*np1+1,3*np1
         white(j) = 0.d0
         black(j) = 0.d0
         hisp(j) = 1.d0
      enddo
      do j = 1,np
         do k = 1,nchoice
            cut(j,k) = black(j)*rblack(k)+white(j)*
     &                 rwhite(k)+hisp(j)*rhisp(k)
         enddo
      enddo
      iseed5 = 4484216
      call rnset(iseed5)
      call rnund(nsim,9999999,iseed6)
      do m = 1,nsim
         call rnset(iseed6(m))
         call rnund(np1,9999999,iseed7)
         do j = 1,np1
            call rnset(iseed7(j))
            CALL DRNUN (nchoice,r2)
            do k = 1,nchoice
               if(r2(k).lt.cut(j,k)) then
                  good(j,k,m) = 1.d0
               else
                  good(j,k,m) = 0.d0
               endif
            enddo
            do k = 1,nchoice
               if(r2(k).lt.cut(j+np1,k)) then
                  good(j+np1,k,m) = 1.d0
               else
                  good(j+np1,k,m) = 0.d0
               endif
            enddo
            do k = 1,nchoice
               if(r2(k).lt.cut(j+2*np1,k)) then
                  good(j+2*np1,k,m) = 1.d0
               else
                  good(j+2*np1,k,m) = 0.d0
               endif
            enddo
         enddo
      enddo
c
      do j = 1,np
         do m1 = 1,nsim
            do m2 = 1,nsim
               wt1(j,m2) = 1.d0
               do k = 1,nchoice
                  wt1(j,m2) = wt1(j,m2)*((((cut(j,k))**
     &                 good(j,k,m2))*((1.d0-cut(j,k))**
     &                 (1.d0-good(j,k,m2))))/(((cut(j,k))
     &                 **good(j,k,m1))*((1.d0-cut(j,k))
     &                 **(1.d0-good(j,k,m1)))))
               enddo
            enddo
            temp = 0.d0
            do m2 = 1,nsim
               temp = temp + wt1(j,m2)
            enddo
            wt2(j,m1) = 1.d0/temp
         enddo
      enddo
c
      do j = 1,np
         acount(j) = 0.d0
         do m = 1,nsim
            do k = 1,nchoice
               acount(j) = acount(j)+good(j,k,m)/(1.d0*nsim)
            enddo
         enddo
      enddo
c
      open(30,file='pstart_common3.txt')
      rewind(30)
      do k = 1,nparam
         read(30,*) b_t(k)
      enddo
c
      do j = 1,np
         do m = 1,nparam
            b(m,j) = b_t(m)
         enddo
      enddo
c
      do j = 1,nchoice
         aschool(j) = (z1(13,j)*z1(14,j)*z1(15,j))**(1.d0/3.d0)
         do k = 1,5
            x(k,j) = 0.d0
         enddo
         x(1,j) = z1(5,j)/1000.d0
         x(2,j) = aschool(j)
         x(3,j) = (z1(19,j))
         x(4,j) = (z1(42,j))
         x(5,j) = dlog(z1(46,j))
      enddo
c
      do j = 1,np
         write(*,*) 1,j  
         do m2 = 1,nsim
            do k = 1,nchoice
               if ((inc(j)-x(1,k)*12.d0).le.0.d0) then
                  util(k) = -999.d0
                  go to 299
               endif
               util(k) = b(nparam1-1)*phisp(k)+
     &                b(nparam1)*(phisp(k)**2.d0)+
     &                b(1)*dlog(inc(j)-x(1,k)*12.d0)
               do i = 2,nx
                  util(k) = util(k)+b(i)*x(i,k)
               enddo
               util(k) = util(k)+b(nparam1+1)*lat(k)+
     &                   b(nparam1+2)*lon(k)
 299           continue               
            enddo
            denom = 0.d0
            do k = 1,nchoice
               denom = denom+good(j,k,m2)*dexp(util(k))
            enddo
            ceuc(m2) = dlog(denom)
         enddo 
         caeuc(j) = 0.d0
         do m2 = 1,nsim
            caeuc(j) = caeuc(j)+ceuc(m2)*wt2(j,m2)
         enddo
      enddo
c
      do j = 1,np
         write(*,*) 2,j  
         mudiff(1) = 1000000.d0
         muguess = 0.01d0
         do m1 = 2,maxit
            do m2 = 1,nsim
               do k = 1,nchoice
                  if ((inc(j)-muguess-x(1,k)*12.d0).le.0.d0) then
                     util(k) = -999.d0
                     go to 298
                  endif
                  util(k) = b(nparam1-1)*phisp(k)+
     &                   b(nparam1)*(phisp(k)**2.d0)+
     &                   b(1)*dlog(inc(j)-muguess-x(1,k)*12.d0)
                  do i = 2,nx
                     util(k) = util(k)+b(i)*x(i,k)
                  enddo
                  util(k) = util(k)+b(nparam1+1)*lat(k)+
     &                      b(nparam1+2)*lon(k)
 298              continue               
               enddo
               denom = 0.d0
               do k = 1,nchoice
                  denom = denom+dexp(util(k))
               enddo
               euc(m2) = dlog(denom)
            enddo 
            aeuc(j) = 0.d0
            do m2 = 1,nsim
               aeuc(j) = aeuc(j)+euc(m2)/(1.d0*nsim)
            enddo
c            write(*,*) j,aeuc(j),caeuc(j)
            mudiff(m1) = aeuc(j)-caeuc(j)
            if ((mudiff(m1).lt.0.d0).and.
     &         (mudiff(m1-1).ge.0.d0)) then
                  go to 300
            else
               muguess = muguess+0.01d0
            endif
         enddo
 300     continue
         cv(j) = muguess
 310     format(i7,2f12.0,i5,f15.4)
      enddo
c      
      do j = 1,np1
         wcount(j) = acount(j)
         bcount(j) = acount(j+np1)
         hcount(j) = acount(j+2*np1)
         bwcount(j) = (acount(j+np1)-acount(j))
         hwcount(j) = (acount(j+2*np1)-acount(j))
      enddo
c
      open(500,file='simres_atl_hisp.csv')
      rewind(500)
      write(500,*) 'id',',','lnincMrent',',',
     &    'gschool',',','cafe',',','assault',
     &    ',','RSEI',',','white',',','white2',
     &    ',','black',',','black2',',','hisp',',','hisp2',
     &    ',','winc',',','binc',',','hinc'
     &    ,',','bwcount',',','hwcount',',','wcutil',',','bcutil'
     &    ,',','hcutil',',','wutil',',','butil',',','hutil',',',
     &    'wev',',','bev',',','hev',',','group',
     &    ',','wcount',',','bcount',',','hcount',',','city'
      do j = 1,np1
          write(500,*) j,',',b(1,j),',',b(2,j),',',b(3,j),',',
     &      b(4,j),',',b(5,j),',',b(6,j),',',b(7,j),',',
     &      b(8,j),',',b(9,j),',',b(10,j),',',b(11,j),',',
     &      inc(j),',',inc(j+np1),',',inc(j+2*np1),',',
     &      bwcount(j),',',hwcount(j),',',
     &      caeuc(j), ',',caeuc(j+np1), ',',caeuc(j+2*np1),
     &      ',',aeuc(j), ',',aeuc(j+np1), ',',aeuc(j+2*np1),
     &      ',',cv(j), ',',cv(j+np1), ',',cv(j+2*np1),
     &      ',',j,',',wcount(j),',',bcount(j),',',hcount(j),
     &      ',',1
      enddo
c
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
