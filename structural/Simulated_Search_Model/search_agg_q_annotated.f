cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This code reads in the results from searchsim_X.f, which carries out 
c simulated search for each city X based on estimated utility parameters.
c Simulated search is conducted over percentiles within cities -- i.e.,
c what is the max level of utility that a simulated individual has achieved
c after searching 1%, 2%,..., 100% of the tracts in a city.  We are thus
c able to combine results across cities of different sizes.c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PROGRAM search
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      USE numerical_libraries
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Setting Model Parameters                             
c nX = number of simulated individuals coming from city X
c nbin = number of search options (100 percentiles)
c n = total number of simulated individuals
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      parameter (n1=500000,n2=500000,n3=500000,n4=500000,n5=500000)
      parameter (nbin=100,n=n1+n2+n3+n4+n5)
c
cccccccccccccccccccccccc
c Variable Declarations
cccccccccccccccccccccccc
c
      integer bin(n),city(n),count(n),low,mid,high,good(nbin),
     &   count_lb(nbin),good_lb(nbin),count_lb50(nbin),
     &   count_lb75(nbin)
      double precision inc(n),w(n),m(n),dif(n),d(n),
     &   sd(n),d_25(nbin),d_50(nbin),d_75(nbin),d_90(nbin),
     &   nchoice,q25,q50,q75,q90,w_25(nbin),w_50(nbin),
     &   w_75(nbin),w_90(nbin),m_25(nbin),m_50(nbin),
     &   m_75(nbin),m_90(nbin),swhite(n),smin(n),
     &   white(n),min(n),ad(nbin),aw(nbin),am(nbin),
     &   sm(n),sw(n),ratio(nbin)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Read in search outcomes for simulated individuals from each city.
c hisp indicates that this simulation is for simulated Hispanic renters.
c m(j) indicates the minority (i.e., Hispanic) utility associated with
c having reached bin(j), and w(j) indicates the corresponding white renter's
c level of utility from the same triplet.  inc(j) measures their common
c income level.  city(j) is an indicator of which city the simulated data
c are coming from.  "e" indicates that expected likelihood of a tract entering 
c the choice set was used in determining search order.
c
      open(100,file='results_final/search_hisp_100e.txt')
      rewind(100)
c
      open(10,file='simdata/s_atl_hisp_100e.txt')
      rewind(10)
      do j = 1,n1
         read(10,*) inc(j),bin(j),w(j),m(j),city(j)
      enddo
      open(20,file='simdata/s_hou_hisp_100e.txt')
      rewind(20)
      do k = 1,n2
         j = n1+k
         read(20,*) inc(j),bin(j),w(j),m(j),city(j)
      enddo
      open(30,file='simdata/s_phl_hisp_100e.txt')
      rewind(30)
      do k = 1,n3
         j = n1+n2+k
         read(30,*) inc(j),bin(j),w(j),m(j),city(j)
      enddo
      open(40,file='simdata/s_cle_hisp_100e.txt')
      rewind(40)
      do k = 1,n4
         j = n1+n2+n3+k
         read(40,*) inc(j),bin(j),w(j),m(j),city(j)
      enddo
      open(50,file='simdata/s_sjc_hisp_100e.txt')
      rewind(50)
      do k = 1,n5
         j = n1+n2+n3+n4+k
         read(50,*) inc(j),bin(j),w(j),m(j),city(j)
      enddo
c
c Create vectors within each bin = 1,...,100 of the white, minority, and difference utilities.  Within each bin = 1,...,100, calculate the 25th, 50th, 75th,
c and 90th percentiles of the 
c
      do k = 1,nbin
         do j = 1,n
            white(j) = 999999999.d0
            min(j) = 999999999.d0
         enddo
         count(k) = 0
         do j = 1,n
            if((bin(j).eq.k)) then
               count(k) = count(k) + 1
               white(count(k)) = w(j)
               min(count(k)) = m(j)
            endif
         enddo
         call dsvrgn(n,white,sw)
         call dsvrgn(n,min,sm)
         q25 = int(1.d0*count(k)*0.25d0)
         q50 = int(1.d0*count(k)*0.5d0)
         q75 = int(1.d0*count(k)*0.75d0)
         q90 = int(1.d0*count(k)*0.9d0)
         w_25(k) = sw(q25)
         w_50(k) = sw(q50)
         w_75(k) = sw(q75)
         w_90(k) = sw(q90)
         m_25(k) = sm(q25)
         m_50(k) = sm(q50)
         m_75(k) = sm(q75)
         m_90(k) = sm(q90)
         good(k) = 1
         do j = 1,count(k)
            if((min(j).lt.-100.d0).or.(white(j).lt.-100.d0)) then
               good(k) = 0
            endif
         enddo
         aw(k) = 0.d0
         am(k) = 0.d0
         do j = 1,count(k)
            aw(k) = aw(k) + white(j)/(1.d0*count(k))
            am(k) = am(k) + min(j)/(1.d0*count(k))
         enddo
         good_lb(k) = 1
         do k1 = 1,k
c            if(good(k-k1).eq.0) then
c               count_lb(k) = 9999
c               good_lb(k) = 0
c               go to 200
c            endif
c            if((aw(k+1-k1).ge.am(k)).and.(aw(k-k1).lt.am(k))) then
c               count_lb(k) = k1
c               go to 200
c            endif
            if((w_50(k+1-k1).ge.m_50(k)).and.
     &         (w_50(k-k1).lt.m_50(k))) then
               count_lb50(k) = k1
               go to 200
            endif
         enddo
200      continue
         do k1 = 1,k
            if((w_75(k+1-k1).ge.m_75(k)).and.
     &         (w_75(k-k1).lt.m_75(k))) then
               count_lb75(k) = k1
               go to 300
            endif
         enddo
300      continue
      enddo
c
c      do k = 4,nbin-1
c         ratio(k) = (count_lb75(k-1)+count_lb75(k)+
c     &              count_lb75(k+1))/3.d0
c         write(100,101) k,ratio(k)
c      enddo
c
      do k = 4,nbin-1
         ratio(k) = (count_lb50(k-1)+count_lb50(k)+
     &              count_lb50(k+1))/3.d0
         write(100,101) k,ratio(k)
      enddo
 101  format(i8,f14.5)
c
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
