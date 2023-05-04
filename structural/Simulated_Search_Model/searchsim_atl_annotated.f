ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This code simulates the "high water mark" level of utility
c achieved by each of np simulated individuals for city X (this
c code simulates search behavior in Atlanta but can be adapted
c for other cities by making minor alterations). These simulated
c individual searches are then combined across cities using
c search_agg_q.f to determine racial differences in search
c outcomes -- in particular, how many more additional searches
c does a renter of color need to engage in to achieve the same
c level of utility as a white renter.
c
c   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PROGRAM searchsim
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE numerical_libraries
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Setting Model Parameters                             
c nchoice = number of tracts in choice set in city X
c nparam = number of parameters in utility function
c nx = number census tract attributes in utility (not incl lat/lon)
c np1 = number of simulated triplets (white, Black, Hispanic)
c np = total number of simulated individuals
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER (nparam=21,nchoice=129,nthresh=100)
      PARAMETER (np1=5000,np=15000,nx=5)
c
cccccccccccccccccccccccc
c Variable Declarations
cccccccccccccccccccccccc
c
      INTEGER iseed1,iseed3,iseed4(np1),
     &   iseed5,iseed6(np),sc,iperm1(nchoice),
     &   iperm2(nchoice),index2,ip1(np,nchoice),
     &   ip2(np,nchoice),index1,index0,count,race,
     &   bpref,hpref,esearch,id1(nchoice),id2(nchoice)
      DOUBLE PRECISION z1(47,nchoice),pblack(nchoice),
     &   pwhite(nchoice),phisp(nchoice),p1(3,nchoice),
     &   rwhite(nchoice),rblack(nchoice),rhisp(nchoice),
     &   ainc,sinc,rincome(5),r1(nchoice),eta(np,nchoice),
     &   white(np),black(np),hisp(np),cut(np,nchoice),
     &   r2(nchoice),good(np,nchoice),b(nparam),
     &   aschool(nchoice),x(5,nchoice),inc(np),
     &   utility(np,nchoice),putility(np,nchoice),
     &   jutility(nchoice),jputility(nchoice),
     &   sjutility(nchoice),sjputility(nchoice),
     &   sutility(np,nchoice),util(nchoice),inflate,
     &   sputility(np,nchoice),thresh(nthresh),
     &   wutility(np,nchoice),umax(np,nthresh),
     &   lat(nchoice),lon(nchoice)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
c Open output file to record search simulation outcomes generated
c by this code.
c
      open(110, file='simdata/s_atl_hisp_100e.txt')
      rewind(110)
c
c Set parameters for search simulation.  Race = 2 (Black) or 3 (Hispanic)
c We run this simulation separately for each of these groups.  When running
c the simulation for Black (race = 2), the only difference between the white
c renter and the Black renter is their likelihoods of receiving a positive 
c response in each census tract.
c
c Race
c
      race = 3
c
c We can set homophily preferences to be those associated with each race group.
c bpref = 1 will give all simulated individuals Black homophily preferences.  
c hpref = 1 will give all simulated individuals Hispanic homophily preferences.
c For our main results, we turn off homophily preferences altogether.
c
c Homophily
c
      bpref = 0
      hpref = 0
c
c The code allows the simulated search process to be conducted over choices in
c an order that accounts for the a priori likelihood of a census tract entering 
c the choice set (esearch = 1) or not (esearch = 0).  If esearch = 1, simulated
c individuals organize their search over tracts placing more priority on tracts
c where they are more likely to receive a response.  If renters of color are 
c less likely to receive a response in high amenity tracts, this could alter
c significantly the order in which they search.
c
c Esearch
c
      esearch = 1
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Read in tract attribute data and probabilities of receiving
c response in each tract
c
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
         read(30,*) id1(j),id2(j),p1(1,j),p1(2,j),p1(3,j)
         rwhite(j) = p1(1,j)
         rblack(j) = p1(2,j)
         rhisp(j) = p1(3,j)
      enddo
c
c Generate common incomes in each city X for each member of 5000 triplets
c (white, Black and Hispanic).  We use a deterministic measure of income
c based on the mean of the income distribution for city X.
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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
c Common Incomes
c
      if (race.eq.2) then
         ainc = 3.502367d0
      endif
      if (race.eq.3) then
         ainc = 3.707266d0
      endif
      do j = 1,np1
         inc(j) = dexp(ainc)
         inc(j+np1) = dexp(ainc)
         inc(j+2*np1) = dexp(ainc)
      enddo
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
c Generate random draws of logit shock to utility for each member of np1 triplets
c (white, Black and Hispanic) -- all three members of the triplet receive the 
c same logit shock.  iseed3 is used to start the process, generating a (np1 x 1)
c vector of random seeds (iseed4) that is then used to generate uniform random draws
c for each triplet that are converted into logit draws 
c
      iseed3 = 3570473
      call rnset(iseed3)
      call rnund(np1,9999999,iseed4)
      do j = 1,np1
         call rnset(iseed4(j))
         CALL DRNUN (nchoice,r1)
         do k = 1,nchoice
            eta(j,k) = -dlog(-dlog(r1(k)))
            eta(j+np1,k) = -dlog(-dlog(r1(k)))
            eta(j+2*np1,k) = -dlog(-dlog(r1(k)))
         enddo
      enddo
c
c Assign race indicators to each triplet member.
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
c
c Generate response probability cutoff values for each triplet member in each
c census tract based on assigned race.  rblack(k), rwhite(k) and rhisp(k) are the
c response probabilities for each race group associated with census tract k. 
c
      do j = 1,np
         do k = 1,nchoice
            cut(j,k) = black(j)*rblack(k)+white(j)*
     &                 rwhite(k)+hisp(j)*rhisp(k)
         enddo
      enddo
c
c Using cutoffs, generate random draws and determine whether each census tract
c enters into the choice set for each simulated individual.
c
      iseed5 = 4484216
      call rnset(iseed5)
      call rnund(np,9999999,iseed6)
      do j = 1,np
         call rnset(iseed6(j))
         CALL DRNUN (nchoice,r2)
         do k = 1,nchoice
            if(r2(k).lt.cut(j,k)) then
               good(j,k) = 1.d0
            else
               good(j,k) = 0.d0
            endif
         enddo
      enddo
c
c Read in baseline utility parameter estimates.
c
      b(1) =     1.3116731556d0
      b(2) =     0.0350116691d0    
      b(3) =     0.0091689081d0   
      b(4) =    -0.0014915635d0  
      b(5) =    -0.3365881964d0
      b(6) =     0.0835504489d0   
      b(7) =    -0.0006750872d0  
      b(8) =     0.1035063390d0  
      b(9) =    -0.0006302912d0 
      b(10) =    0.0804650304d0  
      b(11) =   -0.0008321019d0
      b(12) =    2.4804243823d0   
      b(13) =   -4.0436193755d0  
      b(14) =    1.2488549425d0 
      b(15) =    0.6213647374d0 
      b(16) =    0.6212774945d0
      b(17) =   -0.4488897019d0  
      b(18) =   -3.2344535068d0  
      b(19) =    1.2825831983d0  
      b(20) =    2.3260151054d0  
      b(21) =   -1.2244836297d0
c
c Assign tract attributes to choice set.  x(k,j) = is attribute k = 1,...,5 
c in tract j for city being simulated.  We first generate the geometric average 
c school quality using the great schools indices for elementary, middle and high 
c school for the tract.  x(1,j) then measures monthly rent in thousands (corresponding 
c to our measure of income) for tract j in city X.  x(2,j) measures school quality.
c x(3,j) measures cafes. x(4,j) measures murders, and x(5,j) measures the log
c of the RSEI score (based on TRI pollution.
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
c Generate utility for each simulated individual associated with each census tract
c (irrespective of probability that it enters the choice set).  Code is set up to
c use African American homophily preferences if bpref = 1, Hispanic homophily
c preferences if hpref = 1, and no homophily preferences if bpref = hpref = 0.
c
      do j = 1,np
         do k = 1,nchoice
            if ((inc(j)-x(1,k)*12.d0).le.0.d0) then
               util(k) = -999.d0
               good(j,k) = 0.d0
               go to 299
            endif
            if (hpref.eq.1) then
               util(k) = b(nparam-1)*phisp(k)+
     &             b(nparam)*(phisp(k)**2.d0)+
     &             b(1)*dlog(inc(j)-x(1,k)*12.d0)
            endif
            if (bpref.eq.1) then
               util(k) = b(nparam-3)*pblack(k)+
     &             b(nparam-2)*(pblack(k)**2.d0)+
     &             b(1)*dlog(inc(j)-x(1,k)*12.d0)
            endif
            if ((hpref.eq.0).and.(bpref.eq.0)) then
               util(k) = b(1)*dlog(inc(j)-x(1,k)*12.d0)
            endif
            do i = 2,nx
               util(k) = util(k)+b(i)*x(i,k)
            enddo
            util(k) = util(k)+b(12)*lat(k)+b(13)*lon(k)
 299        continue
            utility(j,k) = util(k)
c            
c putility(j,k) is expected utility for simulated individual j associated with
c choice k, accounting for response probability
c
            putility(j,k) = utility(j,k)*(rwhite(k)*white(j)+
     &                    rblack(k)*black(j)+rhisp(k)*hisp(j)) 
         enddo
c
c jutility(k) and jputility(k) are temporary vectors that assign utility and putility
c for individual j to a 1xk vector for sorting.
c
         do k = 1,nchoice
            jutility(k) = utility(j,k)
            jputility(k) = putility(j,k)
         enddo
c
c The following code sorts options according to jutility (sort index = iperm1) or
c jputility (sort index = iperm2).
c
         do k = 1,nchoice
            iperm1(k) = k
            iperm2(k) = k
         enddo
         call dsvrgp(nchoice,jutility,sjutility,iperm1)
         call dsvrgp(nchoice,jputility,sjputility,iperm2)
         do k = 1,nchoice
            sutility(j,k) = sjutility(k)
            ip1(j,k) = iperm1(k)
            sputility(j,k) = sjputility(k)
            ip2(j,k) = iperm2(k)
         enddo
      enddo
c
c thresh(i) identifies the census tract associated with the i = 1,...,100 percentile
c of the census tracts in a city.  E.g., for a city with 300 census tracts, thresh(1)
c will equal 3.  thresh(2) will equal 6.
c
      do i = 1,nthresh
         thresh(i) = int(i*(1.d0*nchoice)/(1.d0*nthresh))
      enddo
c
c For each simulated individual, first calculate utility for each census tract
c accounting for logit error shock and whether choice was available to this simulated
c individual (good(j,k)=1 if simulated individual j has option k in choice set).
c Searching in order of declining utility (order based on iperm1 if esearch = 0, 
c based on iperm2 if esearch =1).  Keep track of the highest level of utility
c achieved (umax) at each value of thresh = 1,...,100.  E.g., for the city with 300
c census tracts, umax(j,1) will be assigned the utility associated with the third
c choice in the ranking.  If the choice associated with the 6th option provides 
c greater utility, then umax(j,2) takes that value.  If it provides less utility,
c then umax(j,2) preserves the value from umax(j,1).
c
c The code outputs the "high water mark" utility achieved by simulated individual
c j at every point in the search defined by percentiles 1,...,100.  By describing the
c search in terms of percentiles within the city, this allows us to pool across cities
c in search_agg_q.f
c
      do j = 1,np
         do k = 1,nchoice
            wutility(j,k) = good(j,k)*(utility(j,k)+eta(j,k))+
     &                      (1.d0-good(j,k))*(-9999.d0)
         enddo
         if (esearch.eq.0) then
            index1 = ip1(j,1)
         else
            index1 = ip2(j,1)
         endif
         umax(j,1) = wutility(j,index1)
         do i = 2,nthresh
            index0 = thresh(i)
            if (esearch.eq.0) then
               index1 = ip1(j,index0)
            else
               index1 = ip2(j,index0)
            endif
            if (wutility(j,index1).gt.umax(j,i-1)) then
               umax(j,i) = wutility(j,index1)
            else
               umax(j,i) = umax(j,i-1)
            endif
         enddo
      enddo
      do i = 1,nthresh
         do j = 1,np1
            write(110,111) inc(j),i,umax(j,i),
     &            umax(j+(race-1)*np1,i),1
         enddo
      enddo
 111  format(f8.2,i5,2f16.4,i5)
c
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
