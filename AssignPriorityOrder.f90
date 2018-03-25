subroutine AssignPriorityOrder(NumUser,NumSource, &
     	NumRights,NumReservoir,AllocationMode,DrainageOrder,NumDrainage, &
		NumUserSource,NumUserSourceReturn,NumReturnFlow)   ! DGT 9/4/05 added NumReturnFlow because it is used in this subroutine

	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	integer AllocationMode
	integer DrainageOrder(*)
	
! DGT 5/27/12 code to initialize latestdate to avoid uninitialized variable
   if(NumRights .gt. 0)then
     latestdate = Rights(1)%PriorityDate
   else
     latestdate=0
   endif

!  DGT 9/4/05  Code to find the latest Priority date so that users without a priority date specified get
!  a later date
   nrfound=0
	do i=1,NumUser
		do j=1,User(i)%NumSources
		!for each User-Source combination, find the priority date associated with the water right if it exists
			if (NumRights.gt.0 .and. any(User(i)%RightID.gt.0) ) then
				call find1(Rights%RightID,User(i)%RightID(j),NumRights)
				j_rights=ifound(1)	!problem if nfound<>1
				if(nrfound == 0)then
				   latestdate=Rights(j_rights)%PriorityDate
				else
				   if(latestdate < Rights(j_rights)%PriorityDate)latestdate=Rights(j_rights)%PriorityDate
				endif
				nrfound=nrfound+1
			endif
		enddo
	enddo
!  End DGT modifications to find latestdate


!Build a UserSource Table
	NumUserSource=0
	NumUserSourceReturn=0
	do i=1,NumUser
		do j=1,User(i)%NumSources
		!for each User-Source combination, we want to identify the user,
		!the source, the date, and the drainage the water is coming from
			NumUserSource=NumUserSource+1
			UserSourceTable(NumUserSource)%UserID=i
			UserSourceTable(NumUserSource)%SourceCounter=j
			if (NumRights.gt.0 .and. any(User(i)%RightID.gt.0) ) then
				call find1(Rights%RightID,User(i)%RightID(j),NumRights)
				j_rights=ifound(1)	!problem if nfound<>1
				UserSourceTable(NumUserSource)%PriorityDate=Rights(j_rights)%PriorityDate
			else
				UserSourceTable(NumUserSource)%PriorityDate=latestdate+1  !  DGT 9/4/05  Changed from 0.  
				! Where priority date does not exist give it a date later than all where one does exist
			end if
			if (User(i)%UsersType .eq. ReservoirFillUseCode) then !RAW 31-Aug-2005 we always want to give top priority to filling instream reservoirs
				UserSourceTable(NumUserSource)%PriorityDate=-1
			end if
			call find1(Source%SourceID,User(i)%SourceID(j),NumSource)
			j_src=ifound(1)		!problem if nfound<>1
			select case (Source(j_src)%Type) 
			case (StreamSourceCode,GroundwaterSourceCode) !water is coming from stream or groundwater
				ID=Source(j_src)%SourceLocationID !DrainageId is stored with the Source
				if (ID.eq.0) ID=User(i)%POU_ID
			case (ReservoirSourceCode) !water is coming from reservoir
				call find1(Reservoir%ReservoirID,Source(j_src)%SourceLocationID,NumReservoir)
				j_res=ifound(1)	!problem if nfound<>1
				ID=Reservoir(j_res)%DrainageID !DrainageID is with the reservoir
			end select
			UserSourceTable(NumUserSource)%DrainageID=ID
			if (User(i)%ReturnFlowID.ge.0) then
				call find1(ReturnFlow%ReturnFlowID,User(i)%ReturnFlowID,NumReturnFlow)
				j_ret=ifound(1)
				NumUserSourceReturn=NumUserSourceReturn+ReturnFlow(j_ret)%NumReturnFlows
			end if
			!count these return flow so we can dimension an array later
		end do
	end do

!Order that UserSource Table
	if (AllocationMode.eq.WaterRightsAllocationCode) then !sort by date
		call isort(UserSourceTable%PriorityDate,NumUserSource,UserSourceOrder)
	elseif (AllocationMode.eq.DemandAllocationCode .or. AllocationMode.eq.NoAllocationCode) then !sort by drainage order
		k=0
		do i=1,NumDrainage ! we already know what order the drainages are in, so just use it
			call find1(UserSourceTable%DrainageID,DrainageOrder(i),NumUserSource)

			!RAW 31-Aug-2005 extra magic to make sure that (in-stream) reservoirs are filled as highest priority
			if (nfound.ne.0) then
				ifoundresfill=0
				m=1 
				!this loop will hunt through all the users in this drainage, looking for the ReservoirFillUseCode
				!when it finds that ReservoirFillUseCode, it will switch it with the first user in ifound, so that it comes before anything else in this drainage
				!the do while loop ends as soon as this condition is satisfied
				do while (ifoundresfill.eq.0 .and. m.lt.nfound)
					if (User(UserSourceTable(ifound(m))%UserID)%UsersType .eq. ReservoirFillUseCode ) then
						ifoundresfill=1
						ifoundtemp=ifound(m)
						ifound(m)=ifound(1)
						ifound(1)=ifoundtemp
					end if
					m=m+1
				end do

				do m=1,nfound
					UserSourceOrder(k+m)=ifound(m)
				end do
			end if
			k=k+nfound
		end do
	end if

	return
	end

	subroutine isort(ci,n,s)
	integer ci(n),s(n)
	integer, allocatable :: i(:)
	
	allocate (i(n))
	i=ci
	do j=1,n
		s(j)=j
	end do
	iswap=0
	do j=1,n-1
		do k=j+1,n
			if (i(j).ge.i(k)) then
				itemp=i(k)
				i(k)=i(j)
				i(j)=itemp
				itemp=s(k)
				s(k)=s(j)
				s(j)=itemp
			end if
		end do
	end do
	end
