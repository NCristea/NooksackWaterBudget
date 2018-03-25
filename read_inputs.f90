!! EXAMPLE DATA
	subroutine read_inputs(dirname, dt,StartDateTopnet,													&
				NumDrainage,NumStreamNode,NumMeasuredFlowInfo,NumMeasuredFlowData,		&
				NumReservoir,NumUser,NumSource,NumRights,NumSourceMixing,	&
				NumSeasonsDefn,NumReturnFlow,NumMonthlyDemand,NumRunoff,NumBaseflow,NumWWTP)

	use data_array
	use constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use input_structures
	use other_structures
	use findmodule

	integer dd,mm,yyyy,nrec
	character*(128) dirname

	character*(128) filenm
	integer nrows,expected_numcols, hh,mi,ss,dt,StartDateTopnet
	integer,allocatable :: ifound_1(:),ifound_2(:)
	real,allocatable :: drainage_area(:)

	ncommentlines=2

	filenm=dirname(1:len_trim(dirname)) // '/' // 'WatermgmtControl.txt'
	expected_numcols=5
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,nrows) !NumTimesteps	AllocationMode	StartDate	StartOfWaterYear
	RunControl%NumTimesteps=real_array(1,1) !ignore
	RunControl%AllocationMode=real_array(1,2) !use
	RunControl%StartDate=real_array(1,3) !ignore
	RunControl%StartOfWaterYearmm=real_array(1,4) !use
	RunControl%StartOfWaterYeardd=real_array(1,5) !use

!	call datevec(RunControl%StartDate,90000,	&
!				RunControl%StartDateyyyy,RunControl%StartDatemm,RunControl%StartDatedd,hh,mi,ss)
!
!	call datevec(RunControl%StartOfWaterYear,90000,	&
!	  RunControl%StartOfWaterYearyyyy,RunControl%StartOfWaterYearmm,RunControl%StartOfWaterYeardd,	&
!	  hh,mi,ss)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'basinpars.txt'
	expected_numcols=45
	ncommentlines=1
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumDrainage) !CatchID,DrainageID,NodeID,CatchArea
	allocate (Drainage(NumDrainage))
!CatchID, DownCatchID, DrainID, NodeId, Reach_number, Outlet_X, Outlet_Y, direct_area, f, k, dth1, dth2, soilc, c, psif, chv, can_capacity, cr, Albedo, Lapse_rate, average_elevation, lambda, std_dev_of_lambda
	Drainage(:)%DrainageID=real_array(:,1)
	Drainage(:)%DSDrainage=real_array(:,2)
	Drainage(:)%RealDrainageID=real_array(:,3)
	Drainage(:)%NodeID=real_array(:,4)
	Drainage(:)%CatchArea=real_array(:,8)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'nodelinks.txt'
	expected_numcols=10
	ncommentlines=1
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumStreamNode) !NodeID,DownNodeID,DrainageID,NodeCatchID,DoutFlag,Area
	allocate (StreamNode(NumStreamNode))
	StreamNode(:)%NodeID=real_array(:,1)
	StreamNode(:)%DownNodeID=real_array(:,2)
	StreamNode(:)%RealDrainageID=real_array(:,3)
	StreamNode(:)%ProjNodeId=real_array(:,4)
	StreamNode(:)%DOutFlag=real_array(:,5)
	StreamNode(:)%LocalArea=real_array(:,7)
	StreamNode(:)%TotalArea=real_array(:,8)
	StreamNode(:)%X=real_array(:,9)
	StreamNode(:)%Y=real_array(:,10)
	StreamNode(:)%AreaInDrainage=StreamNode(:)%TotalArea

	!Drainage(i)%DSDrainage= find the StreamNode which is outlet of this Drainage, go to Downnode, find CatchID
	do i=1,NumDrainage
		!first find the streamnode at the outlet
		! DGT interprets this loop as identifying the Topnet identifier DrainageID for each node
		! from the RealDrainageID that was read in from Nodelinks
		call find1(StreamNode%RealDrainageID,Drainage(i)%RealDrainageID,NumStreamNode)
		i_outlet=ifound(1)  ! DGT 8/21/05 - This line appears redundant
		!i_outlet is the row in the StreamNode table for the outlet of this drainage
		!StreamNode(i_outlet)%DownNodeID is the node which that StreamNode flows to
		do j=1,nfound; StreamNode(ifound(j))%DrainageID=Drainage(i)%DrainageID; end do
	end do

	do i=1,NumDrainage
		!first find the streamnode at the outlet of this drainage
		! find 2 returns in ifound the list of indices for which firstarg(i)=2ndarg and
! 3rdarg(i)=4tharg.  The search is from 1 to 5tharg
		call find2(StreamNode%RealDrainageID,Drainage(i)%RealDrainageID,StreamNode%DOutFlag,IsDrainageOutlet,NumStreamNode)
		i_outlet=ifound(1)
		!now find all streamnodes in this drainage
		call find1(StreamNode%RealDrainageID,Drainage(i)%RealDrainageID,NumStreamNode)
		nnode=nfound
		if (nnode.eq.1) then
			StreamNode(i_outlet)%FracRunoff=1 !there's only one node in this drainage, so it was easy
			StreamNode(i_outlet)%AreaInDrainage=StreamNode(i_outlet)%LocalArea
		elseif (nnode.gt.1) then !if there are other nodes than the one at the outlet, then we have work to do
			!find the fraction of this drainage area which drains through each stream node in this drainage
			if (allocated(ifound_1)) deallocate(ifound_1); allocate(ifound_1(nfound));
			ifound_1=ifound
			sumarea=0; do j=1,nnode; sumarea=sumarea+StreamNode(ifound_1(j))%LocalArea; end do

			do jj=1,nnode
				j=ifound_1(jj)
				!is this node getting some area directly from another drainage?
				call find2a(StreamNode%DownNodeID,StreamNode(j)%NodeID, &
							StreamNode%DrainageID,Drainage(i)%DrainageID,NumStreamNode) !A&(~B)
				if (allocated(ifound_2)) deallocate(ifound_2); allocate(ifound_2(nfound));
				ifound_2=ifound;nfound2=nfound
				if (nfound2.gt.0) then !for each of the external areas,
					do kk=1,nfound2
						area=StreamNode(ifound_2(kk))%TotalArea
						!now work downstream removing this area from all nodes within this drainage
						k=j
						do while (StreamNode(k)%DrainageID .eq. Drainage(i)%DrainageID .and. nfound.gt.0)
							StreamNode(k)%AreaInDrainage=   &
										StreamNode(k)%AreaInDrainage-area
							call find1(StreamNode%NodeID,StreamNode(k)%DownNodeID,NumStreamNode)
							if(nfound .eq. 0)exit  ! DGT 5/27/12 because with nfound = 0 k was undefined and caused subscript out of range in do while above
							k=ifound(1)
						end do !while
					end do !kk
				end if
			end do !jj
			do jj=1,nnode ! have to delay this operation until all u/s areas have been subtracted
				j=ifound_1(jj)
				StreamNode(j)%FracRunoff=StreamNode(j)%AreaInDrainage/sumarea
			end do !jj
		end if
	end do !i

!	filenm=dirname(1:len_trim(dirname)) // '/' // 'Drainage.txt'
!	expected_numcols=3
!	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumDrainage) !DrainageID	DSDrainage	ColumnInRunoff
!	allocate (Drainage(NumDrainage))
!	Drainage(:)%DrainageID=real_array(:,1)
!	Drainage(:)%DSDrainage=real_array(:,2)
!	Drainage(:)%ColumnInRunoff=real_array(:,3)
!	Drainage(:)%FractionOfRunoff=real_array(:,4)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'MeasuredFlowInfo.txt'
	expected_numcols=4
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumMeasuredFlowInfo) !MeasuredFlowID	DrainageID	ColInMeasFlow
	allocate (MeasuredFlowInfo(NumMeasuredFlowInfo))
	MeasuredFlowInfo(:)%MeasuredFlowID	=real_array(:,1)
	MeasuredFlowInfo(:)%DrainageID		=real_array(:,2)
	MeasuredFlowInfo(:)%ColInMeasFlow	=real_array(:,3)
	MeasuredFlowInfo(:)%ScalingFactor	=real_array(:,4)
!convert any reference to a DrainageID into a CatchID
	do i=1,NumMeasuredFlowInfo
		MeasuredFlowInfo(i)%RealDrainageID=MeasuredFlowInfo(i)%DrainageID
		call find1(Drainage%RealDrainageID,MeasuredFlowInfo(i)%RealDrainageID,NumDrainage)
		MeasuredFlowInfo(i)%DrainageID=Drainage(ifound(1))%DrainageID
	end do

	filenm=dirname(1:len_trim(dirname)) // '/' // 'bndryflow.dat'
	expected_numcols=-1
	ncommentlines=4
	call read_bdryflow(filenm,expected_numcols,ncommentlines,NumMeasuredFlowData) !Flow1	Flow2 Date	Time
	nSites = size(dble_array,DIM=2)-2
	call find1(int(dble_array(1:NumMeasuredFlowData,nSites+1)),StartDateTopnet,NumMeasuredFlowData)
	ioffset=ifound(1)-1
	NumMeasuredFlowData=NumMeasuredFlowData-ioffset
	allocate (MeasuredFlowData(NumMeasuredFlowData))
	do i=1,NumMeasuredFlowData
		do j = 1,nSites
			MeasuredFlowData(i)%Flow(j)	=dble_array(i+ioffset,j)*dt !cumecs to m^3/d
		end do
		MeasuredFlowData(i)%Timestep	=i
		MeasuredFlowData(i)%Date		=dble_array(i+ioffset,nSites+1)
		MeasuredFlowData(i)%Hour		=dble_array(i+ioffset,nSites+2)
	end do
	!use the elements of integer_array to change the value of MeasuredFlowInfo(:)%ColInMeasFlow from a reference
	!into a column number referring to MeasuredFlowData
	do i=1,NumMeasuredFlowInfo
		call find1(integer_array,int(MeasuredFlowInfo(i)%ColInMeasFlow),nSites)
		j=ifound(1)
		MeasuredFlowInfo(i)%ColInMeasFlow=j
	end do

!	filenm=dirname(1:len_trim(dirname)) // '/' // 'Runoff.txt'
!	expected_numcols=-1
!	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumRunoff) !Flow1	Flow2 Date	Time
!	allocate (Runoff(NumRunoff))
!	nSites = size(real_array,DIM=2)-2
!	do i=1,NumRunoff
!!		allocate (Runoff(i)%Runoff(nSites))
!		do j = 1,nSites
!			Runoff(i)%Rate(j)	=real_array(i,j)
!		end do
!		Runoff(i)%Timestep =i
!	end do
!	Runoff(:)%Date		=real_array(:,nSites+1)
!	Runoff(:)%Hour		=real_array(:,nSites+2)


	filenm=dirname(1:len_trim(dirname)) // '/' // 'Reservoir.txt'
	expected_numcols=10
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumReservoir) !ReservoirID	DrainageID	InOffStream	RightID	MaxStore	InitialStore	MinStore	MaxInflow	MaxWithdrawal	MinEnvRelease	LossRate
	allocate (Reservoir(NumReservoir))
	Reservoir(:)%ReservoirID=real_array(:,1)
	Reservoir(:)%DrainageID=real_array(:,2)
	Reservoir(:)%InOffStream=real_array(:,3)
	Reservoir(:)%RightID=real_array(:,4)
	Reservoir(:)%StoreMax=real_array(:,5)
	Reservoir(:)%StoreInitial=real_array(:,6)
	Reservoir(:)%StoreMin=real_array(:,7)
	Reservoir(:)%MaxInflow=real_array(:,8)
	Reservoir(:)%MaxWithdrawal=real_array(:,9)
	Reservoir(:)%MinEnvRelease=real_array(:,10)
!	Reservoir(:)%LossRate=real_array(:,11)

!convert references to a DrainageID into the CatchID
	do i=1,NumReservoir
		Reservoir(i)%RealDrainageID=Reservoir(i)%DrainageID
		call find1(Drainage%RealDrainageID,Reservoir(i)%RealDrainageID,NumDrainage)
		Reservoir(i)%DrainageID=Drainage(ifound(1))%DrainageID
	end do

	filenm=dirname(1:len_trim(dirname)) // '/' // 'User.txt'
	expected_numcols=15
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumUser) !UserID	UserType	POU_ID	DemandType	DemandVble	DemandRate	ReturnFlowID	SourceMixingID	NumSources	SourceID1	RightID1	SourceID2	RightID2	SourceID3	RightID3
	allocate (User(NumUser+2*NumReservoir))
	User(1:NumUser)%UserID=real_array(:,1)
	User(1:NumUser)%UsersType=real_array(:,2)
	User(1:NumUser)%RealPOU_ID=real_array(:,3)
	User(1:NumUser)%DemandVble=real_array(:,4)
	User(1:NumUser)%DemandRate=real_array(:,5)
	User(1:NumUser)%InYearDemandType=real_array(:,6)
	User(1:NumUser)%ReturnFlowID=real_array(:,7)
	User(1:NumUser)%SourceMixingID=real_array(:,8)
	User(1:NumUser)%NumSources=real_array(:,9)

	do i=1,NumUser
		do j=1,User(i)%NumSources
			User(i)%SourceID(j)=real_array(i,9+2*j-1)
			User(i)%RightID(j) =real_array(i,9+2*j)
		end do
	end do

!convert references to a DrainageID into the CatchID
	do i=1,NumUser
		call find1(Drainage%RealDrainageID,User(i)%RealPOU_ID,NumDrainage)
		User(i)%POU_ID=Drainage(ifound(1))%DrainageID
	end do

	filenm=dirname(1:len_trim(dirname)) // '/' // 'Source.txt'
	expected_numcols=5
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumSource) !SourceID	Type	SourceLocationID	PhysicalDailyMax	PhysicalAnnMax
	allocate (Source(NumSource+2*NumReservoir))
	Source(1:NumSource)%SourceID=real_array(:,1)
	Source(1:NumSource)%Type=real_array(:,2)
	Source(1:NumSource)%SourceLocationID=real_array(:,3)
	Source(1:NumSource)%PhysicalDailyMax=real_array(:,4)
	Source(1:NumSource)%PhysicalAnnMax=real_array(:,5)
!convert references to a DrainageID into the CatchID
	do i=1,NumSource
		Source(i)%RealSourceLocationID=Source(i)%SourceLocationID
		call find1(Drainage%RealDrainageID,Source(i)%RealSourceLocationID,NumDrainage)
		if (nfound.eq.1) Source(i)%SourceLocationID=Drainage(ifound(1))%DrainageID
	end do

	filenm=dirname(1:len_trim(dirname)) // '/' // 'Rights.txt'
	expected_numcols=4
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumRights) !RightID	PurposeCode	PriorityDate	LegalDailyMax	LegalAnnMax
	allocate (Rights(NumRights))
	Rights(:)%RightID=real_array(:,1)
	Rights(:)%PriorityDate=real_array(:,2)
	Rights(:)%LegalDailyMax=real_array(:,3)
	Rights(:)%LegalAnnMax=real_array(:,4)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'SourceMixing.txt'
	expected_numcols=6
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumSourceMixing) !SourceMixingID	UsersSourceNum	Units	Amount	SeasonNumber	SeasonsDefnID
	allocate (SourceMixing(NumSourceMixing))
	SourceMixing(:)%SourceMixingID=real_array(:,1)
	SourceMixing(:)%UsersSourceNum=real_array(:,2)
	SourceMixing(:)%Units=real_array(:,3)
	SourceMixing(:)%Amount=real_array(:,4)
	SourceMixing(:)%SeasonNumber=real_array(:,5)
	SourceMixing(:)%SeasonsDefnID=real_array(:,6)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'SeasonsDefn.txt'
	expected_numcols=5
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumSeasonsDefn) !SourceMixingID	UsersSourceNum	Units	Amount	SeasonNumber	SeasonsDefnID
	allocate (SeasonsDefn(NumSeasonsDefn))
	SeasonsDefn(:)%SeasonsDefnID=real_array(:,1)
	do i=1,NumSeasonsDefn
		do j=1,4
			SeasonsDefn(i)%StartDaySeason(j)=real_array(i,j+1)
		end do
	end do

	filenm=dirname(1:len_trim(dirname)) // '/' // 'ReturnFlow.txt'
	expected_numcols=11
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumReturnFlow) !ReturnFlowID	NumReturnFlows	ReturnFlowUnits	ReturnFlowAmt1 ReturnFlowType1	ReturnFlowLocn1	ReturnFlowAmt2	ReturnFlowType2	ReturnFlowLocn2
	allocate (ReturnFlow(NumReturnFlow+2*NumReservoir))
	allocate (WWTP_list(2*(NumReturnFlow+2*NumReservoir)))
	ReturnFlow(1:NumReturnFlow)%ReturnFlowID=real_array(:,1)
	ReturnFlow(1:NumReturnFlow)%NumReturnFlows=real_array(:,2)
	ReturnFlow(1:NumReturnFlow)%ReturnFlowsUnits=real_array(:,3)
	do i=1,NumReturnFlow
		j=ReturnFlow(i)%NumReturnFlows
		do j=1,User(i)%NumSources
			ReturnFlow(i)%ReturnFlowsAmt(j)=real_array(i,3+4*j-3)
			ReturnFlow(i)%ReturnFlowsType(j) =real_array(i,3+4*j-2)
			ReturnFlow(i)%ReturnFlowsLocn(j) =real_array(i,3+4*j-1)
			ReturnFlow(i)%WWTP_ID(j) =real_array(i,3+4*j)
			call find1(WWTP_list,ReturnFlow(i)%WWTP_ID(j),NumWWTP)
			if (ReturnFlow(i)%WWTP_ID(j).gt.0 .and. nfound.eq.0) then
				NumWWTP=NumWWTP+1
				WWTP_list(NumWWTP)=ReturnFlow(i)%WWTP_ID(j)
			end if
			ReturnFlow(i)%RealReturnFlowsLocn(j)=ReturnFlow(i)%ReturnFlowsLocn(j)
			if (ReturnFlow(i)%ReturnFlowsLocn(j).gt.0) then
				call find1(Drainage%RealDrainageID,ReturnFlow(i)%RealReturnFlowsLocn(j),NumDrainage)
				ReturnFlow(i)%ReturnFlowsLocn=Drainage(ifound(1))%DrainageID
			end if
		end do
	end do
	do i=(NumReturnFlow+1),(NumReturnFlow+2*NumReservoir)
		ReturnFlow(i)%ReturnFlowsAmt(1)=0.
		ReturnFlow(i)%ReturnFlowsType(1)=0
		ReturnFlow(i)%ReturnFlowsLocn(1)=0
		ReturnFlow(i)%RealReturnFlowsLocn(1)=0
		ReturnFlow(i)%WWTP_ID(1)=0
	end do

!	filenm=dirname(1:len_trim(dirname)) // '/' // 'DemandCoefficients.txt'
!	expected_numcols=2
!	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumDemandCoefficients) !DemandType	DemandPerUnitPerDay
!	allocate (DemandCoefficients(NumDemandCoefficients))
!	DemandCoefficients(:)%DemandType=real_array(:,1)
!	DemandCoefficients(:)%DemandPerUnitPerDay=real_array(:,2)

	filenm=dirname(1:len_trim(dirname)) // '/' // 'MonthlyDemandFraction.txt'
	expected_numcols=13
	ncommentlines=2
	call read_struct_from_text(filenm,expected_numcols,ncommentlines,NumMonthlyDemand) !InYearDemandType	Month1	Month2	Month3	Month4	Month5	Month6	Month7	Month8	Month9	Month10	Month11	Month12
	allocate (MonthlyDemand(NumMonthlyDemand))
	MonthlyDemand(:)%InYearDemandType=real_array(:,1)
	do i=1,NumMonthlyDemand
		do j=1,12
			MonthlyDemand(i)%Month(j)=real_array(i,1+j)
		end do
	end do

	end

subroutine datevec(yyyymmdd,hhmmss,yyyy,mm,dd,hh,mi,ss)
!split yyyymmdd,hhmmss into components yyyy,mm,dd,hh,mi,ss
integer yyyymmdd,hhmmss,yyyy,mm,dd,hh,mi,ss

	dd=mod(yyyymmdd,100)
	mm=mod((yyyymmdd-dd)/100,100)
	yyyy=int(yyyymmdd/10000)
	ss=mod(hhmmss,100)
	mi=mod((hhmmss-ss)/100,100)
	hh=int(hhmmss/10000)
return
end
subroutine undatevec(yyyy,mm,dd,hh,mi,ss,yyyymmdd,hhmmss)
!combine components yyyy,mm,dd,hh,mi,ss into yyyymmdd,hhmmss
integer yyyymmdd,hhmmss,yyyy,mm,dd,hh,mi,ss

yyyymmdd=10000*yyyy+100*mm+dd
hhmmss=10000*hh+100*mi+ss

return
end
