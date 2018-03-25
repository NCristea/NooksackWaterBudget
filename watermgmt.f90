subroutine watermgmt(StartDateTopnet,StartHourTopnet,Timestep,NSteps,		& !inputs
     runoffTopnet,baseflowTopnet,ArtDrainageTopnet,vol_irrig_demand,MAXSLP,	& !inputs
	 evaporation,precipitation,								& !inputs
     volume_irrig_sup,groundwater_to_take)	!outputs


use constant_definitions
use input_structures
use other_structures
use findmodule

save !so we remember from one timestep to the next without passing stuff

integer StartDateTopnet,Timestep,NSteps,StartHourTopnet
real*8 runoffTopnet(MAXSLP),baseflowTopnet(MAXSLP),vol_irrig_demand(MAXSLP)
real*8 volume_irrig_sup(MAXSLP),groundwater_to_take(MAXSLP)
real*8 evaporation(MAXSLP),precipitation(MAXSLP)
real*8 ArtDrainageTopnet(MAXSLP)  ! DGT 6/28/05   Added ArtDrainage
real*8 scalefactor   ! DGT 6/29/05

character*128 dirname
integer, allocatable :: DrainageOrder(:)
real, allocatable :: DrainageOutFlow(:)
integer*8 i8nowsecs,i8startsecs,i8StartofCalendarYearsecs
integer DOY, ThisYear,ThisMonth,ThisDay,hh,mi,ss, dt,yyyymmdd,hhmmss
	integer, allocatable :: RFType(:),RFLocn(:)

integer nReturnFlows_to_GW
integer, allocatable :: ReturnFlows_to_GW_LinkID(:),ReturnFlows_to_GW_DrainageID(:)

integer MaxNodes,MaxLinks,RFNodeID

if (Timestep.eq.0) then

!!!!!!INITIALISE!!!!!!
if(NSteps > MaxNtimeSteps)then  ! DGT 6/29/05  Checking for too many time steps
   write(6,*)NSteps,' time steps is more than the maximum allowed: ', MaxNtimeSteps
   write(6,*)'Have a programmer adjust the dimensions in types.f90'
   write(6,*)'Exiting'
   stop
endif

	t0=secnds(0.)
	dirname='.' !current directory
	dt=86400
	!dirname='bethexample'
	!dirname='daveexample'
	!dirname='nooksack1'

	call read_inputs(dirname, dt,StartDateTopnet,							&
		NumDrainage,NumStreamNode,NumMeasuredFlowInfo,NumMeasuredFlowData,		&
		NumReservoir,NumUser,NumSource,NumRights,NumSourceMixing,	&
		NumSeasonsDefn,NumReturnFlow,NumMonthlyDemand,NumRunoff,NumBaseflow,NumWWTP)
	RunControl%NumTimesteps=NSteps

	!Build Node Structure
	MaxNodes=3*NumDrainage+NumReservoir+(NumUser+2*NumReservoir)+NumMeasuredFlowData+1
	allocate (Node(MaxNodes))
	call BuildNodeStructure(								&
     NumDrainage, NumUser, NumReservoir,				&
     NumSource,NumReturnFlow,							&
     NumMeasuredFlowInfo,								&
     NumNode)
	allocate (NodeSave(MaxNodes));NodeSave=Node;deallocate(Node);allocate(Node(NumNode))
	Node(1:NumNode)=Nodesave(1:NumNode);deallocate(NodeSave);allocate(NodeSave(NumNode))

	!Build Link Structure
	MaxLinks=3*NumDrainage+5*(NumUser+2*NumReservoir)+NumMeasuredFlowData
	allocate (Link(MaxLinks))
	call BuildLinkStructure(						&
     NumDrainage, NumUser, NumSource,			&
     NumReturnFlow, NumReservoir,				&
     NumMeasuredFlowInfo, NumNode,				&
     NumLink)
	allocate (LinkSave(MaxLinks));LinkSave=Link;deallocate(Link);allocate(Link(NumLink))
	Link(1:NumLink)=Linksave(1:NumLink);deallocate(LinkSave);allocate (LinkSave(NumLink))

	!Build Drainage Ordering
	allocate (DrainageOrder(NumDrainage))
	call BuildDrainageOrder(NumDrainage,DrainageOrder)

	!Assign Priority To User-Source Combinations
	allocate (UserSourceOrder(5*NumUser))
	allocate (UserSourceTable(5*NumUser))
	call AssignPriorityOrder(NumUser,NumSource,					&
   		NumRights,NumReservoir,RunControl%AllocationMode,		&
   		DrainageOrder,NumDrainage,NumUserSource,NumUserSourceReturn, &
		NumReturnFlow)   ! DGT 9/4/05 Added NumReturnFlow so that where it is used in this subroutine it is not undefined

	!Initialise Volume taken by each user from each source
	do i=1,NumUser
		do j=1,User(i)%NumSources
			User(i)%VolumeToDateSource(j)=0
		end do
	end do

	call td8micsec(StartDateTopnet,StartHourTopnet,i8startsecs) !seconds to start time since start of 1 Jan 1940

	allocate (StaticOutput%DrainageInfo(NumUserSourceReturn))
	allocate (StaticOutput%StreamFlowLinks(NumDrainage))
	allocate (StaticOutput%DrainageID(NumDrainage))
	call Initialise_Output_Tables(NumDrainage,NumNode,NumStreamNode,NumLink,NumUser,	&
					RunControl%NumTimesteps,NumReservoir,NumUserSourceReturn,NumReturnFlow)

	!Set up some indices to save time on searching
	do ii=1,NumDrainage
		i=DrainageOrder(ii);
	    call find2(Node%Type,StreamNodeCode,Node%DrainageID,i,NumNode) !6/3 secs
		Drainage(i)%isn=ifound(1)	!problem if nfound<>1

	    call find1(Link%DSNode,Drainage(i)%isn,NumLink) !4/3 secs
		Drainage(i)%n_in=nfound; Drainage(i)%ifound_in(1:nfound)=ifound(1:nfound);

	    call find2a(Link%USNode,Drainage(i)%isn, & !A & (~B)  !7/3 secs
			Link%LinkCode,UnallocatedLinkCode,NumLink);
		Drainage(i)%n_taken=nfound;Drainage(i)%ifound_taken(1:nfound)=ifound(1:nfound);

		call find2(Link%USNode,Drainage(i)%isn,Link%LinkCode,UnallocatedLinkCode,NumLink); !7/3 secs
		Drainage(i)%j_unallocated=ifound(1) !problem if nfound<>1
	end do
	do i=1,NumReservoir
	    call find2(Node%Type,ReservoirNodeCode,Node%SelfID,i,NumNode);
		Reservoir(i)%isn=ifound(1) !problem if nfound<>1
	    call find1(Link%DSNode,Reservoir(i)%isn,NumLink)
		Reservoir(i)%n_in=nfound; Reservoir(i)%ifound_in(1:nfound)=ifound(1:nfound);
	    call find1(Link%USNode,Reservoir(i)%isn,NumLink)
		Reservoir(i)%n_taken=nfound;Reservoir(i)%ifound_taken(1:nfound)=ifound(1:nfound);
	end do

	call find1(Node%Type,SinkNodeCode,NumNode)
	isink=ifound(1)

	do ii=1,NumUserSource
		kk=UserSourceOrder(ii)
		i=UserSourceTable(kk)%UserID;
		j=UserSourceTable(kk)%SourceCounter;
        call find1(Source%SourceID,User(i)%SourceID(j),NumSource);  !User(i)%source_ind(1)=j_source
		j_source=ifound(1) !problem if nfound<>1
        call find2(SourceMixing%SourceMixingID,User(i)%SourceMixingID, &
                         SourceMixing%UsersSourceNum,j,NumSourceMixing);
		User(i)%j_srcmx(j)=ifound(1) !problem if nfound<>1

		!k is node of this user
		k=User(i)%NodeNumber
		iSrcLocnID=Source(j_source)%SourceLocationID;
		if (iSrcLocnID==0) iSrcLocnID=User(i)%POU_ID
		!jj is link from source to user
		call find2(Node%Type,Source(j_source)%Type,Node%SelfID,iSrcLocnID,NumNode);
		i_node=ifound(1) !problem if nfound<>1
		call find3(Link%LinkCode,UserAbstractionLinkCode,Link%USNode,i_node,Link%DSNode,k,	&
					NumLink);
		jj=ifound(1) !problem if nfound<>1
		User(i)%LinkSourceToUser(j)=jj

		call find2(Link%LinkCode,SinkLinkCode,Link%USNode,k,NumLink);
		j_sink=ifound(1) !problem if nfound<>1
		User(i)%LinkSourceToSink=j_sink

		if (User(i)%ReturnFlowID .gt. 0) then
		    call find1(ReturnFlow%ReturnFlowID,User(i)%ReturnFlowID,NumReturnFlow);
			j_r=ifound(1) !problem if nfound<>1
		    NumReturnFlows=ReturnFlow(j_r)%NumReturnFlows;
			if (allocated(RFType)) deallocate(RFType)
			if (allocated(RFLocn)) deallocate(RFLocn)
			allocate(RFType(NumReturnFlows),RFLocn(NumReturnFlows))
			do m=1,NumReturnFlows
				RFType(m)=ReturnFlow(j_r)%ReturnFlowsType(m);
				if (RFType(m).eq.0) RFType(m)=StreamNodeCode
				RFLocn(m)=ReturnFlow(j_r)%ReturnFlowsLocn(m);
!				if (RFLocn(m).eq.0) RFLocn(m)=User(i)%POU_ID  !RAW 2005-sep-05 not general enough to account for Instreamflowuse
!instead insert 7 new lines
				if (RFLocn(m).eq.0) then
					if (User(i)%UsersType == InstreamFlowUseCode) then
						RFLocn(m)=Drainage(User(i)%POU_ID)%DSDRAINAGE   ! This is the downstream drainge
					else
						RFLocn(m)=User(i)%POU_ID
					end if
				end if
				if (RFType(m).eq.1 .and. RFLocn(m)==-1) then
					call find1(Drainage%DrainageID,User(i)%POU_ID,NumDrainage);
					j_drainage=ifound(1) !problem if nfound<>1
					RFlocn(m)=Drainage(j_drainage)%DSDrainage;
				end if
			end do
		elseif (User(i)%ReturnFlowID .lt. 0) then
			NumReturnFlows=0;
		else
			NumReturnFlows=1;
			if (allocated(RFType)) deallocate(RFType)
			if (allocated(RFLocn)) deallocate(RFLocn)
			allocate(RFType(NumReturnFlows),RFLocn(NumReturnFlows))
			NumReturnFlows=1; RFType(1)=1;RFLocn(1)=1
		end if
		do m=1,NumReturnFlows !length(User(i)%ReturnFlowFrac) !return flow(s) from user to return flow nodes
			if (RFLocn(m).gt.0) then
				call find2(Node%Type,RFType(m),Node%SelfID,RFLocn(m),NumNode);
				RFNodeID=ifound(1) !problem if nfound<>1
			else
				RFNodeID=isink
			end if
			!j_return= link from user node to returnflow node
			call find3(Link%LinkCode,ReturnFlowLinkCode,Link%USNode,k, &
                  Link%DSNode,RFNodeID,NumLink);
			!!!j_return=ifound
			User(i)%LinkUserToReturnflow(m)=ifound(1)
		end do

	end do

	call find1(Link%LinkCode,ReturnFlowLinkCode,NumLink); !identify the links which provide return flows
	nReturnFlows_to_GW=0
	allocate(ReturnFlows_to_GW_DrainageID(nfound),ReturnFlows_to_GW_LinkID(nfound))
	do i=1,nfound
		if (Node(Link(ifound(i))%DSNode)%Type .eq. GroundwaterNodeCode) then ! if this returnflow is going to a G/W node, we need to know about it
			nReturnFlows_to_GW=nReturnFlows_to_GW+1
			ReturnFlows_to_GW_LinkID(nReturnFlows_to_GW) = ifound(i)
			ReturnFlows_to_GW_DrainageID(nReturnFlows_to_GW) = Node(Link(ifound(i))%DSNode)%DrainageID
		end if
	end do



	t1=secnds(0.)
	write(6,*)t1-t0,' seconds to read input files'
!  DGT 6/29/05 Stuff for writing files as we go
    icall=0
    call Write_OutputLine('Artificial_Drainage_cms.txt',timestep,ArtDrainageTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1
	call Write_OutputLine('Precipitation_mm.txt',timestep,precipitation,NumDrainage,icall,scalefactor)
	icall=icall+1
	call Write_OutputLine('Evaporation_mm.txt',timestep,evaporation,NumDrainage,icall,scalefactor)
    icall=icall+1;
	call Write_OutputLine('Baseflow_cms.txt',timestep,baseflowTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1;
	call Write_OutputLine('TotalRunoff_noWithdrawal_cms.txt',timestep,RunoffTopnet,NumDrainage,icall,scalefactor)
!  DGT 9/4/05 Added this function to output Local Contributions to each StreamNode
	call Write_OutputLocalContributions(NumStreamNode,NumDrainage,baseflowTopnet,RunoffTopnet,timestep,scalefactor)

!!!!!!END OF INITIALISE!!!!!!

else if (Timestep.gt.0) then
!!!!!do this Timestep !!!!!!
	call timecalcs(Timestep, dt, i8startsecs, &
		DOY, ThisMonth, ThisDay, yyyymmdd, hhmmss)

    if (ThisMonth.eq.RunControl%StartOfWaterYearmm .and. &
		  ThisDay.eq.RunControl%StartOfWaterYeardd) then
		do i=1,NumUser; do j=1,User(i)%NumSources; User(i)%VolumeToDateSource(j)=0; end do; end do
	end if

	do j=1,NumLink; Link(j)%Flow=0; end do

    !use the runoff values to assign flows to links
	if (.not. allocated(DrainageOutFlow) ) allocate (DrainageOutFlow(NumDrainage))
	if (.not. allocated(Runoff) ) allocate (Runoff(NSteps))
	if (.not. allocated(Baseflow) ) allocate (Baseflow(NSteps))
!	if (.not. allocated(ArtDrainage) ) allocate (ArtDrainage(NSteps)) ! DGT 6/28/05
!	write(6,*)'before ',drainageoutflow(1)
	Runoff(Timestep)%Timestep=Timestep;Runoff(Timestep)%Date=yyyymmdd;Runoff(Timestep)%Hour=hhmmss
	Runoff(Timestep)%Rate(1:NumDrainage)=RunoffTopnet(1:NumDrainage) !m3/timestep
	Baseflow(Timestep)%Timestep=Timestep;Baseflow(Timestep)%Date=yyyymmdd;Baseflow(Timestep)%Hour=hhmmss
	Baseflow(Timestep)%Rate(1:NumDrainage)=BaseflowTopnet(1:NumDrainage) !m3/timestep

	call AssignDrainageFlows(Timestep,NumDrainage,NumNode,NumLink, &
							DrainageOrder,NumRunoff,NumBaseflow,DrainageOutFlow)
!	write(6,*)'after ',drainageoutflow(1)


	!compare the measured runoffs at boundary condition sites to the flow
	!in links there, and adjust the runoff and baseflows so the modelled
	!runoffs match the measured flows, and then recalculate the flows in the links
	call ImposeMeasuredFlows(Timestep,NumNode,NumLink,NumRunoff,NumBaseflow, &
			NumDrainage,NumMeasuredFlowInfo,NumMeasuredFlowData,DrainageOrder,DrainageOutFlow)

	call CalculateDemand(ThisMonth,DOY,vol_irrig_demand,NumUser,NumMonthlyDemand)

	groundwater_to_take(:)=0.
	volume_irrig_sup(:)=0.
	Node%StoreOld=Node%Store
	if (RunControl%AllocationMode.ne.NoAllocationCode) then
		call AllocateWaterToUsers(Timestep,NumNode,NumLink, &
			NumUser,NumReservoir,NumSource,NumRights,NumSourceMixing, &
			DrainageOrder,NumDrainage,NumReturnFlow,NumUserSource, &
			volume_irrig_sup,groundwater_to_take,DrainageOutflow);

		do i=1,nReturnFlows_to_GW !if any of the return flows deliver water to GW, tell Topnet about it.
			groundwater_to_take(ReturnFlows_to_GW_DrainageID(i)) = &
				groundwater_to_take(ReturnFlows_to_GW_DrainageID(i)) - Link(ReturnFlows_to_GW_LinkID(i))%Flow;
				!note that the return flows are subtracted because they reduce the amount of water that is to be abstracted from groundwater
				!the array groundwater_to_take is also altered for effects of GW abstraction inside AllocateWaterToUsers
				!thus a value of groundwater_to_take can be either positive (abstraction>returnflow) or negative (returnflow>abstraction)
		end do

	end if

! make some entries in the output tables
	call Append_To_Output_Tables(Timestep,dt,DrainageOutflow,NumDrainage,NumStreamNode,yyyymmdd,hhmmss, &
									precipitation,evaporation,NumLink,NumNode,Nsteps,NumUser);

! DGT 6/28/05 Artificial drainage and write outputs each step
    icall=0
	scalefactor=1./dt   ! Scale factor to change units.  This was previously done in Append_to_output_tables subroutine
    call Write_OutputLine('Artificial_Drainage_cms.txt',timestep,ArtDrainageTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1; scalefactor=1
	call Write_OutputLine('Precipitation_mm.txt',timestep,precipitation,NumDrainage,icall,scalefactor)
    icall=icall+1; scalefactor=1
	call Write_OutputLine('Evaporation_mm.txt',timestep,evaporation,NumDrainage,icall,scalefactor)
    icall=icall+1; scalefactor=1./dt
	! Overwrite BaseflowTopnet for writing.  It is not needed any more
	BaseflowTopnet(1:NumDrainage)=Baseflow(Timestep)%Rate(1:NumDrainage)
	call Write_OutputLine('Baseflow_cms.txt',timestep,BaseflowTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1; scalefactor=1./dt
	RunoffTopnet(1:NumDrainage)=Runoff(Timestep)%Rate(1:NumDrainage)
	call Write_OutputLine('TotalRunoff_noWithdrawal_cms.txt',timestep,RunoffTopnet,NumDrainage,icall,scalefactor)
! 9/4/05  DGT added call below
	call Write_OutputLocalContributions(NumStreamNode,NumDrainage,baseflowTopnet,RunoffTopnet,timestep,scalefactor)

!    if (mod(Timestep,10).eq.0) write(6,*)Timestep
!!!!end do !loop on Timestep

!!!!!END OF do this Timestep !!!!!!

else if (Timestep.lt.0) then

!!!!!FINAL OUTPUTS !!!!!!!!

	t2=secnds(0.)
	write(6,*)t2-t1,' seconds to calculate flows'
	write(6,*)'writing files'

	!now write the output tables
	call Write_Static_Output_Tables(dirname);

	!call CreateMoreOutputTables(NumUser,RunControl%NumTimesteps)

	call Write_TimeVaryingOutput_Tables(dirname,NumUser,RunControl%NumTimesteps,NumNode,NumLink,NumReturnFlow,NumWWTP);

!  DGT 6/29/05 Writing as we go close files
    icall=0
    call Write_OutputLine('Artificial_Drainage_cms.txt',timestep,ArtDrainageTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1
	call Write_OutputLine('Precipitation_mm.txt',timestep,precipitation,NumDrainage,icall,scalefactor)
    icall=icall+1
	call Write_OutputLine('Evaporation.txt',timestep,evaporation,NumDrainage,icall,scalefactor)
    icall=icall+1;
	call Write_OutputLine('Baseflow_cms.txt',timestep,BaseflowTopnet,NumDrainage,icall,scalefactor)
    icall=icall+1;
	call Write_OutputLine('TotalRunoff_noWithdrawal_cms.txt',timestep,RunoffTopnet,NumDrainage,icall,scalefactor)
! 9/4/05  DGT added call below
	call Write_OutputLocalContributions(NumStreamNode,NumDrainage,baseflowTopnet,RunoffTopnet,timestep,scalefactor)

	t3=secnds(0.)
	write(6,*)t3-t2,' seconds to write output files '

!!!!!END OF FINAL OUTPUTS !!!!!!!!
endif

end

subroutine timecalcs(Timestep, dt, i8startsecs, &
		DOY, ThisMonth, ThisDay, yyyymmdd,hhmmss)

integer Timestep, dt, DOY, ThisMonth, ThisDay, yyyymmdd, hhmmss
integer hh,mi,ss,yyyymmdd2,ihour2, ThisYear
integer*8 i8startsecs, i8nowsecs, i8StartofCalendarYearsecs

!inputs: Timestep, dt, i8startsecs
!outputs: DOY, ThisMonth, ThisDay, yyyymmdd,hhmmss
!figure out ThisMonth, ThisDate (and DOY in case we have DailyDemandFraction varying by DOY)
i8nowsecs=i8startsecs+int8((Timestep-1)*dt)
call td81micdh(yyyymmdd,hhmmss,i8nowsecs) !seconds to now since start of 1 Jan 1940
call datevec(yyyymmdd,hhmmss,ThisYear,ThisMonth,ThisDay,hh,mi,ss)
call undatevec(ThisYear,1,1,0,0,0,yyyymmdd2,ihour2)
call td8micsec(yyyymmdd2,ihour2,i8StartofCalendarYearsecs)
DOY=int((i8nowsecs-i8StartofCalendarYearsecs)/86400)

return
end

