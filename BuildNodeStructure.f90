	subroutine BuildNodeStructure(NumDrainage, NumUser, NumReservoir,NumSource,NumReturnFlow,	&
									NumMeasuredFlowInfo,NumNode)
	!inputs
	!Drainage, NumDrainage, NumStreamNode, User, NumUser, Reservoir, NumReservoir, 
	!Source,NumSource,
      !ReturnFlow,NumReturnFlow,MeasuredFlowInfo, NumMeasuredFlowInfo
	
	!outputs: 
	!Node,NumNode,*User,NumUser,*ReturnFlow,NumReturnFlow,*Source,NumSource

!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule

	integer ReturnFlowsLocation,ReservoirReleaseUseCode
	integer i

	![Node,NumNode]=CreateNode(Node,NumNode,Title,NodeCode,IntExtCode,StoreMin,StoreMax,Store,DrainageID,SelfID);

!Drainages
	NumNode=0
	!Node=[]
	do i=1,NumDrainage !Create 3 nodes for each drainage: one for the outlet, one for groundwater, one for runoff-production
		!Stream nodes (one node at the outlet of each model unit drainage)
		call CreateNode(NumNode,'STREAM',StreamNodeCode,InternalCode,0.,0.,0.,i,i)
		!Groundwater nodes (one node per model unit drainage)
		call CreateNode(NumNode,'GWATER',GroundwaterNodeCode,ExternalCode,-1e20,0.,0.,i,i)
     	   !Model Unit Drainage nodes (one node for the production of each local runoff)
		call CreateNode(NumNode,'RUNOFF',DrainageNodeCode,ExternalCode,0.,0.,0.,i,i)
	end do

!Reservoirs
	do i=1,NumReservoir
	!three nodes per reservoir, a user node to take water, a reservoir node to hold it, and a user node to release water
	!(in-stream and off-stream are identical here, only differ in the links they set up)
	!in this loop we only create one node (reservoir itself), and do the
	!preparatory work of creating users, so that two other user nodes per reservoir 
	!can be set up in the next loop. For each user node we will make, we will need to know
	!about the User, including their Source and ReturnFlow details. 

	!Reservoir node which can store water
	!ReservoirID	DrainageID	InOffStream	RightID	StoreMax	StoreInitial	StoreMin	MaxInflow	MaxWithdrawal	MinEnvRelease	LossRate
		call CreateNode(NumNode,'RSVOIR',ReservoirNodeCode,		&
     		InternalCode,Reservoir(i)%StoreMin,Reservoir(i)%StoreMax,	&
     		Reservoir(i)%StoreInitial,Reservoir(i)%DrainageID,i)
		NewReservoirNode=NumNode

		!RAW 29-Aug-2005
		if (Reservoir(i)%InOffStream .eq. InStreamReservoirCode) then
		!Preparation for first new user node
		!Create an entry in the source table so we can get water for this reservoir
		!SourceID	Type	SourceLocationID	PhysicalDailyMax	PhysicalAnnMax
			call CreateSource(NumSource,StreamSourceCode,			&
     			Reservoir(i)%DrainageID,Reservoir(i)%RealDrainageID,Reservoir(i)%MaxInflow,BigReal)
			j_src=NumSource

		!Create an entry in the return flow table, to send water from the user to the reservoir
		!ReturnFlowID	NumReturnFlows	ReturnFlowUnits	ReturnFlowAmt   ReturnFlowType	ReturnFlowLocn
			call CreateReturnFlow(NumReturnFlow,1,				&
     			FractionUnits,1.,ReservoirNodeCode,Reservoir(i)%ReservoirID)
			j_ret=NumReturnFlow

		!Create User to take the water for reservoir 
		!    CreateUser(NumUser,UsersType,POU_ID,DemandType,DemandVble,DemandRate,InYearDemandType, ...
		!                                    ReturnFlowID,SourceMixingID,NumSources,SourceID,RightID);
			temp=min(Reservoir(i)%StoreMax-Reservoir(i)%StoreMin, Reservoir(i)%MaxInflow)
			call CreateUser(NumUser,ReservoirFillUseCode,				&
	     		Reservoir(i)%DrainageID,0,temp,1.,0,		&
     			ReturnFlow(j_ret)%ReturnFlowID,0,1,Source(j_src)%SourceID,	&
     			Reservoir(i)%RightID)
		!END of Preparation for first new user node
		end if

		!Preparation for second new user node
		!Create an entry in the source table so the "release" user can get their water from somewhere
		call CreateSource(NumSource,ReservoirSourceCode,			&
     		Reservoir(i)%ReservoirID,Reservoir(i)%ReservoirID,1e20,1e20)
		j_src=NumSource;
	
		!Create an entry in the return flow table, to send all water from the reservoir to the stream (possibly d/s)
		select case (Reservoir(i)%InOffStream)
			case (InStreamReservoirCode)
				call find1(Drainage%DrainageID,Reservoir(i)%DrainageID,NumDrainage)
				j_source=ifound(1) !problem if nfound<>1
				ReturnFlowsLocation=Drainage(j_source)%DSDrainage
				ReservoirReleaseUseCode=InStreamReservoirReleaseUseCode
			case (OffStreamReservoirCode)
				ReturnFlowsLocation=Reservoir(i)%DrainageID
				ReservoirReleaseUseCode=OffStreamReservoirReleaseUseCode
		end select
		call CreateReturnFlow(NumReturnFlow,1,				&
   			FractionUnits,1.,StreamNodeCode,ReturnFlowsLocation)
			j_ret=NumReturnFlow;
	
		!Create User to take the overflow water and MinEnvRelease from reservoir 
		!    CreateUser(NumUser,UsersType,POU_ID,DemandType,DemandVble,DemandRate,InYearDemandType, ...
		!                                    ReturnFlowID,SourceMixingID,NumSources,SourceID,RightID);
		temp=min(Reservoir(i)%StoreMax-Reservoir(i)%StoreMin, Reservoir(i)%MinEnvRelease) !RAW correction 29-Aug-2005
		call CreateUser(NumUser,ReservoirReleaseUseCode,			&
   			Reservoir(i)%DrainageID,0,temp,1.,0,						&
   			ReturnFlow(j_ret)%ReturnFlowID,0,1,							&
   			Source(j_src)%SourceID,0)
		!END of Preparation for second new user node
	end do !of loop on Reservoirs

!Users
	do i=1,NumUser !create one node for each user
	!one node for each user !!!!Node(k).UsersSourceID=(1:User(i).NumSources); 
		call CreateNode(NumNode,'USER__',UserNodeCode,				&
     		InternalCode,0.,0.,0.,User(i)%POU_ID,i)
		User(i)%NodeNumber=NumNode !this saves us having to find the node for this user
	end do
!Measured Flows
	do i=1,NumMeasuredFlowInfo  !one node per measured inflow, to bring in the required flow difference to get the inflow at the node right
	!one node for each Measured Inflow
		call CreateNode(NumNode,'MEASIN',MeasuredFlowNodeCode,		&
     		ExternalCode,0.,0.,0.,MeasuredFlowInfo(i)%DrainageID,-1)
	end do
!Sink - one node for all outlets (could split later)
		call CreateNode(NumNode,'SINK__',SinkNodeCode,				&
     		ExternalCode,0.,0.,0.,-1,-1)
    
	return
	end

	subroutine CreateNode(NumNode,Title,NodeCode,IntExtCode,		&
     	StoreMin,StoreMax,Store,DrainageID,SelfID)
	!outputs Node, NumNode
	!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use other_structures

	character*6 Title
	integer NodeCode,IntExtCode,DrainageID,SelfID
	real StoreMin,StoreMax,Store
	integer k

	NumNode=NumNode+1
	k=NumNode
	Node(k)%Title=Title
	Node(k)%Type=NodeCode
	Node(k)%IntExt=IntExtCode
	Node(k)%StoreMin=StoreMin
	Node(k)%StoreMax=StoreMax
	Node(k)%Store=Store
	Node(k)%StoreOld=Store
	Node(k)%DrainageID=DrainageID
	Node(k)%SelfID=SelfID !only the user nodes need this SelfID so far (it's an index back into the table that this type of node comes from)
	return
	end

	subroutine CreateUser(NumUser,UsersType,POU_ID,DemandType,	&
     	DemandVble,DemandRate,InYearDemandType,ReturnFlowID,SourceMixingID,NumSources,	&
     	SourceID,RightID)
	!outputs User, NumUser
	!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use input_structures

	integer UsersType,POU_ID,DemandType,								&
     	InYearDemandType,ReturnFlowID,SourceMixingID,NumSources,	&
     	SourceID,RightID
	real DemandVble,DemandToday 
	integer k
	integer idmax

	NumUser=NumUser+1
	k=NumUser
	if (k.eq.1) then
		User(k)%UserID=1
	else
		idmax=0
		do i=1,k-1
			idmax=max(idmax,User(i)%UserID)
		end do
		User(k)%UserID=1+idmax
	endif

	User(k)%UsersType=UsersType
	User(k)%POU_ID=POU_ID
	User(k)%DemandVble=DemandVble
	User(k)%DemandRate=DemandRate
	User(k)%InYearDemandType=InYearDemandType
	User(k)%ReturnFlowID=ReturnFlowID
	User(k)%SourceMixingID=SourceMixingID
	User(k)%NumSources=NumSources
	User(k)%SourceID=SourceID
	User(k)%RightID=RightID
	return
	end

	subroutine CreateSource(NumSource,SourcesType,				&
		SourceLocationID,RealSourceLocationID,PhysicalDailyMax,PhysicalAnnMax)
	!outputs Source, NumSource
	!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use input_structures

	integer SourcesType,SourceLocationID,RealSourceLocationID
	real PhysicalDailyMax,PhysicalAnnMax
	integer k
	integer idmax,ids

	NumSource=NumSource+1
	k=NumSource

	if (k.le.1) then
		ids=1
	else 
		idmax=0		
		do i=1,k-1
			idmax=max(idmax,Source(i)%SourceID)
		end do
		ids=1+idmax
	endif

	Source(k)%SourceID=ids
	Source(k)%Type=SourcesType
	Source(k)%SourceLocationID=SourceLocationID
	Source(k)%RealSourceLocationID=RealSourceLocationID
	Source(k)%PhysicalDailyMax=PhysicalDailyMax
	Source(k)%PhysicalAnnMax=PhysicalAnnMax
	return
	end

	subroutine CreateReturnFlow(NumReturnFlow,				&
     	NumReturnFlows,ReturnFlowsUnits,								&
     	ReturnFlowsAmt,ReturnFlowsType,ReturnFlowsLocn)
	!outputs ReturnFlow, NumReturnFlow
	!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use input_structures

	integer NumReturnFlows,ReturnFlowsUnits,ReturnFlowsType,ReturnFlowsLocn
	real ReturnFlowsAmt
	integer k
	integer idmax

	NumReturnFlow=NumReturnFlow+1;
	k=NumReturnFlow;
	if (k.eq.1) then
		ReturnFlow(k)%ReturnFlowID=1;
	else
		idmax=0		
		do i=1,k-1
			idmax=max(idmax,ReturnFlow(i)%ReturnFlowID)
		end do
		ReturnFlow(k)%ReturnFlowID=1+idmax
	endif
	ReturnFlow(k)%NumReturnFlows=NumReturnFlows
	ReturnFlow(k)%ReturnFlowsUnits=ReturnFlowsUnits
	ReturnFlow(k)%ReturnFlowsAmt(1)=ReturnFlowsAmt
	ReturnFlow(k)%ReturnFlowsType(1)=ReturnFlowsType
	ReturnFlow(k)%ReturnFlowsLocn(1)=ReturnFlowsLocn
	return
	end