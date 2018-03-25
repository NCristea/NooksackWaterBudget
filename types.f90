	integer MaxMeasuredFlowSites,MaxNumTimesteps,MaxNumSources,MaxNumReturnFlows,MaxNumDrainage
	parameter (MaxMeasuredFlowSites=100,MaxNumTimesteps=366*60,MaxNumSources=10)
	parameter (MaxNumReturnFlows=10,MaxNumDrainage=500)
	! MaxNumSources and MaxNumReturnFlows are per each user.  There can be more than these numbers over all users
	
	type RunControlType
		integer NumTimesteps
		integer AllocationMode
		integer StartDate
		integer StartDateyyyy
		integer StartDatemm
		integer StartDatedd
		integer StartDatehh
		integer StartOfWaterYear
		integer StartOfWaterYearyyyy
		integer StartOfWaterYearmm
		integer StartOfWaterYeardd
		integer StartOfWaterYearhh
	end type RunControlType

	type DrainageType 		
		integer DrainageID			!sequential numbers 1 ... NumDrainage, not necessarily ordered
		integer RealDrainageID		!numbering used by client
		integer DSDrainage		!the CatchID of the drainage d/s of this drainage
		integer NodeID			!the NodeID of the StreamNode at the drainage outlet
		real CatchArea		!The area of the Drainage (needs to be in same units as StreamNodeType%Area)
		integer isn,n_in,n_taken,j_unallocated
		integer ifound_in(1000),ifound_taken(1000)
	end type DrainageType

	type StreamNodeType	
		integer NodeID			!arbitrary numbering
		integer DownNodeID		!the NodeID of the stream node just downstream of this one
		integer RealDrainageID		!the numbering used by client for the drainage containing this node
		integer DrainageID		!the numbering used by Topnet for the drainage containing this node
		integer ProjNodeId		!the project's externally-set ID for this node
		integer DOutFlag		!1 if the node is drainage outlet, 0 otherwise 
		real LocalArea			!The area draining only to this node (needs to be in same units as DrainageType%CatchArea)
		real TotalArea			!The area draining to this node (needs to be in same units as DrainageType%CatchArea)
		real FracRunoff		!The fraction of the runoff in this node's drainage which flows through this node
		real X				!The X coord of this node
		real Y				!The Y coord of this node
		real AreaInDrainage !The area drainaing to this node which is in this drainage
	end type StreamNodeType

	type MeasuredFlowInfoType 		
		integer MeasuredFlowID
		integer DrainageID
		integer RealDrainageID
		integer ColInMeasFlow
		real	ScalingFactor
	end type MeasuredFlowInfoType
	
	type MeasuredFlowDataType 		
		integer Timestep
		integer Hour
		integer Date
		real Flow(MaxMeasuredFlowSites)
	end type MeasuredFlowDataType
	 										
	type ReservoirType 		
		integer ReservoirID
		integer DrainageID
		integer RealDrainageID
		integer InOffStream
		integer RightID
		real StoreMax
		real StoreInitial
		real StoreMin
		real MaxInflow
		real MaxWithdrawal
		real MinEnvRelease
		real LossRate
		integer isn,n_in,n_taken
		integer ifound_in(1000),ifound_taken(1000)
	end type ReservoirType
	
	type UserType 		
		integer UserID
		integer UsersType
		integer POU_ID
		integer RealPOU_ID
		real	DemandVble
		real	DemandRate
		integer InYearDemandType
		real	Demand(MaxNumTimesteps)
		real	Deficit(MaxNumTimesteps)
		real	Withdrawal(MaxNumTimesteps)
		real	VolumeToDateSource(MaxNumSources)  ! DGT 9/3/03 changed to MaxNumSources
		integer ReturnFlowID
		integer SourceMixingID
		integer NumSources
		integer SourceID(MaxNumSources)
		integer RightID(MaxNumSources)
		real	DemandToday
		integer NodeNumber
		integer LinkSourceToUser(MaxNumSources)
		integer LinkSourceToSink
		integer LinkUserToReturnflow(MaxNumReturnFlows)
		integer j_srcmx(MaxNumSources)
	end type UserType
				
	type SourceType 		
		integer SourceID
		integer Type
		integer SourceLocationID
		integer RealSourceLocationID
		real PhysicalDailyMax
		real PhysicalAnnMax
	end type SourceType
	
	type RightsType 		
		integer RightID
		integer PriorityDate
		real LegalDailyMax
		real LegalAnnMax
	end type RightsType
	 					
	type SourceMixingType 		
		integer SourceMixingID
		integer UsersSourceNum
		integer Units
		real Amount
		integer SeasonNumber
		integer SeasonsDefnID
	end type SourceMixingType

	type SeasonsDefnType 		
		integer SeasonsDefnID
		integer StartDaySeason(4)
	end type SeasonsDefnType

	type ReturnFlowType 		
		integer ReturnFlowID
		integer NumReturnFlows
		integer ReturnFlowsUnits
		real	ReturnFlowsAmt(MaxNumReturnFlows)
		integer ReturnFlowsType(MaxNumReturnFlows)
		integer ReturnFlowsLocn(MaxNumReturnFlows)
		integer RealReturnFlowsLocn(MaxNumReturnFlows)
		integer WWTP_ID(MaxNumReturnFlows)
	end type ReturnFlowType

!	type DemandCoefficientsType 		
!		integer DemandType
!		real DemandPerUnitPerDay
!	end type DemandCoefficientsType

	type MonthlyDemandType 		
		integer InYearDemandType
		real	Month(12)
	end type MonthlyDemandType

	type RunoffType 		
		integer Timestep
		integer Date
		integer Hour
		real	Rate(MaxNumDrainage)
	end type RunoffType

	type NodeType
		character*6 Title
		integer Type
		integer IntExt
		real StoreMin
		real StoreMax
		real Store
		real StoreOld
		integer DrainageID
		integer SelfID
	end type NodeType
	
	type LinkType
		character*6 Title
		integer LinkCode
		integer IntExtCode
		integer USNode
		integer DSNode
		real Flow
		integer ReturnFlowID
	end type LinkType

	type UserSourceTableType
		integer UserID
		integer SourceCounter
		integer PriorityDate
		integer DrainageID
	end type UserSourceTableType
	
	type StreamFlowLinksType
		integer DrainageID
		integer LinkID
	end type StreamFlowLinksType

	type DrainageIDType
		integer TopnetID
		integer DrainageID
	end type DrainageIDType

	type UserFlowLinksType
		integer UserID
		integer SourceDrainage
		integer DestinationDrainage
		integer TakeLinkID
		integer ReturnLinkID
		integer UsersType
	end type UserFlowLinksType

	type DrainageInfoType
		integer UserSrceReturn_ID
		integer User_ID
		integer UsersType
		integer SourceDrainageID
		integer ReturnLinkID
		integer DestinationDrainageID
		integer WWTP_ID
		integer RF_counter
		integer SourceID
		integer ReturnFlowID
	end type DrainageInfoType

	type StaticOutputTableType
		type (StreamFlowLinksType), allocatable :: StreamFlowLinks(:)
		type (UserFlowLinksType), allocatable :: UserFlowLinks(:)
		type (DrainageIDType), allocatable :: DrainageID(:)
		type (DrainageInfoType), allocatable :: DrainageInfo(:)
	end type StaticOutputTableType
