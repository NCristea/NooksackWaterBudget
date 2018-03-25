!Modules.for
!
	module data_array
	real, allocatable :: real_array(:,:)
	integer, allocatable :: integer_array(:,:)
	real*8, allocatable :: dble_array(:,:)
	integer, parameter :: real_data=1, integer_data=2, dble_data=3
	end module data_array

	module TimeVaryingOutput
!	real, allocatable :: Precipitation_mm(:,:) !Timestep,Location
!	real, allocatable :: Evaporation_mm(:,:) !Timestep,Location
!	real, allocatable :: TotalRunoff_cms(:,:) !Timestep,Location
!	real, allocatable :: Baseflow_cms(:,:) !Timestep,Location
!	real, allocatable :: Artificial_Drainage(:,:) !Timestep,Location  ! DGT 6/28/05
	real, allocatable :: FlowInLinks_cms(:,:) !Timestep,Location
	real, allocatable :: FlowAtStreamNodes_cms(:) !Location   ! DGT 8/12/05 reduced to 1 dimension
	real, allocatable :: ReservoirStorage_m3(:,:) !Timestep,Location
	integer, allocatable :: DateTime_yyyymmdd_hhmmss(:,:) !Timestep, yyyymmdd/hhmmss
	end module TimeVaryingOutput

!Input_Structures
!
	module input_structures
	include 'types.f90'
	type(RunControlType) :: RunControl
	type(DrainageType),allocatable :: Drainage(:)
	type(StreamNodeType),allocatable :: StreamNode(:)
	type(MeasuredFlowInfoType),allocatable :: MeasuredFlowInfo(:)
	type(MeasuredFlowDataType),allocatable :: MeasuredFlowData(:)
	type(RunoffType),allocatable :: Runoff(:), Baseflow(:), ArtDrainage(:)  ! DGT 6/28/05   Added ArtDrainage 
	type(ReservoirType),allocatable :: Reservoir(:)
	type(UserType),allocatable :: User(:)
	type(SourceType),allocatable :: Source(:)
	type(RightsType),allocatable :: Rights(:)
	type(SourceMixingType),allocatable :: SourceMixing(:)
	type(SeasonsDefnType),allocatable :: SeasonsDefn(:)
	type(ReturnFlowType),allocatable :: ReturnFlow(:)
!	type(DemandCoefficientsType),allocatable :: DemandCoefficients(:)
	type(MonthlyDemandType),allocatable :: MonthlyDemand(:)
	end module input_structures

!Other_Structures
!
	module other_structures
	include 'types.f90'
	type(NodeType),allocatable	:: Node(:)
	type(LinkType),allocatable	:: Link(:)
	type(UserSourceTableType),allocatable :: UserSourceTable(:)
	type(StaticOutputTableType) :: StaticOutput
	type(NodeType),allocatable	:: NodeSave(:)
	type(LinkType),allocatable	:: LinkSave(:)
	integer, allocatable :: UserSourceOrder(:)
	integer, allocatable :: WWTP_list(:)
	end module other_structures

!Constant Definitions
!
	module constant_definitions
	include 'types.f90'  ! DGT 6/29/05  to get MaxNumTimesteps
	real, parameter :: BigReal=1e20
	integer, parameter :: MaxNtimeSteps=MaxNumTimesteps  ! DGT 6/29/05
!Define UseType Codes
!UserType Descriptions
!  DGT 9/1/05 redefined these codes for consistency
	integer, parameter :: SoilMoistureIrrigationUseCode=1
	integer, parameter :: FixedDemandIrrigationUseCode=2
	integer, parameter :: DownstreamReservoirReleaseUseCode=3 !RAW 31-Aug-2005
	integer, parameter :: PWSUseCode=4
	integer, parameter :: NonPWSMandIUseCode=5
	integer, parameter :: DairyUseCode=6
	integer, parameter :: RanchUseCode=7
	integer, parameter :: PoultryUseCode=8
	integer, parameter :: ParkGolfCemeteryUseCode=9
	integer, parameter :: InstreamFlowUseCode=10
	integer, parameter :: DiversionUseCode=11
!  The above are user specified.  The below are used internally for automatically created users to handle reservoirs
	integer, parameter :: ReservoirFillUseCode=12
	!integer, parameter :: ReservoirReleaseUseCode=13 RAW 29-Aug-2005 removed - need to discriminate type of reservoir
	integer, parameter :: InStreamReservoirReleaseUseCode=13 !RAW 29-Aug-2005
	integer, parameter :: OffStreamReservoirReleaseUseCode=14 !RAW 29-Aug-2005

!Define SourceType Codes
	integer, parameter :: StreamSourceCode=1
	integer, parameter :: GroundwaterSourceCode=2
	integer, parameter :: ReservoirSourceCode=3

!Define ReservoirType Codes
	integer, parameter :: InStreamReservoirCode=1
	integer, parameter :: OffStreamReservoirCode=2

!Define Internal/External Codes
	integer, parameter :: ExternalCode=1
	integer, parameter :: InternalCode=2

!Define NodeType Codes
	integer, parameter :: StreamNodeCode=1 !Drainage Outlets
	integer, parameter :: GroundwaterNodeCode=2
	integer, parameter :: ReservoirNodeCode=3
	integer, parameter :: DrainageNodeCode=4
	integer, parameter :: UserNodeCode=5
	integer, parameter :: SinkNodeCode=6
	integer, parameter :: MeasuredFlowNodeCode=7

!Define LinkType Nodes
	integer, parameter :: SurfaceRunoffLinkCode=1
	integer, parameter :: SubsurfaceRunoffLinkCode=2
	integer, parameter :: UnallocatedLinkCode=3    ! This is used to designate the link at each node that absorbs 
	! the extra flow when flow at nodes is balanced.  This is always the link from a stream node to the next downstream node.
	integer, parameter :: UserAbstractionLinkCode=5
	integer, parameter :: ReturnFlowLinkCode=6
	integer, parameter :: SinkLinkCode=7
	integer, parameter :: MeasuredFlowLinkCode=8

!Define AllocationMode Types
	integer, parameter :: WaterRightsAllocationCode=1
	integer, parameter :: DemandAllocationCode=2
	integer, parameter :: NoAllocationCode=0

!Define StreamNode Types
	integer, parameter :: IsDrainageOutlet=1
	integer, parameter :: IsNotDrainageOutlet=0

!Define Units Types
	integer, parameter :: FractionUnits=1
	integer, parameter :: VolumeUnits=2
	integer, parameter :: FracMinDemandUnits=3

!Define PurposeList Code
!PurposeUnknown=0;PurposeCI=1;PurposeDG=2;PurposeDI=3;PurposeDM=4;PurposeDS=5;PurposeDY=6;PurposeEN=7;
!PurposeFP=8;PurposeFR=9;PurposeFS=10;PurposeHE=11;PurposeHT=12;PurposeHW=13;PurposeIR=14;
!PurposeMI=15;PurposeMU=16;PurposeOT=17;PurposePO=18;PurposeRE=19;PurposeRB=20;PurposeRW=21;PurposeST=22;PurposeWL=23;PurposeRF=24;
	end module constant_definitions

	module findmodule
	save
	integer, allocatable :: ifound(:)
	integer nfound
	end module findmodule