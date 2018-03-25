subroutine ImposeMeasuredFlows(Timestep,NumNode,NumLink,NumRunoff,NumBaseflow, &
			NumDrainage,NumMeasuredFlowInfo,NumMeasuredFlowData,DrainageOrder,DrainageOutFlow)

	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	real DrainageOutFlow(NumDrainage),NodeOutFlow
	integer Timestep,DrainageOrder(NumDrainage)

! Impose any external measured flows by
! identifying the node with NodeType =  3 – STREAM (whose ID is NODE-ID) at which the flow was measured, 
! noting the ratio of measured to modelled flow at this node, and then
! applying that ratio to Runoff and Baseflow rates for this timestep in any
! drainage which is upstream of this node (including the drainage the node lies in)

!this is different to the previous method i proposed, which just fiddled
!the flow from the measuredflownode to this node until the total inflow to
!this node was as measured
! I changed methods so that we could get the runoff from all catchments set
! properly, because the water quality folks need runoff&baseflow from ALL drainages

do i=1,NumMeasuredFlowInfo
    call find2(Node%Type,StreamNodeCode,Node%DrainageID,MeasuredFlowInfo(i)%DrainageID,NumNode);
	k=ifound(1) !problem if nfound<>1  !warning if nfound=0
    call find1(Link%USNode,k,NumLink);
    NodeOutFlow=0; 
	do j=1,nfound; 
		j_out=ifound(j)
		NodeOutFlow=NodeOutFlow+Link(j_out)%Flow; 
	end do
!  6/10/05 DGT added the check for NodeOutFlow Greater than 0 to avoid infinity and Nan in the results
	if (MeasuredFlowData(Timestep)%Flow(MeasuredFlowInfo(i)%ColInMeasFlow).ge.0 .and. NodeOutFlow .gt. 0.) then
		FlowRatio = MeasuredFlowInfo(i)%ScalingFactor * &
			MeasuredFlowData(Timestep)%Flow(MeasuredFlowInfo(i)%ColInMeasFlow) / NodeOutFlow;
	else
		FlowRatio=1.
	end if
    call ScaleUpstreamRunoff(FlowRatio,k,Timestep,NumNode,NumRunoff,NumBaseflow,NumDrainage);
end do

!now use these new runoffs to calculate flows throughout the network 
call AssignDrainageFlows(Timestep,NumDrainage,NumNode,NumLink, &
							DrainageOrder,NumRunoff,NumBaseflow,DrainageOutFlow)

return 
end

subroutine  ScaleUpstreamRunoff(FlowRatio,k,Timestep,NumNode,NumRunoff,NumBaseflow,NumDrainage);

use constant_definitions
use input_structures
use other_structures
use findmodule
integer i(10),kk(10),Timestep
integer,allocatable :: nodelist(:)
allocate (nodelist(NumNode))

nodelist(1)=k; list_length=1;number_processed=0;
do while (number_processed .lt. list_length)
    j_node=nodelist(number_processed+1)
    j=Drainage(Node(j_node)%DrainageID)%DrainageID
    Runoff(Timestep)%Rate(j)=Runoff(Timestep)%Rate(j)*FlowRatio
    Baseflow(Timestep)%Rate(j)=Baseflow(Timestep)%Rate(j)*FlowRatio
    number_processed=number_processed+1
    call find1(Drainage%DSDrainage,Node(j_node)%DrainageID,NumDrainage) !Drainage(i) is  just upstream of Node(k)
	if (nfound.gt.0) then
		do m=1,nfound; i(m)=ifound(m); end do 
		ni=nfound
		nk=0
		do ii=1,ni !kk will contain list of node indices 
			call find2(Node%Type,StreamNodeCode,Node%DrainageID,Drainage(i(ii))%DrainageID,NumNode)
			kk(ii)=ifound(1)
			nk=nk+nfound
		end do
		do j=1,nk
			nodelist(list_length+j)=kk(j)
		end do 
		list_length=list_length+nk;
	end if
end do
return
end
