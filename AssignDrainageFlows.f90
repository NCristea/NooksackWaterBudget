subroutine AssignDrainageFlows(Timestep,NumDrainage,NumNode,NumLink, &
							DrainageOrder,NumRunoff,NumBaseflow,DrainageOutFlow)

	use constant_definitions
	use input_structures
	use other_structures
	use findmodule

	integer DrainageOrder(NumDrainage),Timestep
	real DrainageOutFlow(NumDrainage)

!Link(k).Flow=Drainage(1:NumDrainage).Flow
!for each drainage, there is a runoff amount.
!find the link which connects the drainage node to its stream node
!assign the runoff to that link
do i=1,NumDrainage
    call find2(Node%Type,StreamNodeCode,Node%DrainageID,i,NumNode)
	isn=ifound(1) !problem if nfound<>1
    call find2(Link%LinkCode,SurfaceRunoffLinkCode,Link%DSNode,isn,NumLink)
	k=ifound(1) !problem if nfound<>1
    Link(k)%Flow=Runoff(Timestep)%Rate(Drainage(i)%DrainageID) - &
		Baseflow(Timestep)%Rate(Drainage(i)%DrainageID)
    call find2(Link%LinkCode,SubsurfaceRunoffLinkCode,Link%DSNode,isn,NumLink)
	k=ifound(1) !problem if nfound<>1
    Link(k)%Flow=Baseflow(Timestep)%Rate(Drainage(i)%DrainageID)
end do

call BalanceFlowsAtStreamNodes(NumNode,NumLink,DrainageOrder,NumDrainage, &
					DrainageOutFlow)
return
end
