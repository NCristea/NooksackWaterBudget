subroutine BalanceFlowsAtStreamNodes(NumNode,NumLink,DrainageOrder,NumDrainage, &
					DrainageOutFlow)
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule

	integer DrainageOrder(NumDrainage)
	real DrainageOutFlow(NumDrainage)
	integer ifound_in(1000),ifound_taken(1000)

!Calculate Flow in all links 
!  Process each stream node in drainage calculation order (from upstream to downstream) assigning the flow to the outgoing 
!  unallocated link the sum of all inflows minus the sum of all withdrawals.

do ii=1,NumDrainage
    i=DrainageOrder(ii);

	isn=Drainage(i)%isn

	n_in=Drainage(i)%n_in;ifound_in=Drainage(i)%ifound_in(1:n_in);

	n_taken=Drainage(i)%n_taken;ifound_taken=Drainage(i)%ifound_taken(1:n_taken);

	nfound=1;j_unallocated=Drainage(i)%j_unallocated;

	flow_in=0
	do j=1,n_in; flow_in=flow_in+Link(ifound_in(j))%Flow; end do
	flow_taken=0
	do j=1,n_taken;flow_taken=flow_taken+Link(ifound_taken(j))%Flow; end do
	flow_unallocated=Link(j_unallocated)%Flow; 

    Link(j_unallocated)%Flow=flow_in-flow_taken;
    DrainageOutFlow(i)=Link(j_unallocated)%Flow;
end do
return
end    