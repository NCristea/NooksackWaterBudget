subroutine BalanceFlowsAtReservoirs(NumNode,NumLink,			&
                     NumUser,NumReservoir,ReservoirNetStorage);
			 
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	real ReservoirNetStorage(NumReservoir)

	integer j_in(1000)
	integer, allocatable :: j_out(:)

!Calculate storage in all reservoir nodes, and if there is overflow, update
!flows in outflow link as required

do i=1,NumReservoir
!    call find2(Node%Type,ReservoirNodeCode,Node%SelfID,i,NumNode);
!	isn=ifound(1) !problem if nfound<>1
!    call find1(Link%DSNode,isn,NumLink); allocate (j_in(nfound)); 
!	j_in=ifound; nj_in=nfound
!    call find1(Link%USNode,isn,NumLink); allocate (j_out(nfound)); 
!	j_out=ifound; nj_out=nfound

	isn=Reservoir(i)%isn
	nj_in=Reservoir(i)%n_in;j_in=Reservoir(i)%ifound_in(1:nj_in)
	nj_out=Reservoir(i)%n_taken;
	if (allocated(j_out)) deallocate(j_out); allocate(j_out(nj_out))
	j_out=Reservoir(i)%ifound_taken(1:nj_out)

    !find the link going to the user node of Type=ReservoirReleaseUseCode that takes reservoir overflows from this reservoir
	if (Reservoir(i)%InOffStream.eq.InStreamReservoirCode) then
		call find1(User(Node(Link(j_out)%DSNode)%SelfID)%UsersType,InStreamReservoirReleaseUseCode,NumUser)
	elseif (Reservoir(i)%InOffStream.eq.OffStreamReservoirCode) then
		call find1(User(Node(Link(j_out)%DSNode)%SelfID)%UsersType,OffStreamReservoirReleaseUseCode,NumUser)
	endif
	j_release=j_out(ifound(1)) !problem if nfound<>1
	call find1(j_out,j_release,nj_out) !problem if nfound<>1
	j_found=ifound(1)
    call remove_element(j_out,nj_out,j_found)
    flow_in=0; do k=1,nj_in; flow_in=flow_in+Link(j_in(k))%Flow; end do
    flow_taken=0; do k=1,nj_out; flow_taken=flow_taken+Link(j_out(k))%Flow; end do
    flow_released=Link(j_release)%Flow

    Node(isn)%Store=Node(isn)%StoreOld+flow_in-flow_taken-flow_released;
    if (Node(isn)%Store .gt. Reservoir(i)%StoreMax) then
        Link(j_release)%Flow=Link(j_release)%Flow+Node(isn)%Store-Reservoir(i)%StoreMax;
        Node(isn)%Store=Reservoir(i)%StoreMax;
        iuser=Link(j_release)%DSNode;  ! This is the user identifier of the dummy user that effects reservoir release
		! Need to find the return flow link associated with this user so that the corresponding return flow can also be
		! adjusted by the additional flow in this link.
        call find2(Link%LinkCode,ReturnFlowLinkCode,Link%USNode,iuser,NumLink);
		jret=ifound(1) !problem if nfound<>1
        Link(jret)%Flow=Link(jret)%Flow+Link(j_release)%Flow; !no consumption at this usernode
		! Logically the line above would be better implemented by 
		!  Link(jret)%Flow=Link(j_release)%Flow
		!  Did not do this because DGT did not want to change code and Ross says will not change result.
    end if
    ReservoirNetStorage(i)=Node(isn)%Store-Reservoir(i)%StoreMin;
end do
return
end

subroutine remove_element(j_out,nj_out,ifound)
integer j_out(nj_out)
do i=ifound,nj_out-1
	j_out(i)=j_out(i+1)
end do
nj_out=nj_out-1
return
end

