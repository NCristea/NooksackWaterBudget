subroutine PropagateWaterViaUser(i,j,Qtry,NumNode,NumLink,NumUser,	&
                    NumReservoir,NumSource,DrainageOrder,NumDrainage,			&
					NumReturnFlow,iFeasible,Capacity);
					
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	integer DrainageOrder(NumDrainage)
	real, allocatable :: DrainageOutFlow(:),ReservoirNetStorage(:)
	real, allocatable :: RFAmt(:)
	integer, allocatable :: RFUnits(:),RFType(:),RFLocn(:)
	integer SrcLocnID,RFNodeID

	allocate (DrainageOutFlow(NumDrainage),ReservoirNetStorage(NumReservoir))

!k is node of this user
k=User(i)%NodeNumber

!jj is link from source to user
jj=User(i)%LinkSourceToUser(j)
Link(jj)%Flow=Link(jj)%Flow+Qtry; !supply flow from source to user 

!j_sink= link from user node to sink node
j_sink=User(i)%LinkSourceToSink
Link(j_sink)%Flow=Link(j_sink)%Flow+Qtry; !consumption from user to sink

if (User(i)%ReturnFlowID .gt. 0) then
    call find1(ReturnFlow%ReturnFlowID,User(i)%ReturnFlowID,NumReturnFlow);
	j_r=ifound(1) !problem if nfound<>1
    NumReturnFlows=ReturnFlow(j_r)%NumReturnFlows;
	allocate (RFAmt(NumReturnFlows),RFUnits(NumReturnFlows),RFType(NumReturnFlows),RFLocn(NumReturnFlows))
    do m=1,NumReturnFlows
        RFAmt(m)=ReturnFlow(j_r)%ReturnFlowsAmt(m);
        RFUnits(m)=ReturnFlow(j_r)%ReturnFlowsUnits;
        RFType(m)=ReturnFlow(j_r)%ReturnFlowsType(m);
        if (RFType(m).eq.0) RFType(m)=StreamNodeCode
        RFLocn(m)=ReturnFlow(j_r)%ReturnFlowsLocn(m);
        if (RFLocn(m).eq.0) RFLocn(m)=User(i)%POU_ID
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
	allocate (RFAmt(NumReturnFlows),RFUnits(NumReturnFlows),RFType(NumReturnFlows),RFLocn(NumReturnFlows))
	RFAmt(1)=1; RFUnits(1)=1; RFType(1)=StreamNodeCode; RFLocn(1)=User(i)%POU_ID;
end if

do m=1,NumReturnFlows !length(User(i)%ReturnFlowFrac) !return flow(s) from user to return flow nodes
	if (RFUnits(m).eq.FractionUnits) then
	    RF = Qtry * RFAmt(m);
	else if (RFUnits(m).eq.VolumeUnits) then
		RF = RFAmt(m);
	else if (RFUnits(m).eq.FracMinDemandUnits) then
		write(6,*)'FracMinDemand not yet implemented'
	    RF = Qtry * RFAmt(m);
	end if
    !j_return= link from user node to returnflow node
	j_return=User(i)%LinkUserToReturnflow(m)
!    do ii=1,nfound
        Link(j_return)%Flow=Link(j_return)%Flow+RF;
!    end do
    Link(j_sink)%Flow=Link(j_sink)%Flow-RF;
end do

call BalanceFlowsAtReservoirs(NumNode,NumLink,			&
                     NumUser,NumReservoir,ReservoirNetStorage);

call BalanceFlowsAtStreamNodes(NumNode,NumLink,	&
									DrainageOrder,NumDrainage,DrainageOutFlow);
iFeasible = 1
do ii=1,NumDrainage 
	if (DrainageOutFlow(ii) .lt. 0) iFeasible=0
end do
do ii=1,NumReservoir 
	if (ReservoirNetStorage(ii) .lt. 0) iFeasible=0
end do
Capacity=min(minval(DrainageOutFlow),minval(ReservoirNetStorage));

return
end
