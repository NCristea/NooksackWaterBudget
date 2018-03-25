subroutine AllocateWaterToUsers(Timestep,NumNode,NumLink, &
			NumUser,NumReservoir,NumSource,NumRights,NumSourceMixing, &
			DrainageOrder,NumDrainage,NumReturnFlow,NumUserSource, &
			volume_irrig_sup,groundwater_to_take,DrainageOutflow);

use constant_definitions
use input_structures
use other_structures
use findmodule

integer DrainageOrder(NumDrainage),Timestep, i_count
real DrainageOutFlow(NumDrainage)

real*8 volume_irrig_sup(*),groundwater_to_take(*)

!a user may need a right to take water from a particular source
!so we process the user-source combinations in order of priority
!this may mean that for example we allocate some water to one of a user's sources today, 
!but then the next water from that source gets allocated to another user. We might
!then later come back to allocate water to other sources which the same user has. 
!It is the user-source combinations that are ordered 
!(either by priority date or by position in the river system), not the users.

do i=1,NumUser
    User(i)%Deficit(Timestep)=User(i)%DemandToday;
    User(i)%Demand(Timestep)=User(i)%DemandToday;
    User(i)%Withdrawal(Timestep)=0;
end do

do ii=1,NumUserSource
	k=UserSourceOrder(ii)
	i=UserSourceTable(k)%UserID;
	if (User(i)%UsersType.eq.InStreamReservoirReleaseUseCode .or. &
			User(i)%UsersType.eq.OffStreamReservoirReleaseUseCode) then
		!  Reservoir release is spill due to fill and spill operation rule.  
		!  The demand associated with a reservoir release user is the storage in excess of capacity
		ilink=User(i)%LinkSourceToUser(1)
		inode=Link(ilink)%USNode
		User(i)%DemandToday=max(Node(inode)%Store-Node(inode)%StoreMax,User(i)%Demand(Timestep)) !RAW 31-Aug-2005
		User(i)%Deficit(Timestep)=User(i)%DemandToday;User(i)%Demand(Timestep)=User(i)%DemandToday;User(i)%Withdrawal(Timestep)=0;
	elseif (User(i)%UsersType.eq.ReservoirFillUseCode) then
	     ! Reservoir fill user requests difference between capacity and storage
		ilink=User(i)%LinkUserToReturnflow(1)
		inode=Link(ilink)%DSNode
		User(i)%DemandToday=max(Node(inode)%StoreMax-Node(inode)%Store,0.)
		User(i)%Deficit(Timestep)=User(i)%DemandToday;User(i)%Demand(Timestep)=User(i)%DemandToday;User(i)%Withdrawal(Timestep)=0;
	end if
    if (User(i)%DemandToday.gt.0) then
        j=UserSourceTable(k)%SourceCounter;
        call find1(Source%SourceID,User(i)%SourceID(j),NumSource);  !User(i)%source_ind(1)=j_source
		j_source=ifound(1) !problem if nfound<>1
        if (User(i)%SourceMixingID.eq.0 .or. User(i)%NumSources.eq.1) then
            DemandByUserOnSource=User(i)%DemandToday;
        else
			j_srcmx=User(i)%j_srcmx(j)
            if (SourceMixing(j_srcmx)%Units .eq. FractionUnits) then
                DemandByUserOnSource=User(i)%DemandToday * SourceMixing(j_srcmx)%Amount;
            else
                DemandByUserOnSource=min( User(i)%DemandToday, SourceMixing(j_srcmx)%Amount );
            end if !SourceMixing(j_srcmx)%Units .eq. FractionUnits
        end if !User(i)%SourceMixingID.eq.0 .or. User(i)%NumSources.eq.1
        call TrialFlow(DemandByUserOnSource,i,j,j_source,NumNode,NumLink, &
                                NumUser,NumSource,NumRights,Qtry0);
        Qtry=Qtry0;Capacity=Qtry  ! 10/5/05 Ross and Dave think that Capacity does not need to be initialized here
        AllocatedFlow=0;i_count=1;LinkSave=Link;NodeSave=Node;   ! This is a potential time consuming operation that could be made more efficient
		i_count_max=100
        do while (Qtry .gt. 0.01*Qtry0 .and. AllocatedFlow.eq.0 .and. i_count.le.i_count_max)
            !give Qtry to the user node and follow it through
			call PropagateWaterViaUser(i,j,Qtry,NumNode,NumLink,NumUser,	&
                NumReservoir,NumSource,DrainageOrder,NumDrainage,		&
				NumReturnFlow,iFeasible,Capacity)                !take Qtry from the source, and follow that through
!            select case (Source(j_source)%Type)
!                case (StreamSourceCode)
!                    call PropagateTakeFromStream(i,j,NumNode, &
!						NumLink,DrainageOrder,NumDrainage,iFeasible,Capacity);
!                case (ReservoirSourceCode,GroundwaterSourceCode)
!                    call PropagateTakeFromStore(Qtry,i,j,NumNode,NumLink, &
!                        NumUser,NumSource,iFeasible,Capacity);
!            end select 
            if (iFeasible.ne.0) then !true: this flow is feasible
                AllocatedFlow=Qtry;
            else
                !Qtry=Qtry*.99;
                Qtry=max(0.,min(Qtry+Capacity,Qtry0*(1.0-float(i_count)/float(i_count_max))));
                Link=LinkSave;Node=NodeSave;
				i_count=i_count+1;
            end if !iFeasible.eq.1
        end do !while.  Here we are done iterating.  Assignments below are permanent
		if (i_count.gt.i_count_max .and. Qtry.gt.0) then
			write(6,*)ii,i,j,i_count,Qtry0,Qtry/Qtry0
		end if
        User(i)%Deficit(Timestep)= max(0.,User(i)%Deficit(Timestep)-Qtry);
		User(i)%Withdrawal(Timestep)= User(i)%Withdrawal(Timestep)+Qtry;
		User(i)%VolumeToDateSource(j) = Qtry + User(i)%VolumeToDateSource(j);  ! DGT 9/3/05 
        call BalanceFlowsAtStreamNodes(NumNode,NumLink,DrainageOrder,NumDrainage, &
			DrainageOutFlow)
	!for passing back to Topnet  ! DGT 8/16/05.  Type -1 left for compatibility with old inputs
	!  type 7 is irrigation with demand calculated based on soil moisture
	!  type 8 is irrigation with demand specified by user files 
!		if (User(i)%UsersType.eq.-1 .or. User(i)%UsersType.eq.7 .or. User(i)%UsersType.eq.8) then
		if (User(i)%UsersType.eq.SoilMoistureIrrigationUseCode .or. User(i)%UsersType .eq. FixedDemandIrrigationUseCode) then
			volume_irrig_sup(User(i)%POU_ID) = Qtry+volume_irrig_sup(User(i)%POU_ID);
		end if !User(i)%UsersType.eq.-1
		if (Source(j_source)%Type .eq. GroundwaterSourceCode) then
			groundwater_to_take(User(i)%POU_ID) = Qtry+groundwater_to_take(User(i)%POU_ID);
		end if !Source(j_source)%Type .eq. GroundwaterSourceCode
	end if ! User(i)%DemandToday.gt.0
end do !ii
! 4. For each user USER__-ID, in descending order of priority
! For each source of that user, in no particular order
! Note the SOURCENODE-ID of the source node for this user-source combination, and find the link LINK-ID with LinkType = 4 – ABSTRCT and LinkUSNode=SOURCENODE-ID and LinkDSNode=USER__-ID
! Note the RETURNNODE-ID of the return node for this user-source combination, and find the link RETURNLINK-ID with LinkType = 5 – RETURN and LinkUSNode=USER__-ID and LinkDSNode=RETURNNODE-ID
! Calculate Qtry = min(demand - Flow in link LINK-ID, daily_max_right, annual_max_take-take_to_date)
! Determine the amount of returnflow to RETURNNODE-ID for Qtry
! Determine whether it is feasible to allocate Qtry to user from source: (if Qtry is small skip to 2.4.1.6)
! increase Flow in the link LINK-ID by Qtry
! increase Flow in the link RETURNLINK-ID by returnflow
! increase Flow in the link SINKNODE-ID by Qtry - returnflow
! if the source node has max-min>0 then 
! if the node storage stays in its bounds, reduce the flow stored at node SOURCENODE-ID by Qtry and this allocation is feasible, else it is not feasible 
! elseif the sourcenode has no storage then recalculate Flow in all links of LinkType = 2 – UNALLC, by adjusting the unallocated flow at every node THISNODE-ID so that there is water balance at the node. Inflow is Sum of Flow in all links with LinkDSNode=THISNODE-ID.   AllocatedOutflow is Sum of Flow in all links with LinkUSNode=THISNODE-ID and LinkType<>2 – UNALLC.  Flow in LinkType=2 – UNALLC is Inflow- AllocatedOutflow. Process the nodes from upstream to downstream. If all unallocated flows are non-negative then this allocation of Qtry is feasible otherwise it is not feasible.
! If the attempted allocation of Qtry was not feasible then subtract recently-added Flow in links LINK-ID RETURNLINK-ID and SINKNODE-ID, else if it was feasible then update the users stats for the year to date
! if Qtry is not too small, reduce Qtry and return to step 2.4.1.5, otherwise set Qtry=0
! We’ve now allocated as much water as possible to this user from this source
! Go on to the next source for this user
! Go on to the next user
! Might need to embed another loop in here so that we process users in priority order 12123123412345…, rather than 12345… 
! For MI=1 to n; For J=1 to M, do steps 2.4.1 and 2.4.2 with user J; next J; next M

return
end

subroutine TrialFlow(DemandByUserOnSource,i,j,j_source,NumNode,NumLink,&
                            NumUser,NumSource,NumRights,Qtry);
use constant_definitions
use input_structures
use other_structures
use findmodule

!find a trial flow which tries to meet demand subject to Daily and Annual Water Right Limits
ianypositive=0; 
do ii=1,User(i)%NumSources; 
	if (User(i)%RightID(ii) .gt.0) then
		ianypositive=1
	end if
end do

if (NumRights .gt. 0 .and. ianypositive .eq. 1) then
	call find1(Rights%RightID,User(i)%RightID(j),NumRights);
	j_rights=ifound(1) !problem if nfound<>1
	xLegalDailyMax=Rights(j_rights)%LegalDailyMax;
	xLegalAnnMax=Rights(j_rights)%LegalAnnMax;
else
    xLegalDailyMax=1e20;xLegalAnnMax=1e20;
end if
Qmin_inst=min(xLegalDailyMax,Source(j_source)%PhysicalDailyMax);
Qmin_ann=min(xLegalAnnMax,Source(j_source)%PhysicalAnnMax) - User(i)%VolumeToDateSource(j);
Qtry = min(Qmin_inst,Qmin_ann,DemandByUserOnSource);
return
end
