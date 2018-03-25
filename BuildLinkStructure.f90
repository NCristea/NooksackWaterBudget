	subroutine BuildLinkStructure(NumDrainage,NumUser,NumSource,	&
		NumReturnFlow,NumReservoir,NumMeasuredFlowInfo,NumNode,NumLink)
	!inputs
	!Drainage, NumDrainage, User, NumUser, Source, NumSource, 
	!ReturnFlow,NumReturnFlow, Reservoir, NumReservoir,
      !MeasuredFlowInfo, NumMeasuredFlowInfo, Node, NumNode,
	
	!outputs: 
	!Link,NumLink

!constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule

	integer USNode, DSNode, SrcLocnID,RFLocn,RFType
	character*6 Title

	NumLink=0
	!Link=[]
! Connect drainage nodes and stream nodes
	do i=1,NumDrainage
		!two links for each drainagenode-streamnode connection, one to carry surface runoff and one for baseflow
		call find2(Node%Type,DrainageNodeCode,Node%DrainageID,i,NumNode)
		idn=ifound(1) !problem if nfound<>1
		call find2(Node%Type,StreamNodeCode,Node%DrainageID,i,NumNode)
		isn=ifound(1) !problem if nfound<>1
		call CreateLink(NumLink,'SRFLOW',SurfaceRunoffLinkCode,ExternalCode,idn,isn,0.,0)

		call find2(Node%Type,GroundwaterNodeCode,Node%DrainageID,i,NumNode)
		ign=ifound(1) !problem if nfound<>1
		call CreateLink(NumLink,'SBFLOW',SubsurfaceRunoffLinkCode,ExternalCode,ign,isn,0.,0)
    
		!one link for each streamnode-downstreamnode connection (including sink)
		Title='UNALLC'
		iType=UnallocatedLinkCode
		if (Drainage(i)%DSDrainage.ge.1) then
			IntExtCode=InternalCode
			call find2(Node%Type,StreamNodeCode,Node%DrainageID,Drainage(i)%DSDrainage, &
							NumNode)
			DSNode=ifound(1) !problem if nfound<>1
		else
			IntExtCode=ExternalCode
			call find1(Node%Type,SinkNodeCode,NumNode)
			DSNode=ifound(1) !problem if nfound<>1
		end if
		call CreateLink(NumLink,'UNALLC',iType,InternalCode,isn,DSNode,0.,0)
	end do

!Connect sources of water to users
	do i=1,NumUser
		do j=1,User(i)%NumSources !one link for each sourcenode-usernode connection
			call find1(Source%SourceID,User(i)%SourceID(j),NumSource)
			k=ifound(1) !problem if nfound<>1
			SrcLocnID=Source(k)%SourceLocationID
			if (SrcLocnID.eq.0) SrcLocnID=User(i)%POU_ID
			select case (Source(k)%Type)
			case (StreamSourceCode)
				call find2(Node%Type,StreamNodeCode,Node%SelfID,SrcLocnID,NumNode)
				USNode=ifound(1) !problem if nfound<>1
			case (GroundwaterSourceCode)
				call find2(Node%Type,GroundwaterNodeCode,Node%SelfID,SrcLocnID,NumNode)
				USNode=ifound(1) !problem if nfound<>1
			case (ReservoirSourceCode)
				call find2(Node%Type,ReservoirNodeCode,Node%SelfID,SrcLocnID, NumNode)
				USNode=ifound(1) !problem if nfound<>1
			end select
			call find2(Node%Type,UserNodeCode,Node%SelfID,i,NumNode)
			DSNode=ifound(1) !problem if nfound<>1
			call CreateLink(NumLink,'USERAB',UserAbstractionLinkCode,InternalCode,USNode,DSNode,0.,0)
		end do
	end do !making links from sources to user nodes

!one link for each return flow pathway, from the user node to the node where return flows go for this use
	call find1(Node%Type,SinkNodeCode,NumNode)
	isink=ifound(1)

	do i=1,NumUser
		if (User(i)%ReturnFlowID.gt.0) then
			call find1(ReturnFlow%ReturnFlowID,User(i)%ReturnFlowID,NumReturnFlow)
			j_ret=ifound(1) !problem if nfound<>1
			NumReturnFlows=ReturnFlow(j_ret)%NumReturnFlows
			do m=1,NumReturnFlows
				call find2(Node%Type,UserNodeCode,Node%SelfID,i,NumNode)
				USNode=ifound(1) !problem if nfound<>1
				RFLocn=ReturnFlow(j_ret)%ReturnFlowsLocn(m)
			! DGT 9/3/05  Added 3 lines below to define RFLocn for InstreamFlowUseCode where RFLocn is 0.  This is to allow 
			! multiple instreamflow uses to reference the same record in returnflow.txt and not require the downstream drainage
			! to be specified.  This was an attempt to overcome the problem with the select case below, but does not work.
			! For now users are required to specify return flow location in returnflow.txt (and get it right) 
				if (RFLocn==0 .and. User(i)%UsersType == InstreamFlowUseCode) then
				  RFLocn=Drainage(User(i)%POU_ID)%DSDRAINAGE   ! This is the downstream drainge
				endif
				if (RFLocn==0) RFLocn=User(i)%POU_ID
				RFType=ReturnFlow(j_ret)%ReturnFlowsType(m)
				if (RFType==0) RFType=StreamNodeCode
				if (RFLocn>0) then
					call find2(Node%Type,RFType,Node%SelfID,RFLocn,NumNode)
					j=ifound(1) !problem if nfound<>1
				else
					j=isink
				end if
!   DGT 9/3/05 abandoned this select case structure.  j is an index for nodes, not drainages so the downstream
!   drainage link is incorrect.  This is handled above.  The user can also specify the RFLocn.
!				select case (User(i)%UsersType) !DSNode depends on the type of use
!				case (InstreamFlowUseCode)
!					DSNode=Drainage(j)%DSDrainage      ! for instream flow give drainageID d/s of source
!				case default
					DSNode=j;                       
!				end select
				call CreateLink(NumLink,'RETURN',ReturnFlowLinkCode,InternalCode, &
							USNode,DSNode,0.,m)
			end do
		end if
	end do !making return flow links from sources to nodes

!Measured Flow    
	do i=1,NumMeasuredFlowInfo !one link for each measured_inflow-streamnode connection
	!find the MeasuredFlow node
		call find2(Node%Type,MeasuredFlowNodeCode,					&
     		Node%DrainageID,MeasuredFlowInfo(i)%DrainageID,NumNode)
		USNode=ifound(1) !problem if nfound<>1
		call find2(Node%Type,StreamNodeCode,							&
     		Node%DrainageID,MeasuredFlowInfo(i)%DrainageID,NumNode)
		DSNode=ifound(1) !problem if nfound<>1
		call CreateLink(NumLink,'MEASIN',MeasuredFlowLinkCode,				&
     					ExternalCode,USNode,DSNode,0.,0)
	end do

!one link for consumptive use, from each source to the sink node
	do i=1,NumUser
		call find2(Node%Type,UserNodeCode,Node%SelfID,i,NumNode)
		USNode=ifound(1) !problem if nfound<>1
		call find1(Node%Type,SinkNodeCode,NumNode)
		DSNode=ifound(1) !problem if nfound<>1
		call CreateLink(NumLink,'WASTE_',SinkLinkCode,ExternalCode,USNode,DSNode,0.,0)
	end do !making links from sources to sink

	return
	end

	subroutine CreateLink(NumLink,Title,LinkCode,IntExtCode,			&
     				USNode,DSNode,Flow,ReturnFlowID)
	!outputs Link,NumLink
	use constant_definitions
	use input_structures
	use other_structures

	integer NumLink
	character*6 Title
	integer LinkCode,IntExtCode,USNode,DSNode,ReturnFlowID
	real Flow
	integer k

	NumLink=NumLink+1
	k=NumLink
	Link(k)%Title=Title
	Link(k)%LinkCode=LinkCode
	Link(k)%IntExtCode=IntExtCode
	Link(k)%USNode=USNode
	Link(k)%DSNode=DSNode
	Link(k)%Flow=Flow
	Link(k)%ReturnFlowID=ReturnFlowID !only used by ReturnFlow links
	return
	end