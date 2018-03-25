subroutine Append_To_Output_Tables(Timestep,dt,DrainageOutflow,NumDrainage,NumStreamNode,yyyymmdd,hhmmss, &
										precipitation,evaporation,NumLink,NumNode,Nsteps,NumUser);
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	use TimeVaryingOutput
	integer Timestep,yyyymmdd,hhmmss,dt,NumDrainage,NumLink,NumNode,NumStreamNode,NumUser
	integer ius,ids,i,method,isn,n_in,j,fid
	real flow_in,flow_notrunoff
	real DrainageOutFlow(NumDrainage)
	integer,allocatable :: ifound_in(:)
	real*8 evaporation(*),precipitation(*)
	character*12, allocatable :: headers(:)
	character*128 filenm, LocationTypeString
	character*20 fmtstr
	character*32767 str


! DGT 6/29/05   Not using these arrays to be more efficient
!do i=1,NumDrainage; 
!	TotalRunoff_cms(Timestep,i)=Runoff(Timestep)%Rate(i)/float(dt); !m3/s = m3/step / (s/step)
!	Baseflow_cms(Timestep,i)=Baseflow(Timestep)%Rate(i)/float(dt); !m3/s = m3/step / (s/step)
!	Artificial_Drainage(Timestep,i)=ArtDrainage(Timestep)%Rate(i)/float(dt);  ! DGT 6/28/05
!	Precipitation_mm(Timestep,i)=precipitation(i);  ! DGT 6/29/05 comment out
!	Evaporation_mm(Timestep,i)=evaporation(i);   ! DGT 6/29/05 comment out
!end do

DateTime_yyyymmdd_hhmmss(Timestep,1)=yyyymmdd !Timestep	Date	Time
DateTime_yyyymmdd_hhmmss(Timestep,2)=hhmmss !Timestep	Date	Time

do i=1,NumLink; 
	FlowInLinks_cms(Timestep,i)=Link(i)%Flow/float(dt); !m3/s = m3/step / (s/step)
end do

! DGT 8/21/05  considered writing outputs as we go here to avoid having to save all the time steps
! but FlowInLinks is used in a post processing calculation of WWTP outflows in Write_Output_tables
! so can not do this without restructuring that.

call find1(Node%Type,ReservoirNodeCode,NumNode);
do i=1,nfound; 
    ReservoirStorage_m3(Timestep,Node(ifound(i))%SelfID)= Node(ifound(i))%Store; 
end do

!Flow through stream nodes
!first the drainage outlets

!old bad routing method
!do i=1,NumDrainage
!	call find2(Node%Type,StreamNodeCode,Node%DrainageID,i,NumNode)
!	isn=ifound(1)	!problem if nfound<>1
 !   call find1(Link%DSNode,isn,NumLink)
!	if (allocated (ifound_in)) deallocate(ifound_in); allocate(ifound_in(nfound))
!	ifound_in=ifound; n_in=nfound
!	flow_in=0
!	do j=1,n_in; flow_in=flow_in+Link(ifound_in(j))%Flow; end do
!	flow_notrunoff=flow_in-Runoff(Timestep)%Rate(i)

!	call find1(StreamNode%RealDrainageID,Drainage(i)%RealDrainageID,NumStreamNode)
!	do j=1,nfound
!		FlowAtStreamNodes_cms(Timestep,ifound(j))=	&
!			(StreamNode(ifound(j))%FracRunoff*Runoff(Timestep)%Rate(i) + flow_notrunoff)/float(dt)
!	end do
!end do

!new routing method

!there are two components to flow at any node
!1. the local runoff - use FracRunoff (see read_inputs) to calculate this (we do not route this water from node to node)
!2. flow(s) from drainages which flow directly into the drainage containing this node, and whose flowpaths do pass through this node

FlowAtStreamNodes_cms(:)=0
do i=1,NumStreamNode
!  if(i.eq.107)then
!  FlowAtStreamNodes_cms(1)=FlowAtStreamNodes_cms(1)
!  endif
	if (StreamNode(i)%DOutFlag.eq.1) then !we need to propagate  this flow to any internal stream nodes
! DGT 8/21/05 replaced DrainageOutflow by Link%Flow because DrainageOutflow is lacking management 
! effects like diversions
        ThisDrainageOutflow=Link(StreamNode(i)%DrainageID*3)%Flow/float(dt)  ! DGT 8/21/05 Rely on the fact that LinkID is 3*DrainageID
		!RAW 29-Aug-2005 look for user that is InStreamReservoirReleaseUseCode 
		!This is the user that sends spill flow and environmental releases to the downstream drainage
		!To make this code faster, do this find2 ahead of time in the first call to watermgmt, and store the results
		!or check for the existence of an instream reservoir located in this drainage
		call find2(User%UsersType,InStreamReservoirReleaseUseCode,User%POU_ID,StreamNode(i)%DrainageID,NumUser)
		if (nfound.eq.1) then
			ThisDrainageOutflow=ThisDrainageOutflow+User(ifound(1))%Withdrawal(Timestep)/float(dt)
		end if
		call find2(User%UsersType,DownStreamReservoirReleaseUseCode,User%POU_ID,StreamNode(i)%DrainageID,NumUser)
		do j=1,nfound
			ThisDrainageOutflow=ThisDrainageOutflow+User(ifound(j))%Withdrawal(Timestep)/float(dt)
		end do
! DGT 9/3/05  Code to also append Instreamflowuse
		call find2(User%UsersType,InStreamFlowUseCode,User%POU_ID,StreamNode(i)%DrainageID,NumUser)
		do j=1,nfound
			ThisDrainageOutflow=ThisDrainageOutflow+User(ifound(j))%Withdrawal(Timestep)/float(dt)
		end do

!		FlowAtStreamNodes_cms(i)=DrainageOutflow(StreamNode(i)%DrainageID)/float(dt)
		FlowAtStreamNodes_cms(i)=ThisDrainageOutflow
		!now propagate this flow thru stream nodes down to next drainage outlet or end of network
		ius=i
!		call find1(StreamNode%NodeId,StreamNode(ius)%DownNodeId,NumStreamNode) !would be more efficient to do this find back in watermgmt
!		ids=ifound(1) !now start propagating downstream until we reach next drainage outlet, or end of network
        ids=StreamNode(ius)%DownNodeId  ! DGT 8/15/05  - find not necessary.  Rely on node numbers in nodelinks.txt being a counting sequence starting at 1
		if(ids .ge. 1)then  ! DGT 5/27/12 - avoid addressing error
		  do while (StreamNode(ius)%DownNodeId .ne. -1 .and. StreamNode(ids)%DOutFlag.ne.1)
			FlowAtStreamNodes_cms(ids)=FlowAtStreamNodes_cms(ids)+ ThisDrainageOutflow
!											DrainageOutflow(StreamNode(i)%DrainageID)/float(dt)
			ius=ids
!			call find1(StreamNode%NodeId,StreamNode(ius)%DownNodeId,NumStreamNode)  !would be more efficient to do this find back in watermgmt
!			ids=ifound(1)
	        ids=StreamNode(ius)%DownNodeId   ! DGT 8/15/05  - find not necessary
	        if(ids .lt. 1)exit  ! DGT 5/27/12 avoid addressing error
		  end do !while
		endif
	else !this is an internal streamnode, we need to calculate local runoff to it
		j=StreamNode(i)%DrainageID !j indicates the drainage which contains this node
		FlowAtStreamNodes_cms(i)=FlowAtStreamNodes_cms(i)+ &
									StreamNode(i)%FracRunoff*Runoff(Timestep)%Rate(j)/float(dt)
	end if
end do !i

!all the water management calculations take place at the drainage outlet
!this calculation point is downstream of all "internal" stream nodes
!this model does not know whereabouts inside the drainage any water mgmt activity occurs
!thus the values of FlowAtStreamNodes_cms for nodes that are not at drainage outlets will not
!reflect the within-drainage water management
!if more spatial detail is requried then a finer drainage discretisation must be used

! Write the outputs as we go to save having to store all the time steps
fid=384
if(Timestep .eq. 1)then
	filenm='FlowAtStreamNodes_cms.txt';
	open(unit=fid,file=filenm,status='unknown',err=1990)
	LocationTypeString='Node'
	if (allocated(headers)) deallocate (headers); allocate(headers(NumStreamNode+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,NumStreamNode
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	write(str,fmt=*,err=1991)(headers(i),i=1,NumStreamNode+1)
	ls=len(trim(str))
	write(unit=fid,fmt='(A)')str(1:ls)
endif
write(unit=fid,fmt=110,err=1991)timestep,(FlowAtStreamNodes_cms(j),j=1,NumStreamNode)
110 format(I12,3000G15.7)   ! DGT 7/23/05 changed to 7 digit precision (from G12.4)
if(Timestep .eq. Nsteps) then
	close(unit=fid)
endif
return

1990	write(6,*)'unable to open file for writing: ',filenm
	return
1991	write(6,*)'error while writing file: ',filenm
	return

end

!	StreamNode(:)%NodeID=real_array(:,1)
!	StreamNode(:)%DownNodeID=real_array(:,2)
!	StreamNode(:)%RealDrainageID=real_array(:,3)
!	StreamNode(:)%ProjNodeId=real_array(:,4)
!	StreamNode(:)%DOutFlag=real_array(:,5)
!	StreamNode(:)%LocalArea=real_array(:,7)
!	StreamNode(:)%TotalArea=real_array(:,8)
!	StreamNode(:)%X=real_array(:,9)
!	StreamNode(:)%Y=real_array(:,10)
!	StreamNode(:)%AreaInDrainage=StreamNode(:)%TotalArea
!StreamNode%DrainageID
!StreamNode%FracRunoff
