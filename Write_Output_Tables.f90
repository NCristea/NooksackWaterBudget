subroutine Write_Static_Output_Tables(dirname);
!4 tables to write: StreamFlowLinks, DrainageID, DrainageInfo and UserFlowLinks
	use data_array
	use constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use other_structures
	character*30 headers(10)
	character*128 dirname,filenm

!this StreamFlowLinks table has a row for every drainage, telling us which column of the LinkFlow
!table will tell us the outflow for this drainage
	headers(1)='DrainageID'
	headers(2)='LinkID'
	ncols=2
    filenm=dirname(1:len_trim(dirname))  // '/' // 'StreamFlowLinks' // '.txt';
	nrows=size(StaticOutput%StreamFlowLinks)
	if (allocated(integer_array)) deallocate (integer_array)
	allocate(integer_array(nrows,ncols))
	integer_array(:,1)=StaticOutput%StreamFlowLinks%DrainageID
	integer_array(:,2)=StaticOutput%StreamFlowLinks%LinkID
    call write_integers_to_text(filenm,headers,nrows,ncols)

!this DrainageID table has a row for every drainage, telling us the connection between the TopnetID of a
!drainage, and the externally specified Drainage Number
	headers(1)='TopnetID'
	headers(2)='DrainageID'
	ncols=2
    filenm=dirname(1:len_trim(dirname))  // '/' // 'DrainageID' // '.txt';
	nrows=size(StaticOutput%DrainageID)
	if (allocated(integer_array)) deallocate (integer_array)
	allocate(integer_array(nrows,ncols))
	integer_array(:,1)=StaticOutput%DrainageID%TopnetID
    integer_array(:,2)=StaticOutput%DrainageID%DrainageID
    call write_integers_to_text(filenm,headers,nrows,ncols)

!this DrainageInfo table provides the connections back from each UserSourceReturnFlow combo
! to the SourceDrainageFlows and DestinationDrainageFlows tables
	headers(1)='UsrSrcRtn_ID';headers(2)='User_ID';headers(3)='UsersType'
	headers(4)='SrcDrainID';headers(5)='DestDrainID';headers(6)='WWTP_ID'
	ncols=6
    filenm=dirname(1:len_trim(dirname))  // '/' // 'DrainageInfo' // '.txt';
	nrows=size(StaticOutput%DrainageInfo)
	if (allocated(integer_array)) deallocate(integer_array)
	allocate(integer_array(nrows,ncols))
	integer_array(:,1)=StaticOutput%DrainageInfo%UserSrceReturn_ID;				integer_array(:,2)=StaticOutput%DrainageInfo%User_ID
    integer_array(:,3)=StaticOutput%DrainageInfo%UsersType;	integer_array(:,4)=StaticOutput%DrainageInfo%SourceDrainageID
    integer_array(:,5)=StaticOutput%DrainageInfo%DestinationDrainageID;
	integer_array(:,6)=StaticOutput%DrainageInfo%WWTP_ID
    call write_integers_to_text(filenm,headers,nrows,ncols)

!this UserFlowLinks table provides the connections back from each User to the LinkFlow table
    if (allocated(StaticOutput%UserFlowLinks)) then
        headers(1)='UserID';headers(2)='SourceDrainage';headers(3)='DestinationDrainage'
        headers(4)='TakeLinkID';headers(5)='ReturnLinkID';headers(6)='UsersType'
        ncols=6
        filenm=dirname(1:len_trim(dirname))  // '/' // 'UserFlowLinks' // '.txt';
        nrows=size(StaticOutput%UserFlowLinks)
        if (allocated(integer_array)) deallocate(integer_array)
        allocate(integer_array(nrows,ncols))

        integer_array(:,1)=StaticOutput%UserFlowLinks%UserID;
        integer_array(:,2)=StaticOutput%UserFlowLinks%SourceDrainage
        integer_array(:,3)=StaticOutput%UserFlowLinks%DestinationDrainage;
        integer_array(:,4)=StaticOutput%UserFlowLinks%TakeLinkID
        integer_array(:,5)=StaticOutput%UserFlowLinks%ReturnLinkID;
        integer_array(:,6)=StaticOutput%UserFlowLinks%UsersType
        call write_integers_to_text(filenm,headers,nrows,ncols)
    else
        write(6,*) "No UserFlowLInks data allocated"
    endif

return
end

subroutine write_integers_to_text(filenm,headers,nrows,ncols)
!write out a data array that has  columns of integer data
use data_array
	character*30 headers(*)
	character*128 dirname,filenm
	character*32767 str,strall
	character*10 integerfmt
integer fid


fid=121

open(unit=fid,file=filenm,status='unknown',err=990)

write(str,fmt=*)(headers(i),i=1,ncols)
ls=len(trim(str))
write(unit=fid,fmt='(A)')str(1:ls)

!if nargin<3 | isempty(fmttype), fmttype='%g';
integerfmt='(1x,300I9)'
do i=1,nrows
	write(unit=fid,fmt=integerfmt)(integer_array(i,j),j=1,ncols)
end do

close(unit=fid);

return

990	write(6,*)'unable to open file for writing: ',filenm
	return
991	write(6,*)'error while writing file: ',filenm
	return
end

subroutine Write_TimeVaryingOutput_Tables(dirname,NumUser,NumTimesteps,NumNode,NumLink,NumReturnFlow,NumWWTP);
!five tables to write out with real-valued time series:
!TotalRunoff_cms,Baseflow_cms,FlowInLinks_cms,FlowAtStreamNodes_cms,ReservoirStorage_m3
! and one with integer-valued time series: DateTime_yyyymmdd_hhmmss
	use data_array
	use constant_definitions !define names for numeric codes (types of nodes, links, users, etc)
	use input_structures
	use other_structures
	use TimeVaryingOutput
	use findmodule

	character*20 fmtstr
	character*12, allocatable :: headers(:)
	character*128 dirname,filenm,LocationTypeString
	real, allocatable :: WWTP_Outflows_cms(:,:),SourceDrainageFlows(:,:)
	real sumsourceflow
	integer, allocatable :: irf_list(:),isrc_list(:)
	integer usernode,max_id,irf,k0,isrc,nsrc

!3 tables to create: 1 with WWTP flows, 1 with all flows from sources to users, 1 with all flows from users to destinations
	!build WWTP times series from FlowInLinks
	!for every returnflow of every user, if there's a WWTP on the rf path, add this flow
	allocate (WWTP_Outflows_cms(NumTimesteps*NumWWTP,3))
	WWTP_Outflows_cms(:,:)=0
	do i=1,NumUser
		irf=User(i)%ReturnFlowID
		if (irf.gt.0) then
			call find2(Node%Type,UserNodeCode,Node%SelfID,i,NumNode)
			usernode=ifound(1)
			call find1(ReturnFlow%ReturnFlowID,irf,NumReturnFlow)
			irf_ind=ifound(1)
			do j=1,ReturnFlow(irf_ind)%NumReturnFlows
				if (ReturnFlow(irf_ind)%WWTP_ID(j).gt.0) then
					id=ReturnFlow(irf_ind)%WWTP_ID(j)
					call find3(Link%LinkCode,ReturnFlowLinkCode,Link%USNode,usernode,Link%ReturnFlowID,j,NumLink)
					j_ret=ifound(1)
					call find1(WWTP_list,id,NumWWTP)
					do k=1,NumTimesteps
						k0=(ifound(1)-1)*NumTimesteps+k
						WWTP_Outflows_cms(k0,1)=k
						WWTP_Outflows_cms(k0,2)=id
						WWTP_Outflows_cms(k0,3)=WWTP_Outflows_cms(k0,3)+FlowInLinks_cms(k,j_ret)
					end do

				end if
			end do
		end if
	end do

	!build DrainageFlows time series from FlowInLinks
	!for every path from a source to a user to a returnflow destination
	!each return flow amount is comprised of water from perhaps more than one source
	!estimate the proportion of that retrun flow which comes from each source, by using the proportion of water taken from each source
	k=0
	nusr=size(StaticOutput%DrainageInfo,1)
	allocate(SourceDrainageFlows(nusr*NumTimesteps,3))
	SourceDrainageFlows(:,:)=0
	do i=1,NumUser
		call find2(Node%Type,UserNodeCode,Node%SelfID,i,NumNode) !find user node
		usernode=ifound(1)
		call find2(Link%LinkCode,ReturnFlowLinkCode,Link%USNode,usernode,NumLink) !find all return flows links from this user
		if (allocated(irf_list)) deallocate(irf_list)
		allocate (irf_list(nfound))
		irf_list=ifound(1:nfound); nrf=nfound    !RAW 4-Jul-2005 changed ifound(1) to ifound(1:nfound) - will only matter if a user has >1 return flow
		call find2(Link%LinkCode,UserAbstractionLinkCode,Link%DSNode,usernode,NumLink) !find all return flows links from this user
		if (allocated(isrc_list)) deallocate(isrc_list)
		allocate (isrc_list(nfound))
		isrc_list=ifound(1:nfound); nsrc=nfound
		!only one of these return flows goes to the destination we are interested in
		do js=1,User(i)%NumSources
			j_src=User(i)%SourceID(js)
			call find2(Node%Type,Source(j_src)%Type,Node%SelfID,Source(j_src)%SourceLocationID,NumNode) !find the source node
			i_src=ifound(1)
			!RAW 4-Jul-2005 changed next line from find2 to find3 to include "Link%DSNode,usernode," in our search - before this fix we were getting the wrong iuslink
			call find3(Link%LinkCode,UserAbstractionLinkCode,Link%USNode,i_src,Link%DSNode,usernode,NumLink) !find the link from that node to the user
			iuslink=ifound(1)
			j_ret=User(i)%ReturnFlowID
			if (j_ret.gt.0) then
				call find1(ReturnFlow%ReturnFlowID,j_ret,NumReturnFlow)
				irf_ind=ifound(1) !problem if nfound<>1
				do jr=1,ReturnFlow(irf_ind)%NumReturnFlows
					k=k+1
					do it=1,NumTimesteps
						sumreturnflow=0
						do jrf=1,nrf
							if (jrf.eq.StaticOutput%DrainageInfo(k)%RF_counter) rf_to_this_dest=FlowInLinks_cms(it,irf_list(jrf))
							sumreturnflow=sumreturnflow+FlowInLinks_cms(it,irf_list(jrf))
						end do !jrf
						sumsourceflow=0
						do jsrc=1,nsrc
							sumsourceflow=sumsourceflow+FlowInLinks_cms(it,isrc_list(jsrc))
						end do !jrf
						if (sumsourceflow.gt.0) then
							proportion_flow_from_this_source=FlowInLinks_cms(it,iuslink)/sumsourceflow
						else
							proportion_flow_from_this_source=1.
						end if
						if (sumreturnflow.gt.0) then
!							frac_of_all_rf_via_this_rfpath=rf_to_this_dest/sumreturnflow
!							flow=flow_from_this_source*frac_of_all_rf_via_this_rfpath
							flow=rf_to_this_dest*proportion_flow_from_this_source
						else
							flow=0
						end if
						k0=(k-1)*NumTimesteps+it
						SourceDrainageFlows(k0,1)=it
						SourceDrainageFlows(k0,2)=k
						SourceDrainageFlows(k0,3)=flow
					end do !it
				end do !jr
			end if !j_ret
		end do !js
	end do !i

icall=0

	icall=icall+1
	nv=size(DateTime_yyyymmdd_hhmmss,2) !second dimension is location
	nrows=size(DateTime_yyyymmdd_hhmmss,1)
	LocationTypeString='Drainage'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'DateTime_yyyymmdd_hhmmss' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	headers(2)='yyyymmdd'
	headers(3)='hhmmss'
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; integer_array(j,i+1)=DateTime_yyyymmdd_hhmmss(j,i); end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
!	nv=size(TotalRunoff_cms,2) !second dimension is location
!	nrows=size(TotalRunoff_cms,1)
!	LocationTypeString='Drainage'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'TotalRunoff_cms' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
 !   do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=TotalRunoff_cms(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

!  Artificial drainage DGT 6/28/05
!	icall=icall+1
!	nv=size(Artificial_Drainage,2) !second dimension is location
!	nrows=size(Artificial_Drainage,1)
!	LocationTypeString='Drainage'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'Artificial_Drainage' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
!   do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=Artificial_Drainage(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)


	icall=icall+1
!	nv=size(Baseflow_cms,2) !second dimension is location
!	nrows=size(Baseflow_cms,1)
!	LocationTypeString='Drainage'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'Baseflow_cms' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
 !   do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=Baseflow_cms(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
	nv=size(FlowInLinks_cms,2) !second dimension is location
	nrows=size(FlowInLinks_cms,1)
	LocationTypeString='LinkID'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'FlowInLinks_cms' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,nv
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; real_array(j,i+1)=FlowInLinks_cms(j,i); end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
!	nv=size(FlowAtStreamNodes_cms,2) !second dimension is location
!	nrows=size(FlowAtStreamNodes_cms,1)
!	LocationTypeString='Node'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'FlowAtStreamNodes_cms' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
!    do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=FlowAtStreamNodes_cms(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
	nv=size(ReservoirStorage_m3,2) !second dimension is location
	nrows=size(ReservoirStorage_m3,1)
	LocationTypeString='Reservoir'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'ReservoirStorage_m3' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,nv
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; real_array(j,i+1)=ReservoirStorage_m3(j,i); end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
! DGT 6/29/05  Want to handle precipitation and evaporation on a line by line basis to be more efficient about memory
!	nv=size(Precipitation_mm,2) !second dimension is location
!	nrows=size(Precipitation_mm,1)
!	LocationTypeString='Drainage'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'Precipitation_mm' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
 !   do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=Precipitation_mm(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
!	nv=size(Evaporation_mm,2) !second dimension is location
!	nrows=size(Evaporation_mm,1)
!	LocationTypeString='Drainage'
!	filenm=dirname(1:len_trim(dirname))  // '/' // 'Evaporation_mm' // '.txt';
!	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
!	headers(1)='TimeStep'
!	lenLTS=len(trim(LocationTypeString))
!	do i=1,nv
!		n=1+int(log10(real(i)))
!		write(fmtstr,'(''(A,i'',i1,'')'')')n
!		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
!	end do
!	ncols=1+nv
!	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
!	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
!	do j=1,nrows
!		integer_array(j,1)=j
!	end do
 !   do i=1,nv;
!		do j=1,nrows; real_array(j,i+1)=Evaporation_mm(j,i); end do
!	end do;
!	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
	nv=NumUser !second dimension is user
	nrows=NumTimesteps
	LocationTypeString='User'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'UserDemand_cms' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,nv
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; real_array(j,i+1)=User(i)%Demand(j)/86400.; end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
	nv=NumUser !second dimension is user
	nrows=NumTimesteps
	LocationTypeString='User'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'UserDeficit_cms' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,nv
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; real_array(j,i+1)=User(i)%Deficit(j)/86400.; end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=icall+1
	nv=NumUser !second dimension is user
	nrows=NumTimesteps
	LocationTypeString='User'
	filenm=dirname(1:len_trim(dirname))  // '/' // 'UserWithdrawal_cms' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(nv+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,nv
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	ncols=1+nv
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=j
	end do
    do i=1,nv;
		do j=1,nrows; real_array(j,i+1)=User(i)%Withdrawal(j)/86400.; end do
	end do;
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

	icall=-1
	nv=size(WWTP_Outflows_cms,2) !second dimension is NOT location
	nrows=size(WWTP_Outflows_cms,1)
	filenm=dirname(1:len_trim(dirname))  // '/' // 'WWTP_Outflows_cms' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(3))
	headers(1)='TimeStep';headers(2)='WWTP_ID';headers(3)='Flow_cms'
	ncols=3
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=WWTP_Outflows_cms(j,1)
		integer_array(j,2)=WWTP_Outflows_cms(j,2)
	end do
		do j=1,nrows; real_array(j,3)=WWTP_Outflows_cms(j,3); end do
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)


	icall=-1
	nv=size(SourceDrainageFlows,2) !second dimension is NOT location
	nrows=size(SourceDrainageFlows,1)
	filenm=dirname(1:len_trim(dirname))  // '/' // 'SourceDrainageFlows' // '.txt';
	if (allocated(headers)) deallocate (headers); allocate(headers(3))
	headers(1)='TimeStep';headers(2)='UsrSrcRtn_ID';headers(3)=' Flow_cms'
	ncols=3
	if (allocated(integer_array)) deallocate (integer_array); allocate(integer_array(nrows,ncols))
	if (allocated(real_array)) deallocate (real_array); allocate(real_array(nrows,ncols))
	do j=1,nrows
		integer_array(j,1)=SourceDrainageFlows(j,1)
		integer_array(j,2)=SourceDrainageFlows(j,2)
	end do
		do j=1,nrows; real_array(j,3)=SourceDrainageFlows(j,3); end do
	call write_struct_to_text(filenm,headers,nrows,ncols,icall)

return
end


subroutine write_struct_to_text(filenm,headers,nrows,ncols,icall)
!write out a data array that has 1 columns of integer data, and then columns of real data
use data_array
	character*12 headers(*)
	character*128 dirname,filenm
	character*32767 str,strall
	character*30 realfmt

integer fid


fid=121

!  open(unit=fid,file=filenm,recl=12*ncols+100,status='unknown',err=1990)
open(unit=fid,file=filenm,status='unknown',err=1990)   ! DGT 7/21/05 No record length limit to increase precision


write(str,fmt=*,err=1991)(headers(i),i=1,ncols)
ls=len(trim(str))
write(unit=fid,fmt='(A)')str(1:ls)

if (icall.gt.1) then
	do i=1,nrows
		write(unit=fid,fmt=110,err=1991)integer_array(i,1),(real_array(i,j),j=2,ncols)
	110 format(I12,3000G15.7)  ! DGT 7/21/05  Increase precision on output
	end do
elseif (icall.eq.1) then
	do i=1,nrows
		write(unit=fid,fmt=120,err=1991)integer_array(i,1),(integer_array(i,j),j=2,ncols)
	120 format(I12,1x,2I12)
	end do
elseif (icall.lt.0) then
	do i=1,nrows
		write(unit=fid,fmt=130,err=1991)(integer_array(i,j),j=1,2),real_array(i,3)
	130 format(2I12,G15.7)    !  DGT 7/21/05   Increase precision on output
	end do
else
	write(6,*)'illegal call to write_struct_to_text with icall=',icall
end if
close(unit=fid);

return

1990	write(6,*)'unable to open file for writing: ',filenm
	return
1991	write(6,*)'error while writing file: ',filenm
	return
end


