!  Subroutine to write output at each time step
!  David Tarboton  6/29/05
subroutine Write_OutputLine(filenm,timestep,Rvariable,NumDrainage,icall,scalefactor)
save
	character*20 fmtstr
	character*12, allocatable :: headers(:)
	 !  DGT 5/27/12 problem here passing fixed string - was character*128 filenm
	character*(*) filenm
	character*128 LocationTypeString
	character*32767 str
	real*8 Rvariable(NumDrainage),scalefactor
	integer timestep,fid

ncols=1+NumDrainage
fid=137+icall  !  A unit id that hopefully does not clash
if(timestep == 0)then  ! Initialize

!	open(unit=fid,file=filenm,recl=12*ncols+100,status='unknown',err=1990)
!  DGT 7/23/05 not specifying recl while precision increased
	open(unit=fid,file=filenm,status='unknown',err=1990)
	LocationTypeString='Drainage'
	if (allocated(headers)) deallocate (headers); allocate(headers(NumDrainage+1))
	headers(1)='TimeStep'
	lenLTS=len(trim(LocationTypeString))
	do i=1,NumDrainage
		n=1+int(log10(real(i)))
		write(fmtstr,'(''(A,i'',i1,'')'')')n
		write(headers(i+1),fmt=fmtstr)LocationTypeString(1:lenLTS),i
	end do
	write(str,fmt=*,err=1991)(headers(i),i=1,ncols)
	ls=len(trim(str))
	write(unit=fid,fmt='(A)')str(1:ls)
else if (timestep > 0)then
	write(unit=fid,fmt=110,err=1991)timestep,(Rvariable(j)*scalefactor,j=1,NumDrainage)
	110 format(I12,3000G15.7)   ! DGT 7/23/05 changed to 7 digit precision (from G12.4)
else
	close(unit=fid)
endif
return
!  Errors
1990	write(6,*)'unable to open file for writing: ',filenm
	return
1991	write(6,*)'error while writing file: ',filenm
	return

end

! DGT 9/4/05   Subroutine added to output local node drainage contributions
subroutine Write_OutputLocalContributions(NumStreamNode,NumDrainage,baseflowTopnet,RunoffTopnet,timestep,scalefactor)
	use constant_definitions
	use input_structures
	use other_structures
	real*8 RunoffTopnet(NumDrainage),BaseflowTopnet(NumDrainage)
	real*8 LocalSurfaceRunoff,LocalBaseflow,fracrunoff,scalefactor
	integer timestep,fid,fid2
save
fid=882  !  A unit id that hopefully does not clash
fid2=883  !  A unit id that hopefully does not clash

if(timestep == 0)then  ! Initialize
	open(unit=fid,file='NodeLocalSurfaceRunoff_cms.txt',status='unknown',err=1990)
	write(fid,'(A)')'TimeStep  Node_ID  Flow_cms'
	open(unit=fid2,file='NodeLocalBaseflow_cms.txt',status='unknown',err=1990)
	write(fid2,'(A)')'TimeStep  Node_ID  Flow_cms'
else if (timestep > 0)then
    do j=1,NumStreamNode
	    iddrain=streamnode(j)%DRAINAGEID
		fracrunoff=streamnode(j)%LOCALAREA/Drainage(iddrain)%CATCHAREA*1e6  ! Drainage(iddrain)%CATCHAREA is in mm^2
		LocalSurfaceRunoff=(RunoffTopnet(iddrain)-baseflowTopnet(iddrain))*fracrunoff*scalefactor
		LocalBaseflow=baseflowTopnet(iddrain)*fracrunoff*scalefactor
		write(unit=fid,fmt=110,err=1991)timestep,streamnode(j)%NODEID,LocalSurfaceRunoff
		write(unit=fid2,fmt=110,err=1991)timestep,streamnode(j)%NODEID,LocalBaseflow
	110 format(2I8,G15.7)
	enddo
else
	close(unit=fid)
	close(unit=fid2)
endif
return
!  Errors
1990	write(6,*)'unable to open file for writing: '
	return
1991	write(6,*)'error while writing file: '
	return

end

