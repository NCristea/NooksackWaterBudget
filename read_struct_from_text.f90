	subroutine read_struct_from_text(filenm,expected_numcols,ncommentlines,nrows)
	use data_array
	character*(*) filenm
	integer nrows,expected_numcols

	character*(256) title,headers
	integer fid

	fid=121
	open(unit=fid,file=filenm,status='old',err=990)
	if (ncommentlines.ge.1) read(unit=fid,fmt=*)title
	if (ncommentlines.ge.2) read(unit=fid,fmt=*)headers

	nrows=0
	do while (nrows.ge.0)
		read(unit=fid,fmt=*,end=90)str
		nrows=nrows+1
	end do
90	close(unit=fid)

	if (nrows.gt.0) then
		open(unit=fid,file=filenm,status='old',err=990)
		if (ncommentlines.ge.2) then
			if (expected_numcols.lt.0) then
				read(unit=fid,fmt=*)expected_numcols
			else
				read(unit=fid,fmt=*)title
			end if
		end if
		if (allocated(real_array)) deallocate (real_array)
		allocate (real_array(nrows,expected_numcols))

		read(unit=fid,fmt=*)headers
		do k=1,nrows
		   read(unit=fid,fmt=*)(real_array(k,i),i=1,expected_numcols)
		end do
		close(unit=fid)
	endif
	return

990	write(6,*)'unable to open file ',filenm
	return
991	write(6,*)'error while reading file ',filenm
	return
	end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	subroutine read_bdryflow(filenm,expected_numcols,ncommentlines,nrows)
	use data_array
	character*(*) filenm
	integer nrows,expected_numcols

	character*(256) title,headers
	integer fid

	fid=121
	open(unit=fid,file=filenm,status='old',err=990)
	read(unit=fid,fmt=*,err=991)title !"This file provides ..."
	read(unit=fid,fmt=*,err=991)title !"Flow values are  ..."
	read(unit=fid,fmt=*,err=991)title !"*12205000*HNW-2052*12208000*"
	allocate(integer_array(100,1))
	read(unit=fid,fmt=*,err=991)title,expected_numcols,(integer_array(i,1),i=1,expected_numcols) !"Ver2 3  8  18  21 Date Hour"
!	if (ncommentlines.ge.1) read(unit=fid,fmt=*)title
!	if (ncommentlines.ge.2) read(unit=fid,fmt=*)headers
	expected_numcols=expected_numcols+2

	nrows=0
	do while (nrows.ge.0)
		read(unit=fid,fmt=*,end=90)str
		nrows=nrows+1
	end do
90	close(unit=fid)

	if (nrows.gt.0) then
		open(unit=fid,file=filenm,status='old',err=990)
		read(unit=fid,fmt=*)title
		read(unit=fid,fmt=*)title
		read(unit=fid,fmt=*)title
		read(unit=fid,fmt=*)title
		if (allocated(dble_array)) deallocate (dble_array)
		allocate (dble_array(nrows,expected_numcols))
		do k=1,nrows
		   read(unit=fid,fmt=*,err=991)(dble_array(k,i),i=1,expected_numcols)
		end do
		close(unit=fid)
	endif
	return

990	write(6,*)'unable to open file ',filenm
	return
991	write(6,*)'error while reading file ',filenm
	return
	end