	subroutine find1(iarray1,ival1,num)
	use findmodule
	integer num,ival1,i
	integer iarray1(num)

	if (allocated(ifound)) deallocate (ifound)
	allocate (ifound(num))
	
	nfound=0 !none found
	do i =1,num
		if (iarray1(i).eq.ival1) then
			nfound=nfound+1
			ifound(nfound)=i
		endif
	end do
	end

	subroutine find2(iarray1,ival1,iarray2,ival2,num)
! this returns in ifound the list of indices for which iarray1(i)=ival1 and 
! iarray2(i)=ival2.  The search is from 1 to num
	use findmodule
	integer num,iarray1(num),ival1,iarray2(num),ival2,i
	
	if (allocated(ifound)) deallocate (ifound)
	allocate (ifound(num))
	
	nfound=0 !none found
	do i =1,num
		if (iarray1(i).eq.ival1 .and. iarray2(i).eq.ival2) then
			nfound=nfound+1
			ifound(nfound)=i
		endif
	end do
	end

	subroutine find3(iarray1,ival1,iarray2,ival2,iarray3,ival3,num)
	use findmodule
	integer num,iarray1(num),ival1,iarray2(num),ival2,iarray3(num),ival3,i
	if (allocated(ifound)) deallocate (ifound)
	allocate (ifound(num))
	
	!A & B & C
	nfound=0 !none found
	do i =1,num
		if ((iarray1(i).eq.ival1) .and. (iarray2(i).eq.ival2) .and. (iarray3(i).eq.ival3)) then
			nfound=nfound+1
			ifound(nfound)=i
		endif
	end do
	end

	subroutine find2a(iarray1,ival1,iarray2,ival2,num)
	use findmodule
	integer num,iarray1(num),ival1,iarray2(num),ival2,iarray3(num),ival3,i
	if (allocated(ifound)) deallocate (ifound)
	allocate (ifound(num))
	
	!A & (~B)
	nfound=0 !none found
	do i =1,num
		if ((iarray1(i).eq.ival1) .and. (iarray2(i).ne.ival2)) then
			nfound=nfound+1
			ifound(nfound)=i
		endif
	end do
	end

	subroutine find3b(iarray1,ival1,iarray2,ival2,iarray3,ival3,num)
	use findmodule
	integer num,iarray1(num),ival1,iarray2(num),ival2,iarray3(num),ival3,i
	if (allocated(ifound)) deallocate (ifound)
	allocate (ifound(num))
	
	!A & (B | C)
	nfound=0 !none found
	do i =1,num
		if ((iarray1(i).eq.ival1) .and. (iarray2(i).eq.ival2) .or. (iarray3(i).eq.ival3)) then
			nfound=nfound+1
			ifound(nfound)=i
		endif
	end do
	end
