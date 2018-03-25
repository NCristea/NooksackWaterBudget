	subroutine BuildDrainageOrder(NumDrainage,DrainageOrder)

	use input_structures

	integer DrainageOrder(NumDrainage),Position
	DrainageOrder(:)=0
	Position=0

	do while (Position.lt.NumDrainage)
		i=1
		do while (DrainageOrder(i).gt.0)
			i=i+1
		end do !find first unordered drainage
		call SetOrder(i,DrainageOrder,Position,NumDrainage)
	end do
	return
	end

!  DGT 5/14/12  explicitly declared as recursive
	recursive subroutine SetOrder(k,DrainageOrder,Position,NumDrainage)
	use input_structures
	use findmodule
	integer DrainageOrder(NumDrainage),Position,jfound(50)
     
     	if (DrainageOrder(k).gt.0) then
		write(6,*)'attempt to order a node twice'
		return
	else
		call find1(Drainage%DSDrainage,k,NumDrainage)
		iallpositive = 1
		do i=1,nfound
			jfound(i)=ifound(i) !we need a local copy that won't get clobbered during recursion
			if (DrainageOrder(jfound(i)).le.0) iallpositive=0
		end do
		if (nfound.gt.0 .and. iallpositive.ne.1 ) then
			do i=1,nfound
				if (DrainageOrder(jfound(i)).le.0) then
				   call SetOrder(jfound(i),DrainageOrder,Position,NumDrainage)
				end if
			end do
		end if
		Position=Position+1;
!		DrainageOrder(k)=Position
		DrainageOrder(Position)=k
      end if
      return
	end
