subroutine Initialise_Output_Tables(NumDrainage,NumNode,NumStreamNode,NumLink,NumUser, &
									NumTimesteps,NumReservoir,NumUserSourceReturn,NumReturnFlow)

	use constant_definitions
	use input_structures
	use other_structures
	use findmodule
	use TimeVaryingOutput
	integer,allocatable :: ids(:),usernodes(:)
	integer uslink, usnode,usernode,dsnode,dslink,resnode

!	allocate (Precipitation_mm(NumTimesteps,NumDrainage));Precipitation_mm=-1   ! DGT 6/29/05 commented out
!	allocate (Evaporation_mm(NumTimesteps,NumDrainage));Evaporation_mm=-1     ! DGT 6/29/05 commented out
!	allocate (TotalRunoff_cms(NumTimesteps,NumDrainage))
!	allocate (Artificial_Drainage(NumTimesteps,NumDrainage))  ! DGT 6/28/05
!	allocate (Baseflow_cms(NumTimesteps,NumDrainage))
	allocate (FlowInLinks_cms(NumTimesteps,NumLink))
	allocate (FlowAtStreamNodes_cms(NumStreamNode))   !  DGT 8/12/05  reduced to 1 dimension
	allocate (ReservoirStorage_m3(NumTimesteps,NumReservoir))
	allocate (DateTime_yyyymmdd_hhmmss(NumTimesteps,2))

	do i=1,NumDrainage
		call find2(Node%Type,StreamNodeCode,Node%SelfID,i,NumNode);
		j=ifound(1) !problem if nfound<>1
		call find2(Link%USNode,j,Link%LinkCode,UnallocatedLinkCode,NumLink);
		k=ifound(1) !problem if nfound<>1
		StaticOutput%StreamFlowLinks(i)%DrainageID=i;
		StaticOutput%StreamFlowLinks(i)%LinkID=k;
	end do

	do i=1,NumDrainage
		StaticOutput%DrainageID(i)%TopnetID=Drainage(i)%DrainageID;
		StaticOutput%DrainageID(i)%DrainageID=Drainage(i)%RealDrainageID;
	end do

	k=0
	do i=1,NumUser
		do js=1,User(i)%NumSources
			j_src=User(i)%SourceID(js)
			j_ret=User(i)%ReturnFlowID
			if (j_ret.gt.0) then
				call find1(ReturnFlow%ReturnFlowID,j_ret,NumReturnFlow)
				j_ret_ind=ifound(1)
				do jr=1,ReturnFlow(j_ret_ind)%NumReturnFlows
					k=k+1
					if (k.gt.NumUserSourceReturn) then
						write(6,*)'too many return flows for the output table'
					end if
					StaticOutput%DrainageInfo(k)%UserSrceReturn_ID=k
					StaticOutput%DrainageInfo(k)%User_ID=i
					StaticOutput%DrainageInfo(k)%UsersType=User(i)%UsersType
					StaticOutput%DrainageInfo(k)%SourceDrainageID=Source(j_src)%RealSourceLocationID
					StaticOutput%DrainageInfo(k)%DestinationDrainageID=ReturnFlow(j_ret_ind)%RealReturnFlowsLocn(jr)
					StaticOutput%DrainageInfo(k)%WWTP_ID=ReturnFlow(j_ret_ind)%WWTP_ID(jr)
					StaticOutput%DrainageInfo(k)%RF_counter=jr
					StaticOutput%DrainageInfo(k)%SourceID=j_src
					StaticOutput%DrainageInfo(k)%ReturnFlowID=j_ret
				end do
			end if
		end do
	end do

return
end
