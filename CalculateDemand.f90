subroutine CalculateDemand(ThisMonth,DOY,vol_irrig_demand,NumUser,NumMonthlyDemand)
	use constant_definitions
	use input_structures
	use other_structures
	use findmodule

	integer ThisMonth,DOY,i,NumUser,NumMonthlyDemand,idt
	real DDF
	real*8 vol_irrig_demand(*)

	do i=1,NumUser;
!  Debugging code to stop at a specific user
!	if (i .eq. 616)then
!	  DDF=DDF
!	endif
		if (User(i)%UsersType.eq.SoilMoistureIrrigationUseCode) then
		! DGT 5/18/05.  SoilMoistureIrrigationUseCode is irrigation demand from soil moisture
!			User(i)%DemandVble=vol_irrig_demand(User(i)%POU_ID) ! DGT 8/20/05 Reinterpret vol_irrig_demand
!           as depth of irrig demand.  Therefore use it as the rate variable.
!           The demand variable is the irrigated area in square meters from user.txt
			User(i)%DemandRate=vol_irrig_demand(User(i)%POU_ID) ! 1.
			DDF=1.
		else
!		if (User(i)%UsersType.ne.-1) then
			if (User(i)%InYearDemandType .eq. 0) then
				DDF=1
			elseif (User(i)%InYearDemandType .gt. 0) then
				call find1(MonthlyDemand%InYearDemandType,User(i)%InYearDemandType, &
								NumMonthlyDemand);
				idt=ifound(1) !problem if nfound<>1
 !               idt=User(i).InYearDemandType  ! DGT 8/20/05 relying on InYearDemandType
				! in MonthlyDemandFraction.txt being a counting sequence starting at 1
				DDF=MonthlyDemand(idt)%Month(ThisMonth);
			else
	            write(6,*)'need to implement daily demand fractions for every day of year!'
				!call find1(DailyDemand.InYearDemandType,User(i).InYearDemandType, &
				!			NumDailyDemand);
				!idt=ifound(1) !problem if nfound<>1
				!DDF=DailyDemand(idt).Day(DOY);
			end if
		end if

		User(i)%DemandToday=User(i)%DemandVble * User(i)%DemandRate * DDF;
	end do

return
end
