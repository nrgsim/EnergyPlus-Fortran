MODULE OutputProcessor

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains the major Output Processor routines.
          ! In addition, in this file are several routines which can be called
          ! without Useing the OutputProcessor Module

          ! METHODOLOGY EMPLOYED:
          ! Lots of pointers and other fancy data stuff.

          ! REFERENCES:
          ! EnergyPlus OutputProcessor specifications.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, OutputFileMeters, HourOfDay, DayOfSim, DayOfSimChr, OutputFileStandard, &
                       MinutesPerTimeStep, ZoneTSReporting, HVACTSReporting, StdOutputRecordCount
USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, ShowContinueError, &
                          ShowRecurringSevereErrorAtEnd
USE DataEnvironment, ONLY: Month, DayOfMonth, Year, DSTIndicator, DayOfWeek, HolidayIndex
USE DataGlobalConstants

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC    ! This module is used by some external routines but not by most.  Anything not
          ! in this file should obey a USE OutputProcessor, ONLY: rule.


          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: ReportEach     =-1   ! Write out each time UpdatedataandReport is called
INTEGER, PARAMETER :: ReportTimeStep =0    ! Write out at 'EndTimeStepFlag'
INTEGER, PARAMETER :: ReportHourly   =1    ! Write out at 'EndHourFlag'
INTEGER, PARAMETER :: ReportDaily    =2    ! Write out at 'EndDayFlag'
INTEGER, PARAMETER :: ReportMonthly  =3    ! Write out at end of month (must be determined)
INTEGER, PARAMETER :: ReportSim      =4    ! Write out once per environment 'EndEnvrnFlag'

INTEGER, PARAMETER :: ReportVDD_No   =0    ! Don't report the variable dictionaries in any form
INTEGER, PARAMETER :: ReportVDD_Yes  =1    ! Report the variable dictionaries in "report format"
INTEGER, PARAMETER :: ReportVDD_IDF  =2    ! Report the variable dictionaries in "IDF format"


REAL(r64), PARAMETER    :: MinSetValue    = 99999999999999.d0
REAL(r64), PARAMETER    :: MaxSetValue    =-99999999999999.d0
INTEGER, PARAMETER :: IMinSetValue   = 999999
INTEGER, PARAMETER :: IMaxSetValue   =-999999

INTEGER, PARAMETER :: ZoneVar        = 1    ! Type value for those variables reported on the Zone Time Step
INTEGER, PARAMETER :: HVACVar        = 2    ! Type value for those variables reported on the System Time Step

INTEGER, PARAMETER :: AveragedVar    = 1    ! Type value for "averaged" variables
INTEGER, PARAMETER :: SummedVar      = 2    ! Type value for "summed" variables

INTEGER, PARAMETER :: VarType_NotFound=0    ! ref: GetVariableKeyCountandType, 0 = not found
INTEGER, PARAMETER :: VarType_Integer =1    ! ref: GetVariableKeyCountandType, 1 = integer
INTEGER, PARAMETER :: VarType_Real    =2    ! ref: GetVariableKeyCountandType, 2 = real
INTEGER, PARAMETER :: VarType_Meter   =3    ! ref: GetVariableKeyCountandType, 3 = meter
INTEGER, PARAMETER :: VarType_Schedule=4    ! ref: GetVariableKeyCountandType, 4 = schedule

INTEGER, PARAMETER :: MeterType_Normal     = 0    ! Type value for normal meters
INTEGER, PARAMETER :: MeterType_Custom     = 1    ! Type value for custom meters
INTEGER, PARAMETER :: MeterType_CustomDec  = 2    ! Type value for custom meters that decrement another meter
INTEGER, PARAMETER :: MeterType_CustomDiff = 3    ! Type value for custom meters that difference another meter

CHARACTER(len=*), PARAMETER :: TimeStampFormat = "(A,',',A,',',i2,',',i2,',',i2,',',i2,',',f5.2,',',f5.2,',',A)"
CHARACTER(len=*), PARAMETER :: DailyStampFormat = "(A,',',A,',',i2,',',i2,',',i2,',',A)"
CHARACTER(len=*), PARAMETER :: MonthlyStampFormat = "(A,',',A,',',i2)"
CHARACTER(len=*), PARAMETER :: RunPeriodStampFormat = "(A,',',A)"
CHARACTER(len=*), PARAMETER :: fmta="(A)"
CHARACTER(len=*), PARAMETER, DIMENSION(12) :: DayTypes=(/"Sunday         ","Monday         ","Tuesday        ",  &
                                                         "Wednesday      ","Thursday       ","Friday         ", &
                                                         "Saturday       ","Holiday        ","SummerDesignDay", &
                                                         "WinterDesignDay","CustomDay1     ","CustomDay2     "/)
CHARACTER(len=*), PARAMETER :: BlankString = ' '
INTEGER, PARAMETER :: UnitsStringLength=16

INTEGER, PARAMETER :: RVarAllocInc=1000
INTEGER, PARAMETER :: LVarAllocInc=1000
INTEGER, PARAMETER :: IVarAllocInc=10

!  For IP Units (tabular reports) certain resources will be put in sub-tables
!INTEGER, PARAMETER :: RT_IPUnits_Consumption=0
INTEGER, PARAMETER :: RT_IPUnits_Electricity=1
INTEGER, PARAMETER :: RT_IPUnits_Gas=2
INTEGER, PARAMETER :: RT_IPUnits_Cooling=3
INTEGER, PARAMETER :: RT_IPUnits_Water=4
INTEGER, PARAMETER :: RT_IPUnits_OtherKG=5
INTEGER, PARAMETER :: RT_IPUnits_OtherM3=6
INTEGER, PARAMETER :: RT_IPUnits_OtherL=7
INTEGER, PARAMETER :: RT_IPUnits_OtherJ=0

          ! DERIVED TYPE DEFINITIONS:
TYPE TimeSteps
  REAL(r64), POINTER :: TimeStep    ! Pointer to the Actual Time Step Variable (Zone or HVAC)
  REAL(r64)     :: CurMinute   ! Current minute (decoded from real Time Step Value)
END TYPE TimeSteps

TYPE RealVariables
  REAL(r64), POINTER      :: Which         ! The POINTER to the actual variable holding the value
  REAL(r64)          :: Value         =0.0d0 ! Current Value of the variable (to resolution of Zone Time Step)
  REAL(r64)          :: TSValue       =0.0d0 ! Value of this variable at the Zone Time Step
  REAL(r64)          :: EITSValue     =0.0d0 ! Value of this variable at the Zone Time Step for external interface
  REAL(r64)          :: StoreValue    =0.0d0 ! At end of Zone Time Step, value is placed here for later reporting
  REAL(r64)          :: NumStored     =0.0d0 ! Number of hours stored
  INTEGER            :: StoreType     =0   ! Variable Type (Summed/Non-Static or Average/Static)
  LOGICAL            :: Stored        =.false. ! True when value is stored
  LOGICAL            :: Report        =.false. ! User has requested reporting of this variable in the IDF
  LOGICAL            :: tsStored      =.false. ! if stored for this zone timestep
  LOGICAL            :: thistsStored      =.false. ! if stored for this zone timestep
  INTEGER            :: thisTScount   =0
  INTEGER            :: ReportFreq    =0   ! How often to report this variable
  REAL(r64)          :: MaxValue      =-9999.d0 ! Maximum reporting (only for Averaged variables, and those greater than Time Step)
  INTEGER            :: MaxValueDate  =0   ! Date stamp of maximum
  REAL(r64)          :: MinValue      =9999.d0 ! Minimum reporting (only for Averaged variables, and those greater than Time Step)
  INTEGER            :: MinValueDate  =0     ! Date stamp of minimum
  INTEGER            :: ReportID      =0     ! Report variable ID number
  CHARACTER(len=16)  :: ReportIDChr   =BlankString ! Report variable ID number (character -- for printing)
  INTEGER            :: SchedPtr      =0     ! If scheduled, this points to the schedule
  INTEGER            :: MeterArrayPtr =0     ! If metered, this points to an array of applicable meters
  INTEGER            :: ZoneMult      =1     ! If metered, Zone Multiplier is applied
  INTEGER            :: ZoneListMult  =1     ! If metered, Zone List Multiplier is applied
END TYPE RealVariables

TYPE IntegerVariables
  INTEGER, POINTER   :: Which         ! The POINTER to the actual variable holding the value
  REAL(r64)          :: Value         =0.0d0 ! Current Value of the variable (to resolution of Zone Time Step)
  REAL(r64)          :: TSValue       =0.0d0 ! Value of this variable at the Zone Time Step
  REAL(r64)          :: EITSValue     =0.0d0 ! Value of this variable at the Zone Time Step for external interface
  REAL(r64)          :: StoreValue    =0.0d0 ! At end of Zone Time Step, value is placed here for later reporting
  REAL(r64)          :: NumStored     =0.0d0 ! Number of hours stored
  INTEGER            :: StoreType     =0   ! Variable Type (Summed/Non-Static or Average/Static)
  LOGICAL            :: Stored        =.false. ! True when value is stored
  LOGICAL            :: Report        =.false. ! User has requested reporting of this variable in the IDF
  LOGICAL            :: tsStored      =.false. ! if stored for this zone timestep
  LOGICAL            :: thistsStored      =.false. ! if stored for this zone timestep
  INTEGER            :: thisTScount   =0
  INTEGER            :: ReportFreq    =0   ! How often to report this variable
  INTEGER            :: MaxValue      =-9999 ! Maximum reporting (only for Averaged variables, and those greater than Time Step)
  INTEGER            :: MaxValueDate  =0   ! Date stamp of maximum
  INTEGER            :: MinValue      =9999  ! Minimum reporting (only for Averaged variables, and those greater than Time Step)
  INTEGER            :: MinValueDate  =0   ! Date stamp of minimum
  INTEGER            :: ReportID      =0   ! Report variable ID number
  CHARACTER(len=16)  :: ReportIDChr   =BlankString ! Report variable ID number (character -- for printing)
  INTEGER            :: SchedPtr      =0   ! If scheduled, this points to the schedule
END TYPE IntegerVariables

TYPE VariableTypeForDDOutput
  INTEGER                          :: IndexType     =0         ! Type whether Zone or HVAC
  INTEGER                          :: StoreType     =0         ! Variable Type (Summed/Non-Static or Average/Static)
  INTEGER                          :: VariableType  = VarType_NotFound ! Integer, Real.
  INTEGER                          :: Next                     ! Next variable of same name (different units)
  LOGICAL                          :: ReportedOnDDFile=.false. ! true after written to .rdd/.mdd file
  CHARACTER(len=MaxNameLength)     :: VarNameOnly =BlankString ! Name of Variable
  CHARACTER(len=UnitsStringLength) :: UnitsString =BlankString ! Units for Variable (no brackets)
END TYPE

TYPE RealVariableType
  INTEGER                          :: IndexType   =0           ! Type whether Zone or HVAC
  INTEGER                          :: StoreType     =0   ! Variable Type (Summed/Non-Static or Average/Static)
  INTEGER                          :: ReportID      =0     ! Report variable ID number
  CHARACTER(len=MaxNameLength*2+1) :: VarName     =BlankString ! Name of Variable key:variable
  CHARACTER(len=MaxNameLength*2+1) :: VarNameUC   =BlankString ! Name of Variable (Uppercase)
  CHARACTER(len=MaxNameLength)     :: VarNameOnly =BlankString ! Name of Variable
  CHARACTER(len=MaxNameLength)     :: VarNameOnlyUC =BlankString ! Name of Variable with out key in uppercase
  CHARACTER(len=MaxNameLength)     :: KeyNameOnlyUC =BlankString ! Name of key only witht out variable in uppercase
  CHARACTER(len=UnitsStringLength) :: UnitsString =BlankString ! Units for Variable (no brackets)
  TYPE(RealVariables), POINTER     :: VarPtr     ! Pointer used to real Variables structure
END TYPE

TYPE IntegerVariableType
  INTEGER                          :: IndexType   =0           ! Type whether Zone or HVAC
  INTEGER                          :: StoreType     =0   ! Variable Type (Summed/Non-Static or Average/Static)
  INTEGER                          :: ReportID      =0     ! Report variable ID number
  CHARACTER(len=MaxNameLength*2+1) :: VarName     =BlankString ! Name of Variable
  CHARACTER(len=MaxNameLength*2+1) :: VarNameUC   =BlankString ! Name of Variable
  CHARACTER(len=MaxNameLength)     :: VarNameOnly =BlankString ! Name of Variable
  CHARACTER(len=UnitsStringLength) :: UnitsString =BlankString ! Units for Variable (no brackets)
  TYPE(IntegerVariables), POINTER  :: VarPtr     ! Pointer used to integer Variables structure
END TYPE

TYPE ReqReportVariables    ! Structure for requested Report Variables
  CHARACTER(len=MaxNameLength) :: Key         =BlankString ! Could be blank or "*"
  CHARACTER(len=MaxNameLength) :: VarName     =BlankString ! Name of Variable
  INTEGER                      :: ReportFreq  =0   ! Reporting Frequency
  INTEGER                      :: SchedPtr    =0   ! Index of the Schedule
  CHARACTER(len=MaxNameLength) :: SchedName   =BlankString ! Schedule Name
  LOGICAL                      :: Used        =.false. ! True when this combination (key, varname, frequency) has been set
END TYPE

TYPE MeterArrayType
  INTEGER               :: NumOnMeters   =0 ! Number of OnMeter Entries for variable
  INTEGER               :: RepVariable   =0 ! Backwards pointer to real Variable
  INTEGER, DIMENSION(6) :: OnMeters      =0 ! Forward pointer to Meter Numbers
  INTEGER               :: NumOnCustomMeters   =0 ! Number of OnCustomMeter Entries for variable
  INTEGER, ALLOCATABLE, DIMENSION(:) :: OnCustomMeters ! Forward pointer to Custom Meter Numbers
END TYPE

TYPE MeterType
  CHARACTER(len=MaxNameLength*2) :: Name        =BlankString  ! Name of the meter
  CHARACTER(len=MaxNameLength)  :: ResourceType=BlankString  ! Resource Type of the meter
  CHARACTER(len=MaxNameLength)  :: EndUse      =BlankString  ! End Use of the meter
  CHARACTER(len=MaxNameLength)  :: EndUseSub   =BlankString  ! End Use subcategory of the meter
  CHARACTER(len=MaxNameLength)  :: Group       =BlankString  ! Group of the meter
  CHARACTER(len=UnitsStringLength) :: Units       =BlankString  ! Units for the Meter
  INTEGER                       :: RT_forIPUnits=0  ! Resource type number for IP Units (tabular) reporting
  INTEGER                       :: TypeOfMeter =MeterType_Normal ! type of meter
  INTEGER                       :: SourceMeter =0  ! for custom decrement meters, this is the meter number for the subtraction
  REAL(r64)                     :: TSValue     =0.0d0  ! TimeStep Value
  REAL(r64)                     :: CurTSValue  =0.0d0  ! Current TimeStep Value (internal access)
  LOGICAL                       :: RptTS       =.false.  ! Report at End of TimeStep (Zone)
  LOGICAL                       :: RptTSFO     =.false.  ! Report at End of TimeStep (Zone) -- meter file only
  INTEGER                       :: TSRptNum    =0  ! Report Number for TS Values
  CHARACTER(len=16)             :: TSRptNumChr =BlankString  ! Report Number for TS Values (character -- for printing)
  REAL(r64)                     :: HRValue     =0.0d0  ! Hourly Value
  LOGICAL                       :: RptHR       =.false.  ! Report at End of Hour
  LOGICAL                       :: RptHRFO     =.false.  ! Report at End of Hour -- meter file only
  REAL(r64)                     :: HRMaxVal    =-99999.d0  ! Maximum Value (Hour)
  INTEGER                       :: HRMaxValDate=0  ! Date stamp of maximum
  REAL(r64)                     :: HRMinVal    =99999.d0  ! Minimum Value (Hour)
  INTEGER                       :: HRMinValDate=0  ! Date stamp of minimum
  INTEGER                       :: HRRptNum    =0  ! Report Number for HR Values
  CHARACTER(len=16)             :: HRRptNumChr =BlankString   ! Report Number for HR Values (character -- for printing)
  REAL(r64)                     :: DYValue     =0.0d0  ! Daily Value
  LOGICAL                       :: RptDY       =.false.  ! Report at End of Day
  LOGICAL                       :: RptDYFO     =.false.  ! Report at End of Day -- meter file only
  REAL(r64)                     :: DYMaxVal    =-99999.d0  ! Maximum Value (Day)
  INTEGER                       :: DYMaxValDate=0  ! Date stamp of maximum
  REAL(r64)                     :: DYMinVal    =99999.d0  ! Minimum Value (Day)
  INTEGER                       :: DYMinValDate=0  ! Date stamp of minimum
  INTEGER                       :: DYRptNum    =0  ! Report Number for DY Values
  CHARACTER(len=16)             :: DYRptNumChr =BlankString   ! Report Number for DY Values (character -- for printing)
  REAL(r64)                     :: MNValue     =0.0d0  ! Monthly Value
  LOGICAL                       :: RptMN       =.false.  ! Report at End of Month
  LOGICAL                       :: RptMNFO     =.false.  ! Report at End of Month -- meter file only
  REAL(r64)                     :: MNMaxVal    =-99999.d0  ! Maximum Value (Month)
  INTEGER                       :: MNMaxValDate=0  ! Date stamp of maximum
  REAL(r64)                     :: MNMinVal    =99999.d0  ! Minimum Value (Month)
  INTEGER                       :: MNMinValDate=0  ! Date stamp of minimum
  INTEGER                       :: MNRptNum    =0  ! Report Number for MN Values
  CHARACTER(len=16)             :: MNRptNumChr =BlankString   ! Report Number for MN Values (character -- for printing)
  REAL(r64)                     :: SMValue     =0.0d0  ! Simulation Value
  LOGICAL                       :: RptSM       =.false.  ! Report at End of Environment/Simulation
  LOGICAL                       :: RptSMFO     =.false.  ! Report at End of Environment/Simulation -- meter file only
  REAL(r64)                     :: SMMaxVal    =-99999.d0  ! Maximum Value (Sim)
  INTEGER                       :: SMMaxValDate=0  ! Date stamp of maximum
  REAL(r64)                     :: SMMinVal    =99999.d0  ! Minimum Value (Sim)
  INTEGER                       :: SMMinValDate=0  ! Date stamp of minimum
  INTEGER                       :: SMRptNum    =0  ! Report Number for SM Values
  CHARACTER(len=16)             :: SMRptNumChr =BlankString   ! Report Number for SM Values (character -- for printing)
  REAL(r64)                     :: LastSMValue     =0.0d0  ! Simulation Value
  REAL(r64)                     :: LastSMMaxVal    =-99999.d0  ! Maximum Value (Sim)
  INTEGER                       :: LastSMMaxValDate=0  ! Date stamp of maximum
  REAL(r64)                     :: LastSMMinVal    =99999.d0  ! Minimum Value (Sim)
  INTEGER                       :: LastSMMinValDate=0  ! Date stamp of minimum
  LOGICAL                       :: RptAccTS    =.false.  ! Report Cumulative Meter at Time Step
  LOGICAL                       :: RptAccTSFO  =.false.  ! Report Cumulative Meter at Time Step -- meter file only
  LOGICAL                       :: RptAccHR    =.false.  ! Report Cumulative Meter at Hour
  LOGICAL                       :: RptAccHRFO  =.false.  ! Report Cumulative Meter at Hour -- meter file only
  LOGICAL                       :: RptAccDY    =.false.  ! Report Cumulative Meter at Day
  LOGICAL                       :: RptAccDYFO  =.false.  ! Report Cumulative Meter at Day -- meter file only
  LOGICAL                       :: RptAccMN    =.false.  ! Report Cumulative Meter at Month
  LOGICAL                       :: RptAccMNFO  =.false.  ! Report Cumulative Meter at Month -- meter file only
  LOGICAL                       :: RptAccSM    =.false.  ! Report Cumulative Meter at Run Period
  LOGICAL                       :: RptAccSMFO  =.false.  ! Report Cumulative Meter at Run Period -- meter file only
  INTEGER                       :: TSAccRptNum =0  ! Report Number for Acc Values
  INTEGER                       :: HRAccRptNum =0  ! Report Number for Acc Values
  INTEGER                       :: DYAccRptNum =0  ! Report Number for Acc Values
  INTEGER                       :: MNAccRptNum =0  ! Report Number for Acc Values
  INTEGER                       :: SMAccRptNum =0  ! Report Number for Acc Values
  INTEGER                       :: InstMeterCacheStart = 0 !index of the beginning of the instant meter cache
  INTEGER                       :: InstMeterCacheEnd = 0   !index of the end of the instant meter cache
END TYPE

INTEGER :: InstMeterCacheSize = 1000    !the maximum size of the instant meter cache used in GetInstantMeterValue
INTEGER :: InstMeterCacheSizeInc = 1000 !the increment for the instant meter cache used in GetInstantMeterValue
INTEGER, ALLOCATABLE, DIMENSION(:) :: InstMeterCache !contains a list of RVariableTypes that make up a specific meter
INTEGER, ALLOCATABLE, DIMENSION(:) :: InstMeterCacheCopy !for dynamic array resizing
INTEGER :: InstMeterCacheLastUsed = 0   !the last item in the instant meter cache used

TYPE EndUseCategoryType
  CHARACTER(len=MaxNameLength)                            :: Name = BlankString ! End use category name
  CHARACTER(len=MaxNameLength)                            :: DisplayName = BlankString ! Display name for output table
  INTEGER                                                 :: NumSubcategories = 0
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SubcategoryName  ! Array of subcategory names
END TYPE

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

TYPE (TimeSteps), DIMENSION(2)        :: TimeValue  ! Pointers to the actual TimeStep variables
TYPE (RealVariableType), &
           DIMENSION(:), ALLOCATABLE  :: RVariableTypes  ! Variable Types structure (use NumOfRVariables to traverse)
TYPE (IntegerVariableType), &
           DIMENSION(:), ALLOCATABLE  :: IVariableTypes  ! Variable Types structure (use NumOfIVariables to traverse)
TYPE (VariableTypeForDDOutput),  &
           DIMENSION(:), ALLOCATABLE  :: DDVariableTypes  ! Variable Types structure (use NumVariablesForOutput to traverse)

INTEGER                               :: CurrentReportNumber =0
INTEGER                               :: NumVariablesForOutput=0
INTEGER                               :: MaxVariablesForOutput=0
INTEGER                               :: NumOfRVariable_Setup=0
INTEGER                               :: NumTotalRVariable=0
INTEGER                               :: NumOfRVariable_Sum=0
INTEGER                               :: NumOfRVariable_Meter=0
INTEGER                               :: NumOfRVariable =0
INTEGER                               :: MaxRVariable =0
INTEGER                               :: NumOfIVariable_Setup=0
INTEGER                               :: NumTotalIVariable=0
INTEGER                               :: NumOfIVariable_Sum=0
INTEGER                               :: NumOfIVariable =0
INTEGER                               :: MaxIVariable =0
LOGICAL                               :: OutputInitialized=.false.
INTEGER                               :: ProduceReportVDD=ReportVDD_No
INTEGER                               :: OutputFileRVDD=0       ! Unit number for Report Variables Data Dictionary (output)
INTEGER                               :: OutputFileMVDD=0       ! Unit number for Meter Variables Data Dictionary (output)
INTEGER                               :: OutputFileMeterDetails=0  ! Unit number for Meter Details file (output)
INTEGER                               :: NumHoursinDay=24
INTEGER                               :: NumHoursinMonth=0
INTEGER                               :: NumHoursinSim  =0
INTEGER, DIMENSION(:), ALLOCATABLE    :: ReportList
INTEGER                               :: NumReportList = 0
INTEGER                               :: NumExtraVars = 0
TYPE (RealVariables), POINTER         :: RVariable
TYPE (IntegerVariables), POINTER      :: IVariable
TYPE (RealVariables), POINTER         :: RVar
TYPE (IntegerVariables), POINTER      :: IVar
CHARACTER(len=80),DIMENSION(-1:4,1:2) :: FreqNotice !=(/'! Each Call','! TimeStep',' !Hourly',',Daily',',Monthly',',Environment'/)

TYPE (ReqReportVariables),    &
  DIMENSION(:), ALLOCATABLE           :: ReqRepVars
INTEGER                               :: NumOfReqVariables =0  ! Current number of Requested Report Variables

TYPE (MeterArrayType),    &
  DIMENSION(:), ALLOCATABLE           :: VarMeterArrays
INTEGER                               :: NumVarMeterArrays =0  ! Current number of Arrays pointing to meters

TYPE (MeterType),         &
  DIMENSION(:), ALLOCATABLE           :: EnergyMeters
INTEGER                               :: NumEnergyMeters =0  ! Current number of Energy Meters
REAL(r64), DIMENSION(:), ALLOCATABLE       :: MeterValue          ! This holds the current timestep value for each meter.

INTEGER                               :: TimeStepStampReportNbr    ! TimeStep and Hourly Report number
CHARACTER(len=3)                      :: TimeStepStampReportChr    ! TimeStep and Hourly Report number (character -- for printing)
LOGICAL                               :: TrackingHourlyVariables = .false.  ! Requested Hourly Report Variables
INTEGER                               :: DailyStampReportNbr       ! Daily Report number
CHARACTER(len=3)                      :: DailyStampReportChr       ! Daily Report number (character -- for printing)
LOGICAL                               :: TrackingDailyVariables = .false.  ! Requested Daily Report Variables
INTEGER                               :: MonthlyStampReportNbr     ! Monthly Report number
CHARACTER(len=3)                      :: MonthlyStampReportChr     ! Monthly Report number (character -- for printing)
LOGICAL                               :: TrackingMonthlyVariables = .false.  ! Requested Monthly Report Variables
INTEGER                               :: RunPeriodStampReportNbr   ! RunPeriod Report number
CHARACTER(len=3)                      :: RunPeriodStampReportChr   ! RunPeriod Report number (character -- for printing)
LOGICAL                               :: TrackingRunPeriodVariables = .false.  ! Requested RunPeriod Report Variables
REAL(r64)                             :: SecondsPerTimeStep     ! Seconds from NumTimeStepInHour
LOGICAL                               :: ErrorsLogged=.false.
LOGICAL                               :: ProduceVariableDictionary=.false.

TYPE (EndUseCategoryType), ALLOCATABLE, DIMENSION(:) :: EndUseCategory
INTEGER                               :: MaxNumSubcategories = 1

! All routines should be listed here whether private or not
PUBLIC  InitializeOutput
PUBLIC  SetupTimePointers
PUBLIC  CheckReportVariable
PRIVATE BuildKeyVarList
PRIVATE AddBlankKeys
PRIVATE GetReportVariableInput
PUBLIC  DetermineFrequency
PUBLIC  ProduceMinMaxString
PUBLIC  ProduceMinMaxStringWStartMinute
PUBLIC  ReallocateRVar
!PUBLIC  ReallocateTVar
PUBLIC  ReallocateIntegerArray
PUBLIC  ReallocateIVar
PUBLIC  ValidateIndexType
PUBLIC  StandardIndexTypeKey
PUBLIC  ValidateVariableType
PUBLIC  StandardVariableTypeKey
!!PUBLIC  SetReportNow
PUBLIC  GetVariableUnitsString
PRIVATE InitializeMeters
PUBLIC  GetCustomMeterInput
PRIVATE AddMeter
PUBLIC  AttachMeters
PRIVATE ValidateNStandardizeMeterTitles
PUBLIC  UpdateMeterValues
PUBLIC  UpdateMeters
PUBLIC  SetMinMax
PUBLIC  ReportTSMeters
PUBLIC  ReportHRMeters
PUBLIC  ReportDYMeters
PUBLIC  ReportMNMeters
PUBLIC  ReportSMMeters
PUBLIC  ReportMeterDetails
PUBLIC  AddEndUseSubcategory
PRIVATE DetermineMeterIPUnits

PUBLIC SetInternalVariableValue

! Routines tagged on the end of this module:
!  AddToOutputVariableList
!  AssignReportNumber
!  GenOutputVariablesAuditReport
!  GetCurrentMeterValue
!  GetInstantMeterValue
!  GetInternalVariableValue
!  GetInternalVariableValueExternalInterface
!  GetMeteredVariables
!  GetMeterIndex
!  GetMeterResourceType
!  GetNumMeteredVariables
!  GetVariableKeyCountandType
!  GetVariableKeys
!  InitPollutionMeterReporting
!  ProduceRDDMDD
!  ReportingThisVariable
!  SetInitialMeterReportingAndOutputNames
!  SetupIntegerOutputVariable
!  SetupRealOutputVariable
!  SetupRealOutputVariable_IntKey
!  UpdateDataandReport
!  UpdateMeterReporting


CONTAINS

SUBROUTINE InitializeOutput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the OutputProcessor data structures.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER, EXTERNAL :: GetNewUnitNumber

  ALLOCATE(RVariableTypes(RVarAllocInc))
  ALLOCATE(RVar)
  MaxRVariable=RVarAllocInc

  ALLOCATE(IVariableTypes(IVarAllocInc))
  ALLOCATE(IVar)
  MaxIVariable=IVarAllocInc

  ! First index is the frequency designation (-1 = each call, etc)
  ! Second index is the variable type (1=Average, 2=Sum)
  ! Note, Meters always report like Average (with min/max, etc) for hourly and above
  FreqNotice(-1,1)=' !Each Call'
  FreqNotice(0,1)=' !TimeStep'
  FreqNotice(1,1)=' !Hourly'
  FreqNotice(2,1)=' !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]'
  FreqNotice(3,1)=' !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]'
  FreqNotice(4,1)=' !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]'
  FreqNotice(-1,2)=' !Each Call'
  FreqNotice(0,2)=' !TimeStep'
  FreqNotice(1,2)=' !Hourly'
  FreqNotice(2,2)=' !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]'
  FreqNotice(3,2)=' !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]'
  FreqNotice(4,2)=' !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]'

  ALLOCATE(ReportList(500))
  NumReportList=500
  ReportList=0
  NumExtraVars=0

  ! Initialize end use category names - the indices must match up with endUseNames in OutputReportTabular
  ALLOCATE(EndUseCategory(NumEndUses))
  EndUseCategory(endUseHeating)%Name = 'Heating'
  EndUseCategory(endUseCooling)%Name = 'Cooling'
  EndUseCategory(endUseInteriorLights)%Name = 'InteriorLights'
  EndUseCategory(endUseExteriorLights)%Name = 'ExteriorLights'
  EndUseCategory(endUseInteriorEquipment)%Name = 'InteriorEquipment'
  EndUseCategory(endUseExteriorEquipment)%Name = 'ExteriorEquipment'
  EndUseCategory(endUseFans)%Name = 'Fans'
  EndUseCategory(endUsePumps)%Name = 'Pumps'
  EndUseCategory(endUseHeatRejection)%Name = 'HeatRejection'
  EndUseCategory(endUseHumidification)%Name = 'Humidifier'
  EndUseCategory(endUseHeatRecovery)%Name = 'HeatRecovery'
  EndUseCategory(endUseWaterSystem)%Name = 'WaterSystems'
  EndUseCategory(endUseRefrigeration)%Name = 'Refrigeration'
  EndUseCategory(endUseCogeneration)%Name = 'Cogeneration'

  ! Initialize display names for output table - this could go away if end use key names are changed to match
  EndUseCategory(endUseHeating)%DisplayName = 'Heating'
  EndUseCategory(endUseCooling)%DisplayName = 'Cooling'
  EndUseCategory(endUseInteriorLights)%DisplayName = 'Interior Lighting'
  EndUseCategory(endUseExteriorLights)%DisplayName = 'Exterior Lighting'
  EndUseCategory(endUseInteriorEquipment)%DisplayName = 'Interior Equipment'
  EndUseCategory(endUseExteriorEquipment)%DisplayName = 'Exterior Equipment'
  EndUseCategory(endUseFans)%DisplayName = 'Fans'
  EndUseCategory(endUsePumps)%DisplayName = 'Pumps'
  EndUseCategory(endUseHeatRejection)%DisplayName = 'Heat Rejection'
  EndUseCategory(endUseHumidification)%DisplayName = 'Humidification'
  EndUseCategory(endUseHeatRecovery)%DisplayName = 'Heat Recovery'
  EndUseCategory(endUseWaterSystem)%DisplayName = 'Water Systems'
  EndUseCategory(endUseRefrigeration)%DisplayName = 'Refrigeration'
  EndUseCategory(endUseCogeneration)%DisplayName = 'Generators'

  OutputInitialized=.true.

  SecondsPerTimeStep=REAL(MinutesPerTimeStep,r64)*60.0d0

  CALL InitializeMeters

  RETURN

END SUBROUTINE InitializeOutput

SUBROUTINE SetupTimePointers(IndexKey,TimeStep)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the derived type for the output processor that
          ! contains pointers to the TimeStep values used in the simulation.

          ! METHODOLOGY EMPLOYED:
          ! Indicate that the TimeStep passed in is a target for the pointer
          ! attributes in the derived types.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, Only: OutputFileStandard
  USE DataInterfaces, Only: ShowSevereError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: IndexKey  ! Which timestep is being set up, 'Zone'=1, 'HVAC'=2
  REAL(r64), TARGET, INTENT(IN)     :: TimeStep  ! The timestep variable.  Used to get the address
                                        ! for the pointer in the derived type.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   CHARACTER(len=25) :: cValue
   INTEGER          :: Index

   Index=ValidateIndexType(IndexKey,'SetupTimePointers')

   SELECT CASE (Index)

   CASE(1)
     TimeValue(Index)%TimeStep=>TimeStep
     TimeValue(Index)%CurMinute=0.0d0

   CASE(2)
     TimeValue(Index)%TimeStep=>TimeStep
     TimeValue(Index)%CurMinute=0.0d0

   CASE DEFAULT
     WRITE(cValue,*) Index
     CALL ShowSevereError('Illegal value passed to SetupTimePointers, must be 1 or 2 == '//trim(cValue),OutputFileStandard)

   END SELECT

  RETURN

END SUBROUTINE SetupTimePointers

SUBROUTINE CheckReportVariable(KeyedValue,VarName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine will get the report variable information from input and
          ! determine if this variable (KeyedValue and VariableName) should be reported
          ! and, if so, what frequency to report.

          ! This routine is called when SetupOutputVariable is called with no "optional"
          ! Reporting Frequency.  It is expected that SetupOutputVariable would only be
          ! called once for each keyed variable to be triggered for output (from the input
          ! requests).  The optional report frequency would only be used for debugging
          ! purposes.  Therefore, this routine will collect all occasions where this
          ! passed variablename would be reported from the requested input.  It builds
          ! a list of these requests (ReportList) so that the calling routine can propagate
          ! the requests into the correct data structure.

          ! METHODOLOGY EMPLOYED:
          ! This instance being requested will always have a key associated with it.  Matching
          ! instances (from input) may or may not have keys, but only one instance of a reporting
          ! frequency per variable is allowed.  ReportList will be populated with ReqRepVars indices
          ! of those extra things from input that satisfy this condition.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VarName    ! String Name of variable (without units)
  CHARACTER(len=*), INTENT(IN) :: KeyedValue ! Associated Key for this variable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag = .true.
  INTEGER Item
  INTEGER Loop
  INTEGER Pos
  INTEGER MinLook
  INTEGER MaxLook

  IF (GetInputFlag) THEN
    CALL GetReportVariableInput
    GetInputFlag=.false.
  ENDIF

  IF (NumOfReqVariables > 0) THEN
    ! Do a quick check
    Item=FindItem(VarName,ReqRepVars(1:NumOfReqVariables)%VarName,NumOfReqVariables)

    NumExtraVars=0
    ReportList=0
    MinLook= 999999999
    MaxLook=-999999999

    IF (Item /= 0) THEN
      Loop=Item
      Pos=Item
      MinLook=MIN(MinLook,Pos)
      MaxLook=MAX(MaxLook,Pos)
      DO WHILE (Loop <= NumOfReqVariables .and. Pos /= 0)
!  Mark all with blank keys as used
        IF (ReqRepVars(Loop)%Key == BlankString) THEN
          ReqRepVars(Loop)%Used=.true.
        ENDIF
        IF (Loop < NumOfReqVariables) THEN
          Pos=FindItem(VarName,ReqRepVars(Loop+1:NumOfReqVariables)%VarName,NumOfReqVariables-Loop)
          IF (Pos /= 0) THEN
            MinLook=MIN(MinLook,Loop+Pos)
            MaxLook=MAX(MaxLook,Loop+Pos)
          ENDIF
        ELSE
          Pos=1
        ENDIF
        Loop=Loop+Pos
      ENDDO
      CALL BuildKeyVarList(KeyedValue,VarName,MinLook,MaxLook)
      CALL AddBlankKeys(VarName,MinLook,MaxLook)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckReportVariable

SUBROUTINE BuildKeyVarList(KeyedValue,VariableName,MinIndx,MaxIndx)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine builds an initial list (from ReqRepVars) of
          ! pointers to that data structure for this KeyedValue and VariableName.

          ! METHODOLOGY EMPLOYED:
          ! Go through the ReqRepVars list and add those
          ! that match (and dont duplicate ones already in the list).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
  CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
  INTEGER, INTENT(IN)          :: MinIndx        ! Min number (from previous routine) for this variable
  INTEGER, INTENT(IN)          :: MaxIndx        ! Max number (from previous routine) for this variable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER Loop1
  LOGICAL Dup
  INTEGER, DIMENSION(:), ALLOCATABLE :: TmpReportList

  DO Loop=MinIndx,MaxIndx
    IF (.not. SameString(ReqRepVars(Loop)%VarName,VariableName)) CYCLE
    IF (.not. SameString(ReqRepVars(Loop)%Key,KeyedValue)) CYCLE

    !   A match.  Make sure doesnt duplicate

    ReqRepVars(Loop)%Used=.true.
    Dup=.false.
    DO Loop1=1,NumExtraVars
      IF (ReqRepVars(ReportList(Loop1))%ReportFreq == ReqRepVars(Loop)%ReportFreq) THEN
        Dup=.true.
      ELSE
        CYCLE
      ENDIF
      !  So Same Report Frequency
      IF (ReqRepVars(ReportList(Loop1))%SchedPtr /= ReqRepVars(Loop)%SchedPtr) Dup=.false.
    ENDDO

    IF (.not. Dup) THEN
      NumExtraVars=NumExtraVars+1
      IF (NumExtraVars == NumReportList) THEN
        ALLOCATE(TmpReportList(NumReportList))
        TmpReportList=0
        TmpReportList(1:NumReportList)=ReportList
        DEALLOCATE(ReportList)
        NumReportList=NumReportList+100
        ALLOCATE(ReportList(NumReportList))
        ReportList=TmpReportList
        DEALLOCATE(TmpReportList)
      ENDIF
      ReportList(NumExtraVars)=Loop
    ENDIF

  ENDDO

  RETURN

END SUBROUTINE BuildKeyVarList

SUBROUTINE AddBlankKeys(VariableName,MinIndx,MaxIndx)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine adds to the ReportList any report variables that have
          ! been requested for all keys of that report variable (if it doesnt duplicate
          ! a frequency already on the list).

          ! METHODOLOGY EMPLOYED:
          ! Go through the ReqRepVars list and add those
          ! that match (and dont duplicate ones already in the list).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
  INTEGER, INTENT(IN)          :: MinIndx        ! Min number (from previous routine) for this variable
  INTEGER, INTENT(IN)          :: MaxIndx        ! Max number (from previous routine) for this variable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER Loop1
  LOGICAL Dup
  INTEGER, DIMENSION(:), ALLOCATABLE :: TmpReportList

  DO Loop=MinIndx,MaxIndx
    IF (ReqRepVars(Loop)%Key /= BlankString) CYCLE
    IF (.not. SameString(ReqRepVars(Loop)%VarName,VariableName)) CYCLE

    !   A match.  Make sure doesnt duplicate

    Dup=.false.
    DO Loop1=1,NumExtraVars
      !IF (ReqRepVars(ReportList(Loop1))%ReportFreq == ReqRepVars(Loop)%ReportFreq) Dup=.true.
      IF (ReqRepVars(ReportList(Loop1))%ReportFreq == ReqRepVars(Loop)%ReportFreq) THEN
        Dup=.true.
      ELSE
        CYCLE
      ENDIF
      !  So Same Report Frequency
      IF (ReqRepVars(ReportList(Loop1))%SchedPtr /= ReqRepVars(Loop)%SchedPtr) Dup=.false.
    ENDDO

    IF (.not. Dup) THEN
      NumExtraVars=NumExtraVars+1
      IF (NumExtraVars == NumReportList) THEN
        ALLOCATE(TmpReportList(NumReportList))
        TmpReportList=0
        TmpReportList(1:NumReportList)=ReportList
        DEALLOCATE(ReportList)
        NumReportList=NumReportList+100
        ALLOCATE(ReportList(NumReportList))
        ReportList=TmpReportList
        DEALLOCATE(TmpReportList)
      ENDIF
      ReportList(NumExtraVars)=Loop
    ENDIF

  ENDDO

  RETURN

END SUBROUTINE AddBlankKeys

SUBROUTINE GetReportVariableInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the requested report variables from
          ! the input file.
          ! Report Variable,
          !        \memo each Report Variable command picks variables to be put onto the standard output file (.eso)
          !        \memo some variables may not be reported for every simulation
          !   A1 , \field Key_Value
          !        \note use '*' (without quotes) to apply this variable to all keys
          !   A2 , \field Variable_Name
          !   A3 , \field Reporting_Frequency
          !        \type choice
          !        \key detailed
          !        \key timestep
          !        \key hourly
          !        \key daily
          !        \key monthly
          !        \key runperiod
          !   A4 ; \field Schedule_Name
          !        \type object-list
          !        \object-list ScheduleNames

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataIPShortCuts
  USE DataGlobals, ONLY: OutputFileInits
  USE InputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE DataSystemVariables, ONLY:cMinReportFrequency,MinReportFrequency

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER NumAlpha
  INTEGER NumNumbers
  INTEGER IOStat
  INTEGER Item
  LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  CHARACTER(len=MaxNameLength), DIMENSION(4) :: cAlphaArgs
  CHARACTER(len=MaxNameLength), DIMENSION(4) :: cAlphaFieldNames
  LOGICAL, DIMENSION(4) :: lAlphaFieldBlanks
  REAL(r64), DIMENSION(1) :: rNumericArgs
  CHARACTER(len=MaxNameLength), DIMENSION(1) :: cNumericFieldNames
  LOGICAL, DIMENSION(1) :: lNumericFieldBlanks

  ! First check environment variable to see of possible override for minimum reporting frequency
  IF (cMinReportFrequency /= ' ') THEN
    CALL DetermineFrequency(cMinReportFrequency,Item)  ! Use local variable Item so as not to possibly confuse things
    MinReportFrequency=MAX(MinReportFrequency,Item)
    WRITE(OutputFileInits,800)
    WRITE(OutputFileInits,801) TRIM(FreqNotice(MinReportFrequency,1)),TRIM(cMinReportFrequency)
  ENDIF
  800 FORMAT('! <Minimum Reporting Frequency (overriding input value)>, Value, Input Value')
  801 FORMAT(' Minimum Reporting Frequency, ',A,',',A)


  cCurrentModuleObject='Output:Variable'
  NumOfReqVariables=GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(ReqRepVars(NumOfReqVariables))

  DO Loop=1,NumOfReqVariables

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ! Check for duplicates?

    ReqRepVars(Loop)%Key=cAlphaArgs(1)
    IF (ReqRepVars(Loop)%Key == '*') THEN
      ReqRepVars(Loop)%Key=BlankString
    ENDIF

    Item=INDEX(cAlphaArgs(2),'[')  ! Remove Units designation if user put it in
    IF (Item /= 0) THEN
      cAlphaArgs(2)=cAlphaArgs(2)(1:Item-1)
    ENDIF
    ReqRepVars(Loop)%VarName=cAlphaArgs(2)

    CALL DetermineFrequency(cAlphaArgs(3),ReqRepVars(Loop)%ReportFreq)

    ! Schedule information
    ReqRepVars(Loop)%SchedName=cAlphaArgs(4)
    IF (ReqRepVars(Loop)%SchedName /= '   ') THEN
        ReqRepVars(Loop)%SchedPtr=GetScheduleIndex(ReqRepVars(Loop)%SchedName)
        IF (ReqRepVars(Loop)%SchedPtr == 0) THEN
          CALL ShowSevereError('GetReportVariableInput: '//TRIM(cCurrentModuleObject)//'="'//  &
             TRIM(cAlphaArgs(1))//':'//TRIM(ReqRepVars(Loop)%VarName)//'" invalid '//  &
             TRIM(cAlphaFieldNames(4))//'="'//TRIM(ReqRepVars(Loop)%SchedName)//  &
                                  '" - not found.')
          ErrorsFound=.true.
        ENDIF
    ELSE
        ReqRepVars(Loop)%SchedPtr=0
    ENDIF

    ReqRepVars(Loop)%Used=.false.

  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetReportVariableInput:'//TRIM(cCurrentModuleObject)//': errors in input.')
  ENDIF

  RETURN

END SUBROUTINE GetReportVariableInput

SUBROUTINE DetermineFrequency(FreqString,ReportFreq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine looks at the passed in report frequency string and
          ! determines the reporting frequency.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          !       \field Reporting Frequency
          !       \type choice
          !       \key Detailed
          !       \note Detailed lists every instance (i.e. HVAC variable timesteps)
          !       \key Timestep
          !       \note Timestep refers to the zone Timestep/Number of Timesteps in hour value
          !       \note RunPeriod, Environment, and Annual are the same
          !       \key Hourly
          !       \key Daily
          !       \key Monthly
          !       \key RunPeriod
          !       \key Environment
          !       \key Annual
          !       \default Hourly
          !       \note RunPeriod, Environment, and Annual are synonymous

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataSystemVariables, ONLY: MinReportFrequency

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(IN) :: FreqString !
  INTEGER, INTENT(OUT)        :: ReportFreq

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=4), PARAMETER, DIMENSION(-1:6) :: PossibleFreq =(/'deta','time','hour','dail','mont','runp','envi','annu'/)
                            !=(/'detail','Timestep','Hourly','Daily','Monthly','RunPeriod','Environment','Annual'/)
  CHARACTER(len=*), PARAMETER, DIMENSION(-1:6) :: ExactFreqString =  &
     (/'Detailed   ',  &
       'Timestep   ',  &
       'Hourly     ',  &
       'Daily      ',  &
       'Monthly    ',  &
       'RunPeriod  ',  &
       'Environment',  &
       'Annual     '/)

  INTEGER, PARAMETER, DIMENSION(-1:6) :: FreqValues=(/-1,0,1,2,3,4,4,4/)
      ! note: runperiod, environment, and annual are synonomous

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER LenString

  ReportFreq=ReportHourly   !Default
  LenString=LEN(FreqString)
  LenString=MIN(LenString,4)

  DO Loop=-1,6
    IF (.not. SameString(FreqString(1:LenString),PossibleFreq(Loop)(1:4))) CYCLE
    IF (.not. SameString(FreqString,ExactFreqString(Loop))) THEN
      CALL ShowWarningError('DetermineFrequency: Entered frequency="'//trim(FreqString)//  &
         '" is not an exact match to key strings.')
      CALL ShowContinueError('Frequency='//trim(ExactFreqString(Loop))//' will be used.')
    ENDIF
    ReportFreq=MAX(FreqValues(Loop),MinReportFrequency)
    EXIT
  ENDDO

  RETURN

END SUBROUTINE DetermineFrequency

SUBROUTINE ProduceMinMaxString(String,DateValue,ReportFreq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces the appropriate min/max string depending
          ! on the reporting frequency.

          ! METHODOLOGY EMPLOYED:
          ! Prior to calling this routine, the basic value string will be
          ! produced, but DecodeMonDayHrMin will not have been called.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: DecodeMonDayHrMin

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(INOUT) :: String      ! Current value
  INTEGER, INTENT(IN)            :: DateValue   ! Date of min/max
  INTEGER, INTENT(IN)            :: ReportFreq  ! Reporting Frequency

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: DayFormat="(A,',',I2,',',I2)"
  CHARACTER(len=*), PARAMETER :: MonthFormat="(A,',',I2,',',I2,',',I2)"
  CHARACTER(len=*), PARAMETER :: EnvrnFormat="(A,',',I2,',',I2,',',I2,',',I2)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Mon,Day,Hour,Minute
  CHARACTER(len=40) TempString
  CHARACTER(len=40) StrOut

  TempString=ADJUSTL(String)
  CALL DecodeMonDayHrMin(DateValue,Mon,Day,Hour,Minute)

  SELECT CASE (ReportFreq)

  CASE(2)    ! Daily
    WRITE(StrOut,DayFormat) TRIM(TempString),Hour,Minute

  CASE(3)    ! Monthly
    WRITE(StrOut,MonthFormat) TRIM(TempString),Day,Hour,Minute

  CASE(4)    ! Environment
    WRITE(StrOut,EnvrnFormat) TRIM(TempString),Mon,Day,Hour,Minute

  CASE DEFAULT  ! Each, TimeStep, Hourly dont have this
    StrOut=BlankString

  END SELECT

  String=StrOut

  RETURN

END SUBROUTINE ProduceMinMaxString

SUBROUTINE ProduceMinMaxStringWStartMinute(String,DateValue,ReportFreq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces the appropriate min/max string depending
          ! on the reporting frequency.  Used in Meter reporting.

          ! METHODOLOGY EMPLOYED:
          ! Prior to calling this routine, the basic value string will be
          ! produced, but DecodeMonDayHrMin will not have been called.  Uses the MinutesPerTimeStep
          ! value to set the StartMinute.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: DecodeMonDayHrMin

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(INOUT) :: String      ! Current value
  INTEGER, INTENT(IN)            :: DateValue   ! Date of min/max
  INTEGER, INTENT(IN)            :: ReportFreq  ! Reporting Frequency

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: HrFormat="(A,',',I2.2,':',I2.2)"
  CHARACTER(len=*), PARAMETER :: DayFormat="(A,',',I2,',',I2.2,':',I2.2)"
  CHARACTER(len=*), PARAMETER :: MonthFormat="(A,',',I2,',',I2,',',I2.2,':',I2.2)"
  CHARACTER(len=*), PARAMETER :: EnvrnFormat="(A,',',I2,',',I2,',',I2,',',I2.2,':',I2.2)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Mon,Day,Hour,Minute
  INTEGER StartMinute
  CHARACTER(len=40) TempString
  CHARACTER(len=40) StrOut

  TempString=ADJUSTL(String)
  CALL DecodeMonDayHrMin(DateValue,Mon,Day,Hour,Minute)

  SELECT CASE (ReportFreq)

  CASE(1)    ! Hourly -- used in meters
    StartMinute=Minute-MinutesPerTimeStep+1
    WRITE(StrOut,HrFormat) TRIM(TempString),StartMinute,Minute

  CASE(2)    ! Daily
    StartMinute=Minute-MinutesPerTimeStep+1
    WRITE(StrOut,DayFormat) TRIM(TempString),Hour,StartMinute,Minute

  CASE(3)    ! Monthly
    StartMinute=Minute-MinutesPerTimeStep+1
    WRITE(StrOut,MonthFormat) TRIM(TempString),Day,Hour,StartMinute,Minute

  CASE(4)    ! Environment
    StartMinute=Minute-MinutesPerTimeStep+1
    WRITE(StrOut,EnvrnFormat) TRIM(TempString),Mon,Day,Hour,StartMinute,Minute

  CASE DEFAULT  ! Each, TimeStep, Hourly dont have this
    StrOut=BlankString

  END SELECT

  String=StrOut

  RETURN

END SUBROUTINE ProduceMinMaxStringWStartMinute

SUBROUTINE ReallocateIntegerArray(Array,ArrayMax,ArrayInc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reallocates (preserving data) an Integer array
          ! for the output processor.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: Array
  INTEGER, INTENT(INOUT)                            :: ArrayMax  ! Current and resultant dimension for Array
  INTEGER, INTENT(IN)                               :: ArrayInc  ! increment for redimension

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(:), ALLOCATABLE    :: NewArray

  ALLOCATE(NewArray(ArrayMax+ArrayInc))
  NewArray=0

  IF (ArrayMax > 0) THEN
    NewArray(1:ArrayMax)=Array(1:ArrayMax)
  ENDIF
  DEALLOCATE(Array)
  ArrayMax=ArrayMax+ArrayInc
  ALLOCATE(Array(ArrayMax))
  Array=NewArray
  DEALLOCATE(NewArray)

  RETURN

END SUBROUTINE ReallocateIntegerArray

SUBROUTINE ReallocateRVar

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reallocates (preserving data) the Real Variable
          ! structure for the output processor.

          ! METHODOLOGY EMPLOYED:
          ! Using the current value of MaxRVariable, this routine allocates
          ! the new dimension for the arrays xxx, xxxx, and RealVariables structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (RealVariableType), &
           DIMENSION(:), ALLOCATABLE    :: Types

  ALLOCATE(Types(MaxRVariable+RVarAllocInc))

  IF (MaxRVariable > 0) THEN
    Types(1:MaxRVariable)=RVariableTypes(1:MaxRVariable)
  ENDIF
  DEALLOCATE(RVariableTypes)
  MaxRVariable=MaxRVariable+RVarAllocInc
  ALLOCATE(RVariableTypes(MaxRVariable))
  RVariableTypes=Types
  DEALLOCATE(Types)

  RETURN

END SUBROUTINE ReallocateRVar

SUBROUTINE ReallocateIVar

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs a "reallocate" on the Integer Report
          ! Variable structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (IntegerVariableType), &
           DIMENSION(:), ALLOCATABLE    :: Types

  ALLOCATE(Types(MaxIVariable+IVarAllocInc))

  IF (MaxIVariable > 0) THEN
    Types(1:MaxIVariable)=IVariableTypes(1:MaxIVariable)
  ENDIF
  DEALLOCATE(IVariableTypes)
  MaxIVariable=MaxIVariable+IVarAllocInc
  ALLOCATE(IVariableTypes(MaxIVariable))
  IVariableTypes=Types
  DEALLOCATE(Types)

  RETURN

END SUBROUTINE ReallocateIVar

INTEGER FUNCTION ValidateIndexType(IndexTypeKey,CalledFrom)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function validates the requested "index" type and returns
          ! the proper value for use inside the OutputProcessor.

          ! METHODOLOGY EMPLOYED:
          ! Look it up in a list of valid index types.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindIteminList, MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: IndexTypeKey  ! Index type (Zone, HVAC) for variables
  CHARACTER(len=*), INTENT(IN) :: CalledFrom    ! Routine called from (for error messages)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=12), SAVE, DIMENSION(3) :: ZoneIndexTypes
  CHARACTER(len=12), SAVE, DIMENSION(3) :: SystemIndexTypes
  LOGICAL,SAVE                    :: Initialized =.false.
  INTEGER Item

  IF (.not. Initialized) THEN
    ZoneIndexTypes(1)='ZONE'
    ZoneIndexTypes(2)='HEATBALANCE'
    ZoneIndexTypes(3)='HEAT BALANCE'
    SystemIndexTypes(1)='HVAC'
    SystemIndexTypes(2)='SYSTEM'
    SystemIndexTypes(3)='PLANT'
    Initialized=.true.
  ENDIF

  ValidateIndexType=1
  Item=FindIteminList(MakeUPPERCase(IndexTypeKey),ZoneIndexTypes,3)
  IF (Item /= 0) RETURN

  ValidateIndexType=2
  Item=FindIteminList(MakeUPPERCase(IndexTypeKey),SystemIndexTypes,3)
  IF (Item /= 0) RETURN

  ValidateIndexType=0
  !  The following should never happen to a user!!!!
  CALL ShowSevereError('OutputProcessor/ValidateIndexType: Invalid Index Key passed to ValidateIndexType='  &
                              //TRIM(IndexTypeKey))
  CALL ShowContinueError('..Should be "ZONE", "SYSTEM", "HVAC"... was called from:'//TRIM(CalledFrom))
  CALL ShowFatalError('Preceding condition causes termination.')

  RETURN

END FUNCTION ValidateIndexType

CHARACTER(len=4) FUNCTION StandardIndexTypeKey(IndexType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function gives the standard string for the index type
          ! given.

          ! METHODOLOGY EMPLOYED:
          ! Look it up in a list of valid index types.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IndexType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  SELECT CASE (IndexType)

  CASE (1)
    StandardIndexTypeKey='Zone'

  CASE (2)
    StandardIndexTypeKey='HVAC'

  CASE DEFAULT
    StandardIndexTypeKey='UNKW'

  END SELECT

  RETURN

END FUNCTION StandardIndexTypeKey

INTEGER FUNCTION ValidateVariableType(VariableTypeKey)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function validates the VariableTypeKey passed to the SetupVariable
          ! routine and assigns it the value used in the OutputProcessor.

          ! METHODOLOGY EMPLOYED:
          ! Look it up in a list of valid variable types.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindIteminList, MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VariableTypeKey

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=8), DIMENSION(3) :: StateVariables
  CHARACTER(len=9), DIMENSION(4) :: NonStateVariables
  LOGICAL,SAVE                   :: Initialized =.false.
  INTEGER Item

  IF (.not. Initialized) THEN
    StateVariables(1)='STATE'
    StateVariables(2)='AVERAGE'
    StateVariables(3)='AVERAGED'
    NonStateVariables(1)='NON STATE'
    NonStateVariables(2)='NONSTATE'
    NonStateVariables(3)='SUM'
    NonStateVariables(4)='SUMMED'
  ENDIF

  ValidateVariableType=1
  Item=FindIteminList(MakeUPPERCase(VariableTypeKey),StateVariables,3)
  IF (Item /= 0) RETURN

  ValidateVariableType=2
  Item=FindIteminList(MakeUPPERCase(VariableTypeKey),NonStateVariables,4)
  IF (Item /= 0) RETURN

  CALL ShowSevereError('Invalid variable type requested='//VariableTypeKey)
  ValidateVariableType=0

  RETURN

END FUNCTION ValidateVariableType

CHARACTER(len=9) FUNCTION StandardVariableTypeKey(VariableType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function gives the standard string for the variable type
          ! given.

          ! METHODOLOGY EMPLOYED:
          ! From variable type value, produce proper string.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VariableType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  SELECT CASE (VariableType)

  CASE (1)
    StandardVariableTypeKey='Average'

  CASE (2)
    StandardVariableTypeKey='Sum'

  CASE DEFAULT
    StandardVariableTypeKey='Unknown'

  END SELECT

  RETURN

END FUNCTION StandardVariableTypeKey

FUNCTION GetVariableUnitsString(VariableName) RESULT(ThisUnitsString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function extracts the units from a Variable Name string supplied by
          ! the developer in the call to SetupOutputVariable(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)      :: VariableName
  CHARACTER(len=UnitsStringLength)  :: ThisUnitsString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER lbpos  ! position of the left bracket
  INTEGER rbpos  ! position of the right bracket

  ! Units are marked with a [

  lbpos=INDEX(VariableName,'[',.true.)  ! from end of variable name

!!! Errors here are fatal because should only be encountered during development.
  ThisUnitsString=BlankString
  if (lbpos > 0) then
    rbpos=INDEX(VariableName,']',.true.)
    if (rbpos == 0 .or. rbpos < lbpos) then
      Call ShowFatalError('Ill formed Variable Name Units String, VariableName='//TRIM(VariableName))
      ThisUnitsString=VariableName(lbpos:)
    else
      if ((rbpos-1)-(lbpos+1)+1 > UnitsStringLength) then
        Call ShowFatalError('Units String too long for VariableName='//TRIM(VariableName)//   &
           '; will be truncated to '//TrimSigDigits(UnitsStringLength)//' characters.')
      endif
      if (lbpos+1 <= rbpos-1) then
        ThisUnitsString=VariableName(lbpos+1:rbpos-1)
      else
        ThisUnitsString=BlankString
      endif
    endif
  endif

  RETURN

END FUNCTION GetVariableUnitsString

! *****************************************************************************
! The following routines implement Energy Meters in EnergyPlus.
! *****************************************************************************

SUBROUTINE InitializeMeters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates the set of meters in EnergyPlus.  In this initial
          ! implementation, it is a static set of meters.

          ! METHODOLOGY EMPLOYED:
          ! Allocate the static set.  Use "AddMeter" with appropriate arguments that will
          ! allow expansion later.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER,EXTERNAL :: GetNewUnitNumber
  INTEGER :: write_stat

  OutputFileMeterDetails=GetNewUnitNumber()
  OPEN(UNIT=OutputFileMeterDetails,FILE='eplusout.mtd', Action='write', iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL ShowFatalError('InitializeMeters: Could not open file "eplusout.mtd" for output (write).')
  ENDIF

  RETURN

END SUBROUTINE InitializeMeters

SUBROUTINE GetCustomMeterInput(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will help implement "custom"/user defined meters.  However, it must be called after all
          ! the other meters are set up and all report variables are established.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Processes the objects:
          ! Meter:Custom,
          !    \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
          !    \memo Used to allow users to combine specific variables and/or meters into
          !    \memo "custom" meter configurations.
          !    A1,  \field Name
          !         \required-field
          !         \reference CustomMeterNames
          !    A2,  \field Fuel Type
          !         \type choice
          !         \key Electricity
          !         \key NaturalGas
          !         \key PropaneGas
          !         \key FuelOil#1
          !         \key FuelOil#2
          !         \key Coal
          !         \key Diesel
          !         \key Gasoline
          !         \key Water
          !         \key Generic
          !         \key OtherFuel1
          !         \key OtherFuel2
          !    A3,  \field Key Name 1
          !         \required-field
          !         \begin-extensible
          !    A4,  \field Report Variable or Meter Name 1
          !         \required-field
          ! <etc>
          ! AND
          ! Meter:CustomDecrement,
          !    \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
          !    \memo Used to allow users to combine specific variables and/or meters into
          !    \memo "custom" meter configurations.
          !    A1,  \field Name
          !         \required-field
          !         \reference CustomMeterNames
          !    A2,  \field Fuel Type
          !         \type choice
          !         \key Electricity
          !         \key NaturalGas
          !         \key PropaneGas
          !         \key FuelOil#1
          !         \key FuelOil#2
          !         \key Coal
          !         \key Diesel
          !         \key Gasoline
          !         \key Water
          !         \key Generic
          !         \key OtherFuel1
          !         \key OtherFuel2
          !    A3,  \field Source Meter Name
          !         \required-field
          !    A4,  \field Key Name 1
          !         \required-field
          !         \begin-extensible
          !    A5,  \field Report Variable or Meter Name 1
          !         \required-field
          ! <etc>

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE DataInterfaces, ONLY:GetVariableKeyCountandType, GetVariableKeys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumAlpha
  INTEGER NumNumbers
  INTEGER Loop
  INTEGER IOStat
  INTEGER NumCustomMeters
  INTEGER NumCustomDecMeters
  LOGICAL IsNotOK
  LOGICAL IsBlank
  INTEGER fldIndex
  LOGICAL KeyIsStar
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
  INTEGER, DIMENSION(:) , ALLOCATABLE                     :: IndexesForKeyVar ! Array index
  CHARACTER(len=MaxNameLength) :: UnitsVar     ! Units sting, may be blank
  CHARACTER(len=MaxNameLength) :: MeterUnits   ! Units sting, may be blank
  INTEGER          :: KeyCount
  INTEGER          :: TypeVar
  INTEGER          :: AvgSumVar
  INTEGER          :: StepTypeVar
  INTEGER          :: iKey
  INTEGER          :: iKey1
  LOGICAL          :: MeterCreated
  INTEGER, DIMENSION(:), ALLOCATABLE :: VarsOnCustomMeter
  INTEGER, DIMENSION(:), ALLOCATABLE :: TempVarsOnCustomMeter
  INTEGER          :: MaxVarsOnCustomMeter
  INTEGER          :: NumVarsOnCustomMeter
  INTEGER, DIMENSION(:), ALLOCATABLE :: VarsOnSourceMeter
  INTEGER, DIMENSION(:), ALLOCATABLE :: TempVarsOnSourceMeter
  INTEGER          :: MaxVarsOnSourceMeter
  INTEGER          :: NumVarsOnSourceMeter
  INTEGER          :: iOnMeter
  INTEGER          :: WhichMeter
  LOGICAL          :: ErrFlag
  LOGICAL          :: BigErrorsFound
  LOGICAL          :: testa
  LOGICAL          :: testb
  LOGICAL          :: Tagged ! variable is appropriate to put on meter
  INTEGER          :: lbrackPos


  BigErrorsFound=.false.

  cCurrentModuleObject='Meter:Custom'
  NumCustomMeters=GetNumObjectsFound(cCurrentModuleObject)

  DO Loop=1,NumCustomMeters
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    lbrackPos=INDEX(cAlphaArgs(1),'[')
    IF (lbrackPos /= 0) cAlphaArgs(1)=cAlphaArgs(1)(1:lbrackPos-1)
    MeterCreated=.false.
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),EnergyMeters%Name,NumEnergyMeters,IsNotOK,IsBlank,'Meter Names')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    IF (ALLOCATED(VarsOnCustomMeter)) DEALLOCATE(VarsOnCustomMeter)
    ALLOCATE(VarsOnCustomMeter(1000))
    VarsOnCustomMeter=0
    MaxVarsOnCustomMeter=1000
    NumVarsOnCustomMeter=0
    DO fldIndex=3,NumAlpha,2
      IF (cAlphaArgs(fldIndex) == '*' .or. lAlphaFieldBlanks(fldIndex)) THEN
        KeyIsStar=.true.
        cAlphaArgs(fldIndex)='*'
      ELSE
        KeyIsStar=.false.
      ENDIF
      IF (lAlphaFieldBlanks(fldIndex+1)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", blank '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'.')
        CALL ShowContinueError('...cannot create custom meter.')
        BigErrorsFound=.true.
        CYCLE
      ENDIF
      IF (BigErrorsFound) CYCLE
      ! Don't build/check things out if there were errors anywhere.  Use "GetVariableKeys" to map to actual variables...
      lbrackPos=INDEX(cAlphaArgs(fldIndex+1),'[')
      IF (lbrackPos /= 0) cAlphaArgs(fldIndex+1)=cAlphaArgs(fldIndex+1)(1:lbrackPos-1)
      Tagged=.false.
      CALL GetVariableKeyCountandType(cAlphaArgs(fldIndex+1),KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
      IF (TypeVar == VarType_NotFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
        CALL ShowContinueError('...will not be shown with the Meter results.')
        CYCLE
      ENDIF
      IF (.not. MeterCreated) THEN
        MeterUnits=UnitsVar  ! meter units are same as first variable on custom meter
        CALL AddMeter(cAlphaArgs(1),UnitsVar,BlankString,BlankString,BlankString,BlankString)
        EnergyMeters(NumEnergyMeters)%TypeOfMeter=MeterType_Custom
        ! Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
        CALL GetStandardMeterResourceType(EnergyMeters(NumEnergyMeters)%ResourceType,MakeUPPERCase(cAlphaArgs(2)),ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('..on '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'".')
          BigErrorsFound=.true.
        ENDIF
        CALL DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%RT_forIPUnits,EnergyMeters(NumEnergyMeters)%ResourceType,  &
           UnitsVar,ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('..on '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'".')
          CALL ShowContinueError('..requests for IP units from this meter will be ignored.')
        ENDIF
!        EnergyMeters(NumEnergyMeters)%RT_forIPUnits=DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%ResourceType,UnitsVar)
        MeterCreated=.true.
      ENDIF
      IF (UnitsVar /= MeterUnits) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", differing units in '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
        CALL ShowContinueError('...will not be shown with the Meter results; units for meter='//TRIM(MeterUnits)//  &
                               ', units for this variable='//TRIM(UnitsVar)//'.')
        CYCLE
      ENDIF
      IF ((TypeVar == VarType_Real .or. TypeVar == VarType_Integer) .and. AvgSumVar == SummedVar) THEN
        Tagged=.true.
        ALLOCATE(NamesOfKeys(KeyCount))
        ALLOCATE(IndexesForKeyVar(KeyCount))
        CALL GetVariableKeys(cAlphaArgs(fldIndex+1),TypeVar,NamesOfKeys,IndexesForKeyVar)
        iOnMeter=0
        IF (KeyIsStar) THEN
          DO iKey = 1, KeyCount
            NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
            IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
              MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
              ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
              TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
              TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
              DEALLOCATE(VarsOnCustomMeter)
              ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
              VarsOnCustomMeter=TempVarsOnCustomMeter
              DEALLOCATE(TempVarsOnCustomMeter)
            ENDIF
            VarsOnCustomMeter(NumVarsOnCustomMeter)=IndexesForKeyVar(iKey)
            iOnMeter=1
          ENDDO
          IF (iOnMeter == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid (all keys) '//  &
               TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
            ErrorsFound=.true.
          ENDIF
        ELSE  ! Key is not "*"
          DO iKey = 1, KeyCount
            IF (NamesOfKeys(iKey) /= cAlphaArgs(fldIndex)) CYCLE
            NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
            IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
              MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
              ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
              TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
              TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
              DEALLOCATE(VarsOnCustomMeter)
              ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
              VarsOnCustomMeter=TempVarsOnCustomMeter
              DEALLOCATE(TempVarsOnCustomMeter)
            ENDIF
            VarsOnCustomMeter(NumVarsOnCustomMeter)=IndexesForKeyVar(iKey)
            iOnMeter=1
          ENDDO
          IF (iOnMeter == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid '//  &
                TRIM(cAlphaArgs(fldIndex))//':'//TRIM(cAlphaArgs(fldIndex+1)))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        DEALLOCATE(NamesOfKeys)
        DEALLOCATE(IndexesForKeyVar)
      ENDIF
      IF (TypeVar == VarType_Meter .and. AvgSumVar == SummedVar) THEN
        Tagged=.true.
        ALLOCATE(NamesOfKeys(KeyCount))
        ALLOCATE(IndexesForKeyVar(KeyCount))
        CALL GetVariableKeys(cAlphaArgs(fldIndex+1),TypeVar,NamesOfKeys,IndexesForKeyVar)
        WhichMeter=IndexesForKeyVar(1)
        DEALLOCATE(NamesOfKeys)
        DEALLOCATE(IndexesForKeyVar)
        ! for meters there will only be one key...  but it has variables associated...
        DO iOnMeter=1,NumVarMeterArrays
          IF (.not. ANY(VarMeterArrays(iOnMeter)%OnMeters == WhichMeter) ) CYCLE
          NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
          IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
            MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
            ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
            TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
            TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
            DEALLOCATE(VarsOnCustomMeter)
            ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
            VarsOnCustomMeter=TempVarsOnCustomMeter
            DEALLOCATE(TempVarsOnCustomMeter)
          ENDIF
          VarsOnCustomMeter(NumVarsOnCustomMeter)=VarMeterArrays(iOnMeter)%RepVariable
        ENDDO
      ENDIF
      IF (.not. Tagged) THEN  ! couldn't find place for this item on a meter
        IF (AvgSumVar /= SummedVar) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", variable not summed variable '//  &
             TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
          CALL ShowContinueError('...will not be shown with the Meter results; units for meter='//TRIM(MeterUnits)//  &
                                 ', units for this variable='//TRIM(UnitsVar)//'.')
        ENDIF
      ENDIF
    ENDDO
    ! Check for duplicates
    DO iKey=1,NumVarsOnCustomMeter
      IF (VarsOnCustomMeter(iKey) == 0) CYCLE
      DO iKey1=iKey+1,NumVarsOnCustomMeter
        IF (iKey == iKey1) CYCLE
        IF (VarsOnCustomMeter(iKey) /= VarsOnCustomMeter(iKey1)) CYCLE
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", duplicate name="'//  &
             TRIM(RVariableTypes(VarsOnCustomMeter(iKey1))%VarName)//'".')
          CALL ShowContinueError('...only one value with this name will be shown with the Meter results.')
          VarsOnCustomMeter(iKey1)=0
      ENDDO
    ENDDO
    DO iKey=1,NumVarsOnCustomMeter
      IF (VarsOnCustomMeter(iKey) == 0) CYCLE
      RVariable=>RVariableTypes(VarsOnCustomMeter(iKey))%Varptr
      CALL AttachCustomMeters(MeterUnits,VarsOnCustomMeter(iKey),RVariable%MeterArrayPtr,  &
                                NumEnergyMeters,ErrorsFound)
    ENDDO
    IF (NumVarsOnCustomMeter == 0) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", no items assigned ')
      CALL ShowContinueError('...will not be shown with the Meter results')
    ENDIF

  ENDDO

  cCurrentModuleObject='Meter:CustomDecrement'
  NumCustomDecMeters=GetNumObjectsFound(cCurrentModuleObject)

  DO Loop=1,NumCustomDecMeters
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    lbrackPos=INDEX(cAlphaArgs(1),'[')
    IF (lbrackPos /= 0) cAlphaArgs(1)=cAlphaArgs(1)(1:lbrackPos-1)
    MeterCreated=.false.
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),EnergyMeters%Name,NumEnergyMeters,IsNotOK,IsBlank,'Meter Names')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    IF (ALLOCATED(VarsOnCustomMeter)) DEALLOCATE(VarsOnCustomMeter)
    ALLOCATE(VarsOnCustomMeter(1000))
    VarsOnCustomMeter=0
    MaxVarsOnCustomMeter=1000
    NumVarsOnCustomMeter=0

    lbrackPos=INDEX(cAlphaArgs(3),'[')
    IF (lbrackPos /= 0) cAlphaArgs(1)=cAlphaArgs(3)(1:lbrackPos-1)
    WhichMeter=FindItem(cAlphaArgs(3),EnergyMeters%Name,NumEnergyMeters)
    IF (WhichMeter == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid '//  &
        TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    !  Set up array of Vars that are on the source meter (for later validation).
    IF (ALLOCATED(VarsOnSourceMeter)) DEALLOCATE(VarsOnSourceMeter)
    ALLOCATE(VarsOnSourceMeter(1000))
    VarsOnSourceMeter=0
    MaxVarsOnSourceMeter=1000
    NumVarsOnSourceMeter=0
    DO iKey=1,NumVarMeterArrays
      IF (VarMeterArrays(iKey)%NumOnMeters == 0 .and. VarMeterArrays(iKey)%NumOnCustomMeters == 0) CYCLE
      !  On a meter
      IF (ANY(VarMeterArrays(iKey)%OnMeters==WhichMeter)) THEN
        NumVarsOnSourceMeter=NumVarsOnSourceMeter+1
        IF (NumVarsOnSourceMeter > MaxVarsOnSourceMeter) THEN
          MaxVarsOnSourceMeter=MaxVarsOnSourceMeter+100
          ALLOCATE(TempVarsOnSourceMeter(MaxVarsOnSourceMeter))
          TempVarsOnSourceMeter(1:MaxVarsOnSourceMeter-100)=VarsOnSourceMeter
          TempVarsOnSourceMeter(MaxVarsOnSourceMeter-100+1:MaxVarsOnSourceMeter)=0
          DEALLOCATE(VarsOnSourceMeter)
          ALLOCATE(VarsOnSourceMeter(MaxVarsOnSourceMeter))
          VarsOnSourceMeter=TempVarsOnSourceMeter
          DEALLOCATE(TempVarsOnSourceMeter)
        ENDIF
        VarsOnSourceMeter(NumVarsOnSourceMeter)=VarMeterArrays(iKey)%RepVariable
        CYCLE
      ENDIF
      IF (VarMeterArrays(iKey)%NumOnCustomMeters == 0) CYCLE
      IF (ANY(VarMeterArrays(iKey)%OnCustomMeters==WhichMeter)) THEN
        NumVarsOnSourceMeter=NumVarsOnSourceMeter+1
        IF (NumVarsOnSourceMeter > MaxVarsOnSourceMeter) THEN
          MaxVarsOnSourceMeter=MaxVarsOnSourceMeter+100
          ALLOCATE(TempVarsOnSourceMeter(MaxVarsOnSourceMeter))
          TempVarsOnSourceMeter(1:MaxVarsOnSourceMeter-100)=VarsOnSourceMeter
          TempVarsOnSourceMeter(MaxVarsOnSourceMeter-100+1:MaxVarsOnSourceMeter)=0
          DEALLOCATE(VarsOnSourceMeter)
          ALLOCATE(VarsOnSourceMeter(MaxVarsOnSourceMeter))
          VarsOnSourceMeter=TempVarsOnSourceMeter
          DEALLOCATE(TempVarsOnSourceMeter)
        ENDIF
        VarsOnSourceMeter(NumVarsOnSourceMeter)=VarMeterArrays(iKey)%RepVariable
        CYCLE
      ENDIF
    ENDDO

    DO fldIndex=4,NumAlpha,2
      IF (cAlphaArgs(fldIndex) == '*' .or. lAlphaFieldBlanks(fldIndex)) THEN
        KeyIsStar=.true.
        cAlphaArgs(fldIndex)='*'
      ELSE
        KeyIsStar=.false.
      ENDIF
      IF (lAlphaFieldBlanks(fldIndex+1)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", blank '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'.')
        CALL ShowContinueError('...cannot create custom meter.')
        BigErrorsFound=.true.
        CYCLE
      ENDIF
      IF (BigErrorsFound) CYCLE
      Tagged=.false.
      lbrackPos=INDEX(cAlphaArgs(fldIndex+1),'[')
      IF (lbrackPos /= 0) cAlphaArgs(fldIndex+1)=cAlphaArgs(fldIndex+1)(1:lbrackPos-1)
      ! Don't build/check things out if there were errors anywhere.  Use "GetVariableKeys" to map to actual variables...
      CALL GetVariableKeyCountandType(cAlphaArgs(fldIndex+1),KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
      IF (TypeVar == VarType_NotFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
        CALL ShowContinueError('...will not be shown with the Meter results.')
        CYCLE
      ENDIF
      IF (.not. MeterCreated) THEN
        MeterUnits=UnitsVar
        CALL AddMeter(cAlphaArgs(1),UnitsVar,BlankString,BlankString,BlankString,BlankString)
        EnergyMeters(NumEnergyMeters)%TypeOfMeter=MeterType_CustomDec
        EnergyMeters(NumEnergyMeters)%SourceMeter=WhichMeter

        ! Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
        CALL GetStandardMeterResourceType(EnergyMeters(NumEnergyMeters)%ResourceType,MakeUPPERCase(cAlphaArgs(2)),ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('..on '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'".')
          BigErrorsFound=.true.
        ENDIF
        CALL DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%RT_forIPUnits,EnergyMeters(NumEnergyMeters)%ResourceType,  &
           UnitsVar,ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('..on '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'".')
          CALL ShowContinueError('..requests for IP units from this meter will be ignored.')
        ENDIF
!        EnergyMeters(NumEnergyMeters)%RT_forIPUnits=DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%ResourceType,UnitsVar)
        MeterCreated=.true.
      ENDIF
      IF (UnitsVar /= MeterUnits) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", differing units in '//  &
           TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
        CALL ShowContinueError('...will not be shown with the Meter results; units for meter='//TRIM(MeterUnits)//  &
                               ', units for this variable='//TRIM(UnitsVar)//'.')
        CYCLE
      ENDIF
      IF ((TypeVar == VarType_Real .or. TypeVar == VarType_Integer) .and. AvgSumVar == SummedVar) THEN
        Tagged=.true.
        ALLOCATE(NamesOfKeys(KeyCount))
        ALLOCATE(IndexesForKeyVar(KeyCount))
        CALL GetVariableKeys(cAlphaArgs(fldIndex+1),TypeVar,NamesOfKeys,IndexesForKeyVar)
        iOnMeter=0
        IF (KeyIsStar) THEN
          DO iKey = 1, KeyCount
            NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
            IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
              MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
              ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
              TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
              TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
              DEALLOCATE(VarsOnCustomMeter)
              ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
              VarsOnCustomMeter=TempVarsOnCustomMeter
              DEALLOCATE(TempVarsOnCustomMeter)
            ENDIF
            VarsOnCustomMeter(NumVarsOnCustomMeter)=IndexesForKeyVar(iKey)
            iOnMeter=1
          ENDDO
          IF (iOnMeter == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid (all keys) '//  &
               TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
            ErrorsFound=.true.
          ENDIF
        ELSE
          DO iKey = 1, KeyCount
            IF (NamesOfKeys(iKey) /= cAlphaArgs(fldIndex)) CYCLE
            NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
            IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
              MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
              ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
              TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
              TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
              DEALLOCATE(VarsOnCustomMeter)
              ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
              VarsOnCustomMeter=TempVarsOnCustomMeter
              DEALLOCATE(TempVarsOnCustomMeter)
            ENDIF
            VarsOnCustomMeter(NumVarsOnCustomMeter)=IndexesForKeyVar(iKey)
            iOnMeter=1
          ENDDO
          IF (iOnMeter == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid '//  &
                TRIM(cAlphaArgs(fldIndex))//':'//TRIM(cAlphaArgs(fldIndex+1)))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        DEALLOCATE(NamesOfKeys)
        DEALLOCATE(IndexesForKeyVar)
      ENDIF
      IF (TypeVar == VarType_Meter .and. AvgSumVar == SummedVar) THEN
        Tagged=.true.
        ALLOCATE(NamesOfKeys(KeyCount))
        ALLOCATE(IndexesForKeyVar(KeyCount))
        CALL GetVariableKeys(cAlphaArgs(fldIndex+1),TypeVar,NamesOfKeys,IndexesForKeyVar)
        WhichMeter=IndexesForKeyVar(1)
        DEALLOCATE(NamesOfKeys)
        DEALLOCATE(IndexesForKeyVar)
        ! for meters there will only be one key...  but it has variables associated...
        DO iOnMeter=1,NumVarMeterArrays
          testa = ANY(VarMeterArrays(iOnMeter)%OnMeters == WhichMeter)
          testb = .false.
          IF (VarMeterArrays(iOnMeter)%NumOnCustomMeters > 0) THEN
            testb = ANY(VarMeterArrays(iOnMeter)%OnCustomMeters == WhichMeter)
          ENDIF
          IF (.not. (testa .or. testb) ) CYCLE
          NumVarsOnCustomMeter=NumVarsOnCustomMeter+1
          IF (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) THEN
            MaxVarsOnCustomMeter=MaxVarsOnCustomMeter+100
            ALLOCATE(TempVarsOnCustomMeter(MaxVarsOnCustomMeter))
            TempVarsOnCustomMeter(1:MaxVarsOnCustomMeter-100)=VarsOnCustomMeter
            TempVarsOnCustomMeter(MaxVarsOnCustomMeter-100+1:MaxVarsOnCustomMeter)=0
            DEALLOCATE(VarsOnCustomMeter)
            ALLOCATE(VarsOnCustomMeter(MaxVarsOnCustomMeter))
            VarsOnCustomMeter=TempVarsOnCustomMeter
            DEALLOCATE(TempVarsOnCustomMeter)
          ENDIF
          VarsOnCustomMeter(NumVarsOnCustomMeter)=VarMeterArrays(iOnMeter)%RepVariable
        ENDDO
      ENDIF
      IF (.not. Tagged) THEN  ! couldn't find place for this item on a meter
        IF (AvgSumVar /= SummedVar) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", variable not summed variable '//  &
             TRIM(cAlphaFieldNames(fldIndex+1))//'="'//TRIM(cAlphaArgs(fldIndex+1))//'".')
          CALL ShowContinueError('...will not be shown with the Meter results; units for meter='//TRIM(MeterUnits)//  &
                                 ', units for this variable='//TRIM(UnitsVar)//'.')
        ENDIF
      ENDIF
    ENDDO
    ! Check for duplicates
    DO iKey=1,NumVarsOnCustomMeter
      IF (VarsOnCustomMeter(iKey) == 0) CYCLE
      DO iKey1=iKey+1,NumVarsOnCustomMeter
        IF (iKey == iKey1) CYCLE
        IF (VarsOnCustomMeter(iKey) /= VarsOnCustomMeter(iKey1)) CYCLE
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", duplicate name="'//  &
             TRIM(RVariableTypes(VarsOnCustomMeter(iKey1))%VarName)//'".')
          CALL ShowContinueError('...only one value with this name will be shown with the Meter results.')
          VarsOnCustomMeter(iKey1)=0
      ENDDO
    ENDDO
    DO iKey=1,NumVarsOnCustomMeter
      IF (VarsOnCustomMeter(iKey) == 0) CYCLE
      RVariable=>RVariableTypes(VarsOnCustomMeter(iKey))%Varptr
      CALL AttachCustomMeters(MeterUnits,VarsOnCustomMeter(iKey),RVariable%MeterArrayPtr,  &
                                NumEnergyMeters,ErrorsFound)
    ENDDO

    ErrFlag=.false.
    DO iKey=1,NumVarsOnCustomMeter
      DO iKey1=1,NumVarsOnSourceMeter
        IF (ANY(VarsOnSourceMeter==VarsOnCustomMeter(iKey))) EXIT
        IF (.not. ErrFlag) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid specification to '//  &
             TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
          ErrFlag=.true.
        ENDIF
        CALL ShowContinueError('..Variable='//TRIM(RVariableTypes(VarsOnCustomMeter(iKey))%VarName))
        ErrorsFound=.true.
        EXIT
      ENDDO
    ENDDO
    IF (NumVarsOnCustomMeter == 0) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", no items assigned ')
      CALL ShowContinueError('...will not be shown with the Meter results')
    ENDIF

    DEALLOCATE(VarsOnCustomMeter)
    DEALLOCATE(VarsOnSourceMeter)
  ENDDO

  IF (BigErrorsFound) ErrorsFound=.true.

  RETURN

END SUBROUTINE GetCustomMeterInput

SUBROUTINE GetStandardMeterResourceType(OutResourceType,UserInputResourceType,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine compares the user input resource type with valid ones and returns
          ! the standard resource type.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(OUT) :: OutResourceType
  CHARACTER(len=*), INTENT(IN)  :: UserInputResourceType
  LOGICAL, INTENT(OUT)          :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ErrorsFound=.false.

      !!!! Basic ResourceType for Meters
  SELECT CASE (MakeUPPERCase(UserInputResourceType))

    CASE ('ELECTRICITY','ELECTRIC','ELEC')
      OutResourceType='Electricity'

    CASE ('GAS','NATURALGAS','NATURAL GAS')
      OutResourceType='Gas'

    CASE ('GASOLINE')
      OutResourceType='Gasoline'

    CASE ('DIESEL')
      OutResourceType='Diesel'

    CASE ('COAL')
      OutResourceType='Coal'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
      OutResourceType='FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
      OutResourceType='FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
      OutResourceType='Propane'

    CASE ('WATER','H2O')
      OutResourceType='Water' ! this is water "use"

    CASE ('ONSITEWATER', 'WATERPRODUCED', 'ONSITE WATER')
      OutResourceType='OnSiteWater' ! these are for supply record keeping

    CASE ('MAINSWATER', 'WATERSUPPLY')
      OutResourceType='MainsWater' ! record keeping

    CASE ('RAINWATER', 'PRECIPITATION')
      OutResourceType='RainWater' ! record keeping

    CASE ('WELLWATER', 'GROUNDWATER')
      OutResourceType='WellWater' ! record keeping

    CASE ('CONDENSATE')
      OutResourceType='Condensate' ! record keeping

    CASE ('ENERGYTRANSFER','ENERGYXFER','XFER')
      OutResourceType='EnergyTransfer'

    CASE ('STEAM')
      OutResourceType='Steam'

    CASE ('DISTRICTCOOLING')
      OutResourceType='DistrictCooling'

    CASE ('DISTRICTHEATING')
      OutResourceType='DistrictHeating'

    CASE ('ELECTRICITYPRODUCED')
      OutResourceType='ElectricityProduced'

    CASE ('ELECTRICITYPURCHASED')
      OutResourceType='ElectricityPurchased'

    CASE ('ELECTRICITYSURPLUSSOLD')
      OutResourceType='ElectricitySurplusSold'

    CASE ('ELECTRICITYNET')
      OutResourceType='ElectricityNet'

    CASE ('SOLARWATER')
      OutResourceType='SolarWater'

    CASE ('SOLARAIR')
      OutResourceType='SolarAir'

    CASE ('SO2')
      OutResourceType='SO2'

    CASE ('NOX')
      OutResourceType='NOx'

    CASE ('N2O')
      OutResourceType='N2O'

    CASE ('PM')
      OutResourceType='PM'

    CASE ('PM2.5')
      OutResourceType='PM2.5'

    CASE ('PM10')
      OutResourceType='PM10'

    CASE ('CO')
      OutResourceType='CO'

    CASE ('CO2')
      OutResourceType='CO2'

    CASE ('CH4')
      OutResourceType='CH4'

    CASE ('NH3')
      OutResourceType='NH3'

    CASE ('NMVOC')
      OutResourceType='NMVOC'

    CASE ('HG')
      OutResourceType='Hg'

    CASE ('PB')
      OutResourceType='Pb'

    CASE ('NUCLEAR HIGH')
      OutResourceType='Nuclear High'

    CASE ('NUCLEAR LOW')
      OutResourceType='Nuclear Low'

    CASE ('WATERENVIRONMENTALFACTORS')
      OutResourceType='WaterEnvironmentalFactors'

    CASE ('CARBON EQUIVALENT')
      OutResourceType='Carbon Equivalent'

    CASE ('SOURCE')
      OutResourceType='Source'

    CASE ('PLANTLOOPHEATINGDEMAND')
      OutResourceType='PlantLoopHeatingDemand'

    CASE ('PLANTLOOPCOOLINGDEMAND')
      OutResourceType='PlantLoopCoolingDemand'

    CASE ('GENERIC') ! only used by custom meters
      OutResourceType='Generic'

    CASE ('OTHERFUEL1') ! other fuel type (defined by user)
      OutResourceType='OtherFuel1'

    CASE ('OTHERFUEL2') ! other fuel type (defined by user)
      OutResourceType='OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError('GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered='//  &
         TRIM(UserInputResourceType))
      ErrorsFound=.true.

  END SELECT

  RETURN

END SUBROUTINE GetStandardMeterResourceType

SUBROUTINE AddMeter(Name,MtrUnits,ResourceType,EndUse,EndUseSub,Group)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine adds a meter to the current definition set of meters.  If the maximum has
          ! already been reached, a reallocation procedure begins.  This action needs to be done at the
          ! start of the simulation, primarily before any output is stored.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Name         ! Name for the meter
  CHARACTER(len=*), INTENT(IN) :: MtrUnits     ! Units for the meter
  CHARACTER(len=*), INTENT(IN) :: ResourceType ! ResourceType for the meter
  CHARACTER(len=*), INTENT(IN) :: EndUse       ! EndUse for the meter
  CHARACTER(len=*), INTENT(IN) :: EndUseSub    ! EndUse subcategory for the meter
  CHARACTER(len=*), INTENT(IN) :: Group        ! Group for the meter

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE (MeterType),         &
    DIMENSION(:), ALLOCATABLE           :: TempMeters

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found
  LOGICAL :: ErrFlag

  ! Make sure this isn't already in the list of meter names

  IF (NumEnergyMeters > 0) THEN
    Found=FindItemInList(Name,EnergyMeters%Name,NumEnergyMeters)
  ELSE
    Found=0
  ENDIF

  IF (Found == 0) THEN
    IF (NumEnergyMeters > 0) THEN
      ALLOCATE(TempMeters(NumEnergyMeters))
      TempMeters(1:NumEnergyMeters)=EnergyMeters
      DEALLOCATE(EnergyMeters)
    ENDIF
    ALLOCATE(EnergyMeters(NumEnergyMeters+1))
    IF (NumEnergyMeters >0) THEN
      EnergyMeters(1:NumEnergyMeters)=TempMeters
      DEALLOCATE(TempMeters)
    ENDIF
    NumEnergyMeters=NumEnergyMeters+1
    EnergyMeters(NumEnergyMeters)%Name=Name
    EnergyMeters(NumEnergyMeters)%ResourceType=ResourceType
    EnergyMeters(NumEnergyMeters)%EndUse=EndUse
    EnergyMeters(NumEnergyMeters)%EndUseSub=EndUseSub
    EnergyMeters(NumEnergyMeters)%Group=Group
    EnergyMeters(NumEnergyMeters)%Units=MtrUnits
    EnergyMeters(NumEnergyMeters)%TSValue=0.0d0
    EnergyMeters(NumEnergyMeters)%CurTSValue=0.0d0
    EnergyMeters(NumEnergyMeters)%RptTS  =.false.
    EnergyMeters(NumEnergyMeters)%RptTSFO=.false.
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%TSRptNum)
    WRITE(EnergyMeters(NumEnergyMeters)%TSRptNumChr,*) EnergyMeters(NumEnergyMeters)%TSRptNum
    EnergyMeters(NumEnergyMeters)%TSRptNumChr=ADJUSTL(EnergyMeters(NumEnergyMeters)%TSRptNumChr)
    EnergyMeters(NumEnergyMeters)%HRValue=0.0d0
    EnergyMeters(NumEnergyMeters)%HRMaxVal=MaxSetValue
    EnergyMeters(NumEnergyMeters)%HRMaxValDate=0
    EnergyMeters(NumEnergyMeters)%HRMinVal=MinSetValue
    EnergyMeters(NumEnergyMeters)%HRMinValDate=0
    EnergyMeters(NumEnergyMeters)%RptHR  =.false.
    EnergyMeters(NumEnergyMeters)%RptHRFO=.false.
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%HRRptNum)
    WRITE(EnergyMeters(NumEnergyMeters)%HRRptNumChr,*) EnergyMeters(NumEnergyMeters)%HRRptNum
    EnergyMeters(NumEnergyMeters)%HRRptNumChr=ADJUSTL(EnergyMeters(NumEnergyMeters)%HRRptNumChr)
    EnergyMeters(NumEnergyMeters)%DYValue=0.0d0
    EnergyMeters(NumEnergyMeters)%DYMaxVal=MaxSetValue
    EnergyMeters(NumEnergyMeters)%DYMaxValDate=0
    EnergyMeters(NumEnergyMeters)%DYMinVal=MinSetValue
    EnergyMeters(NumEnergyMeters)%DYMinValDate=0
    EnergyMeters(NumEnergyMeters)%RptDY  =.false.
    EnergyMeters(NumEnergyMeters)%RptDYFO=.false.
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%DYRptNum)
    WRITE(EnergyMeters(NumEnergyMeters)%DYRptNumChr,*) EnergyMeters(NumEnergyMeters)%DYRptNum
    EnergyMeters(NumEnergyMeters)%DYRptNumChr=ADJUSTL(EnergyMeters(NumEnergyMeters)%DYRptNumChr)
    EnergyMeters(NumEnergyMeters)%MNValue=0.0d0
    EnergyMeters(NumEnergyMeters)%MNMaxVal=MaxSetValue
    EnergyMeters(NumEnergyMeters)%MNMaxValDate=0
    EnergyMeters(NumEnergyMeters)%MNMinVal=MinSetValue
    EnergyMeters(NumEnergyMeters)%MNMinValDate=0
    EnergyMeters(NumEnergyMeters)%RptMN  =.false.
    EnergyMeters(NumEnergyMeters)%RptMNFO=.false.
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%MNRptNum)
    WRITE(EnergyMeters(NumEnergyMeters)%MNRptNumChr,*) EnergyMeters(NumEnergyMeters)%MNRptNum
    EnergyMeters(NumEnergyMeters)%MNRptNumChr=ADJUSTL(EnergyMeters(NumEnergyMeters)%MNRptNumChr)
    EnergyMeters(NumEnergyMeters)%SMValue=0.0d0
    EnergyMeters(NumEnergyMeters)%SMMaxVal=MaxSetValue
    EnergyMeters(NumEnergyMeters)%SMMaxValDate=0
    EnergyMeters(NumEnergyMeters)%SMMinVal=MinSetValue
    EnergyMeters(NumEnergyMeters)%SMMinValDate=0
    EnergyMeters(NumEnergyMeters)%RptSM  =.false.
    EnergyMeters(NumEnergyMeters)%RptSMFO=.false.
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%SMRptNum)
    WRITE(EnergyMeters(NumEnergyMeters)%SMRptNumChr,*) EnergyMeters(NumEnergyMeters)%SMRptNum
    EnergyMeters(NumEnergyMeters)%SMRptNumChr=ADJUSTL(EnergyMeters(NumEnergyMeters)%SMRptNumChr)
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%TSAccRptNum)
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%HRAccRptNum)
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%DYAccRptNum)
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%MNAccRptNum)
    CALL AssignReportNumber(EnergyMeters(NumEnergyMeters)%SMAccRptNum)
  ELSE
    CALL ShowFatalError('Requested to Add Meter which was already present='//TRIM(Name))
  ENDIF
  IF (ResourceType /= blankstring) THEN
    CALL DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%RT_forIPUnits,ResourceType,MtrUnits,ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('..on Meter="'//TRIM(Name)//'".')
      CALL ShowContinueError('..requests for IP units from this meter will be ignored.')
    ENDIF
!    EnergyMeters(NumEnergyMeters)%RT_forIPUnits=DetermineMeterIPUnits(ResourceType,MtrUnits)
  ENDIF
!  write(outputfiledebug,'(A)') 'add meter=NM='//trim(Name)//'; '//  &
!     'RS='//trim(ResourceType)//'; EU='//trim(EndUse)//'; EUS='//  &
!        trim(EndUseSub)//'; GP='//trim(Group)//'; UT='//trim(MtrUnits)



  RETURN

END SUBROUTINE AddMeter

SUBROUTINE AttachMeters(MtrUnits,ResourceType,EndUse,EndUseSub,Group,ZoneName,RepVarNum,MeterArrayPtr,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines which meters this variable will be on (if any),
          ! sets up the meter pointer arrays, and returns a index value to this array which
          ! is stored with the variable.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: MtrUnits      ! Units for this meter
  CHARACTER(len=*), INTENT(INOUT) :: ResourceType  ! Electricity, Gas, etc.
  CHARACTER(len=*), INTENT(INOUT) :: EndUse        ! End-use category (Lights, Heating, etc.)
  CHARACTER(len=*), INTENT(INOUT) :: EndUseSub     ! End-use subcategory (user-defined, e.g., General Lights, Task Lights, etc.)
  CHARACTER(len=*), INTENT(INOUT) :: Group         ! Group key (Facility, Zone, Building, etc.)
  CHARACTER(len=*), INTENT(IN)    :: ZoneName      ! Zone key only applicable for Building group
  INTEGER, INTENT(IN)             :: RepVarNum     ! Number of this report variable
  INTEGER, INTENT(OUT)            :: MeterArrayPtr ! Output set of Pointers to Meters
  LOGICAL, INTENT(INOUT)          :: ErrorsFound   ! True if errors in this call

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (MeterArrayType), DIMENSION(:), ALLOCATABLE :: TempMeterArrays
  INTEGER Found

  IF (SameString(Group,'Building')) THEN
    CALL ValidateNStandardizeMeterTitles(MtrUnits,ResourceType,EndUse,EndUseSub,Group,ErrorsFound,ZoneName)
  ELSE
    CALL ValidateNStandardizeMeterTitles(MtrUnits,ResourceType,EndUse,EndUseSub,Group,ErrorsFound)
  ENDIF

  IF (NumVarMeterArrays > 0) THEN
    ALLOCATE(TempMeterArrays(NumVarMeterArrays))
    TempMeterArrays(1:NumVarMeterArrays)=VarMeterArrays
    DEALLOCATE(VarMeterArrays)
  ENDIF
  ALLOCATE(VarMeterArrays(NumVarMeterArrays+1))
  IF (NumVarMeterArrays >0) THEN
    VarMeterArrays(1:NumVarMeterArrays)=TempMeterArrays
    DEALLOCATE(TempMeterArrays)
  ENDIF
  NumVarMeterArrays=NumVarMeterArrays+1
  MeterArrayPtr=NumVarMeterArrays
  VarMeterArrays(NumVarMeterArrays)%NumOnMeters=0
  VarMeterArrays(NumVarMeterArrays)%RepVariable=RepVarNum
  VarMeterArrays(NumVarMeterArrays)%OnMeters=0
  Found=FindItem(TRIM(ResourceType)//':Facility',EnergyMeters%Name,NumEnergyMeters)
  IF (Found /= 0) THEN
    VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
    VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
  ENDIF
  IF (Group /= BlankString) THEN
    Found=FindItem(TRIM(ResourceType)//':'//TRIM(Group),EnergyMeters%Name,NumEnergyMeters)
    IF (Found /= 0) THEN
      VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
      VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
    ENDIF
    IF (SameString(Group,'Building')) THEN   ! Match to Zone
      Found=FindItem(TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),EnergyMeters%Name,NumEnergyMeters)
      IF (Found /= 0) THEN
        VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
        VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
      ENDIF
    ENDIF
  ENDIF

!!! Following if EndUse is by ResourceType
  IF (EndUse /= BlankString) THEN
    Found=FindItem(TRIM(EndUse)//':'//TRIM(ResourceType),EnergyMeters%Name,NumEnergyMeters)
    IF (Found /= 0) THEN
      VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
      VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
    ENDIF
    IF (SameString(Group,'Building')) THEN   ! Match to Zone
      Found=FindItem(TRIM(EndUse)//':'//TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),EnergyMeters%Name,NumEnergyMeters)
      IF (Found /= 0) THEN
        VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
        VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
      ENDIF
    ENDIF

    ! End use subcategory
    IF (EndUseSub /= BlankString) THEN
      Found=FindItem(TRIM(EndUseSub)//':'//TRIM(EndUse)//':'//TRIM(ResourceType),EnergyMeters%Name,NumEnergyMeters)
      IF (Found /= 0) THEN
        VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
        VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found

        CALL AddEndUseSubcategory(ResourceType, EndUse, EndUseSub)
      ENDIF
      IF (SameString(Group,'Building')) THEN   ! Match to Zone
        Found=FindItem(TRIM(EndUseSub)//':'//TRIM(EndUse)//':'//TRIM(ResourceType)//':Zone:'//TRIM(ZoneName), &
          EnergyMeters%Name,NumEnergyMeters)
        IF (Found /= 0) THEN
          VarMeterArrays(NumVarMeterArrays)%NumOnMeters=VarMeterArrays(NumVarMeterArrays)%NumOnMeters+1
          VarMeterArrays(NumVarMeterArrays)%OnMeters(VarMeterArrays(NumVarMeterArrays)%NumOnMeters)=Found
        ENDIF
      ENDIF
    ENDIF

  ENDIF

  RETURN

END SUBROUTINE AttachMeters

SUBROUTINE AttachCustomMeters(MtrUnits,RepVarNum,MeterArrayPtr,MeterIndex,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines which meters this variable will be on (if any),
          ! sets up the meter pointer arrays, and returns a index value to this array which
          ! is stored with the variable.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: MtrUnits      ! Units for this meter
  INTEGER, INTENT(IN)             :: RepVarNum     ! Number of this report variable
  INTEGER, INTENT(INOUT)          :: MeterArrayPtr ! Input/Output set of Pointers to Meters
  INTEGER, INTENT(IN)             :: MeterIndex    ! Which meter this is
  LOGICAL, INTENT(INOUT)          :: ErrorsFound   ! True if errors in this call

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (MeterArrayType), DIMENSION(:), ALLOCATABLE :: TempMeterArrays
  INTEGER, DIMENSION(:), ALLOCATABLE :: TempOnCustomMeters

  IF (MeterArrayPtr == 0) THEN
    IF (NumVarMeterArrays > 0) THEN
      ALLOCATE(TempMeterArrays(NumVarMeterArrays))
      TempMeterArrays(1:NumVarMeterArrays)=VarMeterArrays
      DEALLOCATE(VarMeterArrays)
    ENDIF
    ALLOCATE(VarMeterArrays(NumVarMeterArrays+1))
    IF (NumVarMeterArrays >0) THEN
      VarMeterArrays(1:NumVarMeterArrays)=TempMeterArrays
      DEALLOCATE(TempMeterArrays)
    ENDIF
    NumVarMeterArrays=NumVarMeterArrays+1
    MeterArrayPtr=NumVarMeterArrays
    VarMeterArrays(NumVarMeterArrays)%NumOnMeters=0
    VarMeterArrays(NumVarMeterArrays)%RepVariable=RepVarNum
    VarMeterArrays(NumVarMeterArrays)%OnMeters=0
    ALLOCATE(VarMeterArrays(NumVarMeterArrays)%OnCustomMeters(1))
    VarMeterArrays(NumVarMeterArrays)%NumOnCustomMeters=1
    VarMeterArrays(NumVarMeterArrays)%OnCustomMeters(VarMeterArrays(NumVarMeterArrays)%NumOnCustomMeters)=MeterIndex
  ELSE
    ! MeterArrayPtr set
    IF (VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters > 0) THEN
      ALLOCATE(TempOnCustomMeters(VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters+1))
      TempOnCustomMeters(1:VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters)=VarMeterArrays(MeterArrayPtr)%OnCustomMeters
      DEALLOCATE(VarMeterArrays(MeterArrayPtr)%OnCustomMeters)
      VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters=VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters+1
      ALLOCATE(VarMeterArrays(MeterArrayPtr)%OnCustomMeters(VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters))
      VarMeterArrays(MeterArrayPtr)%OnCustomMeters=TempOnCustomMeters
      VarMeterArrays(MeterArrayPtr)%OnCustomMeters(VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters)=MeterIndex
      DEALLOCATE(TempOnCustomMeters)
    ELSE
      ALLOCATE(VarMeterArrays(MeterArrayPtr)%OnCustomMeters(1))
      VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters=1
      VarMeterArrays(MeterArrayPtr)%OnCustomMeters(VarMeterArrays(MeterArrayPtr)%NumOnCustomMeters)=MeterIndex
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE AttachCustomMeters

SUBROUTINE ValidateNStandardizeMeterTitles(MtrUnits,ResourceType,EndUse,EndUseSub,Group,ErrorsFound,ZoneName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine uses the keys for the Energy Meters given to the SetupOutputVariable routines
          ! and makes sure they are "standard" as well as creating meters which need to be added as this
          ! is the first use of that kind of meter designation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase, FindItem
  USE DataHeatBalance, ONLY: Zone

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: MtrUnits      ! Units for the meter
  CHARACTER(len=*), INTENT(INOUT) :: ResourceType  ! Electricity, Gas, etc.
  CHARACTER(len=*), INTENT(INOUT) :: EndUse        ! End Use Type (Lights, Heating, etc.)
  CHARACTER(len=*), INTENT(INOUT) :: EndUseSub     ! End Use Sub Type (General Lights, Task Lights, etc.)
  CHARACTER(len=*), INTENT(INOUT) :: Group         ! Group key (Facility, Zone, Building, etc.)
  LOGICAL, INTENT(INOUT)          :: ErrorsFound   ! True if errors in this call
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneName ! ZoneName when Group=Building

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: Found     ! For checking whether meter is already defined
  LOGICAL          :: LocalErrorsFound
  CHARACTER(len=MaxNameLength*2) :: MeterName

  LocalErrorsFound=.false.
!!!! Basic ResourceType Meters
  CALL GetStandardMeterResourceType(ResourceType,MakeUPPERCase(ResourceType),LocalErrorsFound)

  IF (.not. LocalErrorsFound) THEN
    IF (NumEnergyMeters > 0) THEN
      Found=FindItem(TRIM(ResourceType)//':Facility',EnergyMeters%Name,NumEnergyMeters)
    ELSE
      Found=0
    ENDIF
    IF (Found == 0) CALL AddMeter(TRIM(ResourceType)//':Facility',MtrUnits,ResourceType,' ',' ',' ')
  ENDIF

!!!!  Group Meters
  SELECT CASE (MakeUPPERCase(Group))

  CASE (BlankString)

  CASE ('BUILDING')
    Group='Building'

  CASE ('HVAC','SYSTEM')
    Group='HVAC'

  CASE ('PLANT')
    Group='Plant'

  CASE DEFAULT
    CALL ShowSevereError('Illegal Group (for Meters) Entered='//TRIM(Group))
    LocalErrorsFound=.true.

  END SELECT

  IF (.not. LocalErrorsFound .and. Group /= BlankString) THEN
    Found=FindItem(TRIM(ResourceType)//':'//TRIM(Group),EnergyMeters%Name,NumEnergyMeters)
    IF (Found == 0) CALL AddMeter(TRIM(ResourceType)//':'//TRIM(Group),MtrUnits,ResourceType,' ',' ',Group)
    IF (Group == 'Building') THEN
      Found=FindItem(TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),EnergyMeters%Name,NumEnergyMeters)
      IF (Found == 0) THEN
        CALL AddMeter(TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),MtrUnits,ResourceType,' ',' ','Zone')
      ENDIF
    ENDIF
  ENDIF

!!!! EndUse Meters
  SELECT CASE (MakeUPPERCase(EndUse))

  CASE (BlankString)

  CASE ('INTERIOR LIGHTS','INTERIORLIGHTS')
    EndUse='InteriorLights'

  CASE ('EXTERIOR LIGHTS','EXTERIORLIGHTS')
    EndUse='ExteriorLights'

  CASE ('HEATING','HTG')
    EndUse='Heating'

  CASE ('HEATPRODUCED')
    EndUse='HeatProduced'

  CASE ('COOLING','CLG')
    EndUse='Cooling'

  CASE ('DOMESTICHOTWATER','DHW','DOMESTIC HOT WATER')
    EndUse='WaterSystems'

  CASE ('COGEN','COGENERATION')
    EndUse='Cogeneration'

  CASE ('INTERIOREQUIPMENT','INTERIOR EQUIPMENT')
    EndUse='InteriorEquipment'

  CASE ('EXTERIOREQUIPMENT','EXTERIOR EQUIPMENT','EXT EQ','EXTERIOREQ')
    EndUse='ExteriorEquipment'

  CASE ('EXTERIOR:WATEREQUIPMENT')
    EndUse='ExteriorEquipment'

  CASE ('PURCHASEDHOTWATER','DISTRICTHOTWATER','PURCHASED HEATING')
    EndUse='DistrictHotWater'

  CASE ('PURCHASEDCOLDWATER','DISTRICTCHILLEDWATER','PURCHASEDCHILLEDWATER','PURCHASED COLD WATER','PURCHASED COOLING')
    EndUse='DistrictChilledWater'

  CASE ('FANS','FAN')
    EndUse='Fans'

  CASE ('HEATINGCOILS','HEATINGCOIL','HEATING COILS','HEATING COIL')
    EndUse='HeatingCoils'

  CASE ('COOLINGCOILS','COOLINGCOIL','COOLING COILS','COOLING COIL')
    EndUse='CoolingCoils'

  CASE ('PUMPS','PUMP')
    EndUse='Pumps'

  CASE ('FREECOOLING','FREE COOLING')
    EndUse='Freecooling'

  CASE ('LOOPTOLOOP')
    EndUse='LoopToLoop'

  CASE ('CHILLERS','CHILLER')
    EndUse='Chillers'

  CASE ('BOILERS','BOILER')
    EndUse='Boilers'

  CASE ('BASEBOARD','BASEBOARDS')
    EndUse='Baseboard'

  CASE ('HEATREJECTION','HEAT REJECTION')
    EndUse='HeatRejection'

  CASE ('HUMIDIFIER','HUMIDIFIERS')
    EndUse='Humidifier'

  CASE ('HEATRECOVERY','HEAT RECOVERY')
    EndUse='HeatRecovery'

  CASE ('PHOTOVOLTAICS','PV','PHOTOVOLTAIC')
    EndUse='Photovoltaic'

  CASE ('WINDTURBINES','WT','WINDTURBINE')
    EndUse='WindTurbine'

  CASE ('HEAT RECOVERY FOR COOLING','HEATRECOVERYFORCOOLING','HEATRECOVERYCOOLING')
    EndUse='HeatRecoveryForCooling'

  CASE ('HEAT RECOVERY FOR HEATING','HEATRECOVERYFORHEATING','HEATRECOVERYHEATING')
    EndUse='HeatRecoveryForHeating'

  CASE ('ELECTRICEMISSIONS')
    EndUse='ElectricEmissions'

  CASE ('PURCHASEDELECTRICEMISSIONS')
    EndUse='PurchasedElectricEmissions'

  CASE ('SOLDELECTRICEMISSIONS')
    EndUse='SoldElectricEmissions'

  CASE ('NATURALGASEMISSIONS')
    EndUse='NaturalGasEmissions'

  CASE ('FUELOIL#1EMISSIONS')
    EndUse='FuelOil#1Emissions'

  CASE ('FUELOIL#2EMISSIONS')
    EndUse='FuelOil#2Emissions'

  CASE ('COALEMISSIONS')
    EndUse='CoalEmissions'

  CASE ('GASOLINEEMISSIONS')
    EndUse='GasolineEmissions'

  CASE ('PROPANEEMISSIONS')
    EndUse='PropaneEmissions'

  CASE ('DIESELEMISSIONS')
    EndUse='DieselEmissions'

  CASE ('OTHERFUEL1EMISSIONS')
    EndUse='OtherFuel1Emissions'

  CASE ('OTHERFUEL2EMISSIONS')
    EndUse='OtherFuel2Emissions'

  CASE ('CARBONEQUIVALENTEMISSIONS')
    EndUse='CarbonEquivalentEmissions'

  CASE ('REFRIGERATION')
    EndUse='Refrigeration'

  CASE ('COLDSTORAGECHARGE')
    EndUse='ColdStorageCharge'

  CASE ('COLDSTORAGEDISCHARGE')
    EndUse='ColdStorageDischarge'

  CASE ('WATERSYSTEMS' , 'WATERSYSTEM', 'Water System')
    EndUse='WaterSystems'

  CASE ('RAINWATER')
    EndUse = 'Rainwater'

  Case ('CONDENSATE')
    EndUse = 'Condensate'

  Case ('WELLWATER')
    EndUse = 'Wellwater'

  Case ('MAINSWATER', 'PURCHASEDWATER')
    EndUse = 'MainsWater'

  CASE DEFAULT
    CALL ShowSevereError('Illegal EndUse (for Meters) Entered='//TRIM(EndUse))
    LocalErrorsFound=.true.

  END SELECT

!!! Following if we do EndUse by ResourceType
  IF (.not. LocalErrorsFound .and. EndUse /= BlankString) THEN
    Found=FindItem(TRIM(EndUse)//':'//TRIM(ResourceType),EnergyMeters%Name,NumEnergyMeters)
    IF (Found == 0) CALL AddMeter(TRIM(EndUse)//':'//TRIM(ResourceType),MtrUnits,ResourceType,EndUse,' ',' ')

    IF (Group == 'Building') THEN   ! Match to Zone
      Found=FindItem(TRIM(EndUse)//':'//TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),EnergyMeters%Name,NumEnergyMeters)
      IF (Found == 0) THEN
        CALL AddMeter(TRIM(EndUse)//':'//TRIM(ResourceType)//':Zone:'//TRIM(ZoneName),MtrUnits,ResourceType,EndUse,' ','Zone')
      ENDIF
    ENDIF
  ELSEIF (LocalErrorsFound) THEN
    ErrorsFound=.true.
  ENDIF

  ! End-Use Subcategories
  IF (.not. LocalErrorsFound .and. EndUseSub /= BlankString) THEN
    MeterName = TRIM(EndUseSub)//':'//TRIM(EndUse)//':'//TRIM(ResourceType)
    Found = FindItem(MeterName,EnergyMeters%Name,NumEnergyMeters)
    IF (Found == 0) CALL AddMeter(MeterName,MtrUnits,ResourceType,EndUse,EndUseSub,' ')
  ELSEIF (LocalErrorsFound) THEN
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE ValidateNStandardizeMeterTitles

SUBROUTINE DetermineMeterIPUnits(CodeForIPUnits,ResourceType,MtrUnits,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       September 2012; made into subroutine
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! In order to set up tabular reports for IP units, need to search on same strings
          ! that tabular reports does for IP conversion.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! OutputReportTabular looks for:
          ! CONSUMP - not used in meters
          ! ELEC - Electricity (kWH)
          ! GAS - Gas (therm)
          ! COOL - Cooling (ton)
          ! and we need to add WATER (for m3/gal, etc)

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase, SameString
!  USE DataGlobals, ONLY: outputfiledebug

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT)       :: CodeForIPUnits  ! Output Code for IP Units
  CHARACTER(len=*), INTENT(IN) :: ResourceType ! Resource Type
  CHARACTER(len=*), INTENT(IN) :: MtrUnits  ! Meter units
  LOGICAL, INTENT(INOUT)       :: ErrorsFound ! true if errors found during subroutine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: UC_ResourceType

  ErrorsFound=.false.
  UC_ResourceType=MakeUPPERCase(ResourceType)

  CodeForIPUnits=RT_IPUnits_OtherJ
  IF (INDEX(UC_ResourceType,'ELEC') > 0) THEN
    CodeForIPUnits=RT_IPUnits_Electricity
  ELSEIF (INDEX(UC_ResourceType,'GAS') > 0) THEN
    CodeForIPUnits=RT_IPUnits_Gas
  ELSEIF (INDEX(UC_ResourceType,'COOL') > 0) THEN
    CodeForIPUnits=RT_IPUnits_Cooling
  ENDIF
  IF (SameString(MtrUnits,'m3') .and. INDEX(UC_ResourceType,'WATER') > 0) THEN
    CodeForIPUnits=RT_IPUnits_Water
  ELSEIF (SameString(MtrUnits,'m3')) THEN
    CodeForIPUnits=RT_IPUnits_OtherM3
  ENDIF
  IF (SameString(MtrUnits,'kg')) THEN
    CodeForIPUnits=RT_IPUnits_OtherKG
  ENDIF
  IF (SameString(MtrUnits,'L')) THEN
    CodeForIPUnits=RT_IPUnits_OtherL
  ENDIF
!  write(outputfiledebug,*) 'resourcetype=',trim(resourcetype)
!  write(outputfiledebug,*) 'ipunits type=',CodeForIPUnits
  IF (.not. SameString(MtrUnits,'kg') .and. .not. SameString(MtrUnits,'J') .and.   &
      .not. SameString(MtrUnits,'m3') .and. .not. SameString(MtrUnits,'L')) THEN
    CALL ShowWarningError('DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=['//  &
       trim(MtrUnits)//'].')
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE DetermineMeterIPUnits

SUBROUTINE UpdateMeterValues(TimeStepValue,NumOnMeters,OnMeters,NumOnCustomMeters,OnCustomMeters)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates all the meter values in the lists with the current
          ! time step value for this variable.

          ! METHODOLOGY EMPLOYED:
          ! Variables, as they are "setup", may or may not be on one or more meters.
          ! All "metered" variables are on the "facility meter".  Index values will be
          ! set from the variables to the appropriate meters.  Then, the updating of
          ! the meter values is quite simple -- just add the time step value of the variable
          ! (which is passed to this routine) to all the values being kept for the meter.
          ! Reporting of the meters is taken care of in a different routine.  During reporting,
          ! some values will also be reset (for example, after reporting the "hour", the new
          ! "hour" value of the meter is reset to 0.0, etc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                  :: TimeStepValue  ! Value of this variable at the current time step.
  INTEGER, INTENT(IN)               :: NumOnMeters    ! Number of meters this variable is "on".
  INTEGER, INTENT(IN), DIMENSION(:) :: OnMeters       ! Which meters this variable is on (index values)
  INTEGER, INTENT(IN), OPTIONAL               :: NumOnCustomMeters  ! Number of custom meters this variable is "on".
  INTEGER, INTENT(IN), DIMENSION(:), OPTIONAL :: OnCustomMeters       ! Which custom meters this variable is on (index values)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Meter   ! Loop Control
  INTEGER Which   ! Index value for the meter


  DO Meter=1,NumOnMeters
    Which=OnMeters(Meter)
    MeterValue(Which)=MeterValue(Which)+TimeStepValue
  ENDDO

  ! This calculates the basic values for decrement/difference meters -- UpdateMeters then calculates the actual.
  IF (PRESENT(NumOnCustomMeters)) THEN
    DO Meter=1,NumOnCustomMeters
      Which=OnCustomMeters(Meter)
      MeterValue(Which)=MeterValue(Which)+TimeStepValue
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE UpdateMeterValues

SUBROUTINE UpdateMeters(TimeStamp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the meters with the current time step value
          ! for each meter.  Also, sets min/max values for hourly...run period reporting.

          ! METHODOLOGY EMPLOYED:
          ! Goes thru the number of meters, setting min/max as appropriate.  Uses timestamp
          ! from calling program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)               :: TimeStamp      ! Current TimeStamp (for max/min)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Meter   ! Loop Control

  DO Meter=1,NumEnergyMeters
    IF (EnergyMeters(Meter)%TypeOfMeter /= MeterType_CustomDec .and.   &
        EnergyMeters(Meter)%TypeOfMeter /= MeterType_CustomDiff) THEN
      EnergyMeters(Meter)%TSValue=EnergyMeters(Meter)%TSValue+MeterValue(Meter)
      EnergyMeters(Meter)%HRValue=EnergyMeters(Meter)%HRValue+MeterValue(Meter)
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%HRMaxVal,EnergyMeters(Meter)%HRMaxValDate,  &
                                             EnergyMeters(Meter)%HRMinVal,EnergyMeters(Meter)%HRMinValDate)
      EnergyMeters(Meter)%DYValue=EnergyMeters(Meter)%DYValue+MeterValue(Meter)
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%DYMaxVal,EnergyMeters(Meter)%DYMaxValDate,  &
                                             EnergyMeters(Meter)%DYMinVal,EnergyMeters(Meter)%DYMinValDate)
      EnergyMeters(Meter)%MNValue=EnergyMeters(Meter)%MNValue+MeterValue(Meter)
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%MNMaxVal,EnergyMeters(Meter)%MNMaxValDate,  &
                                             EnergyMeters(Meter)%MNMinVal,EnergyMeters(Meter)%MNMinValDate)
      EnergyMeters(Meter)%SMValue=EnergyMeters(Meter)%SMValue+MeterValue(Meter)
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%SMMaxVal,EnergyMeters(Meter)%SMMaxValDate,  &
                                             EnergyMeters(Meter)%SMMinVal,EnergyMeters(Meter)%SMMinValDate)
    ELSE
      EnergyMeters(Meter)%TSValue=EnergyMeters(EnergyMeters(Meter)%SourceMeter)%TSValue-MeterValue(Meter)
      EnergyMeters(Meter)%HRValue=EnergyMeters(Meter)%HRValue + EnergyMeters(Meter)%TSValue
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%HRMaxVal,EnergyMeters(Meter)%HRMaxValDate,  &
                                             EnergyMeters(Meter)%HRMinVal,EnergyMeters(Meter)%HRMinValDate)
      EnergyMeters(Meter)%DYValue=EnergyMeters(Meter)%DYValue + EnergyMeters(Meter)%TSValue
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%DYMaxVal,EnergyMeters(Meter)%DYMaxValDate,  &
                                             EnergyMeters(Meter)%DYMinVal,EnergyMeters(Meter)%DYMinValDate)
      EnergyMeters(Meter)%MNValue=EnergyMeters(Meter)%MNValue + EnergyMeters(Meter)%TSValue
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%MNMaxVal,EnergyMeters(Meter)%MNMaxValDate,  &
                                             EnergyMeters(Meter)%MNMinVal,EnergyMeters(Meter)%MNMinValDate)
      EnergyMeters(Meter)%SMValue=EnergyMeters(Meter)%SMValue + EnergyMeters(Meter)%TSValue
      CALL SetMinMax(EnergyMeters(Meter)%TSValue,TimeStamp,EnergyMeters(Meter)%SMMaxVal,EnergyMeters(Meter)%SMMaxValDate,  &
                                             EnergyMeters(Meter)%SMMinVal,EnergyMeters(Meter)%SMMinValDate)
    ENDIF
  ENDDO

  MeterValue=0.0d0   ! Ready for next update

  RETURN

END SUBROUTINE UpdateMeters

SUBROUTINE SetMinMax(TestValue,TimeStamp,CurMaxValue,CurMaxValDate,CurMinValue,CurMinValDate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine looks at the current value, comparing against the current max and
          ! min for this meter/variable and resets along with a timestamp if applicable.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)       :: TestValue      ! Candidate new value
  INTEGER, INTENT(IN)    :: TimeStamp      ! TimeStamp to be stored if applicable
  REAL(r64), INTENT(INOUT)    :: CurMaxValue    ! Current Maximum Value
  INTEGER, INTENT(INOUT) :: CurMaxValDate  ! Current Maximum Value Date Stamp
  REAL(r64), INTENT(INOUT)    :: CurMinValue    ! Current Minimum Value
  INTEGER, INTENT(INOUT) :: CurMinValDate  ! Current Minimum Value Date Stamp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (TestValue > CurMaxValue) THEN
    CurMaxValue=TestValue
    CurMaxValDate=TimeStamp
  ENDIF
  IF (TestValue < CurMinValue) THEN
    CurMinValue=TestValue
    CurMinValDate=TimeStamp
  ENDIF

  RETURN

END SUBROUTINE SetMinMax

SUBROUTINE ReportTSMeters(StartMinute,EndMinute,PrintESOTimeStamp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the meters that have been requested for
          ! reporting on each time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RemoveTrailingZeros,TrimSigDigits
  USE SQLiteProcedures, ONLY: SQLdbTimeIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)       :: StartMinute ! Start Minute for TimeStep
  REAL(r64), INTENT(IN)       :: EndMinute   ! End Minute for TimeStep
  LOGICAL, INTENT(INOUT) :: PrintESOTimeStamp ! True if the ESO Time Stamp also needs to be printed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control
  LOGICAL PrintTimeStamp
  INTEGER CurDayType
  REAL(r64) :: rDummy1=0.0d0
  REAL(r64) :: rDummy2=0.0d0
  INTEGER :: iDummy1=0
  INTEGER :: iDummy2=0
  CHARACTER(len=16) :: cReportID

  PrintTimeStamp=.true.
  DO Loop=1,NumEnergyMeters
    EnergyMeters(Loop)%CurTSValue=EnergyMeters(Loop)%TSValue
    IF (.not. EnergyMeters(Loop)%RptTS .and. .not. EnergyMeters(Loop)%RptAccTS) CYCLE
    IF (PrintTimeStamp) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileMeters, ReportEach, TimeStepStampReportNbr, TimeStepStampReportChr, &
            DayOfSim, DayOfSimChr, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator, &
            DayTypes(CurDayType))
      PrintTimeStamp=.false.
    ENDIF

    IF (PrintESOTimeStamp .and. .not. EnergyMeters(Loop)%RptTSFO .and. .not. EnergyMeters(Loop)%RptAccTSFO) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportEach, TimeStepStampReportNbr, TimeStepStampReportChr, &
            DayOfSim, DayOfSimChr, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator, &
            DayTypes(CurDayType))
      PrintESOTimeStamp=.false.
    ENDIF

    IF (EnergyMeters(Loop)%RptTS) THEN
      CALL WriteReportMeterData (EnergyMeters(Loop)%TSRptNum, EnergyMeters(Loop)%TSRptNumChr, SQLdbTimeIndex,   &
            EnergyMeters(Loop)%TSValue, &
            ReportTimeStep, &
            rDummy1,iDummy1, &
            rDummy2,iDummy2, &
            EnergyMeters(Loop)%RptTSFO)
    ENDIF

    IF (EnergyMeters(Loop)%RptAccTS) THEN
      WRITE(cReportID,*) EnergyMeters(Loop)%TSAccRptNum
      cReportID=adjustl(cReportID)
      CALL WriteCumulativeReportMeterData (EnergyMeters(Loop)%TSAccRptNum, cReportID, SQLdbTimeIndex, &
            EnergyMeters(Loop)%SMValue, EnergyMeters(Loop)%RptAccTSFO)
    ENDIF
  ENDDO

  IF (NumEnergyMeters > 0) THEN
    EnergyMeters%TSValue=0.0d0
  ENDIF

  RETURN

END SUBROUTINE ReportTSMeters

SUBROUTINE ReportHRMeters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the meters that have been requested for
          ! reporting on each hour.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RemoveTrailingZeros,TrimSigDigits
  USE SQLiteProcedures, ONLY: SQLdbTimeIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control
  LOGICAL PrintTimeStamp
  INTEGER CurDayType
  REAL(r64) :: rDummy1=0.0d0
  REAL(r64) :: rDummy2=0.0d0
  INTEGER :: iDummy1=0
  INTEGER :: iDummy2=0
  CHARACTER(len=16) :: cReportID

  PrintTimeStamp=.true.
  DO Loop=1,NumEnergyMeters
    IF (.not. EnergyMeters(Loop)%RptHR .and. .not. EnergyMeters(Loop)%RptAccHR) CYCLE
    IF (PrintTimeStamp) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileMeters, ReportHourly, TimeStepStampReportNbr, TimeStepStampReportChr, &
            DayOfSim, DayOfSimChr, Month, DayOfMonth, HourOfDay, DST=DSTIndicator, DayType=DayTypes(CurDayType))
      PrintTimeStamp=.false.
    ENDIF

    IF (EnergyMeters(Loop)%RptHR) THEN
      CALL WriteReportMeterData (EnergyMeters(Loop)%HRRptNum, EnergyMeters(Loop)%HRRptNumChr, SQLdbTimeIndex,   &
            EnergyMeters(Loop)%HRValue, &
            ReportHourly, &
            rDummy1,iDummy1, & !EnergyMeters(Loop)%HRMinVal, EnergyMeters(Loop)%HRMinValDate, &
            rDummy2,iDummy2, & !EnergyMeters(Loop)%HRMaxVal, EnergyMeters(Loop)%HRMaxValDate, &
            EnergyMeters(Loop)%RptHRFO)
      EnergyMeters(Loop)%HRValue=0.0d0
      EnergyMeters(Loop)%HRMinVal=MinSetValue
      EnergyMeters(Loop)%HRMaxVal=MaxSetValue
    ENDIF

    IF (EnergyMeters(Loop)%RptAccHR) THEN
      WRITE(cReportID,*) EnergyMeters(Loop)%HRAccRptNum
      cReportID=adjustl(cReportID)
      CALL WriteCumulativeReportMeterData (EnergyMeters(Loop)%HRAccRptNum, cReportID, SQLdbTimeIndex, &
            EnergyMeters(Loop)%SMValue, EnergyMeters(Loop)%RptAccHRFO)
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportHRMeters

SUBROUTINE ReportDYMeters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the meters that have been requested for
          ! reporting on each day.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RemoveTrailingZeros,TrimSigDigits
  USE SQLiteProcedures, ONLY: SQLdbTimeIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control
  LOGICAL PrintTimeStamp
  INTEGER CurDayType
  CHARACTER(len=16) :: cReportID

  PrintTimeStamp=.true.
  DO Loop=1,NumEnergyMeters
    IF (.not. EnergyMeters(Loop)%RptDY .and. .not. EnergyMeters(Loop)%RptAccDY) CYCLE
    IF (PrintTimeStamp) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileMeters, ReportDaily, DailyStampReportNbr, DailyStampReportChr, &
            DayOfSim, DayOfSimChr, Month, DayOfMonth, DST=DSTIndicator, DayType=DayTypes(CurDayType))
      PrintTimeStamp=.false.
    ENDIF

    IF (EnergyMeters(Loop)%RptDY) THEN
      CALL WriteReportMeterData (EnergyMeters(Loop)%DYRptNum, EnergyMeters(Loop)%DYRptNumChr, SQLdbTimeIndex,   &
            EnergyMeters(Loop)%DYValue, &
            ReportDaily, &
            EnergyMeters(Loop)%DYMinVal, EnergyMeters(Loop)%DYMinValDate, &
            EnergyMeters(Loop)%DYMaxVal, EnergyMeters(Loop)%DYMaxValDate, &
            EnergyMeters(Loop)%RptDYFO)
      EnergyMeters(Loop)%DYValue=0.0d0
      EnergyMeters(Loop)%DYMinVal=MinSetValue
      EnergyMeters(Loop)%DYMaxVal=MaxSetValue
    ENDIF

    IF (EnergyMeters(Loop)%RptAccDY) THEN
      WRITE(cReportID,*) EnergyMeters(Loop)%DYAccRptNum
      cReportID=adjustl(cReportID)
      CALL WriteCumulativeReportMeterData (EnergyMeters(Loop)%DYAccRptNum, cReportID, SQLdbTimeIndex, &
            EnergyMeters(Loop)%SMValue, EnergyMeters(Loop)%RptAccDYFO)
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportDYMeters

SUBROUTINE ReportMNMeters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the meters that have been requested for
          ! reporting on each month.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RemoveTrailingZeros,TrimSigDigits
  USE SQLiteProcedures, ONLY: SQLdbTimeIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control
  LOGICAL PrintTimeStamp
  CHARACTER(len=16) :: cReportID

  PrintTimeStamp=.true.
  DO Loop=1,NumEnergyMeters
    IF (.not. EnergyMeters(Loop)%RptMN .and. .not. EnergyMeters(Loop)%RptAccMN) CYCLE
    IF (PrintTimeStamp) THEN
     SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileMeters, ReportMonthly, &
            MonthlyStampReportNbr, MonthlyStampReportChr, DayOfSim, DayOfSimChr, Month)
      PrintTimeStamp=.false.
    ENDIF

    IF (EnergyMeters(Loop)%RptMN) THEN
      CALL WriteReportMeterData (EnergyMeters(Loop)%MNRptNum, EnergyMeters(Loop)%MNRptNumChr, SQLdbTimeIndex,   &
            EnergyMeters(Loop)%MNValue, &
            ReportMonthly, &
            EnergyMeters(Loop)%MNMinVal, EnergyMeters(Loop)%MNMinValDate, &
            EnergyMeters(Loop)%MNMaxVal, EnergyMeters(Loop)%MNMaxValDate, &
            EnergyMeters(Loop)%RptMNFO)
      EnergyMeters(Loop)%MNValue=0.0d0
      EnergyMeters(Loop)%MNMinVal=MinSetValue
      EnergyMeters(Loop)%MNMaxVal=MaxSetValue
    ENDIF

    IF (EnergyMeters(Loop)%RptAccMN) THEN
      WRITE(cReportID,*) EnergyMeters(Loop)%MNAccRptNum
      cReportID=adjustl(cReportID)
      CALL WriteCumulativeReportMeterData (EnergyMeters(Loop)%MNAccRptNum, cReportID, SQLdbTimeIndex, &
            EnergyMeters(Loop)%SMValue, EnergyMeters(Loop)%RptAccMNFO)
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportMNMeters

SUBROUTINE ReportSMMeters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the meters that have been requested for
          ! reporting on each environment/run period.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RemoveTrailingZeros,TrimSigDigits
  USE SQLiteProcedures, ONLY: SQLdbTimeIndex
!  USE OutputReportPredefined
  use dataglobals, only: outputfiledebug !,DoingPredefinedAndTabularReporting

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: convertJtoGJ = 1.0d0/1000000000.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control
  LOGICAL PrintTimeStamp
  CHARACTER(len=16) :: cReportID

  PrintTimeStamp=.true.
  DO Loop=1,NumEnergyMeters
    EnergyMeters(Loop)%LastSMValue=EnergyMeters(Loop)%SMValue
    EnergyMeters(Loop)%LastSMMinVal=EnergyMeters(Loop)%SMMinVal
    EnergyMeters(Loop)%LastSMMinValDate=EnergyMeters(Loop)%SMMinValDate
    EnergyMeters(Loop)%LastSMMaxVal=EnergyMeters(Loop)%SMMaxVal
    EnergyMeters(Loop)%LastSMMaxValDate=EnergyMeters(Loop)%SMMaxValDate
    IF (.not. EnergyMeters(Loop)%RptSM .and. .not. EnergyMeters(Loop)%RptAccSM) CYCLE
    IF (PrintTimeStamp) THEN
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileMeters, ReportSim, &
            RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr)
      PrintTimeStamp=.false.
    ENDIF

    IF (EnergyMeters(Loop)%RptSM) THEN
      CALL WriteReportMeterData (EnergyMeters(Loop)%SMRptNum, EnergyMeters(Loop)%SMRptNumChr, SQLdbTimeIndex,   &
            EnergyMeters(Loop)%SMValue, &
            ReportSim, &
            EnergyMeters(Loop)%SMMinVal, EnergyMeters(Loop)%SMMinValDate, &
            EnergyMeters(Loop)%SMMaxVal, EnergyMeters(Loop)%SMMaxValDate, &
            EnergyMeters(Loop)%RptSMFO)
    ENDIF

    IF (EnergyMeters(Loop)%RptAccSM) THEN
      WRITE(cReportID,*) EnergyMeters(Loop)%SMAccRptNum
      cReportID=adjustl(cReportID)
      CALL WriteCumulativeReportMeterData (EnergyMeters(Loop)%SMAccRptNum, cReportID, SQLdbTimeIndex, &
            EnergyMeters(Loop)%SMValue, EnergyMeters(Loop)%RptAccSMFO)
    ENDIF
  ENDDO

  IF (NumEnergyMeters > 0) THEN
    EnergyMeters%SMValue=0.0d0
    EnergyMeters%SMMinVal=MinSetValue
    EnergyMeters%SMMaxVal=MaxSetValue
  ENDIF

  RETURN

END SUBROUTINE ReportSMMeters

SUBROUTINE ReportForTabularReports

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called after all the simulation is done and before
          ! tabular reports in order to reduce the number of calls to the predefined routine
          ! for SM (Simulation period) meters, the value of the last calculation is stored
          ! in the data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE OutputReportPredefined

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: convertJtoGJ = 1.0d0/1000000000.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control

  DO Loop=1,NumEnergyMeters
    SELECT CASE(EnergyMeters(Loop)%RT_forIPUnits)
      CASE(RT_IPUnits_Electricity)
        CALL PreDefTableEntry(pdchEMelecannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue*convertJtoGJ)
        CALL PreDefTableEntry(pdchEMelecminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMelecminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMelecmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMelecmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_Gas)
        CALL PreDefTableEntry(pdchEMgasannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue*convertJtoGJ)
        CALL PreDefTableEntry(pdchEMgasminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMgasminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMgasmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMgasmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_Cooling)
        CALL PreDefTableEntry(pdchEMcoolannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue*convertJtoGJ)
        CALL PreDefTableEntry(pdchEMcoolminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMcoolminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMcoolmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMcoolmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_Water)
        CALL PreDefTableEntry(pdchEMwaterannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue)
        CALL PreDefTableEntry(pdchEMwaterminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMwaterminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMwatermaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMwatermaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_OtherKG)
        CALL PreDefTableEntry(pdchEMotherKGannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue)
        CALL PreDefTableEntry(pdchEMotherKGminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherKGminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMotherKGmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherKGmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_OtherM3)
        CALL PreDefTableEntry(pdchEMotherM3annual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue,3)
        CALL PreDefTableEntry(pdchEMotherM3minvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherM3minvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMotherM3maxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherM3maxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE(RT_IPUnits_OtherL)
        CALL PreDefTableEntry(pdchEMotherLannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue,3)
        CALL PreDefTableEntry(pdchEMotherLminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherLminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMotherLmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep,3)
        CALL PreDefTableEntry(pdchEMotherLmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
      CASE DEFAULT
        CALL PreDefTableEntry(pdchEMotherJannual,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMValue*convertJtoGJ)
        CALL PreDefTableEntry(pdchEMotherJminvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMinVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMotherJminvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMinValDate))
        CALL PreDefTableEntry(pdchEMotherJmaxvalue,EnergyMeters(Loop)%Name,EnergyMeters(Loop)%LastSMMaxVal/SecondsPerTimeStep)
        CALL PreDefTableEntry(pdchEMotherJmaxvaluetime,EnergyMeters(Loop)%Name,  &
           DateToStringWithMonth(EnergyMeters(Loop)%LastSMMaxValDate))
    END SELECT
  ENDDO

  RETURN

END SUBROUTINE ReportForTabularReports

FUNCTION DateToStringWithMonth(codedDate) RESULT (stringOut)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Convert the coded date format into a usable
          !   string

USE General, ONLY: DecodeMonDayHrMin

IMPLICIT NONE

INTEGER, INTENT(IN)  :: codedDate   ! word containing encoded month, day, hour, minute
                                    ! ((month*100 + day)*100 + hour)*100 + minute
CHARACTER(len=*), PARAMETER :: DateFmt="(I2.2,'-',A3,'-',I2.2,':',I2.2)"

CHARACTER(len=12) :: stringOut
INTEGER :: Month  ! month in integer format (1-12)
INTEGER :: Day    ! day in integer format (1-31)
INTEGER :: Hour   ! hour in integer format (1-24)
INTEGER :: Minute ! minute in integer format (0:59)
CHARACTER(LEN=3) :: monthName

IF (codedDate /= 0) THEN
  monthName =''
  CALL DecodeMonDayHrMin(codedDate,Month,Day,Hour,Minute)
  Hour = Hour - 1
  IF (Minute .EQ. 60) THEN
    Hour = Hour + 1
    Minute = 0
  END IF
  SELECT CASE (MONTH)
    CASE (1)
      monthName = 'JAN'
    CASE (2)
      monthName = 'FEB'
    CASE (3)
      monthName = 'MAR'
    CASE (4)
      monthName = 'APR'
    CASE (5)
      monthName = 'MAY'
    CASE (6)
      monthName = 'JUN'
    CASE (7)
      monthName = 'JUL'
    CASE (8)
      monthName = 'AUG'
    CASE (9)
      monthName = 'SEP'
    CASE (10)
      monthName = 'OCT'
    CASE (11)
      monthName = 'NOV'
    CASE (12)
      monthName = 'DEC'
    CASE DEFAULT
      monthName = '***'
  END SELECT
  WRITE(FMT=DateFmt, UNIT=stringOut) Day,MonthName,Hour,Minute
  IF (INDEX(stringOut,'*') .GT. 0) THEN
    stringOut = '-'
  END IF
ELSE  ! codeddate = 0
    stringOut = '-'
ENDIF

END FUNCTION DateToStringWithMonth

SUBROUTINE ReportMeterDetails

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Writes the meter details report.  This shows which variables are on
          ! meters as well as the meter contents.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: VarMeter
  INTEGER :: VarMeter1
  INTEGER :: Meter
  CHARACTER(len=16)  :: MtrUnits    ! Units for Meter
  INTEGER :: I
  CHARACTER(len=32) :: String
  CHARACTER(len=80) :: Multipliers
  INTEGER  :: ZoneMult        ! Zone Multiplier
  INTEGER  :: ZoneListMult    ! Zone List Multiplier
  LOGICAL CustDecWritten

  DO VarMeter=1,NumVarMeterArrays

    MtrUnits=RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%UnitsString

    Multipliers = ''
    ZoneMult     = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneMult
    ZoneListMult = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneListMult

    IF (ZoneMult > 1 .or. ZoneListMult > 1) THEN
      WRITE(String,*) ZoneMult * ZoneListMult
      Multipliers = ' * '//ADJUSTL(String)
      WRITE(String,*) ZoneMult
      Multipliers = TRIM(Multipliers)//'  (Zone Multiplier = '//ADJUSTL(String)
      WRITE(String,*) ZoneListMult
      Multipliers = TRIM(Multipliers)//', Zone List Multiplier = '//TRIM(ADJUSTL(String))//')'
    ENDIF

    WRITE(OutputFileMeterDetails,'(/,A)') ' Meters for '//  &
        TRIM(RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ReportIDChr)//','//  &
        TRIM(RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarName)//  &
                                   ' ['//TRIM(MtrUnits)//']'// &
        TRIM(Multipliers)

    DO I=1,VarMeterArrays(VarMeter)%NumOnMeters
      WRITE(OutputFileMeterDetails,fmta) '  OnMeter='//  &
                        TRIM(EnergyMeters(VarMeterArrays(VarMeter)%OnMeters(I))%Name)// &
                                   ' ['//TRIM(MtrUnits)//']'
    ENDDO

    DO I=1,VarMeterArrays(VarMeter)%NumOnCustomMeters
      WRITE(OutputFileMeterDetails,fmta) '  OnCustomMeter='//  &
                        TRIM(EnergyMeters(VarMeterArrays(VarMeter)%OnCustomMeters(I))%Name)// &
                                   ' ['//TRIM(MtrUnits)//']'
    ENDDO
  ENDDO

  DO Meter=1,NumEnergyMeters
    WRITE(OutputFileMeterDetails,'(/,A)',ADVANCE='No') ' For Meter='//TRIM(EnergyMeters(Meter)%Name)//' ['//  &
                                                         TRIM(EnergyMeters(Meter)%Units)//']'
    IF (EnergyMeters(Meter)%ResourceType /= ' ') WRITE(OutputFileMeterDetails,fmta,ADVANCE='No')   &
                   ', ResourceType='//TRIM(EnergyMeters(Meter)%ResourceType)
    IF (EnergyMeters(Meter)%EndUse /= ' ') WRITE(OutputFileMeterDetails,fmta,ADVANCE='No')   &
                   ', EndUse='//TRIM(EnergyMeters(Meter)%EndUse)
    IF (EnergyMeters(Meter)%Group /= ' ') WRITE(OutputFileMeterDetails,fmta,ADVANCE='No')   &
                   ', Group='//TRIM(EnergyMeters(Meter)%Group)
    WRITE(OutputFileMeterDetails,fmta) ', contents are:'

    CustDecWritten=.false.

    DO VarMeter=1,NumVarMeterArrays
      IF (EnergyMeters(Meter)%TypeOfMeter == MeterType_Normal) THEN
        IF (ANY(VarMeterArrays(VarMeter)%OnMeters == Meter) ) THEN
          DO VarMeter1=1,VarMeterArrays(VarMeter)%NumOnMeters
            IF (VarMeterArrays(VarMeter)%OnMeters(VarMeter1) /= Meter) CYCLE

            Multipliers = ''
            ZoneMult = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneMult
            ZoneListMult = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneListMult

            IF (ZoneMult > 1 .or. ZoneListMult > 1) THEN
              WRITE(String,*) ZoneMult * ZoneListMult
              Multipliers = ' * '//ADJUSTL(String)
              WRITE(String,*) ZoneMult
              Multipliers = TRIM(Multipliers)//'  (Zone Multiplier = '//ADJUSTL(String)
              WRITE(String,*) ZoneListMult
              Multipliers = TRIM(Multipliers)//', Zone List Multiplier = '//TRIM(ADJUSTL(String))//')'
            END IF

            WRITE(OutputFileMeterDetails,fmta) '  '//TRIM(RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarName)// &
              TRIM(Multipliers)
          ENDDO
        ENDIF
      ENDIF
      IF (EnergyMeters(Meter)%TypeOfMeter /= MeterType_Normal) THEN
        IF (VarMeterArrays(VarMeter)%NumOnCustomMeters > 0) THEN
          IF (ANY(VarMeterArrays(VarMeter)%OnCustomMeters == Meter) ) THEN
            IF (.not. CustDecWritten .and. EnergyMeters(Meter)%TypeOfMeter == MeterType_CustomDec) THEN
              WRITE(OutputFileMeterDetails,fmta) ' Values for this meter will be Source Meter='//  &
                               TRIM(EnergyMeters(EnergyMeters(Meter)%SourceMeter)%Name)//'; but will be decremented by:'
              CustDecWritten=.true.
            ENDIF
            DO VarMeter1=1,VarMeterArrays(VarMeter)%NumOnCustomMeters
              IF (VarMeterArrays(VarMeter)%OnCustomMeters(VarMeter1) /= Meter) CYCLE

              Multipliers = ''
              ZoneMult = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneMult
              ZoneListMult = RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarPtr%ZoneListMult

              IF (ZoneMult > 1 .OR. ZoneListMult > 1) THEN
                WRITE(String,*) ZoneMult * ZoneListMult
                Multipliers = ' * '//ADJUSTL(String)
                WRITE(String,*) ZoneMult
                Multipliers = TRIM(Multipliers)//'  (Zone Multiplier = '//ADJUSTL(String)
                WRITE(String,*) ZoneListMult
                Multipliers = TRIM(Multipliers)//', Zone List Multiplier = '//TRIM(ADJUSTL(String))//')'
              END IF

              WRITE(OutputFileMeterDetails,fmta) '  '//TRIM(RVariableTypes(VarMeterArrays(VarMeter)%RepVariable)%VarName)// &
                TRIM(Multipliers)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO

  RETURN

END SUBROUTINE ReportMeterDetails

! *****************************************************************************
! End of routines for Energy Meters implementation in EnergyPlus.
! *****************************************************************************

SUBROUTINE AddEndUseSubcategory(ResourceName, EndUseName, EndUseSubName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the list of subcategories for each end-use category.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ResourceName
  CHARACTER(len=*), INTENT(IN) :: EndUseName
  CHARACTER(len=*), INTENT(IN) :: EndUseSubName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL           :: Found
  INTEGER           :: EndUseNum
  INTEGER           :: EndUseSubNum
  INTEGER           :: NumSubs
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SubcategoryNameTemp

  Found = .FALSE.
  DO EndUseNum = 1, NumEndUses
    IF (SameString(EndUseCategory(EndUseNum)%Name, EndUseName)) THEN

      DO EndUseSubNum = 1, EndUseCategory(EndUseNum)%NumSubcategories
        IF (SameString(EndUseCategory(EndUseNum)%SubcategoryName(EndUseSubNum), EndUseSubName)) THEN
          ! Subcategory already exists, no further action required
          Found = .TRUE.
          EXIT
        END IF
      END DO

      IF (.NOT. Found) THEN
        ! Add the subcategory by reallocating the array
        NumSubs = EndUseCategory(EndUseNum)%NumSubcategories
        IF (NumSubs > 0) THEN
          ALLOCATE(SubcategoryNameTemp(NumSubs))
          SubcategoryNameTemp = EndUseCategory(EndUseNum)%SubcategoryName
          DEALLOCATE(EndUseCategory(EndUseNum)%SubcategoryName)
        END IF

        ALLOCATE(EndUseCategory(EndUseNum)%SubcategoryName(NumSubs + 1))
        IF (NumSubs > 0) THEN
          EndUseCategory(EndUseNum)%SubcategoryName(1:NumSubs) = SubcategoryNameTemp(1:NumSubs)
          DEALLOCATE(SubcategoryNameTemp)
        END IF

        EndUseCategory(EndUseNum)%NumSubcategories = NumSubs + 1
        EndUseCategory(EndUseNum)%SubcategoryName(NumSubs + 1) = EndUseSubName

        IF (EndUseCategory(EndUseNum)%NumSubcategories > MaxNumSubcategories) THEN
          MaxNumSubcategories = EndUseCategory(EndUseNum)%NumSubcategories
        END IF

        Found = .TRUE.
      END IF
      EXIT
    END IF
  END DO

  IF (.NOT. Found) THEN
    CALL ShowSevereError('Nonexistent end use passed to AddEndUseSubcategory='//TRIM(EndUseName))
  END IF

  RETURN

END SUBROUTINE AddEndUseSubcategory

INTEGER FUNCTION WriteTimeStampFormatData (unitNumber, reportingInterval, reportID, reportIDString,   &
   DayOfSim, DayOfSimChr, Month, DayOfMonth, Hour, EndMinute, StartMinute, DST, DayType)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function reports the timestamp data for the output processor
    ! Much of the code in this function was embedded in earlier versions of EnergyPlus
    ! and was moved to this location to simplify maintenance and to allow for data output
    ! to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: unitNumber               ! the Fortran output unit number
    INTEGER, INTENT(IN) :: reportingInterval    ! See Module Parameter Definitons for ReportEach, ReportTimeStep, ReportHourly, etc.
    INTEGER, INTENT(IN) :: reportID ! The ID of the time stamp
    CHARACTER(len=*), INTENT(IN)  :: reportIDString ! The ID of the time stamp
    INTEGER, INTENT(IN) :: DayOfSim    ! the number of days simulated so far
    CHARACTER(len=*), INTENT(IN)  :: DayOfSimChr    ! the number of days simulated so far
    INTEGER, INTENT(IN), OPTIONAL :: Month          ! the month of the reporting interval
    INTEGER, INTENT(IN), OPTIONAL :: DayOfMonth     ! The day of the reporting interval
    INTEGER, INTENT(IN), OPTIONAL :: Hour           ! The hour of the reporting interval
    REAL(r64), INTENT(IN), OPTIONAL :: EndMinute      ! The last minute in the reporting interval
    REAL(r64), INTENT(IN), OPTIONAL :: StartMinute    ! The starting minute of the reporting interval
    INTEGER, INTENT(IN), OPTIONAL :: DST            ! A flag indicating whether daylight savings time is observed
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: DayType  ! The day tied for the data (e.g., Monday)

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxMessageSize) :: cmessageBuffer
    INTEGER :: TimeIndex

        SELECT CASE (reportingInterval)

            CASE(ReportEach, ReportTimeStep)
                WRITE (unitNumber, TimeStampFormat) TRIM(reportIDString), TRIM(DayOfSimChr), &
                    Month, DayOfMonth, DST, Hour, StartMinute, EndMinute, DayType
                TimeIndex = -1   ! Signals that writing to the SQL database is not active

                IF (WriteOutputToSQLite) &
                  TimeIndex = CreateSQLiteTimeIndexRecord(reportingInterval, reportID, DayOfSim, &
                    Month, DayOfMonth, Hour, EndMinute, StartMinute, DST, DayType)

            CASE(ReportHourly)
                WRITE (unitNumber, TimeStampFormat) TRIM(reportIDString), TRIM(DayOfSimChr), Month, &
                    DayOfMonth, DST, Hour, 0.0, 60.0, DayType
                TimeIndex = -1   ! Signals that writing to the SQL database is not active

                IF (WriteOutputToSQLite) &
                  TimeIndex = CreateSQLiteTimeIndexRecord(reportingInterval, reportID, DayOfSim, &
                    Month, DayOfMonth, Hour, DST=DST, DayType=DayType)

            CASE(ReportDaily)
                WRITE (unitNumber, DailyStampFormat) TRIM(reportIDString), TRIM(DayOfSimChr), Month, &
                    DayOfMonth, DST, DayType
                TimeIndex = -1   ! Signals that writing to the SQL database is not active

                IF (WriteOutputToSQLite) &
                  TimeIndex = CreateSQLiteTimeIndexRecord(reportingInterval, reportID, DayOfSim, &
                    Month, DayOfMonth, DST=DST, DayType=DayType)

            CASE(ReportMonthly)
                WRITE (unitNumber, MonthlyStampFormat) TRIM(reportIDString), TRIM(DayOfSimChr), Month
                TimeIndex = -1   ! Signals that writing to the SQL database is not active
                IF (WriteOutputToSQLite) &
                  TimeIndex = CreateSQLiteTimeIndexRecord(ReportMonthly, reportID, DayOfSim, Month)

            CASE(ReportSim)
                WRITE (unitNumber, RunPeriodStampFormat) TRIM(reportIDString), TRIM(DayOfSimChr)
                TimeIndex = -1   ! Signals that writing to the SQL database is not active
                IF (WriteOutputToSQLite) &
                  TimeIndex = CreateSQLiteTimeIndexRecord(reportingInterval, reportID, DayOfSim)

            CASE DEFAULT
                Write(cmessageBuffer,'(A,I5)') 'Illegal reportingInterval passed to WriteTimeStampFormatData: ', reportingInterval
                IF (WriteOutputToSQLite) &
                  CALL SQLiteWriteMessageMacro (cmessageBuffer)
                TimeIndex = -1   ! Signals that writing to the SQL database is not active

        END SELECT

    WriteTimeStampFormatData = TimeIndex

    RETURN

END FUNCTION WriteTimeStampFormatData

SUBROUTINE WriteReportVariableDictionaryItem (reportingInterval, storeType, reportID, indexGroupKey, &
           indexGroup, reportIDChr, keyedValue, variableName, indexType, unitsString, scheduleName)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the ESO data dictionary information to the output files
    ! and the SQL database

    ! METHODOLOGY EMPLOYED:
    !

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals, ONLY: OutputFileStandard
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                    :: reportingInterval     ! The reporting interval (e.g., hourly, daily)
    INTEGER, INTENT(IN)                    :: storeType
    INTEGER, INTENT(IN)                    :: reportID           ! The reporting ID for the data
    INTEGER, INTENT(IN)                    :: indexGroupKey         ! The reporting group (e.g., Zone, Plant Loop, etc.)
    CHARACTER(len=*), INTENT(IN)           :: indexGroup            ! The reporting group (e.g., Zone, Plant Loop, etc.)
    CHARACTER(len=*), INTENT(IN)           :: reportIDChr           ! The reporting ID for the data
    CHARACTER(len=*), INTENT(IN)           :: keyedValue            ! The key name for the data
    CHARACTER(len=*), INTENT(IN)           :: variableName          ! The variable's actual name
    INTEGER, INTENT(IN)                    :: indexType
    CHARACTER(len=*), INTENT(IN)           :: unitsString           ! The variables units
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: scheduleName


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=120) FreqString

    FreqString = FreqNotice (reportingInterval, storeType)

    IF (PRESENT(scheduleName)) THEN
      FreqString = TRIM(FreqString)//','//TRIM(scheduleName)
    ENDIF

    SELECT CASE (reportingInterval)

        CASE (ReportEach, ReportTimeStep)
          WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,'//  &
             TRIM(keyedValue)//','//TRIM(variableName)//' ['//trim(unitsString)//']'//TRIM(FreqString)

        CASE (ReportHourly)
          TrackingHourlyVariables=.true.
          WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,'//  &
             TRIM(keyedValue)//','//TRIM(variableName)//' ['//trim(unitsString)//']'//TRIM(FreqString)

        CASE (ReportDaily)
          TrackingDailyVariables=.true.
          WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',7,'//  &
             TRIM(keyedValue)//','//TRIM(variableName)//' ['//trim(unitsString)//']'//TRIM(FreqString)

        CASE (ReportMonthly)
          TrackingMonthlyVariables=.true.
          WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',9,'//  &
             TRIM(keyedValue)//','//TRIM(variableName)//' ['//trim(unitsString)//']'//TRIM(FreqString)

        CASE (ReportSim)
          TrackingRunPeriodVariables=.true.
          WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',11,'//  &
             TRIM(keyedValue)//','//TRIM(variableName)//' ['//trim(unitsString)//']'//TRIM(FreqString)

    END SELECT

    IF (WriteOutputToSQLite) THEN
        IF (.not. PRESENT(scheduleName)) THEN
            CALL CreateSQLiteReportVariableDictionaryRecord (reportID, storeType, indexGroup, &
                 keyedValue, variableName, indexType, unitsString, reportingInterval)
        ELSE
            CALL CreateSQLiteReportVariableDictionaryRecord (reportID, storeType, indexGroup, &
                 keyedValue, variableName, indexType, unitsString, reportingInterval, scheduleName)
        END IF
    END IF

    RETURN

END SUBROUTINE WriteReportVariableDictionaryItem

SUBROUTINE WriteMeterDictionaryItem (reportingInterval, storeType, reportID, indexGroupKey, &
           indexGroup, reportIDChr, meterName, unitsString, cumulativeMeterFlag, meterFileOnlyFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! The subroutine writes meter data dictionary information to the output files
    ! and the SQL database. Much of the code here was embedded in other subroutines
    ! and was moved here for the purposes of ease of maintenance and to allow easy
    ! data reporting to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals, ONLY: OutputFileMeters, OutputFileStandard
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)          :: reportingInterval   ! The reporting interval (e.g., hourly, daily)
    INTEGER, INTENT(IN)          :: storeType
    INTEGER, INTENT(IN)          :: reportID            ! The reporting ID in for the variable
    INTEGER, INTENT(IN)          :: indexGroupKey       ! The reporting group for the variable
    CHARACTER(len=*), INTENT(IN) :: indexGroup          ! The reporting group for the variable
    CHARACTER(len=*), INTENT(IN) :: reportIDChr         ! The reporting ID in for the variable
    CHARACTER(len=*), INTENT(IN) :: meterName           ! The variable's meter name
    CHARACTER(len=*), INTENT(IN) :: unitsString         ! The variables units
    LOGICAL, INTENT(IN)          :: cumulativeMeterFlag ! A flag indicating cumulative data
    LOGICAL, INTENT(IN)          :: meterFileOnlyFlag   ! A flag indicating whether the data is to be written to standard output


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=120) :: FreqString
    CHARACTER(len=20)  :: keyedValueString = 'Cumulative '
    INTEGER :: lenString

    FreqString = FreqNotice (reportingInterval, storeType)

    SELECT CASE (reportingInterval)

        CASE (ReportEach, ReportTimeStep, ReportHourly) ! -1, 0, 1
          IF (.not. cumulativeMeterFlag) THEN
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',1,'//TRIM(meterName)// &
              ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
          ELSE
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
              ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
          ENDIF

          IF (.not. meterFileOnlyFlag) THEN
            IF (.not. cumulativeMeterFlag) THEN
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
            ELSE
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
            ENDIF
          END IF

        CASE (ReportDaily)  !  2
          IF (.not. cumulativeMeterFlag) THEN
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',7,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
          ELSE
            lenString=INDEX(FreqString,'[')
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
          ENDIF
          IF (.not. meterFileOnlyFlag) THEN
            IF (.not. cumulativeMeterFlag) THEN
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',7,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
            ELSE
              lenString=INDEX(FreqString,'[')
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
            ENDIF
          ENDIF

        CASE (ReportMonthly)  !  3
          IF (.not. cumulativeMeterFlag) THEN
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',9,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
          ELSE
            lenString=INDEX(FreqString,'[')
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
          ENDIF
          IF (.not. meterFileOnlyFlag) THEN
            IF (.not. cumulativeMeterFlag) THEN
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',9,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
            ELSE
              lenString=INDEX(FreqString,'[')
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
            ENDIF
          ENDIF

        CASE (ReportSim)  !  4
          IF (.not. cumulativeMeterFlag) THEN
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',11,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
          ELSE
            lenString=INDEX(FreqString,'[')
            WRITE(OutputFileMeters, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
          ENDIF
          IF (.not. meterFileOnlyFlag) THEN
            IF (.not. cumulativeMeterFlag) THEN
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',11,'//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString)
            ELSE
              lenString=INDEX(FreqString,'[')
              WRITE(OutputFileStandard, fmta) TRIM(reportIDChr)//',1,Cumulative '//TRIM(meterName)// &
                  ' ['//TRIM(unitsString)//']'//TRIM(FreqString(1:lenString-1))
            ENDIF
          ENDIF

    END SELECT

    IF (WriteOutputToSQLite) THEN
        IF (cumulativeMeterFlag) THEN
            keyedValueString = 'Cumulative '
        ELSE
            keyedValueString = ''
        END IF
        CALL CreateSQLiteMeterDictionaryRecord (reportID, storeType, &
             indexGroup, keyedValueString, meterName, 1, unitsString, reportingInterval)

        IF (.not. meterFileOnlyFlag) THEN
            CALL CreateSQLiteReportVariableDictionaryRecord (reportID, storeType, indexGroup, &
                 keyedValueString, meterName, 1, unitsString, reportingInterval)
        END IF

    END IF

    RETURN

END SUBROUTINE WriteMeterDictionaryItem

SUBROUTINE WriteRealVariableOutput (reportType, timeIndex)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes real report variable data to the output file and
    ! SQL database. Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportType  ! The report type or interval (e.g., hourly)
    INTEGER, INTENT(IN) :: timeIndex   ! An index that points to the timestamp

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    IF (RVar%Report .and. RVar%ReportFreq == reportType .and. RVar%Stored) THEN
        IF (RVar%NumStored > 0.0d0) THEN
          CALL WriteReportRealData (RVar%ReportID, RVar%ReportIDChr, TimeIndex, &
                    RVar%StoreValue, RVar%StoreType, RVar%NumStored, RVar%ReportFreq, RVar%MinValue, &
                    RVar%minValueDate, RVar%MaxValue, RVar%maxValueDate)
          StdOutputRecordCount = StdOutputRecordCount + 1
        ENDIF

        RVar%StoreValue = 0.0d0
        RVar%NumStored = 0.0d0
        RVar%MinValue = MinSetValue
        RVar%MaxValue = MaxSetValue
        RVar%Stored = .false.

    ENDIF

    RETURN

END SUBROUTINE WriteRealVariableOutput

SUBROUTINE WriteReportRealData (reportID, creportID, timeIndex, repValue, storeType, numOfItemsStored, &
                                reportingInterval, minValue, minValueDate, maxValue, maxValueDate)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the average real data to the output files and
    ! SQL database. It supports the WriteRealVariableOutput subroutine.
    ! Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: OutputFileStandard
    USE General, ONLY: RemoveTrailingZeros
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportID            ! The variable's report ID
    CHARACTER(len=*), INTENT(IN) :: creportID  ! variable ID in characters
    INTEGER, INTENT(IN) :: timeIndex           ! An index that points to the timestamp
    REAL(r64), INTENT(IN) :: repValue             ! The variable's value
    INTEGER, INTENT(IN)  :: storeType          ! Averaged or Sum
    REAL(r64), INTENT(IN) :: numOfItemsStored  ! The number of items (hours or timesteps) of data stored
    INTEGER, INTENT(IN) :: reportingInterval   ! The variable's reporting interval (e.g., daily)
    REAL(r64), INTENT(IN) :: maxValue          ! The variable's maximum value during the reporting interval
    INTEGER, INTENT(IN) :: maxValueDate        ! The date the maximum value occurred
    REAL(r64), INTENT(IN) :: minValue          ! The variable's minimum value during the reporting interval
    INTEGER, INTENT(IN) :: minValueDate        ! The date the minimum value occurred

    ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: fmta="(A)"

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=32) :: NumberOut    ! Character for producing "number out"
    CHARACTER(len=55) :: MaxOut       ! Character for Max out string
    CHARACTER(len=55) :: MinOut       ! Character for Min out string
    REAL(r64) :: repVal             ! The variable's value

    repVal=repValue
    IF (storeType == AveragedVar) repVal=repVal/numOfItemsStored
    IF (repVal == 0.0d0) THEN
        NumberOut = '0.0'
    ELSE
        WRITE(NumberOut,*) repVal
        NumberOut = ADJUSTL(NumberOut)
        NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF

    IF (maxValue == 0.0d0) THEN
        MaxOut = '0.0'
    ELSE
        WRITE(MaxOut,*) maxValue
        MaxOut = ADJUSTL(MaxOut)
        MaxOut = RemoveTrailingZeros(MaxOut)
    ENDIF

    IF (minValue == 0.0d0) THEN
        MinOut = '0.0'
    ELSE
        WRITE(MinOut,*) minValue
        MinOut = ADJUSTL(MinOut)
        MinOut = RemoveTrailingZeros(MinOut)
    ENDIF

    ! Append the min and max strings with date information
    CALL ProduceMinMaxString (MinOut, minValueDate, reportingInterval)
    CALL ProduceMinMaxString (MaxOut, maxValueDate, reportingInterval)

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repVal, reportingInterval, &
            minValue, minValueDate, maxValue, maxValueDate)
    END IF

    SELECT CASE (reportingInterval)

      CASE (ReportEach, ReportTimeStep, ReportHourly) ! -1, 0, 1
        WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)

      CASE (ReportDaily, ReportMonthly, ReportSim)  !  2, 3, 4
        WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)//','//TRIM(MinOut)//','//TRIM(MaxOut)

    END SELECT

    RETURN

END SUBROUTINE WriteReportRealData


SUBROUTINE WriteCumulativeReportMeterData (reportID, creportID, timeIndex, repValue, meterOnlyFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the cumulative meter data to the output files and
    ! SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals, ONLY: OutputFileMeters, OutputFileStandard, StdOutputRecordCount, StdMeterRecordCount
    USE General, ONLY: RemoveTrailingZeros
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportID      ! The variable's report ID
    CHARACTER(len=*), INTENT(IN) :: creportID  ! variable ID in characters
    INTEGER, INTENT(IN) :: timeIndex     ! An index that points to the timestamp
    REAL(r64), INTENT(IN) :: repValue       ! The variable's value
    LOGICAL, INTENT(IN) :: meterOnlyFlag ! A flag that indicates if the data should be written to standard output

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=32) :: NumberOut       ! Character for producing "number out"

    IF (repValue == 0.0d0) THEN
        NumberOut = '0.0'
    ELSE
        WRITE(NumberOut,*) repValue
        NumberOut = ADJUSTL(NumberOut)
        NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteMeterRecord(reportID, TimeIndex, repValue)
    END IF

    WRITE(OutputFileMeters, fmta) TRIM(creportID)//','//TRIM(NumberOut)
    StdMeterRecordCount = StdMeterRecordCount + 1

    IF (.not. meterOnlyFlag) THEN
        IF (WriteOutputToSQLite) THEN
            CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repValue)
        END IF

        WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)
        StdOutputRecordCount = StdOutputRecordCount + 1
    END IF

    RETURN

END SUBROUTINE WriteCumulativeReportMeterData

SUBROUTINE WriteReportMeterData (reportID, creportID, timeIndex, repValue, reportingInterval, &
    minValue, minValueDate, maxValue, maxValueDate, meterOnlyFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes for the non-cumulative meter data to the output files and
    ! SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: OutputFileStandard, OutputFileMeters, StdOutputRecordCount, StdMeterRecordCount
    USE General, ONLY: RemoveTrailingZeros
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportID          ! The variable's report ID
    CHARACTER(len=*), INTENT(IN) :: creportID  ! variable ID in characters
    INTEGER, INTENT(IN) :: timeIndex         ! An index that points to the timestamp
    REAL(r64), INTENT(IN) :: repValue           ! The variable's value
    INTEGER, INTENT(IN) :: reportingInterval ! The variable's reporting interval (e.g., hourly)
    REAL(r64), INTENT(IN) :: maxValue        ! The variable's maximum value during the reporting interval
    INTEGER, INTENT(IN) :: maxValueDate      ! The date of the maximum value
    REAL(r64), INTENT(IN) :: minValue        ! The variable's minimum value during the reporting interval
    INTEGER, INTENT(IN) :: minValueDate      ! The date the minimum value occurred
    LOGICAL, INTENT(IN) :: meterOnlyFlag     ! Indicates whether the data is for the meter file only

    ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: fmta="(A)"

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=32) :: NumberOut    ! Character for producing "number out"
    CHARACTER(len=55) :: MaxOut       ! Character for Max out string
    CHARACTER(len=55) :: MinOut       ! Character for Min out string

    IF (repValue == 0.0d0) THEN
        NumberOut = '0.0'
    ELSE
        WRITE(NumberOut,*) repValue
        NumberOut = ADJUSTL(NumberOut)
        NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF

    IF (maxValue == 0.0d0) THEN
        MaxOut = '0.0'
    ELSE
        WRITE(MaxOut,*) maxValue
        MaxOut = ADJUSTL(MaxOut)
        MaxOut = RemoveTrailingZeros(MaxOut)
    ENDIF

    IF (minValue == 0.0d0) THEN
        MinOut = '0.0'
    ELSE
        WRITE(MinOut,*) minValue
        MinOut = ADJUSTL(MinOut)
        MinOut = RemoveTrailingZeros(MinOut)
    ENDIF

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteMeterRecord(reportID, TimeIndex, repValue, reportingInterval, &
            minValue, minValueDate, maxValue, maxValueDate, MinutesPerTimeStep)
    END IF

    ! Append the min and max strings with date information
!    CALL ProduceMinMaxStringWStartMinute (MinOut, minValueDate, reportingInterval)
!    CALL ProduceMinMaxStringWStartMinute (MaxOut, maxValueDate, reportingInterval)
    CALL ProduceMinMaxString (MinOut, minValueDate, reportingInterval)
    CALL ProduceMinMaxString (MaxOut, maxValueDate, reportingInterval)

    SELECT CASE (reportingInterval)

      CASE (ReportEach, ReportTimeStep, ReportHourly) ! -1, 0, 1
        WRITE(OutputFileMeters, fmta) TRIM(creportID)//','//TRIM(NumberOut)
        StdMeterRecordCount = StdMeterRecordCount + 1

      CASE (ReportDaily, ReportMonthly, ReportSim)  !  2, 3, 4
        WRITE(OutputFileMeters, fmta) TRIM(creportID)//','//TRIM(NumberOut)//','//TRIM(MinOut)//','//TRIM(MaxOut)
        StdMeterRecordCount = StdMeterRecordCount + 1

    END SELECT

    IF (.not. meterOnlyFlag) THEN
        IF (WriteOutputToSQLite) THEN
            CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repValue, reportingInterval, &
                minValue, minValueDate, maxValue, maxValueDate, MinutesPerTimeStep)
        END IF

      SELECT CASE (reportingInterval)

        CASE (ReportEach, ReportTimeStep, ReportHourly) ! -1, 0, 1
          WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)
          StdOutputRecordCount = StdOutputRecordCount + 1
        CASE (ReportDaily, ReportMonthly, ReportSim)  !  2, 3, 4
          WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)//','//TRIM(MinOut)//','//TRIM(MaxOut)
          StdOutputRecordCount = StdOutputRecordCount + 1
      END SELECT

    END IF

    RETURN

END SUBROUTINE WriteReportMeterData

SUBROUTINE WriteRealData (reportID, creportID, timeIndex, repValue)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes real data to the output files and
    ! SQL database. It supports the WriteRealVariableOutput subroutine.
    ! Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals, ONLY: OutputFileStandard
    USE General, ONLY: RemoveTrailingZeros
    USE DataSystemVariables, ONLY: ReportDuringWarmup, UpdateDataDuringWarmupExternalInterface
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportID  ! The variable's reporting ID
    CHARACTER(len=*), INTENT(IN) :: creportID  ! variable ID in characters
    INTEGER, INTENT(IN) :: timeIndex ! An index that points to the timestamp for the variable
    REAL(r64), INTENT(IN) :: repValue   ! The variable's value

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=32) :: NumberOut       ! Character for producing "number out"

    IF (UpdateDataDuringWarmupExternalInterface .and. .not. ReportDuringWarmup) RETURN

    IF (repValue == 0.0d0) THEN
        NumberOut = '0.0'
    ELSE
        WRITE(NumberOut,*) repValue
        NumberOut = ADJUSTL(NumberOut)
        NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repValue)
    END IF

    WRITE(OutputFileStandard, fmta) TRIM(creportID)//','//TRIM(NumberOut)

    RETURN

END SUBROUTINE WriteRealData

SUBROUTINE WriteIntegerVariableOutput (reportType, timeIndex)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes integer report variable data to the output file and
    ! SQL database. Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataSystemVariables, ONLY: ReportDuringWarmup, UpdateDataDuringWarmupExternalInterface

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportType  ! The report type (i.e., the reporting interval)
    INTEGER, INTENT(IN) :: timeIndex   ! An index that points to the timestamp for this data

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    IF (UpdateDataDuringWarmupExternalInterface .and. .not. ReportDuringWarmup) RETURN

    IF (IVar%Report .and. IVar%ReportFreq == reportType .and. IVar%Stored) THEN
        IF (IVar%NumStored > 0.0d0) THEN
          CALL WriteReportIntegerData (IVar%ReportID, IVar%ReportIDChr, TimeIndex, IVar%StoreValue, IVar%StoreType,   &
                    IVar%NumStored, IVar%ReportFreq, IVar%MinValue, IVar%minValueDate, IVar%MaxValue, IVar%maxValueDate)
          StdOutputRecordCount = StdOutputRecordCount + 1
        ENDIF

        IVar%StoreValue = 0.0d0
        IVar%NumStored = 0.0d0
        IVar%MinValue = IMinSetValue
        IVar%MaxValue = IMaxSetValue
        IVar%Stored = .false.

    ENDIF

    RETURN

END SUBROUTINE WriteIntegerVariableOutput

SUBROUTINE WriteReportIntegerData (reportID, reportIDString, timeIndex, repValue, storeType,   &
                  numOfItemsStored, reportingInterval,  minValue, minValueDate, maxValue, maxValueDate)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       April 2011; Linda Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes averaged integer data to the output files and
    ! SQL database. It supports the WriteIntegerVariableOutput subroutine.
    ! Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: OutputFileStandard
    USE General, ONLY: RemoveTrailingZeros
    USE SQLiteProcedures

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportID                     ! The variable's reporting ID
    CHARACTER(len=*), INTENT(IN) :: reportIDString      ! The variable's reporting ID (character)
    INTEGER, INTENT(IN) :: timeIndex                    ! An index that points to this timestamp for this data
    REAL(r64), INTENT(IN) :: repValue                      ! The variable's value
    INTEGER, INTENT(IN)  :: storeType                   ! Type of item (averaged or summed)
    REAL(r64), INTENT(IN), OPTIONAL :: numOfItemsStored ! The number of items (hours or timesteps) of data stored !Objexx:OPTIONAL Used without PRESENT check
    INTEGER, INTENT(IN), OPTIONAL :: reportingInterval  ! The reporting interval (e.g., monthly) !Objexx:OPTIONAL Used without PRESENT check
    INTEGER, INTENT(IN), OPTIONAL :: maxValue           ! The variable's maximum value during the reporting interval !Objexx:OPTIONAL Used without PRESENT check
    INTEGER, INTENT(IN), OPTIONAL :: maxValueDate       ! The date the maximum value occurred !Objexx:OPTIONAL Used without PRESENT check
    INTEGER, INTENT(IN), OPTIONAL :: minValue           ! The variable's minimum value during the reporting interval !Objexx:OPTIONAL Used without PRESENT check
    INTEGER, INTENT(IN), OPTIONAL :: minValueDate       ! The date the minimum value occurred !Objexx:OPTIONAL Used without PRESENT check

    ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: fmta="(A)"

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=32) :: NumberOut    ! Character for producing "number out"
    CHARACTER(len=55) :: MaxOut       ! Character for Max out string
    CHARACTER(len=55) :: MinOut       ! Character for Min out string
    real(r64) :: rmaxValue, rminValue
    REAL(r64) :: repVal             ! The variable's value

    repVal=repValue
    IF (storeType == AveragedVar) repVal=repVal/numOfItemsStored
    IF (repValue == 0.0d0) THEN
        NumberOut = '0.0'
    ELSE
        WRITE(NumberOut,*) repVal
        NumberOut = ADJUSTL(NumberOut)
        NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF

    ! Append the min and max strings with date information
    WRITE(MinOut,*) minValue
    WRITE(MaxOut,*) maxValue
    CALL ProduceMinMaxString(MinOut, minValueDate, reportingInterval)
    CALL ProduceMinMaxString(MaxOut, maxValueDate, reportingInterval)

    IF (WriteOutputToSQLite) THEN
        rminValue=minValue
        rmaxValue=maxValue
        CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repVal, reportingInterval, &
            rminValue, minValueDate, rmaxValue, maxValueDate)
    END IF

    SELECT CASE (reportingInterval)

      CASE (ReportEach, ReportTimeStep, ReportHourly) ! -1, 0, 1
        WRITE(OutputFileStandard, fmta) TRIM(reportIDString)//','//TRIM(NumberOut)

      CASE (ReportDaily, ReportMonthly, ReportSim)  !  2, 3, 4
        WRITE(OutputFileStandard, fmta) TRIM(reportIDString)//','//TRIM(NumberOut)//','//TRIM(MinOut)//','//TRIM(MaxOut)

    END SELECT

    RETURN

END SUBROUTINE WriteReportIntegerData

SUBROUTINE WriteIntegerData (reportID, reportIDString, timeIndex, IntegerValue, RealValue)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes integer data to the output files and
    ! SQL database. It supports the WriteIntegerVariableOutput subroutine.
    ! Much of the code here was an included in earlier versions
    ! of the UpdateDataandReport subroutine. The code was moved to facilitate
    ! easier maintenance and writing of data to the SQL database.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
  USE DataGlobals, ONLY: OutputFileStandard
  USE General, ONLY: RemoveTrailingZeros
  USE SQLiteProcedures

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: reportID  ! the reporting ID of the data
  CHARACTER(len=*), INTENT(IN) :: reportIDString   ! the reporting ID of the data (character)
  INTEGER, INTENT(IN) :: timeIndex ! an index that points to the data's timestamp
  INTEGER, INTENT(IN), OPTIONAL :: IntegerValue   ! the value of the data
  REAL(r64), INTENT(IN), OPTIONAL :: RealValue   ! the value of the data

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=32) :: NumberOut       ! Character for producing "number out"
  REAL(r64) :: repValue  ! for SQLite

  IF (PRESENT(IntegerValue)) THEN
    WRITE(NumberOut,*) IntegerValue
    NumberOut = ADJUSTL(NumberOut)
    repValue=IntegerValue
  ENDIF
  IF (PRESENT(RealValue)) THEN
    repValue=RealValue
    IF (RealValue == 0.0d0) THEN
      NumberOut = '0.0'
    ELSE
      WRITE(NumberOut,*) RealValue
      NumberOut = ADJUSTL(NumberOut)
      NumberOut = RemoveTrailingZeros(NumberOut)
    ENDIF
  ENDIF

  IF (WriteOutputToSQLite) THEN
      CALL CreateSQLiteReportVariableDataRecord(reportID, TimeIndex, repValue)
  END IF

  WRITE(OutputFileStandard, fmta) TRIM(reportIDString)//','//TRIM(NumberOut)

  RETURN

END SUBROUTINE WriteIntegerData

INTEGER FUNCTION DetermineIndexGroupKeyFromMeterName (meterName)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function attemps to guess determine how a meter variable should be
    ! grouped.  It does this by parsing the meter name and then assigns a
    ! indexGroupKey based on the name

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: meterName   ! the meter name

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: indexGroupKey = -1

  ! Facility indices are in the 100s
  IF (INDEX(meterName, 'Electricity:Facility') > 0) THEN
      indexGroupKey = 100
  ELSEIF (INDEX(meterName, 'Gas:Facility') > 0) THEN
      indexGroupKey = 101
  ELSEIF (INDEX(meterName, 'DistricHeating:Facility') > 0) THEN
      indexGroupKey = 102
  ELSEIF (INDEX(meterName, 'DistricCooling:Facility') > 0) THEN
      indexGroupKey = 103
  ELSEIF (INDEX(meterName, 'ElectricityNet:Facility') > 0) THEN
      indexGroupKey = 104

  ! Building indices are in the 200s
  ELSEIF (INDEX(meterName, 'Electricity:Building') > 0) THEN
      indexGroupKey = 201
  ELSEIF (INDEX(meterName, 'Gas:Building') > 0) THEN
      indexGroupKey = 202

  ! HVAC indices are in the 300s
  ELSEIF (INDEX(meterName, 'Electricity:HVAC') > 0) THEN
      indexGroupKey = 301

  ! InteriorLights:Electricity indices are in the 400s
  ELSEIF (INDEX(meterName, 'InteriorLights:Electricity') > 0) THEN
      indexGroupKey = 401

  ! InteriorLights:Electricity:Zone indices are in the 500s
  ELSEIF (INDEX(meterName, 'InteriorLights:Electricity:Zone') > 0) THEN
      indexGroupKey = 501

  ! Unknown items have negative indices
  ELSE
      indexGroupKey = -11
  ENDIF

  DetermineIndexGroupKeyFromMeterName = indexGroupKey

  RETURN

END FUNCTION DetermineIndexGroupKeyFromMeterName

FUNCTION DetermineIndexGroupFromMeterGroup (meter) RESULT (indexGroup)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function attemps to determine how a meter variable should be
    ! grouped.  It does this by parsing the meter group

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE (MeterType), INTENT(IN) :: meter   ! the meter

  CHARACTER(len=MaxNameLength)  :: indexGroup

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  IF (LEN(TRIM(meter%Group)) > 0) THEN
    indexGroup = TRIM(meter%Group)
  ELSE
    indexGroup = 'Facility'
  ENDIF

  IF (LEN(TRIM(meter%ResourceType)) > 0) THEN
    indexGroup = TRIM(indexGroup) // ':' // TRIM(meter%ResourceType)
  ENDIF

  IF (LEN(TRIM(meter%EndUse)) > 0) THEN
    indexGroup = TRIM(indexGroup) // ':' // TRIM(meter%EndUse)
  ENDIF

  IF (LEN(TRIM(meter%EndUseSub)) > 0) THEN
    indexGroup = TRIM(indexGroup) // ':' // TRIM(meter%EndUseSub)
  ENDIF

  RETURN

END FUNCTION DetermineIndexGroupFromMeterGroup

SUBROUTINE SetInternalVariableValue(varType, keyVarIndex, SetRealVal, SetIntVal)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is a simple set routine for output pointers
          ! It is intended for special use to reinitializations those pointers used for EMS sensors

          ! METHODOLOGY EMPLOYED:
          ! given a variable type and variable index,
          ! assign the pointers the values passed in.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: varType      ! 1=integer, 2=real, 3=meter
  INTEGER,   INTENT(IN)    :: keyVarIndex  ! Array index
  REAL(r64), INTENT(IN)    :: SetRealVal   ! real value to set, if type is real or meter
  INTEGER,   INTENT(IN)    :: SetIntVal    ! integer value to set if type is integer

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  SELECT CASE (varType)

    CASE (1)  ! Integer
      IVar=>IVariableTypes(keyVarIndex)%VarPtr
      IVar%Which = SetIntVal
    CASE (2)  ! real
      RVar=>RVariableTypes(keyVarIndex)%VarPtr
      RVar%Which = setRealVal
    CASE (3)  ! meter
      EnergyMeters(keyVarIndex)%CurTSValue = setRealVal
  END SELECT

  RETURN

END SUBROUTINE SetInternalVariableValue


END MODULE OutputProcessor

!==============================================================================================
! *****************************************************************************
! These routines are available outside the OutputProcessor Module (i.e. calling
! routines do not have to "USE OutputProcessor".  But each of these routines
! will use the OutputProcessor and take advantage that everything is PUBLIC
! within the OutputProcessor.
! *****************************************************************************

SUBROUTINE SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
           ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult,indexGroupKey)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       January 2001; Implement Meters
          !                      August 2008; Implement SQL output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the variable data structure that will be used
          ! to track values of the output variables of EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE DataOutputs, ONLY: FindItemInVariableList
  USE InputProcessor, ONLY: FindItem,MakeUPPERCase,SameString
  USE General, ONLY: TrimSigDigits
  USE SQLiteProcedures

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)           :: VariableName    ! String Name of variable (with units)
  CHARACTER(len=*), INTENT(IN)           :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
  CHARACTER(len=*), INTENT(IN)           :: VariableTypeKey ! State, Average=1, NonState, Sum=2
  REAL(r64), INTENT(IN), TARGET          :: ActualVariable  ! Actual Variable, used to set up pointer
  CHARACTER(len=*), INTENT(IN)           :: KeyedValue      ! Associated Key for this variable
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Lights, Heating, Cooling, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseSubKey    ! Meter End Use Sub Key (General Lights, Task Lights, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneKey         ! Meter Zone Key (zone name)
  INTEGER, INTENT(IN), OPTIONAL          :: ZoneMult        ! Zone Multiplier, defaults to 1
  INTEGER, INTENT(IN), OPTIONAL          :: ZoneListMult    ! Zone List Multiplier, defaults to 1
  INTEGER, INTENT(IN), OPTIONAL          :: indexGroupKey   ! Group identifier for SQL output

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER CV
  CHARACTER(len=20) IDOut
  INTEGER          :: IndexType      ! 1=TimeStepZone, 2=TimeStepSys
  INTEGER          :: VariableType    ! 1=Average, 2=Sum, 3=Min/Max
  INTEGER          :: Loop
  INTEGER          :: RepFreq
  LOGICAL          :: OnMeter      ! True if this variable is on a meter
  CHARACTER(len=MaxNameLength) :: VarName       ! Variable name without units
!  CHARACTER(len=MaxNameLength) :: VariableNamewithUnits ! Variable name with units std format
  CHARACTER(len=MaxNameLength) :: ResourceType  ! Will hold value of ResourceTypeKey
  CHARACTER(len=MaxNameLength) :: EndUse        ! Will hold value of EndUseKey
  CHARACTER(len=MaxNameLength) :: EndUseSub     ! Will hold value of EndUseSubKey
  CHARACTER(len=MaxNameLength) :: Group         ! Will hold value of GroupKey
  CHARACTER(len=MaxNameLength) :: ZoneName      ! Will hold value of ZoneKey
  LOGICAL :: ErrorsFound=.false.  ! True if Errors Found
  INTEGER Item
  CHARACTER(len=16)             :: MtrUnits    ! Units for Meter
  LOGICAL :: ThisOneOnTheList
  CHARACTER(len=UnitsStringLength) :: UnitsString =BlankString ! Units for Variable (no brackets)
  INTEGER                      :: localIndexGroupKey
  LOGICAL          :: invalidUnits

  IF (.not. OutputInitialized) CALL InitializeOutput

!! Errors are severe and fatal because should only be encountered during development.
  Item=INDEX(VariableName,'[')
  IF (Item /= 0) THEN
    UnitsString=GetVariableUnitsString(VariableName)
    UnitsString=ADJUSTL(UnitsString)
    VarName=ADJUSTL(VariableName(1:Item-1))
!    VariableNamewithUnits=trim(VarName)//' ['//trim(UnitsString)//']'
    ! Check name length for variable name
    invalidUnits=.false.
    IF (UnitsString(1:1) == '-') invalidUnits=.true.
    IF (SameString(UnitsString,'dimensionless')) invalidUnits=.true.
    IF (LEN_TRIM(ADJUSTL(VariableName)) > MaxNameLength) THEN
      CALL ShowSevereError('Variable Name length (including units) ['//  &
         trim(TrimSigDigits(LEN_TRIM(ADJUSTL(VariableName))))//'] exceeds maximum='//TRIM(VariableName))
      IF (invalidUnits) CALL ShowSevereError('Variable has invalid units in call Variable='//trim(VariableName)//  &
         ', Units='//trim(UnitsString))
      CALL ShowFatalError('Program terminates.')
    ENDIF
    IF (invalidUnits) THEN
      CALL ShowSevereError('Variable has invalid units in call Variable='//trim(VariableName)//  &
       ', Units='//trim(UnitsString))
      CALL ShowFatalError('Program terminates.')
    ENDIF
  ELSE ! no units
    UnitsString=BlankString
    VarName=ADJUSTL(VariableName)
!    VariableNamewithUnits=trim(VarName)//' ['//trim(UnitsString)//']'
    IF (LEN_TRIM(ADJUSTL(VariableName)) > MaxNameLength) THEN
      CALL ShowSevereError('Variable Name has no units in call='//TRIM(VariableName))
      CALL ShowSevereError('Variable Name length exceeds maximum='//TRIM(VariableName))
      CALL ShowFatalError('Program terminates.')
    ENDIF
    CALL ShowSevereError('Variable Name has no units in call='//TRIM(VariableName))
    CALL ShowFatalError('Program terminates.')
  ENDIF

  ! Determine whether to Report or not
  CALL CheckReportVariable(KeyedValue,VarName)

  IF (NumExtraVars == 0) THEN
    NumExtraVars=1
    ReportList=-1
  ENDIF

  ! If ReportFreq present, overrides input
  IF (PRESENT(ReportFreq)) THEN
    CALL DetermineFrequency(ReportFreq,RepFreq)
    NumExtraVars=1
    ReportList=0
  ENDIF

  ThisOneOnTheList=FindItemInVariableList(KeyedValue,VarName)
  OnMeter=.false.  ! just a safety initialization

  DO Loop=1,NumExtraVars

    IF (Loop == 1) NumOfRVariable_Setup=NumOfRVariable_Setup+1

    IF (Loop == 1) THEN
      OnMeter=.false.
      IF (PRESENT(ResourceTypeKey)) THEN
        ResourceType=ResourceTypeKey
        OnMeter=.true.
      ELSE
        ResourceType=' '
      ENDIF
      IF (PRESENT(EndUseKey)) THEN
        EndUse=EndUseKey
        OnMeter=.true.
      ELSE
        EndUse=' '
      ENDIF
      IF (PRESENT(EndUseSubKey)) THEN
        EndUseSub=EndUseSubKey
        OnMeter=.true.
      ELSE
        EndUseSub=' '
      ENDIF
      IF (PRESENT(GroupKey)) THEN
        Group=GroupKey
        OnMeter=.true.
      ELSE
        Group=' '
      ENDIF
      IF (PRESENT(ZoneKey)) THEN
        ZoneName=ZoneKey
        OnMeter=.true.
      ELSE
        ZoneName=' '
      ENDIF
    ENDIF

    IndexType=ValidateIndexType(IndexTypeKey,'SetupRealOutputVariable')
    VariableType=ValidateVariableType(VariableTypeKey)

    CALL AddToOutputVariableList(VarName,IndexType,VariableType,VarType_Real,UnitsString)
    NumTotalRVariable=NumTotalRVariable+1

    IF (.not. OnMeter .and. .not. ThisOneOnTheList) CYCLE

    NumOfRVariable=NumOfRVariable+1
    IF (Loop == 1 .and. VariableType == SummedVar) THEN
      NumOfRVariable_Sum=NumOfRVariable_Sum+1
      IF (PRESENT(ResourceTypeKey)) THEN
        IF (ResourceTypeKey /= BlankString) NumOfRVariable_Meter=NumOfRVariable_Meter+1
      ENDIF
    ENDIF
    IF (NumOfRVariable > MaxRVariable) THEN
      CALL ReallocateRVar
    ENDIF
    CV=NumOfRVariable
    RVariableTypes(CV)%IndexType=IndexType
    RVariableTypes(CV)%StoreType=VariableType
    RVariableTypes(CV)%VarName=TRIM(KeyedValue)//':'//TRIM(VarName)
    RVariableTypes(CV)%VarNameOnly=TRIM(VarName)
    RVariableTypes(CV)%VarNameOnlyUC=MakeUPPERCase(VarName)
    RVariableTypes(CV)%VarNameUC=MakeUPPERCase(RVariableTypes(CV)%VarName)
    RVariableTypes(CV)%KeyNameOnlyUC=MakeUPPERCase(KeyedValue)
    RVariableTypes(CV)%UnitsString=UnitsString
    CALL AssignReportNumber(CurrentReportNumber)
    WRITE(IDOut,*) CurrentReportNumber
    IDOut=ADJUSTL(IDOut)

    ALLOCATE(RVariable)
    RVariable%Value=0.0d0
    RVariable%TSValue=0.0d0
    RVariable%StoreValue=0.0d0
    RVariable%NumStored=0.0d0
    RVariable%MaxValue=MaxSetValue
    RVariable%maxValueDate=0
    RVariable%MinValue=MinSetValue
    RVariable%minValueDate=0

    RVariableTypes(CV)%Varptr=>RVariable
    RVariable%Which=>ActualVariable
    RVariable%ReportID=CurrentReportNumber
    RVariableTypes(CV)%ReportID=CurrentReportNumber
    RVariable%ReportIDChr=IDOut(1:15)
    RVariable%StoreType=VariableType
    RVariable%Stored=.false.
    RVariable%Report=.false.
    RVariable%ReportFreq=ReportHourly
    RVariable%SchedPtr=0
    RVariable%MeterArrayPtr=0
    RVariable%ZoneMult=1
    RVariable%ZoneListMult=1
    IF (PRESENT(ZoneMult) .AND. PRESENT(ZoneListMult)) THEN
      RVariable%ZoneMult = ZoneMult
      RVariable%ZoneListMult = ZoneListMult
    ENDIF

    IF (Loop == 1) THEN
      IF (OnMeter) THEN
        IF (VariableType == AveragedVar) THEN
          CALL ShowSevereError('Meters can only be "Summed" variables')
          CALL ShowContinueError('..reference variable='//TRIM(KeyedValue)//':'//TRIM(VariableName))
          ErrorsFound=.true.
        ELSE
          MtrUnits=RVariableTypes(CV)%UnitsString
          ErrorsFound=.false.
          CALL AttachMeters(MtrUnits,ResourceType,EndUse,EndUseSub,Group,ZoneName,CV,RVariable%MeterArrayPtr,ErrorsFound)
          IF (ErrorsFound) THEN
            CALL ShowContinueError('Invalid Meter spec for variable='//TRIM(KeyedValue)//':'//TRIM(VariableName))
            ErrorsLogged=.true.
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    IF (ReportList(Loop) == -1) CYCLE

    RVariable%Report=.true.

    IF (ReportList(Loop) == 0) THEN
      RVariable%ReportFreq=RepFreq
      RVariable%SchedPtr=0
    ELSE
      RVariable%ReportFreq=ReqRepVars(ReportList(Loop))%ReportFreq
      RVariable%SchedPtr=ReqRepVars(ReportList(Loop))%SchedPtr
    ENDIF

    IF (RVariable%Report) THEN
      IF (PRESENT(indexGroupKey)) THEN
        localIndexGroupKey = indexGroupKey
      ELSE
        localIndexGroupKey = -999  ! Unknown Group
      ENDIF

      IF (RVariable%SchedPtr /= 0) THEN
        CALL WriteReportVariableDictionaryItem (RVariable%ReportFreq, RVariable%StoreType, &
            RVariable%ReportID, localIndexGroupKey, IndexTypeKey, RVariable%ReportIDChr, KeyedValue, VarName, &
            RVariableTypes(CV)%IndexType, RVariableTypes(CV)%UnitsString, &
            ReqRepVars(ReportList(Loop))%SchedName)
      ELSE
        CALL WriteReportVariableDictionaryItem (RVariable%ReportFreq, RVariable%StoreType, &
            RVariable%ReportID, localIndexGroupKey, IndexTypeKey, RVariable%ReportIDChr, KeyedValue, VarName, &
            RVariableTypes(CV)%IndexType, RVariableTypes(CV)%UnitsString)
      END IF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE SetupRealOutputVariable


SUBROUTINE SetupIntegerOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,ReportFreq,indexGroupKey)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       August 2008; Added SQL output capability
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the variable data structure that will be used
          ! to track values of the output variables of EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE InputProcessor, ONLY: FindItem,MakeUPPERCase,SameString
  USE General, ONLY: TrimSigDigits
  USE DataOutputs, ONLY: FindItemInVariableList
  USE SQLiteProcedures

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)           :: VariableName    ! String Name of variable
  CHARACTER(len=*), INTENT(IN)           :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
  CHARACTER(len=*), INTENT(IN)           :: VariableTypeKey ! State, Average=1, NonState, Sum=2
  INTEGER, INTENT(IN), TARGET            :: ActualVariable  ! Actual Variable, used to set up pointer
  CHARACTER(len=*), INTENT(IN)           :: KeyedValue      ! Associated Key for this variable
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
  INTEGER, INTENT(IN), OPTIONAL          :: indexGroupKey   ! Group identifier for SQL output

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER CV
  INTEGER Item
  CHARACTER(len=20) IDOut
  CHARACTER(len=MaxNameLength) :: VarName ! Variable without units
!  CHARACTER(len=MaxNameLength) :: VariableNamewithUnits ! Variable name with units std format
  INTEGER          :: IndexType      ! 1=TimeStepZone, 2=TimeStepSys
  INTEGER          :: VariableType    ! 1=Average, 2=Sum, 3=Min/Max
  INTEGER          :: localIndexGroupKey
  LOGICAL          :: ThisOneOnTheList
  LOGICAL          :: invalidUnits
  CHARACTER(len=UnitsStringLength) :: UnitsString =BlankString ! Units for Variable (no brackets)
  INTEGER          :: Loop
  INTEGER          :: RepFreq

  IF (.not. OutputInitialized) CALL InitializeOutput

!! Errors are severe and fatal because should only be encountered during development.
  Item=INDEX(VariableName,'[')
  IF (Item /= 0) THEN
    UnitsString=GetVariableUnitsString(VariableName)
    UnitsString=ADJUSTL(UnitsString)
    invalidUnits=.false.
    IF (UnitsString(1:1) == '-') invalidUnits=.true.
    IF (SameString(UnitsString,'dimensionless')) invalidUnits=.true.
    VarName=ADJUSTL(VariableName(1:Item-1))
!    VariableNamewithUnits=trim(VarName)//' ['//trim(UnitsString)//']'
    ! Check name length for variable name
    IF (LEN_TRIM(ADJUSTL(VariableName)) > MaxNameLength) THEN
      CALL ShowSevereError('Variable Name length (including units) ['//  &
         trim(TrimSigDigits(LEN_TRIM(ADJUSTL(VariableName))))//'] exceeds maximum='//TRIM(VariableName))
      IF (invalidUnits) CALL ShowSevereError('Variable has invalid units in call Variable='//trim(VariableName)//  &
         ', Units='//trim(UnitsString))
      CALL ShowFatalError('Program terminates.')
    ENDIF
    IF (invalidUnits) THEN
      CALL ShowSevereError('Variable has invalid units in call Variable='//trim(VariableName)//  &
       ', Units='//trim(UnitsString))
      CALL ShowFatalError('Program terminates.')
    ENDIF
  ELSE
    UnitsString=BlankString
    VarName=ADJUSTL(VariableName)
!    VariableNamewithUnits=trim(VarName)//' ['//trim(UnitsString)//']'
    IF (LEN_TRIM(ADJUSTL(VariableName)) > MaxNameLength) THEN
      CALL ShowSevereError('Variable Name has no units in call='//TRIM(VariableName))
      CALL ShowSevereError('Variable Name length exceeds maximum='//TRIM(VariableName))
      CALL ShowFatalError('Program terminates.')
    ENDIF
    CALL ShowSevereError('Variable Name has no units in call='//TRIM(VariableName))
    CALL ShowFatalError('Program terminates.')
  ENDIF

  ! Determine whether to Report or not
  CALL CheckReportVariable(KeyedValue,VarName)

  IF (NumExtraVars == 0) THEN
    NumExtraVars=1
    ReportList=-1
  ENDIF

  ! If ReportFreq present, overrides input
  IF (PRESENT(ReportFreq)) THEN
    CALL DetermineFrequency(ReportFreq,RepFreq)
    NumExtraVars=1
    ReportList=0
  ELSE
    RepFreq=ReportHourly
  ENDIF

  ThisOneOnTheList=FindItemInVariableList(KeyedValue,VarName)

  DO Loop=1,NumExtraVars

    IF (Loop == 1) NumOfIVariable_Setup=NumOfIVariable_Setup+1

    IndexType=ValidateIndexType(IndexTypeKey,'SetupIntegerOutputVariable')
    VariableType=ValidateVariableType(VariableTypeKey)

    CALL AddToOutputVariableList(VarName,IndexType,VariableType,VarType_Integer,UnitsString)
    NumTotalIVariable=NumTotalIVariable+1

    IF (.not. ThisOneOnTheList) CYCLE

    NumOfIVariable=NumOfIVariable+1
    IF (Loop == 1 .and. VariableType == SummedVar) THEN
      NumOfIVariable_Sum=NumOfIVariable_Sum+1
    ENDIF
    IF (NumOfIVariable > MaxIVariable) THEN
      CALL ReallocateIVar
    ENDIF

    CV=NumOfIVariable
    IVariableTypes(CV)%IndexType=IndexType
    IVariableTypes(CV)%StoreType=VariableType
    IVariableTypes(CV)%VarName=TRIM(KeyedValue)//':'//TRIM(VarName)
    IVariableTypes(CV)%VarNameOnly=TRIM(VarName)
    IVariableTypes(CV)%VarNameUC=MakeUPPERCase(IVariableTypes(CV)%VarName)
    IVariableTypes(CV)%UnitsString=UnitsString
    CALL AssignReportNumber(CurrentReportNumber)
    WRITE(IDOut,*) CurrentReportNumber
    IDOut=ADJUSTL(IDOut)

    ALLOCATE(IVariable)
    IVariable%Value=0.0d0
    IVariable%StoreValue=0.0d0
    IVariable%TSValue=0.0d0
    IVariable%NumStored=0.0d0
!    IVariable%LastTSValue=0
    IVariable%MaxValue=IMaxSetValue
    IVariable%maxValueDate=0
    IVariable%MinValue=IMinSetValue
    IVariable%minValueDate=0

    IVariableTypes(CV)%Varptr=>IVariable
    IVariable%Which=>ActualVariable
    IVariable%ReportID=CurrentReportNumber
    IVariableTypes(CV)%ReportID=CurrentReportNumber
    IVariable%ReportIDChr=IDOut(1:15)
    IVariable%StoreType=VariableType
    IVariable%Stored=.false.
    IVariable%Report=.false.
    IVariable%ReportFreq=ReportHourly
    IVariable%SchedPtr=0

    IF (ReportList(Loop) == -1) CYCLE

    IVariable%Report=.true.

    IF (ReportList(Loop) == 0) THEN
      IVariable%ReportFreq=RepFreq
      IVariable%SchedPtr=0
    ELSE
      IVariable%ReportFreq=ReqRepVars(ReportList(Loop))%ReportFreq
      IVariable%SchedPtr=ReqRepVars(ReportList(Loop))%SchedPtr
    ENDIF

    IF (IVariable%Report) THEN
      IF (PRESENT(indexGroupKey)) THEN
        localIndexGroupKey = indexGroupKey
      ELSE
        localIndexGroupKey = -999  ! Unknown Group
      ENDIF

      IF (IVariable%SchedPtr /= 0) THEN
        CALL WriteReportVariableDictionaryItem (IVariable%ReportFreq, IVariable%StoreType,      &
             IVariable%ReportID, localIndexGroupKey, IndexTypeKey, IVariable%ReportIDChr,       &
             KeyedValue, VarName, IVariableTypes(CV)%IndexType, IVariableTypes(CV)%UnitsString, &
             ReqRepVars(ReportList(Loop))%SchedName)
      ELSE
        CALL WriteReportVariableDictionaryItem (IVariable%ReportFreq, IVariable%StoreType,      &
             IVariable%ReportID, localIndexGroupKey, IndexTypeKey, IVariable%ReportIDChr,       &
             KeyedValue, VarName, IVariableTypes(CV)%IndexType, IVariableTypes(CV)%UnitsString)
      END IF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE SetupIntegerOutputVariable

SUBROUTINE SetupRealOutputVariable_IntKey(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
           ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult,indexGroupKey)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 1999
          !       MODIFIED       January 2001; Implement Meters
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allows an integer key for a variable.  Changes this to a
          ! standard character variable and passes everything to SetupOutputVariable.

          ! METHODOLOGY EMPLOYED:
          ! Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)           :: VariableName    ! String Name of variable
  CHARACTER(len=*), INTENT(IN)           :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
  CHARACTER(len=*), INTENT(IN)           :: VariableTypeKey ! State, Average=1, NonState, Sum=2
  REAL(r64), INTENT(IN), TARGET          :: ActualVariable  ! Actual Variable, used to set up pointer
  INTEGER, INTENT(IN)                    :: KeyedValue      ! Associated Key for this variable
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Lights, Heating, Cooling, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseSubKey    ! Meter End Use Sub Key (General Lights, Task Lights, etc)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneKey         ! Meter Zone Key (zone name)
  INTEGER, INTENT(IN), OPTIONAL          :: ZoneMult        ! Zone Multiplier, defaults to 1
  INTEGER, INTENT(IN), OPTIONAL          :: ZoneListMult    ! Zone List Multiplier, defaults to 1
  INTEGER, INTENT(IN), OPTIONAL          :: indexGroupKey   ! Group identifier for SQL output

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE SetupRealOutputVariable
    SUBROUTINE SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
               ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult,IndexGroupKey)
    USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL(r64), INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseSubKey    ! Meter End Use Sub Key (General Lights, Task Lights, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneKey         ! Meter Zone Key (zone name)
      INTEGER, INTENT(IN), OPTIONAL :: ZoneMult ! Zone Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: ZoneListMult ! Zone List Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: IndexGroupKey ! Group identifier for SQL output
    END SUBROUTINE
 END INTERFACE

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=20) IDOut

  ! Not checking for valid number

  WRITE(IDOut,*) KeyedValue
  IDOut=ADJUSTL(IDOut)

  IF (PRESENT(indexGroupKey)) THEN
      CALL SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,IDOut,  &
           ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult, &
           indexGroupKey)
  ELSE
      CALL SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,IDOut,  &
           ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult)
  ENDIF

  RETURN

END SUBROUTINE SetupRealOutputVariable_IntKey

SUBROUTINE UpdateDataandReport(IndexTypeKey)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       January 2001; Resolution integrated at the Zone TimeStep intervals
          !       MODIFIED       August 2008; Added SQL output capability
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes the actual report variable (for user requested
          ! Report Variables) strings to the standard output file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals, ONLY: HourOfDay, DayOfSimChr, EndHourFlag, EndDayFlag, EndEnvrnFlag
  USE DataInterfaces, ONLY: ShowRecurringWarningErrorAtEnd
  USE DataEnvironment, ONLY: EndMonthFlag
  USE General, ONLY: RemoveTrailingZeros, EncodeMonDayHrMin
  USE SQLiteProcedures

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop              ! Loop Variable
  INTEGER :: IndexType         ! Translate Zone=>1, HVAC=>2
  REAL(r64) :: CurVal          ! Current value for real variables
  REAL(r64) :: ICurVal         ! Current value for integer variables
  INTEGER :: MDHM              ! Month,Day,Hour,Minute
  LOGICAL :: TimePrint         ! True if the time needs to be printed
  REAL(r64) :: StartMinute     ! StartMinute for UpdateData call
  REAL(r64) :: MinuteNow       ! What minute it is now
  LOGICAL :: ReportNow         ! True if this variable should be reported now
  INTEGER :: CurDayType        ! What kind of day it is (weekday (sunday, etc) or holiday)
  INTEGER,SAVE :: LHourP      =-1      ! Helps set hours for timestamp output
  REAL(r64),SAVE :: LStartMin =-1.0d0  ! Helps set minutes for timestamp output
  REAL(r64),SAVE :: LEndMin   =-1.0d0  ! Helps set minutes for timestamp output
  LOGICAL,SAVE :: EndTimeStepFlag =.false.   ! True when it's the end of the Zone Time Step
  REAL(r64) :: rxTime  ! (MinuteNow-StartMinute)/REAL(MinutesPerTimeStep,r64) - for execution time

  IndexType=IndexTypeKey
  IF (IndexType /= ZoneTSReporting .and. IndexType /= HVACTSReporting) THEN
    CALL ShowFatalError('Invalid reporting requested -- UpdateDataAndReport')
  ENDIF

  SELECT CASE (IndexType)

  CASE(ZoneVar:HVACVar)

  ! Basic record keeping and report out if "detailed"

    StartMinute=TimeValue(IndexType)%CurMinute
    TimeValue(IndexType)%CurMinute=TimeValue(IndexType)%CurMinute+TimeValue(IndexType)%TimeStep*60.0d0
    IF (IndexType == HVACVar .and. TimeValue(HVACVar)%CurMinute == TimeValue(ZoneVar)%CurMinute) THEN
      EndTimeStepFlag=.true.
    ELSEIF (IndexType == ZoneVar) THEN
      EndTimeStepFlag=.true.
    ELSE
      EndTimeStepFlag=.false.
    ENDIF
    MinuteNow=TimeValue(IndexType)%CurMinute

    CALL EncodeMonDayHrMin(MDHM,Month,DayOfMonth,HourOfDay,INT(MinuteNow))
    TimePrint=.true.

    rxTime=(MinuteNow-StartMinute)/REAL(MinutesPerTimeStep,r64)

    ! Main "Record Keeping" Loops for R and I variables
    DO Loop=1,NumOfRVariable
      IF (RVariableTypes(Loop)%IndexType /= IndexType) CYCLE

      ! Act on the RVariables variable using the RVar structure
      RVar=>RVariableTypes(Loop)%VarPtr
      RVar%Stored=.true.
      IF (RVar%StoreType == AveragedVar) THEN
        CurVal=RVar%Which*rxTime
!        CALL SetMinMax(RVar%Which,MDHM,RVAR%MaxValue,RVar%maxValueDate,RVar%MinValue,RVar%minValueDate)
        IF (RVar%Which > RVAR%MaxValue) THEN
          RVAR%MaxValue=RVar%Which
          RVar%maxValueDate=MDHM
        ENDIF
        IF (RVar%Which < RVAR%MinValue) THEN
          RVAR%MinValue=RVar%Which
          RVar%minValueDate=MDHM
        ENDIF
        RVar%TSValue=RVar%TSValue+CurVal
        RVar%EITSValue = RVar%TSValue !CR - 8481 fix - 09/06/2011
      ELSE
!        CurVal=RVar%Which
        IF (RVar%Which > RVAR%MaxValue) THEN
          RVAR%MaxValue=RVar%Which
          RVar%maxValueDate=MDHM
        ENDIF
        IF (RVar%Which < RVAR%MinValue) THEN
          RVAR%MinValue=RVar%Which
          RVar%minValueDate=MDHM
        ENDIF
        RVar%TSValue=RVar%TSValue+RVar%Which
        RVar%EITSValue = RVar%TSValue !CR - 8481 fix - 09/06/2011
      ENDIF

      ! End of "record keeping"  Report if applicable
      IF (.not. RVar%Report) CYCLE
      ReportNow=.true.
      IF (RVar%SchedPtr > 0) &
        ReportNow=(GetCurrentScheduleValue(RVar%SchedPtr) /= 0.0d0)  ! SetReportNow(RVar%SchedPtr)
      IF (.not. ReportNow) CYCLE
      RVar%tsStored=.true.
      if (.not. RVar%thistsStored) then
        RVar%thisTScount=RVar%thisTScount+1
        RVar%thistsStored=.true.
      endif

      IF (RVar%ReportFreq == ReportEach) THEN
        IF (TimePrint) THEN
          IF (LHourP /= HourOfDay .or. ABS(LStartMin-StartMinute) > .001d0    &
              .or. ABS(LEndMin-TimeValue(IndexType)%CurMinute) > .001d0) THEN
            CurDayType=DayOfWeek
            IF (HolidayIndex > 0) THEN
              CurDayType=7+HolidayIndex
            ENDIF
            SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportEach, &
                  TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, DayOfSimChr, &
                  Month, DayOfMonth, HourOfDay, &
                  TimeValue(IndexType)%CurMinute, StartMinute, &
                  DSTIndicator, DayTypes(CurDayType))
            LHourP=HourOfDay
            LStartMin=StartMinute
            LEndMin=TimeValue(IndexType)%CurMinute
          ENDIF
          TimePrint=.false.
        ENDIF

        CALL WriteRealData (RVar%ReportID, RVar%ReportIDChr, SQLdbTimeIndex, RVar%Which)

        StdOutputRecordCount=StdOutputRecordCount+1
      ENDIF
    ENDDO

    DO Loop=1,NumOfIVariable
      IF (IVariableTypes(Loop)%IndexType /= IndexType) CYCLE

      ! Act on the IVariables variable using the IVar structure
      IVar=>IVariableTypes(Loop)%VarPtr
      IVar%Stored=.true.
!      ICurVal=IVar%Which
      IF (IVar%StoreType == AveragedVar) THEN
        ICurVal=IVar%Which*rxTime
        IVar%TSValue=IVar%TSValue+ICurVal
        IVar%EITSValue = IVar%TSValue !CR - 8481 fix - 09/06/2011
        IF (NINT(ICurVal) > IVar%MaxValue) THEN
          IVar%MaxValue=NINT(ICurVal)   ! Record keeping for date and time go here too
          IVar%maxValueDate=MDHM !+ TimeValue(IndexType)%TimeStep
        ENDIF
        IF (NINT(ICurVal) < IVar%MinValue) THEN
          IVar%MinValue=NINT(ICurVal)
          IVar%minValueDate=MDHM !+ TimeValue(IndexType)%TimeStep
        ENDIF
      ELSE
        IF (IVar%Which > IVar%MaxValue) THEN
          IVar%MaxValue=IVar%Which   ! Record keeping for date and time go here too
          IVar%maxValueDate=MDHM !+ TimeValue(IndexType)%TimeStep
        ENDIF
        IF (IVar%Which < IVar%MinValue) THEN
          IVar%MinValue=IVar%Which
          IVar%minValueDate=MDHM !+ TimeValue(IndexType)%TimeStep
        ENDIF
        IVar%TSValue=IVar%TSValue+IVar%Which
        IVar%EITSValue = IVar%TSValue !CR - 8481 fix - 09/06/2011
      ENDIF

      IF (.not. IVar%Report) CYCLE
      ReportNow=.true.
      IF (IVar%SchedPtr > 0) &
        ReportNow=(GetCurrentScheduleValue(IVar%SchedPtr) /= 0.0d0)  !SetReportNow(IVar%SchedPtr)
      IF (.not. ReportNow) CYCLE
      IVar%tsStored=.true.
      if (.not. IVar%thistsStored) then
        IVar%thisTScount=IVar%thisTScount+1
        IVar%thistsStored=.true.
      endif

      IF (IVar%ReportFreq == ReportEach) THEN
        IF (TimePrint) THEN
          IF (LHourP /= HourOfDay .or. ABS(LStartMin-StartMinute) > .001d0    &
              .or. ABS(LEndMin-TimeValue(IndexType)%CurMinute) > .001d0) THEN
            CurDayType=DayOfWeek
            IF (HolidayIndex > 0) THEN
              CurDayType=7+HolidayIndex
            ENDIF
            SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportEach, &
                    TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, DayOfSimChr, &
                    Month, DayOfMonth, HourOfDay, &
                    TimeValue(IndexType)%CurMinute, StartMinute, &
                    DSTIndicator, DayTypes(CurDayType))
            LHourP=HourOfDay
            LStartMin=StartMinute
            LEndMin=TimeValue(IndexType)%CurMinute
          ENDIF
          TimePrint=.false.
        ENDIF
        ! only time integer vars actual report as integer only is "detailed"
         CALL WriteIntegerData (IVar%ReportID, IVar%ReportIDChr, SQLdbTimeIndex, IntegerValue=IVar%Which)
        StdOutputRecordCount=StdOutputRecordCount+1
      ENDIF
    ENDDO

  CASE DEFAULT
    CALL ShowSevereError('Illegal Index passed to Report Variables')

  END SELECT

  IF (IndexType == HVACVar) RETURN  ! All other stuff happens at the "zone" time step call to this routine.

!! TimeStep Block (Report on Zone TimeStep)

  IF (EndTimeStepFlag) THEN

    DO IndexType = 1,2
      DO Loop=1,NumOfRVariable
        IF (RVariableTypes(Loop)%IndexType /= IndexType) CYCLE
        RVar=>RVariableTypes(Loop)%VarPtr
        ! Update meters on the TimeStep  (Zone)
        IF (RVar%MeterArrayPtr /= 0) THEN
          IF (VarMeterArrays(RVar%MeterArrayPtr)%NumOnCustomMeters <= 0) THEN
            CALL UpdateMeterValues(RVar%TSValue * RVar%ZoneMult * RVar%ZoneListMult,  &
                       VarMeterArrays(RVar%MeterArrayPtr)%NumOnMeters,VarMeterArrays(RVar%MeterArrayPtr)%OnMeters)
          ELSE
            CALL UpdateMeterValues(RVar%TSValue * RVar%ZoneMult * RVar%ZoneListMult,  &
                       VarMeterArrays(RVar%MeterArrayPtr)%NumOnMeters,VarMeterArrays(RVar%MeterArrayPtr)%OnMeters,  &
                       VarMeterArrays(RVar%MeterArrayPtr)%NumOnCustomMeters,VarMeterArrays(RVar%MeterArrayPtr)%OnCustomMeters)
          ENDIF
        ENDIF
        ReportNow=.true.
        IF (RVar%SchedPtr > 0) &
          ReportNow=(GetCurrentScheduleValue(RVar%SchedPtr) /= 0.0d0)  !SetReportNow(RVar%SchedPtr)
        IF (.not. ReportNow .or. .not. RVar%Report) THEN
          RVar%TSValue=0.0d0
        ENDIF
!        IF (RVar%StoreType == AveragedVar) THEN
!          RVar%Value=RVar%Value+RVar%TSValue/NumOfTimeStepInHour
!        ELSE
          RVar%Value=RVar%Value+RVar%TSValue
!        ENDIF

        IF (.not. ReportNow .or. .not. RVar%Report) CYCLE

        IF (RVar%ReportFreq == ReportTimeStep) THEN
          IF (TimePrint) THEN
            IF (LHourP /= HourOfDay .or. ABS(LStartMin-StartMinute) > .001d0    &
                .or. ABS(LEndMin-TimeValue(IndexType)%CurMinute) > .001d0) THEN
              CurDayType=DayOfWeek
              IF (HolidayIndex > 0) THEN
                CurDayType=7+HolidayIndex
              ENDIF
              SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportEach, &
                    TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, DayOfSimChr, &
                    Month, DayOfMonth, HourOfDay, &
                    TimeValue(IndexType)%CurMinute, StartMinute, &
                    DSTIndicator, DayTypes(CurDayType))
              LHourP=HourOfDay
              LStartMin=StartMinute
              LEndMin=TimeValue(IndexType)%CurMinute
            ENDIF
            TimePrint=.false.
          ENDIF

          CALL WriteRealData (RVar%ReportID, RVar%ReportIDChr, SQLdbTimeIndex, RVar%TSValue)
          StdOutputRecordCount=StdOutputRecordCount+1
        ENDIF
        RVar%TSValue=0.0d0
        RVar%thisTSstored=.false.
      ENDDO  ! Number of R Variables

      DO Loop=1,NumOfIVariable
        IF (IVariableTypes(Loop)%IndexType /= IndexType) CYCLE
        IVar=>IVariableTypes(Loop)%VarPtr
        ReportNow=.true.
        IF (IVar%SchedPtr > 0) &
          ReportNow=(GetCurrentScheduleValue(IVar%SchedPtr) /= 0.0d0)  ! SetReportNow(IVar%SchedPtr)
        IF (.not. ReportNow) THEN
          IVar%TSValue=0.0d0
        ENDIF
!        IF (IVar%StoreType == AveragedVar) THEN
!          IVar%Value=IVar%Value+REAL(IVar%TSValue,r64)/REAL(NumOfTimeStepInHour,r64)
!        ELSE
          IVar%Value=IVar%Value+IVar%TSValue
!        ENDIF

        IF (.not. ReportNow .or. .not. IVar%Report) CYCLE

        IF (IVar%ReportFreq == ReportTimeStep) THEN
          IF (TimePrint) THEN
            IF (LHourP /= HourOfDay .or. ABS(LStartMin-StartMinute) > .001d0    &
                .or. ABS(LEndMin-TimeValue(IndexType)%CurMinute) > .001d0) THEN
              CurDayType=DayOfWeek
              IF (HolidayIndex > 0) THEN
                CurDayType=7+HolidayIndex
              ENDIF
              SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportEach, &
                    TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, DayOfSimChr, &
                    Month, DayOfMonth, HourOfDay, &
                    TimeValue(IndexType)%CurMinute, StartMinute, &
                    DSTIndicator, DayTypes(CurDayType))
              LHourP=HourOfDay
              LStartMin=StartMinute
              LEndMin=TimeValue(IndexType)%CurMinute
            ENDIF
            TimePrint=.false.
          ENDIF

          CALL WriteIntegerData (IVar%ReportID, IVar%ReportIDChr, SQLdbTimeIndex, RealValue=IVar%TSValue)
          StdOutputRecordCount=StdOutputRecordCount+1
        ENDIF
        IVar%TSValue=0.0d0
        IVar%thisTSstored=.false.
      ENDDO  ! Number of I Variables
    ENDDO    ! Index Type (Zone or HVAC)

    CALL UpdateMeters(MDHM)

    CALL ReportTSMeters(StartMinute,TimeValue(1)%CurMinute,TimePrint)

  ENDIF  ! TimeStep Block


!!   Hour Block
  IF (EndHourFlag) THEN
    IF (TrackingHourlyVariables) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportHourly, &
            TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, DayOfSimChr,  &
            Month, DayOfMonth, HourOfDay, &
            DST=DSTIndicator, DayType=DayTypes(CurDayType))
   ENDIF

    DO IndexType=1,2  ! Zone, HVAC
      TimeValue(IndexType)%CurMinute=0.0d0
      DO Loop=1,NumOfRVariable
        IF (RVariableTypes(Loop)%IndexType /= IndexType) CYCLE
        RVar=>RVariableTypes(Loop)%VarPtr
!        ReportNow=.true.
!        IF (RVar%SchedPtr > 0) &
!          ReportNow=(GetCurrentScheduleValue(RVar%SchedPtr) /= 0.0)  !SetReportNow(RVar%SchedPtr)

!        IF (ReportNow) THEN
        IF (RVar%tsStored) THEN
          IF (RVar%StoreType == AveragedVar) THEN
            RVar%Value=RVar%Value/REAL(RVar%thisTSCount,r64)
          ENDIF
          IF (RVar%Report .and. RVar%ReportFreq == ReportHourly .and. RVar%Stored) THEN
            CALL WriteRealData (RVar%ReportID, RVar%ReportIDChr, SQLdbTimeIndex, RVar%Value)
            StdOutputRecordCount=StdOutputRecordCount+1
            RVar%Stored=.false.
          ENDIF
          RVar%StoreValue=RVar%StoreValue+RVar%Value
          RVar%NumStored=RVar%NumStored+1.0d0
        ENDIF
        RVar%tsStored=.false.
        RVar%thisTSstored=.false.
        RVar%thisTScount=0
        RVar%Value=0.0d0
      ENDDO ! Number of R Variables

      DO Loop=1,NumOfIVariable
        IF (IVariableTypes(Loop)%IndexType /= IndexType) CYCLE
        IVar=>IVariableTypes(Loop)%VarPtr
!        ReportNow=.true.
!        IF (IVar%SchedPtr > 0) &
!          ReportNow=(GetCurrentScheduleValue(IVar%SchedPtr) /= 0.0)  !SetReportNow(IVar%SchedPtr)
!        IF (ReportNow) THEN
        IF (IVar%tsStored) THEN
          IF (IVar%StoreType == AveragedVar) THEN
            IVar%Value=IVar%Value/REAL(IVar%thisTSCount,r64)
          ENDIF
          IF (IVar%Report .and. IVar%ReportFreq == ReportHourly .and. IVar%Stored) THEN
            CALL WriteIntegerData (IVar%ReportID, IVar%ReportIDChr, SQLdbTimeIndex, RealValue=IVar%Value)
            StdOutputRecordCount=StdOutputRecordCount+1
            IVar%Stored=.false.
          ENDIF
          IVar%StoreValue=IVar%StoreValue+IVar%Value
          IVar%NumStored=IVar%NumStored+1.0d0
        ENDIF
        IVar%tsStored=.false.
        IVar%thisTSstored=.false.
        IVar%thisTScount=0
        IVar%Value=0.0d0
      ENDDO ! Number of I Variables
    ENDDO   ! IndexType (Zone or HVAC)

    CALL ReportHRMeters

  ENDIF   ! Hour Block

  IF (.not. EndHourFlag) RETURN

!!    Day Block
  IF (EndDayFlag) THEN
    IF (TrackingDailyVariables) THEN
      CurDayType=DayOfWeek
      IF (HolidayIndex > 0) THEN
        CurDayType=7+HolidayIndex
      ENDIF
      SQLdbTimeIndex = WriteTimeStampFormatData(OutputFileStandard, ReportDaily, &
            DailyStampReportNbr, DailyStampReportChr, DayOfSim, DayOfSimChr,  &
            Month, DayOfMonth,  &
            DST=DSTIndicator, DayType=DayTypes(CurDayType))
    ENDIF
    NumHoursinMonth=NumHoursinMonth+24
    DO IndexType=1,2
      DO Loop=1,NumOfRVariable
        IF (RVariableTypes(Loop)%IndexType == IndexType) THEN
          RVar=>RVariableTypes(Loop)%VarPtr
          CALL WriteRealVariableOutput (ReportDaily, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of R Variables

      DO Loop=1,NumOfIVariable
        IF (IVariableTypes(Loop)%IndexType == IndexType) THEN
          IVar=>IVariableTypes(Loop)%VarPtr
          CALL WriteIntegerVariableOutput (ReportDaily, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of I Variables
    ENDDO    ! Index type (Zone or HVAC)

    CALL ReportDYMeters

  ENDIF  ! Day Block

  ! Only continue if EndDayFlag is set
  IF (.not. EndDayFlag) RETURN

!!  Month Block
  IF (EndMonthFlag .or. EndEnvrnFlag) THEN
    IF (TrackingMonthlyVariables) THEN
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportMonthly, &
            MonthlyStampReportNbr, MonthlyStampReportChr, DayOfSim, DayOfSimChr, Month)
    ENDIF
    NumHoursinSim=NumHoursinSim+NumHoursinMonth
    EndMonthFlag=.false.
    DO IndexType=1,2  ! Zone, HVAC
      DO Loop=1,NumOfRVariable
        IF (RVariableTypes(Loop)%IndexType == IndexType) THEN
          RVar=>RVariableTypes(Loop)%VarPtr
          CALL WriteRealVariableOutput (ReportMonthly, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of R Variables

      DO Loop=1,NumOfIVariable
        IF (IVariableTypes(Loop)%IndexType == IndexType) THEN
          IVar=>IVariableTypes(Loop)%VarPtr
          CALL WriteIntegerVariableOutput (ReportMonthly, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of I Variables
    ENDDO    ! IndexType (Zone, HVAC)

    CALL ReportMNMeters

    NumHoursInMonth=0
  ENDIF  ! Month Block

!!  Sim/Environment Block
  IF (EndEnvrnFlag) THEN
    IF (TrackingRunPeriodVariables) THEN
      SQLdbTimeIndex = WriteTimeStampFormatData (OutputFileStandard, ReportSim, &
            RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr)
    ENDIF
    DO IndexType=1,2  ! Zone, HVAC
      DO Loop=1,NumOfRVariable
        IF (RVariableTypes(Loop)%IndexType == IndexType) THEN
          RVar=>RVariableTypes(Loop)%VarPtr
          CALL WriteRealVariableOutput (ReportSim, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of R Variables

      DO Loop=1,NumOfIVariable
        IF (IVariableTypes(Loop)%IndexType == IndexType) THEN
          IVar=>IVariableTypes(Loop)%VarPtr
          CALL WriteIntegerVariableOutput (ReportSim, SQLdbTimeIndex)
        END IF
      ENDDO  ! Number of I Variables
    ENDDO    ! Index Type (Zone, HVAC)

    CALL ReportSMMeters

    NumHoursInSim=0
  ENDIF

  RETURN

END SUBROUTINE UpdateDataandReport


SUBROUTINE AssignReportNumber(ReportNumber)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns the next report number available.  The report number
          ! is used in output reports as a key.

          ! METHODOLOGY EMPLOYED:
          ! Use internal ReportNumberCounter to maintain current report numbers.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(OUT) :: ReportNumber

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER,SAVE  :: ReportNumberCounter=0

  ReportNumberCounter=ReportNumberCounter+1
  ReportNumber=ReportNumberCounter

  RETURN

END SUBROUTINE AssignReportNumber

SUBROUTINE GenOutputVariablesAuditReport

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports (to the .err file) any report variables
          ! which were requested but not "setup" during the run.  These will
          ! either be items that were not used in the IDF file or misspellings
          ! of report variable names.

          ! METHODOLOGY EMPLOYED:
          ! Use flagged data structure in OutputProcessor.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE DataGlobals,    ONLY: DisplayAdvancedReportVariables
  USE DataInterfaces, ONLY: ShowWarningError, ShowMessage, ShowContinueError
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(-1:4) :: ReportFrequency =(/'Detailed','Timestep','Hourly  ',  &
                                                                     'Daily   ','Monthly ','Annual  '/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: Rept=.false.
  INTEGER :: Loop
  LOGICAL,SAVE :: OpaqSurfWarned=.false.

  DO Loop=1,NumOfReqVariables
    IF (ReqRepVars(Loop)%Used) CYCLE
    IF (ReqRepVars(Loop)%Key == '    ') ReqRepVars(Loop)%Key='*'
    IF (INDEX(ReqRepVars(Loop)%VarName,'OPAQUE SURFACE INSIDE FACE CONDUCTION') > 0 .and.  &
              .not. DisplayAdvancedReportVariables .and. .not. OpaqSurfWarned) THEN
      CALL ShowWarningError('Variables containing "Opaque Surface Inside Face Conduction" are now "advanced" variables.')
      CALL ShowContinueError('You must enter the "Output:Diagnostics,DisplayAdvancedReportVariables;" statement to view.')
      CALL ShowContinueError('First, though, read cautionary statements in the "InputOutputReference" document.')
      OpaqSurfWarned=.true.
    ENDIF
    IF (.not. Rept) THEN
      CALL ShowWarningError('The following Report Variables were requested but not generated')
      CALL ShowContinueError('because IDF did not contain these elements or misspelled variable name -- check .rdd file')
      Rept=.true.
    ENDIF
    CALL ShowMessage('Key='//TRIM(ReqRepVars(Loop)%Key)//', VarName='//TRIM(ReqRepVars(Loop)%VarName)//  &
                     ', Frequency='//TRIM(ReportFrequency(ReqRepVars(Loop)%ReportFreq)))
  ENDDO

  RETURN

END SUBROUTINE GenOutputVariablesAuditReport

SUBROUTINE UpdateMeterReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       February 2007 -- add cumulative meter reporting
          !                      January 2012 -- add predefined tabular meter reporting
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called at the end of the first HVAC iteration and
          ! sets up the reporting for the Energy Meters.  It also may show a fatal error
          ! if errors occurred during initial SetupOutputVariable processing.  It "gets"
          ! the Report Meter input:
          ! Report Meter,
          !        \memo Meters requested here show up on eplusout.eso and eplusout.mtr
          !   A1 , \field Meter_Name
          !        \required-field
          !        \note Form is EnergyUseType:..., e.g. Electricity:* for all Electricity meters
          !        \note or EndUse:..., e.g. InteriorLights:* for all interior lights
          !        \note Report MeterFileOnly puts results on the eplusout.mtr file only
          !   A2 ; \field Reporting_Frequency
          !        \type choice
          !        \key timestep
          !        \note timestep refers to the zone timestep/timestep in hour value
          !        \note runperiod, environment, and annual are the same
          !        \key hourly
          !        \key daily
          !        \key monthly
          !        \key runperiod
          !        \key environment
          !        \key annual
          !        \note runperiod, environment, and annual are synonymous
          !
          ! Report MeterFileOnly,
          !        \memo same reporting as Report Meter -- goes to eplusout.mtr only
          !   A1 , \field Meter_Name
          !        \required-field
          !        \note Form is EnergyUseType:..., e.g. Electricity:* for all Electricity meters
          !        \note or EndUse:..., e.g. InteriorLights:* for all interior lights
          !        \note Report MeterFileOnly puts results on the eplusout.mtr file only
          !   A2 ; \field Reporting_Frequency
          !        \type choice
          !        \key timestep
          !        \note timestep refers to the zone timestep/timestep in hour value
          !        \note runperiod, environment, and annual are the same
          !        \key hourly
          !        \key daily
          !        \key monthly
          !        \key runperiod
          !        \key environment
          !        \key annual
          !        \note runperiod, environment, and annual are synonymous


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE DataPrecisionGlobals
  USE InputProcessor
  USE OutputProcessor
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  CHARACTER(len=MaxNameLength), DIMENSION(2) :: Alphas
  REAL(r64), DIMENSION(1)                         :: Numbers
  INTEGER NumAlpha
  INTEGER NumNumbers
  INTEGER IOStat
  INTEGER WildCard
  INTEGER TestLen
  INTEGER varnameLen
  INTEGER NumReqMeters
  INTEGER NumReqMeterFOs
  INTEGER Meter
  INTEGER ReportFreq
  LOGICAL NeverFound

  LOGICAL :: ErrorsFound = .false.   ! If errors detected in input

  CALL GetCustomMeterInput(ErrorsFound)
  IF (ErrorsFound) THEN
    ErrorsLogged=.true.
  ENDIF

  cCurrentModuleObject='Output:Meter'
  NumReqMeters=GetNumObjectsFound(cCurrentModuleObject)

  DO Loop=1,NumReqMeters

    CALL GetObjectItem(cCurrentModuleObject,Loop,Alphas,NumAlpha,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    varnameLen=INDEX(Alphas(1),'[')
    IF (varnameLen /= 0) Alphas(1)=Alphas(1)(1:varnameLen-1)

    WildCard=INDEX(Alphas(1),'*')
    IF (WildCard /= 0) THEN
      TestLen=WildCard-1
    ENDIF

    CALL DetermineFrequency(Alphas(2),ReportFreq)

    IF (WildCard == 0) THEN
      Meter=FindItem(Alphas(1),EnergyMeters%Name,NumEnergyMeters)
      IF (Meter == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
        CYCLE
      ENDIF

      CALL SetInitialMeterReportingAndOutputNames(Meter,.false.,ReportFreq,.false.)

    ELSE   ! Wildcard input
      NeverFound=.true.
      DO Meter=1,NumEnergyMeters
        IF (.not. SameString(EnergyMeters(Meter)%Name(1:TestLen),Alphas(1)(1:TestLen))) CYCLE
        NeverFound=.false.

        CALL SetInitialMeterReportingAndOutputNames(Meter,.false.,ReportFreq,.false.)

      ENDDO
      IF (NeverFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
      ENDIF
    ENDIF

  ENDDO

  cCurrentModuleObject='Output:Meter:MeterFileOnly'
  NumReqMeterFOs=GetNumObjectsFound(cCurrentModuleObject)
  DO Loop=1,NumReqMeterFOs

    CALL GetObjectItem(cCurrentModuleObject,Loop,Alphas,NumAlpha,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    varnameLen=INDEX(Alphas(1),'[')
    IF (varnameLen /= 0) Alphas(1)=Alphas(1)(1:varnameLen-1)

    WildCard=INDEX(Alphas(1),'*')
    IF (WildCard /= 0) THEN
      TestLen=WildCard-1
    ENDIF

    CALL DetermineFrequency(Alphas(2),ReportFreq)

    IF (WildCard == 0) THEN
      Meter=FindItem(Alphas(1),EnergyMeters%Name,NumEnergyMeters)
      IF (Meter == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
        CYCLE
      ENDIF

      CALL SetInitialMeterReportingAndOutputNames(Meter,.true.,ReportFreq,.false.)

    ELSE   ! Wildcard input
      NeverFound=.true.
      DO Meter=1,NumEnergyMeters
        IF (.not. SameString(EnergyMeters(Meter)%Name(1:TestLen),Alphas(1)(1:TestLen))) CYCLE
        NeverFound=.false.

        CALL SetInitialMeterReportingAndOutputNames(Meter,.true.,ReportFreq,.false.)

      ENDDO
      IF (NeverFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
      ENDIF
    ENDIF

  ENDDO

  cCurrentModuleObject='Output:Meter:Cumulative'
  NumReqMeters=GetNumObjectsFound(cCurrentModuleObject)

  DO Loop=1,NumReqMeters

    CALL GetObjectItem(cCurrentModuleObject,Loop,Alphas,NumAlpha,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    varnameLen=INDEX(Alphas(1),'[')
    IF (varnameLen /= 0) Alphas(1)=Alphas(1)(1:varnameLen-1)

    WildCard=INDEX(Alphas(1),'*')
    IF (WildCard /= 0) THEN
      TestLen=WildCard-1
    ENDIF

    CALL DetermineFrequency(Alphas(2),ReportFreq)

    IF (WildCard == 0) THEN
      Meter=FindItem(Alphas(1),EnergyMeters%Name,NumEnergyMeters)
      IF (Meter == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
        CYCLE
      ENDIF

      CALL SetInitialMeterReportingAndOutputNames(Meter,.false.,ReportFreq,.true.)

    ELSE   ! Wildcard input
      NeverFound=.true.
      DO Meter=1,NumEnergyMeters
        IF (.not. SameString(EnergyMeters(Meter)%Name(1:TestLen),Alphas(1)(1:TestLen))) CYCLE
        NeverFound=.false.

        CALL SetInitialMeterReportingAndOutputNames(Meter,.false.,ReportFreq,.true.)

      ENDDO
      IF (NeverFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
      ENDIF
    ENDIF

  ENDDO

  cCurrentModuleObject='Output:Meter:Cumulative:MeterFileOnly'
  NumReqMeterFOs=GetNumObjectsFound(cCurrentModuleObject)
  DO Loop=1,NumReqMeterFOs

    CALL GetObjectItem(cCurrentModuleObject,Loop,Alphas,NumAlpha,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    varnameLen=INDEX(Alphas(1),'[')
    IF (varnameLen /= 0) Alphas(1)=Alphas(1)(1:varnameLen-1)

    WildCard=INDEX(Alphas(1),'*')
    IF (WildCard /= 0) THEN
      TestLen=WildCard-1
    ENDIF

    CALL DetermineFrequency(Alphas(2),ReportFreq)

    IF (WildCard == 0) THEN
      Meter=FindItem(Alphas(1),EnergyMeters%Name,NumEnergyMeters)
      IF (Meter == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
        CYCLE
      ENDIF

      CALL SetInitialMeterReportingAndOutputNames(Meter,.true.,ReportFreq,.true.)

    ELSE   ! Wildcard input
      NeverFound=.true.
      DO Meter=1,NumEnergyMeters
        IF (.not. SameString(EnergyMeters(Meter)%Name(1:TestLen),Alphas(1)(1:TestLen))) CYCLE
        NeverFound=.false.

        CALL SetInitialMeterReportingAndOutputNames(Meter,.true.,ReportFreq,.true.)

      ENDDO
      IF (NeverFound) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
           TRIM(Alphas(1))//'" - not found.')
      ENDIF
    ENDIF

  ENDDO

  CALL ReportMeterDetails

  IF (ErrorsLogged) THEN
    CALL ShowFatalError('UpdateMeterReporting: Previous Meter Specification errors cause program termination.')
  ENDIF

  ALLOCATE(MeterValue(NumEnergyMeters))
  MeterValue=0.0d0

  RETURN

END SUBROUTINE UpdateMeterReporting

SUBROUTINE SetInitialMeterReportingAndOutputNames(WhichMeter,MeterFileOnlyIndicator,FrequencyIndicator,CumulativeIndicator)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set values and output initial names to output files.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WhichMeter             ! Which meter number
  LOGICAL, INTENT(IN) :: MeterFileOnlyIndicator ! true if this is a meter file only reporting
  INTEGER, INTENT(IN) :: FrequencyIndicator     ! at what frequency is the meter reported
  LOGICAL, INTENT(IN) :: CumulativeIndicator    ! true if this is a Cumulative meter reporting

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: indexGroupKey
  CHARACTER(len=MaxNameLength)  :: indexGroup

  SELECT CASE (FrequencyIndicator)
  CASE (-1:0)  ! roll "detailed" into TimeStep
    IF (.not. CumulativeIndicator) THEN
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptTS) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "'//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (TimeStep), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptTS) THEN
        EnergyMeters(WhichMeter)%RptTS=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptTSFO=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%TSRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(WhichMeter)%TSRptNumChr, EnergyMeters(WhichMeter)%Name, &
             EnergyMeters(WhichMeter)%Units, .false., MeterFileOnlyIndicator)
      ENDIF
    ELSE
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptAccTS) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "Cumulative '//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (TimeStep), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptAccTS) THEN
        EnergyMeters(WhichMeter)%RptAccTS=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptAccTSFO=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%TSAccRptNum, &
             indexGroupKey, indexGroup, TrimSigDigits(EnergyMeters(WhichMeter)%TSAccRptNum), &
             EnergyMeters(WhichMeter)%Name, &
             EnergyMeters(WhichMeter)%Units, .true., MeterFileOnlyIndicator)
      ENDIF
    ENDIF
  CASE (1)
    IF (.not. CumulativeIndicator) THEN
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptHR) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "'//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Hourly), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptHR) THEN
        EnergyMeters(WhichMeter)%RptHR=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptHRFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingHourlyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%HRRptNum, indexGroupKey,  &
                indexGroup,EnergyMeters(WhichMeter)%HRRptNumChr, EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .false., MeterFileOnlyIndicator)
      ENDIF
    ELSE
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptAccHR) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "Cumulative '//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Hourly), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptAccHR) THEN
        EnergyMeters(WhichMeter)%RptAccHR=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptAccHRFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingHourlyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%HRAccRptNum, indexGroupKey,   &
                indexGroup,TrimSigDigits(EnergyMeters(WhichMeter)%HRAccRptNum), EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .true., MeterFileOnlyIndicator)
      ENDIF
    ENDIF
  CASE (2)
    IF (.not. CumulativeIndicator) THEN
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptDY) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "'//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Daily), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptDY) THEN
        EnergyMeters(WhichMeter)%RptDY=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptDYFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingDailyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%DYRptNum, indexGroupKey,  &
                indexGroup,EnergyMeters(WhichMeter)%DYRptNumChr, EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .false., MeterFileOnlyIndicator)
      ENDIF
    ELSE
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptAccDY) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "Cumulative '//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Hourly), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptAccDY) THEN
        EnergyMeters(WhichMeter)%RptAccDY=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptAccDYFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingDailyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%DYAccRptNum, indexGroupKey,  &
                indexGroup,TrimSigDigits(EnergyMeters(WhichMeter)%DYAccRptNum), EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .true., MeterFileOnlyIndicator)
      ENDIF
    ENDIF
  CASE (3)
    IF (.not. CumulativeIndicator) THEN
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptMN) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "'//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Monthly), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptMN) THEN
        EnergyMeters(WhichMeter)%RptMN=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptMNFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingMonthlyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%MNRptNum, indexGroupKey,  &
                indexGroup,EnergyMeters(WhichMeter)%MNRptNumChr, EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .false., MeterFileOnlyIndicator)
      ENDIF
    ELSE
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptAccMN) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "Cumulative '//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (Monthly), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptAccMN) THEN
        EnergyMeters(WhichMeter)%RptAccMN=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptAccMNFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingMonthlyVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%MNAccRptNum, indexGroupKey,  &
                indexGroup,TrimSigDigits(EnergyMeters(WhichMeter)%MNAccRptNum), EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .true., MeterFileOnlyIndicator)
      ENDIF
    ENDIF
  CASE (4)
    IF (.not. CumulativeIndicator) THEN
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptSM) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "'//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (RunPeriod), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptSM) THEN
        EnergyMeters(WhichMeter)%RptSM=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptSMFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingRunPeriodVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%SMRptNum, indexGroupKey,  &
                indexGroup,EnergyMeters(WhichMeter)%SMRptNumChr, EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .false., MeterFileOnlyIndicator)
      ENDIF
    ELSE
      IF (MeterFileOnlyIndicator) THEN
        IF (EnergyMeters(WhichMeter)%RptAccSM) THEN
          CALL ShowWarningError('Output:Meter:MeterFileOnly requested for "Cumulative '//  &
                                TRIM(EnergyMeters(WhichMeter)%Name)//'" (RunPeriod), '//  &
                                'already on "Output:Meter". Will report to both eplusout.eso and eplusout.mtr.')
        ENDIF
      ENDIF
      IF (.not. EnergyMeters(WhichMeter)%RptAccSM) THEN
        EnergyMeters(WhichMeter)%RptAccSM=.true.
        IF (MeterFileOnlyIndicator) EnergyMeters(WhichMeter)%RptAccSMFO=.true.
        IF (.not. MeterFileOnlyIndicator) TrackingRunPeriodVariables=.true.
        indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(WhichMeter)%Name)
        indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(WhichMeter))
        CALL WriteMeterDictionaryItem (FrequencyIndicator, SummedVar, EnergyMeters(WhichMeter)%SMAccRptNum, indexGroupKey,  &
                indexGroup,TrimSigDigits(EnergyMeters(WhichMeter)%SMAccRptNum), EnergyMeters(WhichMeter)%Name, &
                EnergyMeters(WhichMeter)%Units, .true., MeterFileOnlyIndicator)
      ENDIF
    ENDIF
  CASE DEFAULT
  END SELECT

  RETURN

END SUBROUTINE SetInitialMeterReportingAndOutputNames

FUNCTION GetMeterIndex(MeterName) RESULT(MeterIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns a index to the meter "number" (aka assigned report number)
          ! for the meter name.  If none active for this run, a zero is returned.  This is used later to
          ! obtain a meter "value".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE InputProcessor, ONLY: MakeUPPERCase,FindItemInSortedList
  USE SortAndStringUtilities, ONLY: SetupAndSort

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: MeterName
  INTEGER                      :: MeterIndex

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  ! Valid Meter names because matching case insensitive
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:), SAVE :: ValidMeterNames
  INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: iValidMeterNames
  INTEGER,SAVE :: NumValidMeters=0
  INTEGER      :: Found
  LOGICAL,SAVE :: FirstCall=.true.

  IF (FirstCall) THEN
    NumValidMeters=NumEnergyMeters
    ALLOCATE(ValidMeterNames(NumValidMeters))
    ALLOCATE(iValidMeterNames(NumValidMeters))
    iValidMeterNames=0
    DO Found=1,NumValidMeters
      ValidMeterNames(Found)=MakeUPPERCase(EnergyMeters(Found)%Name)
    ENDDO
    FirstCall=.false.
    CALL SetupAndSort(ValidMeterNames,iValidMeterNames)
  ELSEIF (NumValidMeters /= NumEnergyMeters) THEN
    DEALLOCATE(ValidMeterNames)
    DEALLOCATE(iValidMeterNames)
    NumValidMeters=NumEnergyMeters
    ALLOCATE(ValidMeterNames(NumValidMeters))
    ALLOCATE(iValidMeterNames(NumValidMeters))
    iValidMeterNames=0
    DO Found=1,NumValidMeters
      ValidMeterNames(Found)=MakeUPPERCase(EnergyMeters(Found)%Name)
    ENDDO
    CALL SetupAndSort(ValidMeterNames,iValidMeterNames)
  ENDIF

  MeterIndex=FindItemInSortedList(MakeUPPERCase(MeterName),ValidMeterNames,NumValidMeters)
  IF (MeterIndex /= 0) MeterIndex=iValidMeterNames(MeterIndex)

  RETURN

END FUNCTION GetMeterIndex

FUNCTION GetMeterResourceType(MeterNumber) RESULT(ResourceType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the character string of Resource Type for the
          ! given meter number/index. If MeterNumber is 0, ResourceType=Invalid/Unknown.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: MeterNumber ! Which Meter Number (from GetMeterIndex)
  CHARACTER(len=MaxNameLength)  :: ResourceType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  IF (MeterNumber > 0) THEN
    ResourceType=EnergyMeters(MeterNumber)%ResourceType
  ELSE
    ResourceType='Invalid/Unknown'
  ENDIF

  RETURN

END FUNCTION GetMeterResourceType

FUNCTION GetCurrentMeterValue(MeterNumber) RESULT(CurrentMeterValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the current meter value (timestep) for the meter number indicated.

          ! METHODOLOGY EMPLOYED:
          ! Uses internal EnergyMeters structure to get value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: MeterNumber ! Which Meter Number (from GetMeterIndex)
  REAL(r64)           :: CurrentMeterValue

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (MeterNumber > 0) THEN
    CurrentMeterValue=EnergyMeters(MeterNumber)%CurTSValue
  ELSE
    CurrentMeterValue=0.0d0
  ENDIF

  RETURN

END FUNCTION GetCurrentMeterValue


FUNCTION GetInstantMeterValue(MeterNumber,IndexType) RESULT(InstantMeterValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the Instantaneous meter value (timestep) for the meter number indicated
          !  using IndexType to differentiate between Zone and HVAC values.

          ! METHODOLOGY EMPLOYED:
          ! Uses internal EnergyMeters structure to get value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: MeterNumber ! Which Meter Number (from GetMeterIndex)
  INTEGER, INTENT(IN) :: IndexType ! Whether this is zone of HVAC
  REAL(r64)           :: InstantMeterValue

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
 INTEGER :: Loop
 INTEGER :: Meter

 InstantMeterValue = 0.0d0
!      EnergyMeters(Meter)%TSValue=EnergyMeters(EnergyMeters(Meter)%SourceMeter)%TSValue-MeterValue(Meter)


  If(MeterNumber == 0) THEN
     InstantMeterValue = 0.0d0
  ElseIf (EnergyMeters(MeterNumber)%TypeOfMeter /= MeterType_CustomDec) THEN
      ! section added to speed up the execution of this routine
      ! instead of looping through all the VarMeterArrays to see if a RVariableType is used for a
      ! specific meter, create a list of all the indexes for RVariableType that are used for that
      ! meter.
      IF (EnergyMeters(MeterNumber)%InstMeterCacheStart .EQ. 0) THEN !not yet added to the cache
        DO Loop=1,NumVarMeterArrays
          DO Meter=1,VarMeterArrays(Loop)%NumOnMeters
            IF(VarMeterArrays(Loop)%OnMeters(Meter) == MeterNumber) THEN
              CALL IncrementInstMeterCache
              EnergyMeters(MeterNumber)%InstMeterCacheEnd = InstMeterCacheLastUsed
              IF (EnergyMeters(MeterNumber)%InstMeterCacheStart .EQ. 0) THEN
                EnergyMeters(MeterNumber)%InstMeterCacheStart = InstMeterCacheLastUsed
              END IF
              InstMeterCache(InstMeterCacheLastUsed) = VarMeterArrays(Loop)%RepVariable
              EXIT
            END IF
          END Do
          DO Meter=1,VarMeterArrays(Loop)%NumOnCustomMeters
            IF(VarMeterArrays(Loop)%OnCustomMeters(Meter) == MeterNumber) THEN
              CALL IncrementInstMeterCache
              EnergyMeters(MeterNumber)%InstMeterCacheEnd = InstMeterCacheLastUsed
              IF (EnergyMeters(MeterNumber)%InstMeterCacheStart .EQ. 0) THEN
                EnergyMeters(MeterNumber)%InstMeterCacheStart = InstMeterCacheLastUsed
              END IF
              InstMeterCache(InstMeterCacheLastUsed) = VarMeterArrays(Loop)%RepVariable
              EXIT
            END IF
          END DO  ! End Number of Meters Loop
        END DO
      END IF
      DO loop = EnergyMeters(MeterNumber)%InstMeterCacheStart, EnergyMeters(MeterNumber)%InstMeterCacheEnd
        RVar=>RVariableTypes(InstMeterCache(loop))%VarPtr
        !Separate the Zone variables from the HVAC variables using IndexType
        IF (RVariableTypes(InstMeterCache(loop))%IndexType == IndexType) THEN
          !Add to the total all of the appropriate variables
          InstantMeterValue=InstantMeterValue+RVar%Which*RVar%ZoneMult*RVar%ZoneListMult
        END IF
      END DO
  Else  ! MeterType_CustomDec
      ! Get Source Meter value
      !Loop through all report meters to find correct report variables to add to instant meter total
      Do Loop=1,NumVarMeterArrays

        DO Meter=1,VarMeterArrays(Loop)%NumOnMeters
           IF(VarMeterArrays(Loop)%OnMeters(Meter) == EnergyMeters(MeterNumber)%SourceMeter) THEN
              RVar=>RVariableTypes(VarMeterArrays(Loop)%RepVariable)%VarPtr
                !Separate the Zone variables from the HVAC variables using IndexType
              IF (RVariableTypes(VarMeterArrays(Loop)%RepVariable)%IndexType == IndexType) THEN
                 !Add to the total all of the appropriate variables
                InstantMeterValue=InstantMeterValue+RVar%Which*RVar%ZoneMult*RVar%ZoneListMult
                Exit
              END IF
           End IF
        End Do

        DO Meter=1,VarMeterArrays(Loop)%NumOnCustomMeters
          IF(VarMeterArrays(Loop)%OnCustomMeters(Meter) == EnergyMeters(MeterNumber)%SourceMeter) THEN
            RVar=>RVariableTypes(VarMeterArrays(Loop)%RepVariable)%VarPtr
               !Separate the Zone variables from the HVAC variables using IndexType
            IF (RVariableTypes(VarMeterArrays(Loop)%RepVariable)%IndexType == IndexType) THEN
               !Add to the total all of the appropriate variables
              InstantMeterValue=InstantMeterValue+RVar%Which*RVar%ZoneMult*RVar%ZoneListMult
              Exit
            END IF
          End IF
        ENDDO

      END DO  ! End Number of Meters Loop
      Do Loop=1,NumVarMeterArrays

        DO Meter=1,VarMeterArrays(Loop)%NumOnMeters
           IF(VarMeterArrays(Loop)%OnMeters(Meter) == MeterNumber) THEN
              RVar=>RVariableTypes(VarMeterArrays(Loop)%RepVariable)%VarPtr
                !Separate the Zone variables from the HVAC variables using IndexType
              IF (RVariableTypes(VarMeterArrays(Loop)%RepVariable)%IndexType == IndexType) THEN
                 !Add to the total all of the appropriate variables
                InstantMeterValue=InstantMeterValue-RVar%Which*RVar%ZoneMult*RVar%ZoneListMult
                Exit
              END IF
           End IF
        End Do

        DO Meter=1,VarMeterArrays(Loop)%NumOnCustomMeters
          IF(VarMeterArrays(Loop)%OnCustomMeters(Meter) == MeterNumber) THEN
            RVar=>RVariableTypes(VarMeterArrays(Loop)%RepVariable)%VarPtr
               !Separate the Zone variables from the HVAC variables using IndexType
            IF (RVariableTypes(VarMeterArrays(Loop)%RepVariable)%IndexType == IndexType) THEN
               !Add to the total all of the appropriate variables
              InstantMeterValue=InstantMeterValue-RVar%Which*RVar%ZoneMult*RVar%ZoneListMult
              Exit
            END IF
          End IF
        ENDDO

      END DO  ! End Number of Meters Loop
  End IF

  RETURN

END FUNCTION GetInstantMeterValue

SUBROUTINE IncrementInstMeterCache
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Manage the InstMeterCache array

          ! METHODOLOGY EMPLOYED:
          ! When the array grows to large, double it.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

IF (.NOT. ALLOCATED(InstMeterCache)) THEN
  ALLOCATE(InstMeterCache(InstMeterCacheSizeInc))
  InstMeterCache = 0 !zero the entire array
  InstMeterCacheLastUsed = 1
ELSE
  InstMeterCacheLastUsed = InstMeterCacheLastUsed + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (InstMeterCacheLastUsed .GT. InstMeterCacheSize) THEN
    ALLOCATE(InstMeterCacheCopy(InstMeterCacheSize))
    InstMeterCacheCopy = InstMeterCache
    DEALLOCATE(InstMeterCache)
    ! increment by cachesize
    ALLOCATE(InstMeterCache(InstMeterCacheSize + InstMeterCacheSizeInc))
    InstMeterCache = 0
    InstMeterCache(1:InstMeterCacheSize) = InstMeterCacheCopy
    DEALLOCATE(InstMeterCacheCopy)
    InstMeterCacheSize = InstMeterCacheSize + InstMeterCacheSizeInc
  END IF
END IF
END SUBROUTINE

FUNCTION GetInternalVariableValue(varType, keyVarIndex) RESULT (resultVal)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 2000
          !       MODIFIED       August 2003, M. J. Witte
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the current value of the Internal Variable assigned to
          ! the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
          ! report variables and meter variables.  The variable type (varType) may be
          ! determined by calling subroutine and GetVariableKeyCountandType.  The
          ! index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

          ! METHODOLOGY EMPLOYED:
          ! Uses Internal OutputProcessor data structure to return value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: keyVarIndex  ! Array index
  INTEGER, INTENT(IN)      :: varType      ! 1=integer, 2=real, 3=meter

  REAL(r64)                :: resultVal    ! value returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  REAL(r64), external :: GetCurrentMeterValue

          ! Select based on variable type:  integer, real, or meter
  SELECT CASE (varType)

    CASE (0)  ! Variable not a found variable
      resultVal = 0.0d0

    CASE (1)  ! Integer
      IF (keyVarIndex  .GT. NumOfIVariable) THEN
        CALL ShowFatalError('GetInternalVariableValue: passed index beyond range of array.')
      ENDIF
      IF (keyVarIndex  .LT. 1) THEN
        CALL ShowFatalError('GetInternalVariableValue: passed index beyond range of array.')
      ENDIF

      IVar=>IVariableTypes(keyVarIndex)%VarPtr
      ! must use %Which, %Value is always zero if variable is not a requested report variable
      resultVal = REAL(IVar%Which,r64)

    CASE (2)  ! real
      IF (keyVarIndex  .GT. NumOfRVariable) THEN
        CALL ShowFatalError('GetInternalVariableValue: passed index beyond range of array.')
      ENDIF
      IF (keyVarIndex  .LT. 1) THEN
        CALL ShowFatalError('GetInternalVariableValue: passed index beyond range of array.')
      ENDIF

      RVar=>RVariableTypes(keyVarIndex)%VarPtr
      ! must use %Which, %Value is always zero if variable is not a requested report variable
      resultVal = RVar%Which

    CASE (3)  ! Meter
      resultVal = GetCurrentMeterValue(keyVarIndex)

    CASE (4)  ! Schedule
      resultVal = GetCurrentScheduleValue(keyVarIndex)

    CASE DEFAULT
      resultVal = 0.0d0

  END SELECT

END FUNCTION GetInternalVariableValue

FUNCTION GetInternalVariableValueExternalInterface(varType, keyVarIndex) RESULT (resultVal) !CR - 8481 fix - 08/19/2011
          ! FUNCTION INFORMATION:
          !       AUTHOR         Thierry S. Nouidui
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the last zone-timestep value of the Internal Variable assigned to
          ! the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
          ! report variables and meter variables.  The variable type (varType) may be
          ! determined by calling subroutine and GetVariableKeyCountandType.  The
          ! index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

          ! METHODOLOGY EMPLOYED:
          ! Uses Internal OutputProcessor data structure to return value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: keyVarIndex  ! Array index
  INTEGER, INTENT(IN)      :: varType      ! 1=integer, 2=REAL(r64), 3=meter

  REAL(r64)                :: resultVal    ! value returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  REAL(r64), external :: GetCurrentMeterValue

          ! Select based on variable type:  integer, REAL(r64), or meter
  SELECT CASE (varType)

    CASE (0)  ! Variable not a found variable
      resultVal = 0.0d0

    CASE (1)  ! Integer
      IF (keyVarIndex  .GT. NumOfIVariable) THEN
        CALL ShowFatalError('GetInternalVariableValueExternalInterface: passed index beyond range of array.')
      ENDIF
      IF (keyVarIndex  .LT. 1) THEN
        CALL ShowFatalError('GetInternalVariableValueExternalInterface: passed index beyond range of array.')
      ENDIF

      IVar=>IVariableTypes(keyVarIndex)%VarPtr
      ! must use %EITSValue, %This is the last-zonetimestep value
      resultVal = REAL(IVar%EITSValue,r64)

    CASE (2)  ! REAL(r64)
      IF (keyVarIndex  .GT. NumOfRVariable) THEN
        CALL ShowFatalError('GetInternalVariableValueExternalInterface: passed index beyond range of array.')
      ENDIF
      IF (keyVarIndex  .LT. 1) THEN
        CALL ShowFatalError('GetInternalVariableValueExternalInterface: passed index beyond range of array.')
      ENDIF

      RVar=>RVariableTypes(keyVarIndex)%VarPtr
      ! must use %EITSValue, %This is the last-zonetimestep value
      resultVal = RVar%EITSValue

    CASE (3)  ! Meter
      resultVal = GetCurrentMeterValue(keyVarIndex)

    CASE (4)  ! Schedule
      resultVal = GetCurrentScheduleValue(keyVarIndex)

    CASE DEFAULT
      resultVal = 0.0d0

  END SELECT

END FUNCTION GetInternalVariableValueExternalInterface

FUNCTION GetNumMeteredVariables(ComponentType,ComponentName) RESULT(NumVariables)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function counts the number of metered variables associated with the
          ! given ComponentType/Name.   This resultant number would then be used to
          ! allocate arrays for a call the GetMeteredVariables routine.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType  ! Given Component Type
  CHARACTER(len=*), INTENT(IN) :: ComponentName  ! Given Component Name (user defined)
  INTEGER                      :: NumVariables

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: Pos

  NumVariables=0
  DO Loop=1,NumOfRVariable
!    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
!    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
    IF (ComponentName /= RVariableTypes(Loop)%KeyNameOnlyUC) CYCLE
    RVar=>RVariableTypes(Loop)%VarPtr
    IF (RVar%MeterArrayPtr == 0) CYCLE
    NumVariables=NumVariables+1
  ENDDO

  RETURN

END FUNCTION GetNumMeteredVariables

SUBROUTINE GetMeteredVariables(ComponentType,ComponentName,VarIndexes,VarTypes,IndexTypes,  &
                               UnitsStrings,ResourceTypes,EndUses,Groups,Names,NumFound,VarIDs)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the variable names and other associated information
          ! for metered variables associated with the given ComponentType/Name.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE InputProcessor, ONLY: MakeUPPERCase
  USE DataGlobalConstants
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),      INTENT(IN)            :: ComponentType  ! Given Component Type
  CHARACTER(len=*),      INTENT(IN)            :: ComponentName  ! Given Component Name (user defined)
  INTEGER, DIMENSION(:), INTENT(OUT)           :: VarIndexes     ! Variable Numbers
  INTEGER, DIMENSION(:), INTENT(OUT)           :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
  INTEGER, DIMENSION(:), INTENT(OUT)           :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
  CHARACTER(len=*), DIMENSION(:), INTENT(OUT)  :: UnitsStrings   ! UnitsStrings for each variable
  INTEGER, DIMENSION(:), INTENT(OUT)  :: ResourceTypes  ! ResourceTypes for each variable
  CHARACTER(len=*), DIMENSION(:),   &
                        OPTIONAL, INTENT(OUT)  :: EndUses        ! EndUses for each variable
  CHARACTER(len=*), DIMENSION(:),   &
                        OPTIONAL, INTENT(OUT)  :: Groups         ! Groups for each variable
  CHARACTER(len=*), DIMENSION(:),   &
                        OPTIONAL, INTENT(OUT)  :: Names          ! Variable Names for each variable
  INTEGER, OPTIONAL, INTENT(OUT)               :: NumFound       ! Number Found
  INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: VarIDs         ! Variable Report Numbers

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: Pos
  INTEGER :: NumVariables
  INTEGER :: MeterPtr
  INTEGER :: NumOnMeterPtr
  INTEGER :: MeterNum

  NumVariables=0

  DO Loop=1,NumOfRVariable
!    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
!    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
    IF (ComponentName /= RVariableTypes(Loop)%KeyNameOnlyUC) CYCLE
    RVar=>RVariableTypes(Loop)%VarPtr
    IF (RVar%MeterArrayPtr == 0) CYCLE
    NumOnMeterPtr=VarMeterArrays(RVar%MeterArrayPtr)%NumOnMeters
    MeterPtr=VarMeterArrays(RVar%MeterArrayPtr)%OnMeters(1)
    NumVariables=NumVariables+1
    VarIndexes(NumVariables)=Loop
    VarTypes(NumVariables)=2
    IndexTypes(NumVariables)=RVariableTypes(Loop)%IndexType
    UnitsStrings(NumVariables)=RVariableTypes(Loop)%UnitsString

    ResourceTypes(NumVariables)=AssignResourceTypeNum(MakeUPPERCase(EnergyMeters(MeterPtr)%ResourceType))
    IF (PRESENT(Names)) THEN
      Names(NumVariables)=RVariableTypes(Loop)%VarNameUC
    ENDIF
    IF (PRESENT(EndUses)) THEN
      DO MeterNum=1,NumOnMeterPtr
        MeterPtr=VarMeterArrays(RVar%MeterArrayPtr)%OnMeters(MeterNum)
        IF (EnergyMeters(MeterPtr)%EndUse /= ' ') THEN
          EndUses(NumVariables)=MakeUPPERCase(EnergyMeters(MeterPtr)%EndUse)
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF (PRESENT(Groups)) THEN
      DO MeterNum=1,NumOnMeterPtr
        MeterPtr=VarMeterArrays(RVar%MeterArrayPtr)%OnMeters(MeterNum)
        IF (EnergyMeters(MeterPtr)%Group /= ' ') THEN
          Groups(NumVariables)=MakeUPPERCase(EnergyMeters(MeterPtr)%Group)
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF (PRESENT(VarIDs)) THEN
      VarIDs(NumVariables)=RVar%ReportID
    ENDIF
  ENDDO

  IF (PRESENT(NumFound)) THEN
    NumFound=NumVariables
  ENDIF

  RETURN

END SUBROUTINE GetMeteredVariables

SUBROUTINE GetVariableKeyCountandType(varName,numKeys,varType,varAvgSum,varStepType,varUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns the variable TYPE (Real, integer, meter, schedule, etc.)
          ! (varType) whether it is an averaged or summed variable (varAvgSum),
          ! whether it is a zone or HVAC time step (varStepType),
          ! and the number of keynames for a given report variable or report meter name
          ! (varName).  The variable type (varType) and number of keys (numKeys) are
          ! used when calling subroutine GetVariableKeys to obtain a list of the
          ! keynames for a particular variable and a corresponding list of indexes.

          ! METHODOLOGY EMPLOYED:
          ! Uses Internal OutputProcessor data structure to search for varName
          ! in each of the three output data arrays:
          !       RVariableTypes - real report variables
          !       IVariableTypes - integer report variables
          !       EnergyMeters   - report meters (via GetMeterIndex function)
          !       Schedules      - specific schedule values
          !
          ! When the variable is found, the variable type (varType) is set and the
          ! number of associated keys is counted.
          !
          ! varType is assigned as follows:
          !       0 = not found
          !       1 = integer
          !       2 = real
          !       3 = meter
          !       4 = schedule
          !
          !  varAvgSum is assigned as follows:
          !       1 = averaged
          !       2 = summed
          !
          !  varStepType is assigned as follows:
          !       1 = zone time step
          !       2 = HVAC time step

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE InputProcessor, ONLY: MakeUPPERCase, FindItemInSortedList
  USE DataGlobals, ONLY: MaxNameLength
  USE OutputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex, GetScheduleType
  USE SortAndStringUtilities, ONLY: SetupAndSort

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)                  :: varName      ! Standard variable name

  INTEGER, INTENT(OUT)                          :: varType      ! 0=not found, 1=integer, 2=real, 3=meter
  INTEGER, INTENT(OUT)                          :: numKeys      ! Number of keys found
  INTEGER, INTENT(OUT)                          :: varAvgSum    ! Variable  is Averaged=1 or Summed=2
  INTEGER, INTENT(OUT)                          :: varStepType  ! Variable time step is Zone=1 or HVAC=2
  CHARACTER(len=*), INTENT(OUT)     :: varUnits     ! Units sting, may be blank

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(:),ALLOCATABLE, SAVE   :: keyVarIndexes   ! Array index for specific key name
  INTEGER, SAVE                             :: curkeyVarIndexLimit ! current limit for keyVarIndexes
  LOGICAL, SAVE                             :: InitFlag=.true.   ! for initting the keyVarIndexes array
  INTEGER, EXTERNAL                         :: GetMeterIndex
  INTEGER                                   :: Loop, Loop2     ! Loop counters
  INTEGER                                   :: Position        ! Starting point of search string
  INTEGER                                   :: VFound          ! Found integer/real variable attributes
  LOGICAL                                   :: Found           ! True if varName is found
  LOGICAL                                   :: Duplicate       ! True if keyname is a duplicate
  CHARACTER (len=MaxNameLength*2+1)         :: VarKeyPlusName  ! Full variable name including keyname and units
  CHARACTER (len=MaxNameLength*2+1)         :: varNameUpper    ! varName pushed to all upper case
  CHARACTER (len=MaxNameLength),   &
     DIMENSION(:), ALLOCATABLE, SAVE        :: varNames        ! stored variable names
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE  :: ivarNames       ! pointers for sorted information
  INTEGER, SAVE                             :: numvarNames     ! number of variable names


          ! INITIALIZATIONS
  IF (InitFlag) THEN
    curKeyVarIndexLimit=1000
    ALLOCATE(keyVarIndexes(curKeyVarIndexLimit))
    numvarNames=NumVariablesForOutput
    allocate(varNames(numvarNames))
    allocate(ivarNames(numvarNames))
    ivarNames=0
    DO Loop = 1, NumVariablesForOutput
       varNames(Loop) = MakeUPPERCase(DDVariableTypes(Loop)%VarNameOnly)
    ENDDO
    CALL SetupAndSort(varNames,ivarNames)
    InitFlag=.false.
  ENDIF

  IF (numVarNames /= NumVariablesForOutput) THEN
    deallocate(varnames)
    deallocate(ivarnames)
    numvarNames=NumVariablesForOutput
    allocate(varNames(numvarNames))
    allocate(ivarNames(numvarNames))
    ivarNames=0
    DO Loop = 1, NumVariablesForOutput
       varNames(Loop) = MakeUPPERCase(DDVariableTypes(Loop)%VarNameOnly)
    ENDDO
    CALL SetupAndSort(varNames,ivarNames)
  ENDIF

  keyVarIndexes = 0
  varType       = VarType_NotFound
  numKeys       = 0
  varAvgSum     = 0
  varStepType   = 0
  varUnits      = ' '
  Found         = .FALSE.
  Duplicate     = .FALSE.
  varNameUpper  = varName

  ! Search Variable List First
  VFound=FindItemInSortedList(varNameUpper,varNames,numvarNames)
  IF (VFound /= 0) THEN
    varType=DDVariableTypes(ivarNames(VFound))%VariableType
  ENDIF

  IF (varType == VarType_Integer) THEN
          ! Search Integer Variables
    DO Loop = 1, NumOfIVariable
      VarKeyPlusName = IVariableTypes(Loop)%VarNameUC
      Position = INDEX(TRIM(VarKeyPlusName), &
                 ':'//TRIM(varNameUpper), .TRUE.)
      IF (Position > 0) THEN
        IF (VarKeyPlusName(Position+1:) == varNameUpper) THEN
          Found = .TRUE.
          varType = VarType_Integer
          Duplicate = .FALSE.
                ! Check if duplicate - duplicates happen if the same report variable/key name
                ! combination is requested more than once in the idf at different reporting
                ! frequencies
          DO Loop2 = 1, numKeys
            IF (VarKeyPlusName == IVariableTypes(keyVarIndexes(Loop2))%VarNameUC) Duplicate = .TRUE.
          ENDDO
          IF (.NOT. Duplicate) THEN
            numKeys = numKeys + 1
            IF (numKeys > curkeyVarIndexLimit) THEN
              CALL ReallocateIntegerArray(keyVarIndexes,curkeyVarIndexLimit,500)
            ENDIF
            keyVarIndexes(numKeys) = Loop
            varAvgSum   = DDVariableTypes(ivarNames(VFound))%StoreType
            varStepType = DDVariableTypes(ivarNames(VFound))%IndexType
            varUnits    = DDVariableTypes(ivarNames(VFound))%UnitsString
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ELSEIF (varType == VarType_Real) THEN
          ! Search real Variables Next
    DO Loop = 1, NumOfRVariable
        IF (RVariableTypes(Loop)%VarNameOnlyUC == varNameUpper) THEN
          Found = .TRUE.
          varType = VarType_Real
          Duplicate = .FALSE.
              ! Check if duplicate - duplicates happen if the same report variable/key name
              ! combination is requested more than once in the idf at different reporting
              ! frequencies
          VarKeyPlusName = RVariableTypes(Loop)%VarNameUC
          DO Loop2 = 1, numKeys
            IF (VarKeyPlusName == RVariableTypes(keyVarIndexes(Loop2))%VarNameUC) Duplicate = .TRUE.
          ENDDO
          IF (.NOT. Duplicate) THEN
            numKeys = numKeys + 1
            IF (numKeys > curkeyVarIndexLimit) THEN
              CALL ReallocateIntegerArray(keyVarIndexes,curkeyVarIndexLimit,500)
            ENDIF
            keyVarIndexes(numKeys) = Loop
            varAvgSum   = DDVariableTypes(ivarNames(VFound))%StoreType
            varStepType = DDVariableTypes(ivarNames(VFound))%IndexType
            varUnits    = DDVariableTypes(ivarNames(VFound))%UnitsString
          ENDIF
        ENDIF
    ENDDO
  ENDIF

          ! Search Meters if not found in integers or reals
          ! Use the GetMeterIndex function
          ! Meters do not have keys, so only one will be found
  IF (.NOT. Found) THEN
    keyVarIndexes(1) = GetMeterIndex(varName)
    IF (keyVarIndexes(1) > 0) THEN
       Found = .TRUE.
       numKeys = 1
       varType = VarType_Meter
       varUnits = EnergyMeters(keyVarIndexes(1))%Units
       varAvgSum = SummedVar
       varStepType = ZoneVar
    ENDIF
  ENDIF

          ! Search schedules if not found in integers, reals, or meters
          ! Use the GetScheduleIndex function
          ! Schedules do not have keys, so only one will be found
  IF (.NOT. Found) THEN
    keyVarIndexes(1) = GetScheduleIndex(varName)
    IF (keyVarIndexes(1) > 0) THEN
       Found = .TRUE.
       numKeys = 1
       varType = VarType_Schedule
       varUnits = GetScheduleType(keyVarIndexes(1))
       varAvgSum = AveragedVar
       varStepType = ZoneVar
    ENDIF
  ENDIF

END SUBROUTINE GetVariableKeyCountandType


SUBROUTINE GetVariableKeys(varName,varType,keyNames,keyVarIndexes)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns a list of keynames and indexes associated
          ! with a particular report variable or report meter name (varName).
          ! This routine assumes that the variable TYPE (Real, integer, meter, etc.)
          ! may be determined by calling GetVariableKeyCountandType.  The variable type
          ! and index can then be used with function GetInternalVariableValue to
          ! to retrieve the current value of a particular variable/keyname combination.

          ! METHODOLOGY EMPLOYED:
          ! Uses Internal OutputProcessor data structure to search for varName
          ! and build list of keynames and indexes.  The indexes are the array index
          ! in the data array for the

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE InputProcessor, ONLY: MakeUPPERCase
  USE OutputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)                  :: varName       ! Standard variable name
  INTEGER, INTENT(IN)                           :: varType       ! 1=integer, 2=real, 3=meter
  CHARACTER(len=*), INTENT(OUT), DIMENSION(:)   :: keyNames      ! Specific key name
  INTEGER, INTENT(OUT), DIMENSION(:)            :: keyVarIndexes ! Array index for

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL                      :: GetMeterIndex
  INTEGER                                :: Loop, Loop2      ! Loop counters
  INTEGER                                :: Position         ! Starting point of search string
  LOGICAL                                :: Duplicate        ! True if keyname is a duplicate
  INTEGER                                :: maxKeyNames      ! Max allowable # of key names=size of keyNames array
  INTEGER                                :: maxkeyVarIndexes ! Max allowable # of key indexes=size of keyVarIndexes array
  INTEGER                                :: numKeys          ! Number of keys found
  CHARACTER (len=MaxNameLength*2+1)      :: VarKeyPlusName   ! Full variable name including keyname and units
  CHARACTER (len=MaxNameLength*2+1)      :: varNameUpper     ! varName pushed to all upper case

          ! INITIALIZATIONS
  keyNames         = ' '
  keyVarIndexes    = 0
  numKeys          = 0
  Duplicate        = .FALSE.
  maxKeyNames      = SIZE(keyNames)
  maxkeyVarIndexes = SIZE(keyVarIndexes)
  varNameUpper     = MakeUPPERCase(varName)

          ! Select based on variable type:  integer, real, or meter
  SELECT CASE (varType)

    CASE (VarType_Integer)  ! Integer
      DO Loop = 1, NumOfIVariable
        VarKeyPlusName = IVariableTypes(Loop)%VarNameUC
        Position = INDEX(TRIM(VarKeyPlusName), &
                   ':'//TRIM(varNameUpper), .TRUE.)
        IF (Position > 0) THEN
          IF (VarKeyPlusName(Position+1:) == varNameUpper) THEN
            Duplicate = .FALSE.
               ! Check if duplicate - duplicates happen if the same report variable/key name
               ! combination is requested more than once in the idf at different reporting
               ! frequencies
            DO Loop2 = 1, numKeys
              IF (VarKeyPlusName == IVariableTypes(keyVarIndexes(Loop2))%VarNameUC) Duplicate = .TRUE.
            ENDDO
            IF (.NOT. Duplicate) THEN
              numKeys = numKeys + 1
              IF ((numKeys > maxKeyNames) .OR. (numKeys > maxkeyVarIndexes)) THEN
                CALL ShowFatalError('Invalid array size in GetVariableKeys')
              ENDIF
              keyNames(numKeys) = IVariableTypes(Loop)%VarNameUC(1:Position-1)
              keyVarIndexes(numKeys) = Loop
            ENDIF
          ENDIF
        ENDIF
      ENDDO


    CASE (VarType_Real)  ! Real
      DO Loop = 1, NumOfRVariable
          IF (RVariableTypes(Loop)%VarNameOnlyUC == varNameUpper) THEN
            Duplicate = .FALSE.
               ! Check if duplicate - duplicates happen if the same report variable/key name
               ! combination is requested more than once in the idf at different reporting
               ! frequencies
            VarKeyPlusName = RVariableTypes(Loop)%VarNameUC
            DO Loop2 = 1, numKeys
              IF (VarKeyPlusName == RVariableTypes(keyVarIndexes(Loop2))%VarNameUC) Duplicate = .TRUE.
            ENDDO
            IF (.NOT. Duplicate) THEN
              numKeys = numKeys + 1
              IF ((numKeys > maxKeyNames) .OR. (numKeys > maxkeyVarIndexes)) THEN
                CALL ShowFatalError('Invalid array size in GetVariableKeys')
              ENDIF
              keyNames(numKeys) = RVariableTypes(Loop)%KeyNameOnlyUC
              keyVarIndexes(numKeys) = Loop
            ENDIF
          ENDIF
      ENDDO

    CASE (VarType_Meter)  ! Meter
      numKeys = 1
      IF ((numKeys > maxKeyNames) .OR. (numKeys > maxkeyVarIndexes)) THEN
        CALL ShowFatalError('Invalid array size in GetVariableKeys')
      ENDIF
      keyNames(1) = 'Meter'
      keyVarIndexes(1) = GetMeterIndex(varName)

    CASE (VarType_Schedule)  ! Schedule
      numKeys = 1
      IF ((numKeys > maxKeyNames) .OR. (numKeys > maxkeyVarIndexes)) THEN
        CALL ShowFatalError('Invalid array size in GetVariableKeys')
      ENDIF
      keyNames(1) = 'Environment'
      keyVarIndexes(1) = GetScheduleIndex(varName)

    CASE DEFAULT
      ! do nothing

  END SELECT

END SUBROUTINE GetVariableKeys

FUNCTION ReportingThisVariable(RepVarName) RESULT(BeingReported)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function scans the report variables and reports back
          ! if user has requested this variable be reported.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE OutputProcessor
  USE InputProcessor, ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: RepVarName
  LOGICAL :: BeingReported

            ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found

  BeingReported=.false.
  Found=FindItem(RepVarName,ReqRepVars%VarName,NumOfReqVariables)
  IF (Found > 0) THEN
    BeingReported=.true.
  ENDIF

  IF (.not. BeingReported) THEN  ! check meter names too
    Found=FindItem(RepVarname,EnergyMeters%Name,NumEnergyMeters)
    IF (Found > 0) THEN
      IF (EnergyMeters(Found)%RptTS .or. EnergyMeters(Found)%RptHR .or. EnergyMeters(Found)%RptDY .or. &
          EnergyMeters(Found)%RptMN .or. EnergyMeters(Found)%RptSM .or.  &
          EnergyMeters(Found)%RptTSFO .or. EnergyMeters(Found)%RptHRFO .or. EnergyMeters(Found)%RptDYFO .or. &
          EnergyMeters(Found)%RptMNFO .or. EnergyMeters(Found)%RptSMFO .or.  &
          EnergyMeters(Found)%RptAccTS .or. EnergyMeters(Found)%RptAccHR .or. EnergyMeters(Found)%RptAccDY .or. &
          EnergyMeters(Found)%RptAccMN .or. EnergyMeters(Found)%RptAccSM .or.  &
          EnergyMeters(Found)%RptAccTSFO .or. EnergyMeters(Found)%RptAccHRFO .or. EnergyMeters(Found)%RptAccDYFO .or. &
          EnergyMeters(Found)%RptAccMNFO .or. EnergyMeters(Found)%RptAccSMFO) THEN
        BeingReported=.true.
      ENDIF
    ENDIF
  ENDIF

  RETURN

END FUNCTION ReportingThisVariable

SUBROUTINE InitPollutionMeterReporting(ReportFreqName)

          ! SUBROUTINE INFORMATION:Richard Liesen
          !       DATE WRITTEN   July 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called at the end of the first HVAC iteration and
          ! sets up the reporting for the Pollution Meters.
          ! ReportPollutionOutput,
          !   A1 ; \field Reporting_Frequency
          !        \type choice
          !        \key timestep
          !        \key hourly
          !        \key daily
          !        \key monthly
          !        \key runperiod
          !
          ! METHODOLOGY EMPLOYED:
          ! The program tries to setup all of the following meters if the Pollution Report is initiated.
          !       Electricity:Facility [J]
          !       Diesel:Facility [J]
          !       DistrictCooling:Facility [J]
          !       DistrictHeating:Facility [J]
          !       Gas:Facility [J]
          !       GASOLINE:Facility [J]
          !       COAL:Facility [J]
          !       FuelOil#1:Facility [J]
          !       FuelOil#2:Facility [J]
          !       Propane:Facility [J]
          !       ElectricityProduced:Facility [J]
          !       Pollutant:CO2
          !       Pollutant:CO
          !       Pollutant:CH4
          !       Pollutant:NOx
          !       Pollutant:N2O
          !       Pollutant:SO2
          !       Pollutant:PM
          !       Pollutant:PM10
          !       Pollutant:PM2.5
          !       Pollutant:NH3
          !       Pollutant:NMVOC
          !       Pollutant:Hg
          !       Pollutant:Pb
          !       Pollutant:WaterEnvironmentalFactors
          !       Pollutant:Nuclear High
          !       Pollutant:Nuclear Low
          !       Pollutant:Carbon Equivalent
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE InputProcessor, ONLY: FindItem
  USE OutputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) :: ReportFreqName

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(1:29) :: PollutionMeters =  &
            (/'Electricity:Facility               ', &
              'Diesel:Facility                    ', &
              'DistrictCooling:Facility           ', &
              'DistrictHeating:Facility           ', &
              'Gas:Facility                       ', &
              'GASOLINE:Facility                  ', &
              'COAL:Facility                      ', &
              'FuelOil#1:Facility                 ', &
              'FuelOil#2:Facility                 ', &
              'Propane:Facility                   ', &
              'ElectricityProduced:Facility       ', &
              'Steam:Facility                     ', &
!             Now for the Pollution Meters
              'CO2:Facility                       ', &
              'CO:Facility                        ', &
              'CH4:Facility                       ', &
              'NOx:Facility                       ', &
              'N2O:Facility                       ', &
              'SO2:Facility                       ', &
              'PM:Facility                        ', &
              'PM10:Facility                      ', &
              'PM2.5:Facility                     ', &
              'NH3:Facility                       ', &
              'NMVOC:Facility                     ', &
              'Hg:Facility                        ', &
              'Pb:Facility                        ', &
              'WaterEnvironmentalFactors:Facility ', &
              'Nuclear High:Facility              ', &
              'Nuclear Low:Facility               ', &
              'Carbon Equivalent:Facility         '/)


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER NumReqMeters
  INTEGER Meter
  INTEGER ReportFreq

  INTEGER                       :: indexGroupKey
  CHARACTER(len=MaxNameLength)  :: indexGroup
  NumReqMeters = 29
  CALL DetermineFrequency(ReportFreqName,ReportFreq)

  DO Loop=1,NumReqMeters


    Meter = FindItem(PollutionMeters(Loop),EnergyMeters%Name,NumEnergyMeters)
    If (Meter > 0) Then !All the active meters for this run are set, but all are still searched for.

      indexGroupKey = DetermineIndexGroupKeyFromMeterName (EnergyMeters(Meter)%Name)
      indexGroup    = DetermineIndexGroupFromMeterGroup (EnergyMeters(Meter))
      !All of the specified meters are checked and the headers printed to the meter file if this
      !  has not been done previously
      SELECT CASE (ReportFreq)

      CASE (ReportTimeStep)
        If(EnergyMeters(Meter)%RptTS) Then
           EnergyMeters(Meter)%RptTS=.true.
        Else
          EnergyMeters(Meter)%RptTS=.true.
          CALL WriteMeterDictionaryItem (ReportFreq, SummedVar, EnergyMeters(Meter)%TSRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(Meter)%TSRptNumChr, EnergyMeters(Meter)%Name, &
             EnergyMeters(Meter)%Units, .false., .false.)
        End If

      CASE (ReportHourly)
        If(EnergyMeters(Meter)%RptHR) Then
          EnergyMeters(Meter)%RptHR=.true.
          TrackingHourlyVariables=.true.
        Else
          EnergyMeters(Meter)%RptHR=.true.
          TrackingHourlyVariables=.true.
          CALL WriteMeterDictionaryItem (ReportFreq, SummedVar, EnergyMeters(Meter)%HRRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(Meter)%HRRptNumChr, EnergyMeters(Meter)%Name, &
             EnergyMeters(Meter)%Units, .false., .false.)
        End If

      CASE (ReportDaily)
        If(EnergyMeters(Meter)%RptDY) Then
          EnergyMeters(Meter)%RptDY=.true.
          TrackingDailyVariables=.true.
        Else
          EnergyMeters(Meter)%RptDY=.true.
          TrackingDailyVariables=.true.
          CALL WriteMeterDictionaryItem (ReportFreq, SummedVar, EnergyMeters(Meter)%DYRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(Meter)%DYRptNumChr, EnergyMeters(Meter)%Name, &
             EnergyMeters(Meter)%Units, .false., .false.)
        End If

      CASE (ReportMonthly)
        If(EnergyMeters(Meter)%RptMN) Then
          EnergyMeters(Meter)%RptMN=.true.
          TrackingMonthlyVariables=.true.
        Else
          EnergyMeters(Meter)%RptMN=.true.
          TrackingMonthlyVariables=.true.
          CALL WriteMeterDictionaryItem (ReportFreq, SummedVar, EnergyMeters(Meter)%MNRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(Meter)%MNRptNumChr, EnergyMeters(Meter)%Name, &
             EnergyMeters(Meter)%Units, .false., .false.)
        End If

      CASE (ReportSim)
        If(EnergyMeters(Meter)%RptSM) Then
          EnergyMeters(Meter)%RptSM=.true.
          TrackingRunPeriodVariables=.true.
        Else
          EnergyMeters(Meter)%RptSM=.true.
          TrackingRunPeriodVariables=.true.
          CALL WriteMeterDictionaryItem (ReportFreq, SummedVar, EnergyMeters(Meter)%SMRptNum, &
             indexGroupKey, indexGroup, EnergyMeters(Meter)%SMRptNumChr, EnergyMeters(Meter)%Name, &
             EnergyMeters(Meter)%Units, .false., .false.)
        End If

      CASE DEFAULT

      END SELECT
    End If

  ENDDO

  RETURN

END SUBROUTINE InitPollutionMeterReporting

SUBROUTINE ProduceRDDMDD

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a single call for writing out the Report Data Dictionary and Meter Data Dictionary.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: VerString,IDDVerString
  USE InputProcessor, ONLY: SameString,FindItemInList
  USE OutputProcessor
  USE SortAndStringUtilities, ONLY: SetupAndSort
  USE General, ONLY: ScanForReports

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: RealType=1
  INTEGER, PARAMETER :: IntegerType=2

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE VariableTypes
    INTEGER :: RealIntegerType=0  ! Real= 1, Integer=2
    INTEGER :: VarPtr=0           ! pointer to real/integer VariableTypes structures
    INTEGER :: IndexType=0
    INTEGER :: StoreType=0
    CHARACTER(len=UnitsStringLength) :: UnitsString=' '
  END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=MaxNameLength) :: VarOption1
  CHARACTER(len=MaxNameLength) :: VarOption2
  LOGICAL DoReport
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: VariableNames
  INTEGER, ALLOCATABLE, DIMENSION(:) :: iVariableNames
  INTEGER :: Item
  LOGICAL :: SortByName
  INTEGER :: ItemPtr
  INTEGER :: write_stat

  !  See if Report Variables should be turned on

  SortByName=.false.
  CALL ScanForReports('VariableDictionary',DoReport,Option1=VarOption1,Option2=VarOption2)
!  IF (.not. DoReport) RETURN

  IF (DoReport) THEN
    ProduceReportVDD=ReportVDD_Yes
    IF (VarOption1 == 'IDF') THEN
      ProduceReportVDD=ReportVDD_IDF
    ENDIF
    IF (VarOption2 /= ' ') THEN
      IF (SameString(VarOption2,'Name') .or. SameString(VarOption2,'AscendingName')) THEN
        SortByName=.true.
      ENDIF
    ENDIF
  ENDIF

  IF (ProduceReportVDD == ReportVDD_Yes) THEN
    OutputFileRVDD=GetNewUnitNumber()
    OPEN(OutputFileRVDD,File='eplusout.rdd', Action='write', iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('ProduceRDDMDD: Could not open file "eplusout.rdd" for output (write).')
    ENDIF
    WRITE(OutputFileRVDD,fmta) 'Program Version,'//TRIM(VerString)//','//TRIM(IDDVerString)
    WRITE(OutputFileRVDD,fmta) 'Var Type (reported time step),Var Report Type,Variable Name [Units]'
    OutputFileMVDD=GetNewUnitNumber()
    OPEN(OutputFileMVDD,File='eplusout.mdd', Action='write', iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('ProduceRDDMDD: Could not open file "eplusout.mdd" for output (write).')
    ENDIF
    WRITE(OutputFileMVDD,fmta) 'Program Version,'//TRIM(VerString)//','//TRIM(IDDVerString)
    WRITE(OutputFileMVDD,fmta) 'Var Type (reported time step),Var Report Type,Variable Name [Units]'
  ELSEIF  (ProduceReportVDD == ReportVDD_IDF) THEN
    OutputFileRVDD=GetNewUnitNumber()
    OPEN(OutputFileRVDD,File='eplusout.rdd', Action='write', iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('ProduceRDDMDD: Could not open file "eplusout.rdd" for output (write).')
    ENDIF
    WRITE(OutputFileRVDD,fmta) '! Program Version,'//TRIM(VerString)//','//TRIM(IDDVerString)
    WRITE(OutputFileRVDD,fmta) '! Output:Variable Objects (applicable to this run)'
    OutputFileMVDD=GetNewUnitNumber()
    OPEN(OutputFileMVDD,File='eplusout.mdd', Action='write', iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('ProduceRDDMDD: Could not open file "eplusout.mdd" for output (write).')
    ENDIF
    WRITE(OutputFileMVDD,fmta) '! Program Version,'//TRIM(VerString)//','//TRIM(IDDVerString)
    WRITE(OutputFileMVDD,fmta) '! Output:Meter Objects (applicable to this run)'
  ENDIF

  ALLOCATE(VariableNames(NumVariablesForOutput))
  VariableNames(1:NumVariablesForOutput)=DDVariableTypes(1:NumVariablesForOutput)%VarNameOnly
  ALLOCATE(iVariableNames(NumVariablesForOutput))
  iVariableNames=0

  IF (SortByName) THEN
    CALL SetupAndSort(VariableNames,iVariableNames)
  ELSE
    DO Item=1,NumVariablesForOutput
      iVariableNames(Item)=Item
    ENDDO
  ENDIF

  DO Item=1,NumVariablesForOutput
    IF (ProduceReportVDD == ReportVDD_Yes) THEN
      ItemPtr=iVariableNames(Item)
      IF (.not. DDVariableTypes(ItemPtr)%ReportedOnDDFile) THEN
        WRITE(OutputFileRVDD,fmta) trim(StandardIndexTypeKey(DDVariableTypes(ItemPtr)%IndexType))//','//   &
                 trim(StandardVariableTypeKey(DDVariableTypes(ItemPtr)%StoreType))//','//              &
                 trim(VariableNames(Item))//                                                        &
                 ' ['//trim(DDVariableTypes(ItemPtr)%UnitsString)//']'
        DDVariableTypes(ItemPtr)%ReportedOnDDFile=.true.
        DO WHILE (DDVariableTypes(ItemPtr)%Next /= 0)
          IF (SortByName) THEN
            ItemPtr=ItemPtr+1
          ELSE
            ItemPtr=DDVariableTypes(ItemPtr)%Next
          ENDIF
          WRITE(OutputFileRVDD,fmta) trim(StandardIndexTypeKey(DDVariableTypes(ItemPtr)%IndexType))//','//   &
                 trim(StandardVariableTypeKey(DDVariableTypes(ItemPtr)%StoreType))//','//              &
                 trim(VariableNames(Item))//                                                        &
                 ' ['//trim(DDVariableTypes(ItemPtr)%UnitsString)//']'
          DDVariableTypes(ItemPtr)%ReportedOnDDFile=.true.
        ENDDO
      ENDIF
    ELSEIF  (ProduceReportVDD == ReportVDD_IDF) THEN
      ItemPtr=iVariableNames(Item)
      IF (.not. DDVariableTypes(ItemPtr)%ReportedOnDDFile) THEN
        WRITE(OutputFileRVDD,fmta) 'Output:Variable,*,'//trim(VariableNames(Item))//                    &
               ',hourly; !- '//trim(StandardIndexTypeKey(DDVariableTypes(ItemPtr)%IndexType))//' '//       &
               trim(StandardVariableTypeKey(DDVariableTypes(ItemPtr)%StoreType))//                         &
               ' ['//trim(DDVariableTypes(ItemPtr)%UnitsString)//']'
        DDVariableTypes(ItemPtr)%ReportedOnDDFile=.true.
        DO WHILE (DDVariableTypes(ItemPtr)%Next /= 0)
          IF (SortByName) THEN
            ItemPtr=ItemPtr+1
          ELSE
            ItemPtr=DDVariableTypes(ItemPtr)%Next
          ENDIF
          WRITE(OutputFileRVDD,fmta) 'Output:Variable,*,'//trim(VariableNames(Item))//                    &
                 ',hourly; !- '//trim(StandardIndexTypeKey(DDVariableTypes(ItemPtr)%IndexType))//' '//       &
                 trim(StandardVariableTypeKey(DDVariableTypes(ItemPtr)%StoreType))//                         &
                 ' ['//trim(DDVariableTypes(ItemPtr)%UnitsString)//']'
          DDVariableTypes(ItemPtr)%ReportedOnDDFile=.true.
        ENDDO
      ENDIF
    ENDIF
  ENDDO

  DEALLOCATE(VariableNames)
  DEALLOCATE(iVariableNames)

!  Now EnergyMeter variables
  IF (SortByName) THEN
    ALLOCATE(VariableNames(NumEnergyMeters))
    DO Item=1,NumEnergyMeters
      VariableNames(Item)=EnergyMeters(Item)%Name
    ENDDO
    ALLOCATE(iVariableNames(NumEnergyMeters))
    iVariableNames=0
    CALL SetupAndSort(VariableNames,iVariableNames)
  ELSE
    ALLOCATE(VariableNames(NumEnergyMeters))
    ALLOCATE(iVariableNames(NumEnergyMeters))
    iVariableNames=0

    DO Item=1,NumEnergyMeters
      VariableNames(Item)=EnergyMeters(Item)%Name
      iVariableNames(Item)=Item
    ENDDO
  ENDIF

  DO Item=1,NumEnergyMeters
    ItemPtr=iVariableNames(Item)
    IF (ProduceReportVDD == ReportVDD_Yes) THEN
      WRITE(OutputFileMVDD,fmta) 'Zone,Meter,'//trim(EnergyMeters(ItemPtr)%Name)//' ['//trim(EnergyMeters(ItemPtr)%Units)//']'
    ELSEIF (ProduceReportVDD == ReportVDD_IDF) THEN
      WRITE(OutputFileMVDD,fmta) 'Output:Meter,'//trim(EnergyMeters(ItemPtr)%Name)//',hourly; !- ['//                         &
         trim(EnergyMeters(ItemPtr)%Units)//']'
      WRITE(OutputFileMVDD,fmta) 'Output:Meter:Cumulative,'//trim(EnergyMeters(ItemPtr)%Name)//',hourly; !- ['//              &
         trim(EnergyMeters(ItemPtr)%Units)//']'
    ENDIF
  ENDDO

  DEALLOCATE(VariableNames)
  DEALLOCATE(iVariableNames)

  ! DEALLOCATE(DDVariableTypes)

  RETURN

END SUBROUTINE ProduceRDDMDD

SUBROUTINE AddToOutputVariableList(VarName,IndexType,StateType,VariableType,UnitsString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine maintains a unique list of Output Variables for the
          ! Variable Dictionary output.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE OutputProcessor
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VarName  ! Variable Name
  INTEGER, INTENT(IN)          :: IndexType
  INTEGER, INTENT(IN)          :: StateType
  INTEGER, INTENT(IN)          :: VariableType
  CHARACTER(len=*), INTENT(IN) :: UnitsString

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   TYPE (VariableTypeForDDOutput),  &
           DIMENSION(:), ALLOCATABLE  :: tmpDDVariableTypes  ! Variable Types structure (temp for reallocate)
   INTEGER :: dup   ! for duplicate variable name
   INTEGER :: dup2  ! for duplicate variable name

   dup=0
   IF (NumVariablesForOutput > 0) THEN
     dup=FindItemInList(VarName,DDVariableTypes%VarNameOnly,NumVariablesForOutput)
   ELSE  !
     ALLOCATE(DDVariableTypes(LVarAllocInc))
     MaxVariablesForOutput=LVarAllocInc
   ENDIF
   IF (dup == 0) THEN
     NumVariablesForOutput=NumVariablesForOutput+1
     IF (NumVariablesForOutput > MaxVariablesForOutput) THEN
       ALLOCATE(tmpDDVariableTypes(MaxVariablesForOutput))
       tmpDDVariableTypes(1:MaxVariablesForOutput)=DDVariableTypes(1:MaxVariablesForOutput)
       DEALLOCATE(DDVariableTypes)
       ALLOCATE(DDVariableTypes(MaxVariablesForOutput+LVarAllocInc))
       DDVariableTypes(1:MaxVariablesForOutput)=tmpDDVariableTypes(1:MaxVariablesForOutput)
       DEALLOCATE(tmpDDVariableTypes)
       MaxVariablesForOutput=MaxVariablesForOutput+LVarAllocInc
     ENDIF
     DDVariableTypes(NumVariablesForOutput)%IndexType=IndexType
     DDVariableTypes(NumVariablesForOutput)%StoreType=StateType
     DDVariableTypes(NumVariablesForOutput)%VariableType=VariableType
     DDVariableTypes(NumVariablesForOutput)%VarNameOnly=VarName
     DDVariableTypes(NumVariablesForOutput)%UnitsString=UnitsString
   ELSEIF (UnitsString /= DDVariableTypes(dup)%UnitsString) THEN ! not the same as first units
     dup2=0
     DO WHILE(DDVariableTypes(dup)%Next /= 0)
       IF (UnitsString /= DDVariableTypes(DDVariableTypes(dup)%Next)%UnitsString) THEN
         dup=DDVariableTypes(dup)%Next
         CYCLE
       ENDIF
       dup2=DDVariableTypes(dup)%Next
       EXIT
     ENDDO
     IF (dup2 == 0) THEN
       NumVariablesForOutput=NumVariablesForOutput+1
       IF (NumVariablesForOutput > MaxVariablesForOutput) THEN
         ALLOCATE(tmpDDVariableTypes(MaxVariablesForOutput))
         tmpDDVariableTypes(1:MaxVariablesForOutput)=DDVariableTypes(1:MaxVariablesForOutput)
         DEALLOCATE(DDVariableTypes)
         ALLOCATE(DDVariableTypes(MaxVariablesForOutput+LVarAllocInc))
         DDVariableTypes(1:MaxVariablesForOutput)=tmpDDVariableTypes(1:MaxVariablesForOutput)
         DEALLOCATE(tmpDDVariableTypes)
         MaxVariablesForOutput=MaxVariablesForOutput+LVarAllocInc
       ENDIF
       DDVariableTypes(NumVariablesForOutput)%IndexType=IndexType
       DDVariableTypes(NumVariablesForOutput)%StoreType=StateType
       DDVariableTypes(NumVariablesForOutput)%VariableType=VariableType
       DDVariableTypes(NumVariablesForOutput)%VarNameOnly=VarName
       DDVariableTypes(NumVariablesForOutput)%UnitsString=UnitsString
       DDVariableTypes(dup)%Next=NumVariablesForOutput
     ENDIF
   ENDIF

  RETURN

END SUBROUTINE AddToOutputVariableList


!     NOTICE
!
!     Copyright © 1996-2013 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

