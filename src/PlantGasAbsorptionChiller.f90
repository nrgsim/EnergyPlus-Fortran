MODULE ChillerGasAbsorption  !Based on ChillerAbsorption

! MODULE INFORMATION:
!    AUTHOR         Jason Glazer of GARD Analytics, Inc.
!                   for Gas Research Institute
!    DATE WRITTEN   March 2001
!    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
!    RE-ENGINEERED  na
!
! PURPOSE OF THIS MODULE:
!    This module simulates the performance of the direct fired
!    absorption chiller.
!
! METHODOLOGY EMPLOYED:
!    Once the PlantLoopManager determines that the absorber chiller
!    is available to meet a loop cooling demand, it calls SimGasAbsorption
!    which in turn calls the appropriate Absorption Chiller model.
!
! REFERENCES:
!    DOE-2.1e Supplement
!    PG&E CoolTools GasMod
!
! OTHER NOTES:
!    The curves on this model follow the DOE-2 approach of using
!    electric and heat input ratios.  In addition, the temperature
!    correction curve has two independent variables for the
!    chilled water temperature and either the entering or leaving
!    condenser water temperature.
!    The code was originally adopted from the ChillerAbsorption
!    routine but has been extensively modified.
!
!    Development of this module was funded by the Gas Research Institute.
!    (Please see copyright and disclaimer information at end of module)


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataInterfaces
USE DataGlobals ,   ONLY : MaxNameLength, BigNumber, InitConvTemp, SecInHour
USE DataHVACGlobals, ONLY: SmallWaterVolFlow

USE General,         ONLY: TrimSigDigits

IMPLICIT NONE

          !MODULE PARAMETER DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
PRIVATE
INTEGER, SAVE            :: NumGasAbsorbers =0 ! number of Absorption Chillers specified in input

TYPE GasAbsorberSpecs
! Parts of Type that do not correspond with IDD definition
       LOGICAL           :: Available          =.false. ! need an array of logicals--load identifiers of available equipment
       LOGICAL           :: ON                 =.false. ! simulate the machine at it's operating part load ratio
       LOGICAL           :: InCoolingMode      = .FALSE.
       LOGICAL           :: InHeatingMode      = .FALSE.
! Part of Type that directly corresponds with IDD definition
       CHARACTER(len=MaxNameLength) :: Name    =' ' ! user identifier
       CHARACTER(len=MaxNameLength) ::FuelType =' ' ! Type of Fuel - DIESEL, GASOLINE, GAS
       REAL(r64)         :: NomCoolingCap      =0.0d0 ! W - design nominal capacity of Absorber
       REAL(r64)         :: NomHeatCoolRatio   =0.0d0 ! ratio of heating to cooling capacity
       REAL(r64)         :: FuelCoolRatio      =0.0d0 ! ratio of fuel input to cooling output
       REAL(r64)         :: FuelHeatRatio      =0.0d0 ! ratio of fuel input to heating output
       REAL(r64)         :: ElecCoolRatio      =0.0d0 ! ratio of electricity input to cooling output
       REAL(r64)         :: ElecHeatRatio      =0.0d0 ! ratio of electricity input to heating output
       INTEGER           :: ChillReturnNodeNum =0   ! Node number on the inlet side of the plant
       INTEGER           :: ChillSupplyNodeNum =0   ! Node number on the outlet side of the plant
       LOGICAL           :: ChillSetpointErrDone = .FALSE. ! flag to report missing setpoint on CW outlet
       LOGICAL           :: ChillSetpointSetToLoop = .FALSE. ! flag to use overall loop setpoint
       INTEGER           :: CondReturnNodeNum  =0   ! Node number on the inlet side of the condenser
       INTEGER           :: CondSupplyNodeNum  =0   ! Node number on the outlet side of the condenser
       INTEGER           :: HeatReturnNodeNum  =0   ! absorber steam inlet node number, water side
       INTEGER           :: HeatSupplyNodeNum  =0   ! absorber steam outlet node number, water side
       LOGICAL           :: HeatSetpointErrDone = .FALSE. ! flag to report missing setpoint on HW outlet
       LOGICAL           :: HeatSetpointSetToLoop = .FALSE. ! flag to use overall loop setpoint
       REAL(r64)         :: MinPartLoadRat     =0.0d0 ! min allowed operating frac full load
       REAL(r64)         :: MaxPartLoadRat     =0.0d0 ! max allowed operating frac full load
       REAL(r64)         :: OptPartLoadRat     =0.0d0 ! optimal operating frac full load
       REAL(r64)         :: TempDesCondReturn  =0.0d0 ! design secondary loop fluid temperature at the Absorber condenser side inlet
       REAL(r64)         :: TempDesCHWSupply   =0.0d0 ! design chilled water supply temperature
       REAL(r64)         :: EvapVolFlowRate    =0.0d0 ! m**3/s - design nominal water volumetric flow rate through the evaporator
       REAL(r64)         :: CondVolFlowRate    =0.0d0 ! m**3/s - design nominal water volumetric flow rate through the condenser
       REAL(r64)         :: HeatVolFlowRate    =0.0d0 ! m**3/s - design nominal water volumetric flow rate through the heater side
       REAL(r64)         :: SizFac             =0.0d0 ! sizing factor
       INTEGER           :: CoolCapFTCurve     =0   ! cooling capacity as a function of temperature curve (chilled water temp,
                                                    ! condenser water temp)
       INTEGER           :: FuelCoolFTCurve    =0   ! Fuel-Input-to cooling output Ratio Function of Temperature Curve (chilled
                                                    ! water temp, condenser water temp)
       INTEGER           :: FuelCoolFPLRCurve  =0   ! Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
       INTEGER           :: ElecCoolFTCurve    =0   ! Electric-Input-to cooling output Ratio Function of Temperature Curve
                                                    ! (chilled water temp, condenser water temp)
       INTEGER           :: ElecCoolFPLRCurve  =0   ! Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
       INTEGER           :: HeatCapFCoolCurve  =0   ! Heating Capacity Function of Cooling Capacity Curve
       INTEGER           :: FuelHeatFHPLRCurve =0   ! Fuel Input to heat output ratio during heating only function
       LOGICAL           :: isEnterCondensTemp =.false. ! if using entering conderser water temperature is TRUE, exiting is FALSE
       LOGICAL           :: isWaterCooled      =.false. ! if water cooled it is TRUE
       REAL(r64)         :: CHWLowLimitTemp    =0.0d0 ! Chilled Water Lower Limit Temperature
       REAL(r64)         :: FuelHeatingValue   =0.0d0
! Calculated design values
       REAL(r64)         :: DesCondMassFlowRate=0.0d0    ! design nominal mass flow rate of water through the condenser [kg/s]
       REAL(r64)         :: DesHeatMassFlowRate=0.d0   ! design nominal mass flow rate of water through the hot water side [kg/s]
       REAL(r64)         :: DesEvapMassFlowRate=0.d0   ! design nominal mass flow rate of water through chilled water side [kg/s]
! other values used during simulation
       INTEGER           :: DeltaTempCoolErrCount  = 0  ! error count for Delta Temp = 0 while cooling
       INTEGER           :: DeltaTempHeatErrCount  = 0  ! error count for Delta Temp = 0 while heating
       INTEGER           :: CondErrCount       = 0      ! error count for poor Condenser Supply Estimate
       LOGICAL           :: PossibleSubCooling = .false. !Flag to determine whether plant is overcooled
  !loop topology variables
       INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
       INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
       INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
       INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
       INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
       INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
       INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
       INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
       INTEGER           :: HWLoopNum     = 0  ! hot water plant loop side index
       INTEGER           :: HWLoopSideNum = 0  ! hot water plant loop side index
       INTEGER           :: HWBranchNum   = 0  ! hot water plant loop branch index
       INTEGER           :: HWCompNum     = 0  ! hot water plant loop component index

END TYPE GasAbsorberSpecs

! This type holds the output from the algorithm i.e., the Report Variables
TYPE ReportVars
  REAL(r64)    :: CoolingLoad        =0.0d0 ! cooling load on the chiller (previously called QEvap)
  REAL(r64)    :: CoolingEnergy      =0.0d0 ! variable to track total cooling load for period (was EvapEnergy)
  REAL(r64)    :: HeatingLoad        =0.0d0 ! heating load on the chiller
  REAL(r64)    :: HeatingEnergy      =0.0d0 ! heating energy
  REAL(r64)    :: TowerLoad          =0.0d0 ! load on the cooling tower/condenser (previously called QCond)
  REAL(r64)    :: TowerEnergy        =0.0d0 ! variable to track total tower load for a period (was CondEnergy)
  REAL(r64)    :: FuelUseRate        =0.0d0 ! instantaneous use of gas for period
  REAL(r64)    :: FuelEnergy         =0.0d0 ! variable to track total fuel used for a period
  REAL(r64)    :: CoolFuelUseRate    =0.0d0 ! instantaneous use of gas for period for cooling
  REAL(r64)    :: CoolFuelEnergy     =0.0d0 ! variable to track total fuel used for a period for cooling
  REAL(r64)    :: HeatFuelUseRate    =0.0d0 ! instantaneous use of gas for period for heating
  REAL(r64)    :: HeatFuelEnergy     =0.0d0 ! variable to track total fuel used for a period for heating
  REAL(r64)    :: ElectricPower      =0.0d0 ! parasitic electric power used (was PumpingPower)
  REAL(r64)    :: ElectricEnergy     =0.0d0 ! track the total electricity used for a period (was PumpingEnergy)
  REAL(r64)    :: CoolElectricPower  =0.0d0 ! parasitic electric power used  for cooling
  REAL(r64)    :: CoolElectricEnergy =0.0d0 ! track the total electricity used for a period for cooling
  REAL(r64)    :: HeatElectricPower  =0.0d0 ! parasitic electric power used  for heating
  REAL(r64)    :: HeatElectricEnergy =0.0d0 ! track the total electricity used for a period for heating
  REAL(r64)    :: ChillReturnTemp    =0.0d0 ! reporting: evaporator inlet temperature (was EvapInletTemp)
  REAL(r64)    :: ChillSupplyTemp    =0.0d0 ! reporting: evaporator outlet temperature (was EvapOutletTemp)
  REAL(r64)    :: ChillWaterFlowRate =0.0d0 ! reporting: evaporator mass flow rate (was Evapmdot)
  REAL(r64)    :: CondReturnTemp     =0.0d0 ! reporting: condenser inlet temperature (was CondInletTemp)
  REAL(r64)    :: CondSupplyTemp     =0.0d0 ! reporting: condenser outlet temperature (was CondOutletTemp)
  REAL(r64)    :: CondWaterFlowRate  =0.0d0 ! reporting: condenser mass flow rate (was Condmdot)
  REAL(r64)    :: HotWaterReturnTemp =0.0d0 ! reporting: hot water return (inlet) temperature
  REAL(r64)    :: HotWaterSupplyTemp =0.0d0 ! reporting: hot water supply (outlet) temperature
  REAL(r64)    :: HotWaterFlowRate   =0.0d0 ! reporting: hot water mass flow rate
  REAL(r64)    :: CoolPartLoadRatio  =0.0d0     ! operating part load ratio (load/capacity for cooling)
  REAL(r64)    :: HeatPartLoadRatio  =0.0d0 ! operating part load ratio (load/capacity for heating)
  REAL(r64)    :: CoolingCapacity    =0.0d0 ! current capacity after temperature adjustment
  REAL(r64)    :: HeatingCapacity    =0.0d0 ! current heating capacity
  REAL(r64)    :: FractionOfPeriodRunning =0.0d0 ! fraction of the time period that the unit is operating
  REAL(r64)    :: FuelCOP            =0.0d0 ! reporting: cooling output/fuel input = CoolingLoad/CoolFuelUseRate

END TYPE ReportVars

TYPE (GasAbsorberSpecs), ALLOCATABLE, DIMENSION(:)  :: GasAbsorber  !dimension to number of machines
TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::GasAbsorberReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcGasAbsorberChillerModel
PRIVATE    CalcGasAbsorberHeaterModel
PRIVATE    GetGasAbsorberInput
PRIVATE    InitGasAbsorber
PRIVATE    SizeGasAbsorber
PRIVATE    UpdateGasAbsorberCoolRecords
PRIVATE    UpdateGasAbsorberHeatRecords
PUBLIC     SimGasAbsorber


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Absorption Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimGasAbsorber(AbsorberType,AbsorberName,EquipFlowCtrl, CompIndex,RunFlag,FirstIteration,InitLoopEquip,&
                          MyLoad,BranchInletNodeNum,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor,   &
                          TempCondInDesign,TempEvapOutDesign)
! SUBROUTINE INFORMATION:
!       AUTHOR         Jason Glazer
!       DATE WRITTEN   March 2001
!       MODIFIED       na
!       RE-ENGINEERED  na

! PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
! gets the input for the models, initializes simulation variables, call
! the appropriate model and sets up reporting variables.

! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : TypeOf_Chiller_DFAbsorption
  USE PlantUtilities,  ONLY :   UpdateChillerComponentCondenserSide
  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: AbsorberType   ! type of Absorber
  CHARACTER(len=*), INTENT(IN) :: AbsorberName   ! user specified name of Absorber
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER , INTENT(IN)   :: BranchInletNodeNum   ! node number of inlet to calling branch,
                                                 ! used to determine if heating side or cooling
                                                 ! side of chiller-heater is being called
  LOGICAL , INTENT(IN)   :: RunFlag             ! simulate Absorber when TRUE
  LOGICAL , INTENT(IN)   :: FirstIteration      ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT) :: InitLoopEquip       ! If not false, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)    :: MyLoad         ! loop demand component will meet
  REAL(r64), INTENT(OUT) :: MinCap              ! W - minimum operating capacity of Absorber
  REAL(r64), INTENT(OUT) :: MaxCap              ! W - maximum operating capacity of Absorber
  REAL(r64), INTENT(OUT) :: OptCap              ! W - optimal operating capacity of Absorber
  INTEGER, INTENT(INOUT) :: CompIndex           ! Absorber number counter
  LOGICAL, INTENT(IN)    :: GetSizingFactor     ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT) :: SizingFactor        ! sizing factor
  REAL(r64), INTENT(INOUT)    :: TempCondInDesign
  REAL(r64), INTENT(INOUT)    :: TempEvapOutDesign
         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)         :: HeatCap = 0.0d0       ! W - nominal heating capacity
  LOGICAL, SAVE     :: GetInput = .TRUE.   ! then TRUE, calls subroutine to read input file.
  INTEGER :: ChillNum            ! Absorber number counter

          !Get Absorber data from input file
  IF (GetInput) THEN
    CALL GetGasAbsorberInput
    GetInput = .FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(AbsorberName,GasAbsorber%Name,NumGasAbsorbers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimGasAbsorber: Unit not found='//TRIM(AbsorberName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumGasAbsorbers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimGasAbsorber:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumGasAbsorbers))//  &
                          ', Entered Unit name='//TRIM(AbsorberName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (AbsorberName /= GasAbsorber(ChillNum)%Name) THEN
        CALL ShowFatalError('SimGasAbsorber: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(AbsorberName)//', stored Unit Name for that index='//  &
                            TRIM(GasAbsorber(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF

          ! Check that this is a valid call
  IF (InitLoopEquip) THEN
    TempEvapOutDesign  = GasAbsorber(ChillNum)%TempDesCHWSupply
    TempCondInDesign   = GasAbsorber(ChillNum)%TempDesCondReturn
    CALL InitGasAbsorber(ChillNum,RunFlag)
    CALL SizeGasAbsorber(ChillNum)
    ! Match inlet node name of calling branch to determine if this call is for heating or cooling
    IF (BranchInletNodeNum == GasAbsorber(ChillNum)%ChillReturnNodeNum) THEN ! Operate as chiller
      MinCap = GasAbsorber(ChillNum)%NomCoolingCap*GasAbsorber(ChillNum)%MinPartLoadRat
      MaxCap = GasAbsorber(ChillNum)%NomCoolingCap*GasAbsorber(ChillNum)%MaxPartLoadRat
      OptCap = GasAbsorber(ChillNum)%NomCoolingCap*GasAbsorber(ChillNum)%OptPartLoadRat
    ELSEIF (BranchInletNodeNum == GasAbsorber(ChillNum)%HeatReturnNodeNum) THEN ! Operate as heater
      HeatCap = GasAbsorber(ChillNum)%NomCoolingCap*GasAbsorber(ChillNum)%NomHeatCoolRatio
      MinCap  = HeatCap*GasAbsorber(ChillNum)%MinPartLoadRat
      MaxCap  = HeatCap*GasAbsorber(ChillNum)%MaxPartLoadRat
      OptCap  = HeatCap*GasAbsorber(ChillNum)%OptPartLoadRat
    ELSEIF (BranchInletNodeNum == GasAbsorber(ChillNum)%CondReturnNodeNum) THEN ! called from condenser loop
      HeatCap = 0.d0
      MinCap  = 0.d0
      MaxCap  = 0.d0
      OptCap  = 0.d0
    ELSE  ! Error, nodes do not match
      CALL ShowSevereError('SimGasAbsorber: Invalid call to Gas Absorbtion Chiller-Heater '//TRIM(AbsorberName))
      CALL ShowContinueError('Node connections in branch are not consistent with object nodes.')
      CALL ShowFatalError('Preceding conditions cause termination.')
    ENDIF ! Operate as Chiller or Heater
    IF (GetSizingFactor) THEN
      SizingFactor = GasAbsorber(ChillNum)%SizFac
    END IF
    RETURN
  END IF

! Match inlet node name of calling branch to determine if this call is for heating or cooling
  IF (BranchInletNodeNum .EQ. GasAbsorber(ChillNum)%ChillReturnNodeNum) THEN ! Operate as chiller
            ! Calculate Node Values
            ! Calculate Equipment and Update Variables
    IF (RunFlag) THEN
      GasAbsorber(ChillNum)%InCoolingMode = .TRUE.
    ELSE
      GasAbsorber(ChillNum)%InCoolingMode = .FALSE.
    ENDIF
    CALL InitGasAbsorber(ChillNum,RunFlag)
    CALL CalcGasAbsorberChillerModel(ChillNum,MyLoad,Runflag)
    CALL UpdateGasAbsorberCoolRecords(MyLoad,RunFlag,ChillNum)
  ELSEIF (BranchInletNodeNum .EQ. GasAbsorber(ChillNum)%HeatReturnNodeNum) THEN ! Operate as heater
            ! Calculate Node Values
            ! Calculate Equipment and Update Variables
    IF (RunFlag) THEN
      GasAbsorber(ChillNum)%InHeatingMode = .TRUE.
    ELSE
      GasAbsorber(ChillNum)%InHeatingMode = .FALSE.
    ENDIF
    CALL InitGasAbsorber(ChillNum,RunFlag)
    CALL CalcGasAbsorberHeaterModel(ChillNum,MyLoad,Runflag)
    CALL UpdateGasAbsorberHeatRecords(MyLoad,RunFlag,ChillNum)
  ELSEIF (BranchInletNodeNum == GasAbsorber(ChillNum)%CondReturnNodeNum) THEN ! called from condenser loop
      CALL UpdateChillerComponentCondenserSide(GasAbsorber(ChillNum)%CDLoopNum, &
                                     GasAbsorber(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_DFAbsorption,                     &
                                     GasAbsorber(ChillNum)%CondReturnNodeNum, &
                                     GasAbsorber(ChillNum)%CondSupplyNodeNum,  &
                                     GasAbsorberReport(ChillNum)%TowerLoad,             &
                                     GasAbsorberReport(ChillNum)%CondReturnTemp,    &
                                     GasAbsorberReport(ChillNum)%CondSupplyTemp,     &
                                     GasAbsorberReport(ChillNum)%CondWaterFlowRate,          &
                                     FirstIteration)

  ELSE  ! Error, nodes do not match
    CALL ShowSevereError('Invalid call to Gas Absorber Chiller '//TRIM(AbsorberName))
    CALL ShowContinueError('Node connections in branch are not consistent with object nodes.')
    CALL ShowFatalError('Preceding conditions cause termination.')
  ENDIF

RETURN
END SUBROUTINE SimGasAbsorber

! End Absorption Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Absorption Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetGasAbsorberInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Jason Glazer
            !       DATE WRITTEN:    March 2001

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Direct Fired Absorption chiller modelin the object ChillerHeater:Absorption:DirectFired

            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE CurveManager, ONLY: GetCurveCheck
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: AbsorberNum !Absorber counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  CHARACTER(len=MaxNameLength)               :: ChillerName
  LOGICAL :: errflag
  LOGICAL :: Okay

         !FLOW
  cCurrentModuleObject = 'ChillerHeater:Absorption:DirectFired'
  NumGasAbsorbers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumGasAbsorbers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment found in input file')
    ErrorsFound=.true.
  ENDIF

  IF (ALLOCATED(GasAbsorber)) RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (GasAbsorber(NumGasAbsorbers))

  ALLOCATE (GasAbsorberReport(NumGasAbsorbers))
  ALLOCATE(CheckEquipName(NumGasAbsorbers))
  CheckEquipName=.true.

         !LOAD ARRAYS

  DO AbsorberNum = 1 , NumGasAbsorbers
    CALL GetObjectItem(cCurrentModuleObject,AbsorberNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),GasAbsorber%Name,AbsorberNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    GasAbsorber(AbsorberNum)%Name                = cAlphaArgs(1)
    ChillerName = TRIM(cCurrentModuleObject)//' Named ' // TRIM(GasAbsorber(AbsorberNum)%Name)

            ! Assign capacities
    GasAbsorber(AbsorberNum)%NomCoolingCap       = rNumericArgs(1)
    GasAbsorber(AbsorberNum)%NomHeatCoolRatio    = rNumericArgs(2)
            ! Assign efficiencies
    GasAbsorber(AbsorberNum)%FuelCoolRatio       = rNumericArgs(3)
    GasAbsorber(AbsorberNum)%FuelHeatRatio       = rNumericArgs(4)
    GasAbsorber(AbsorberNum)%ElecCoolRatio       = rNumericArgs(5)
    GasAbsorber(AbsorberNum)%ElecHeatRatio       = rNumericArgs(6)

            ! Assign Node Numbers to specified nodes
    GasAbsorber(AbsorberNum)%ChillReturnNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(2), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    GasAbsorber(AbsorberNum)%ChillSupplyNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(3), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')
            ! Condenser node processing depends on condenser type, see below
    GasAbsorber(AbsorberNum)%HeatReturnNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(6), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 3, ObjectIsNotParent)
    GasAbsorber(AbsorberNum)%HeatSupplyNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(7), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 3, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Hot Water Nodes')
    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in processing node input for '// &
            TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .FALSE.
    END IF

            ! Assign Part Load Ratios
    GasAbsorber(AbsorberNum)%MinPartLoadRat      = rNumericArgs(7)
    GasAbsorber(AbsorberNum)%MaxPartLoadRat      = rNumericArgs(8)
    GasAbsorber(AbsorberNum)%OptPartLoadRat      = rNumericArgs(9)
            ! Assign Design Conditions
    GasAbsorber(AbsorberNum)%TempDesCondReturn   = rNumericArgs(10)
    GasAbsorber(AbsorberNum)%TempDesCHWSupply    = rNumericArgs(11)
    GasAbsorber(AbsorberNum)%EvapVolFlowRate     = rNumericArgs(12)
    IF (SameString(cAlphaArgs(16),'AirCooled') ) THEN
      GasAbsorber(AbsorberNum)%CondVolFlowRate   = 0.0011d0 ! Condenser flow rate not used for this cond type
    ELSE
      GasAbsorber(AbsorberNum)%CondVolFlowRate   = rNumericArgs(13)
    ENDIF
    GasAbsorber(AbsorberNum)%HeatVolFlowRate     = rNumericArgs(14)
            ! Assign Curve Numbers
    GasAbsorber(AbsorberNum)%CoolCapFTCurve      = GetCurveCheck(cAlphaArgs(8),  ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%FuelCoolFTCurve     = GetCurveCheck(cAlphaArgs(9),  ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%FuelCoolFPLRCurve   = GetCurveCheck(cAlphaArgs(10), ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%ElecCoolFTCurve     = GetCurveCheck(cAlphaArgs(11), ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%ElecCoolFPLRCurve   = GetCurveCheck(cAlphaArgs(12), ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%HeatCapFCoolCurve   = GetCurveCheck(cAlphaArgs(13), ErrorsFound, ChillerName)
    GasAbsorber(AbsorberNum)%FuelHeatFHPLRCurve  = GetCurveCheck(cAlphaArgs(14), ErrorsFound, ChillerName)
    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in processing curve input for '// &
            TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .FALSE.
    END IF
    IF (SameString(cAlphaArgs(15),'LeavingCondenser')) THEN
      GasAbsorber(AbsorberNum)%isEnterCondensTemp = .FALSE.
    ELSEIF (SameString(cAlphaArgs(15),'EnteringCondenser')) THEN
      GasAbsorber(AbsorberNum)%isEnterCondensTemp = .TRUE.
    ELSE
      GasAbsorber(AbsorberNum)%isEnterCondensTemp = .TRUE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid value')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(15))//'="'//TRIM(cAlphaArgs(15))//'"')
      CALL ShowContinueError('resetting to EnteringCondenser, simulation continues')
    ENDIF
            ! Assign Other Paramters
    IF (SameString(cAlphaArgs(16),'AirCooled') ) THEN
      GasAbsorber(AbsorberNum)%isWaterCooled     = .FALSE.
    ELSEIF (SameString(cAlphaArgs(16),'WaterCooled')) THEN
      GasAbsorber(AbsorberNum)%isWaterCooled     = .TRUE.
    ELSE
      GasAbsorber(AbsorberNum)%isWaterCooled     = .TRUE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid value')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(16))//'='//TRIM(cAlphaArgs(16)))
      CALL ShowContinueError('resetting to WaterCooled, simulation continues')
    ENDIF
    IF (GasAbsorber(AbsorberNum)%isWaterCooled) THEN
      GasAbsorber(AbsorberNum)%CondReturnNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(4), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
                 NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      GasAbsorber(AbsorberNum)%CondSupplyNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
                 NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser Water Nodes')
    ELSE
      GasAbsorber(AbsorberNum)%CondReturnNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(4), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Air, &
                 NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      GasAbsorber(AbsorberNum)%CondSupplyNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Air, &
                 NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      ! Connection not required for air or evap cooled condenser so no call to TestCompSet here
      CALL CheckAndAddAirNodeNumber(GasAbsorber(AbsorberNum)%CondReturnNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(4)))
      ENDIF
    ENDIF
    GasAbsorber(AbsorberNum)%CHWLowLimitTemp     = rNumericArgs(15)
    GasAbsorber(AbsorberNum)%FuelHeatingValue    = rNumericArgs(16)
    GasAbsorber(AbsorberNum)%SizFac              = rNumericArgs(17)

    !Fuel Type Case Statement
    SELECT CASE (cAlphaArgs(18))
    CASE ('GAS','NATURALGAS','NATURAL GAS')
      GasAbsorber(AbsorberNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      GasAbsorber(AbsorberNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      GasAbsorber(AbsorberNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       GasAbsorber(AbsorberNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       GasAbsorber(AbsorberNum)%FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
       GasAbsorber(AbsorberNum)%FuelType = 'Propane'

    CASE ('OTHERFUEL1')
       GasAbsorber(AbsorberNum)%FuelType = 'OtherFuel1'

    CASE ('OTHERFUEL2')
       GasAbsorber(AbsorberNum)%FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid value')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(18))//'='//TRIM(cAlphaArgs(18)))
      CALL ShowContinueError('Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,'//  &
       'OtherFuel1 or OtherFuel2')
      ErrorsFound=.true.
    END SELECT

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO AbsorberNum = 1, NumGasAbsorbers
    ChillerName = GasAbsorber(AbsorberNum)%Name

    CALL SetupOutputVariable('Chiller Heater Evaporator Cooling Rate [W]', &
         GasAbsorberReport(AbsorberNum)%CoolingLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Cooling Energy [J]', &
         GasAbsorberReport(AbsorberNum)%CoolingEnergy,'System','Sum',ChillerName,  &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater Heating Rate [W]', &
         GasAbsorberReport(AbsorberNum)%HeatingLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Energy [J]', &
         GasAbsorberReport(AbsorberNum)%HeatingEnergy,'System','Sum',ChillerName, &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BOILERS',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Rate [W]', &
         GasAbsorberReport(AbsorberNum)%TowerLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Energy [J]', &
         GasAbsorberReport(AbsorberNum)%TowerEnergy,'System','Sum',ChillerName, &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater '// TRIM(GasAbsorber(AbsorberNum)%FuelType)//' Rate [W]', &
         GasAbsorberReport(AbsorberNum)%FuelUseRate  ,'System','Average',ChillerName)
         ! Do not include this on meters, this would duplicate the cool fuel and heat fuel
    CALL SetupOutputVariable('Chiller Heater '// TRIM(GasAbsorber(AbsorberNum)%FuelType)//' Energy [J]', &
         GasAbsorberReport(AbsorberNum)%FuelEnergy,'System','Sum',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Cooling ' &
         // TRIM(GasAbsorber(AbsorberNum)%FuelType)//' Rate [W]', &
         GasAbsorberReport(AbsorberNum)%CoolFuelUseRate  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Cooling ' &
         // TRIM(GasAbsorber(AbsorberNum)%FuelType)//' Energy [J]', &
         GasAbsorberReport(AbsorberNum)%CoolFuelEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey=GasAbsorber(AbsorberNum)%FuelType,GroupKey='Plant',EndUseKey='Cooling')

    CALL SetupOutputVariable('Chiller Heater Cooling COP [W/W]', &
         GasAbsorberReport(AbsorberNum)%FuelCOP ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Heating '// TRIM(GasAbsorber(AbsorberNum)%FuelType)//  &
         ' Rate [W]',GasAbsorberReport(AbsorberNum)%HeatFuelUseRate  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating '// TRIM(GasAbsorber(AbsorberNum)%FuelType)//  &
         ' Energy [J]',GasAbsorberReport(AbsorberNum)%HeatFuelEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey=GasAbsorber(AbsorberNum)%FuelType,GroupKey='Plant',EndUseKey='Heating')

    CALL SetupOutputVariable('Chiller Heater Electric Power [W]', &
         GasAbsorberReport(AbsorberNum)%ElectricPower  ,'System','Average',ChillerName)
         ! Do not include this on meters, this would duplicate the cool electric and heat electric
    CALL SetupOutputVariable('Chiller Heater Electric Energy [J]', &
         GasAbsorberReport(AbsorberNum)%ElectricEnergy,'System','Sum',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Cooling Electric Power [W]', &
         GasAbsorberReport(AbsorberNum)%CoolElectricPower  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Cooling Electric Energy [J]', &
         GasAbsorberReport(AbsorberNum)%CoolElectricEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey='Electricity',GroupKey='Plant',EndUseKey='Cooling')

    CALL SetupOutputVariable('Chiller Heater Heating Electric Power [W]', &
         GasAbsorberReport(AbsorberNum)%HeatElectricPower  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Electric Energy [J]', &
         GasAbsorberReport(AbsorberNum)%HeatElectricEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey='Electricity',GroupKey='Plant',EndUseKey='Heating')

    CALL SetupOutputVariable('Chiller Heater Evaporator Inlet Temperature [C]', &
         GasAbsorberReport(AbsorberNum)%ChillReturnTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Outlet Temperature [C]', &
         GasAbsorberReport(AbsorberNum)%ChillSupplyTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Mass Flow Rate [kg/s]', &
         GasAbsorberReport(AbsorberNum)%ChillWaterFlowRate  ,'System','Average',ChillerName)

    IF (GasAbsorber(AbsorberNum)%isWaterCooled) THEN
      CALL SetupOutputVariable('Chiller Heater Condenser Inlet Temperature [C]', &
           GasAbsorberReport(AbsorberNum)%CondReturnTemp  ,'System','Average',ChillerName)
      CALL SetupOutputVariable('Chiller Heater Condenser Outlet Temperature [C]', &
           GasAbsorberReport(AbsorberNum)%CondSupplyTemp  ,'System','Average',ChillerName)
      CALL SetupOutputVariable('Chiller Heater Condenser Mass Flow Rate [kg/s]', &
           GasAbsorberReport(AbsorberNum)%CondWaterFlowRate  ,'System','Average',ChillerName)
    ELSE
      CALL SetupOutputVariable('Chiller Heater Condenser Inlet Temperature [C]', &
           GasAbsorberReport(AbsorberNum)%CondReturnTemp  ,'System','Average',ChillerName)
    ENDIF

    CALL SetupOutputVariable('Chiller Heater Heating Inlet Temperature [C]', &
         GasAbsorberReport(AbsorberNum)%HotWaterReturnTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Outlet Temperature [C]', &
         GasAbsorberReport(AbsorberNum)%HotWaterSupplyTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Mass Flow Rate [kg/s]', &
         GasAbsorberReport(AbsorberNum)%HotWaterFlowRate  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Cooling Part Load Ratio []', &
         GasAbsorberReport(AbsorberNum)%CoolPartLoadRatio  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Maximum Cooling Rate [W]', &
         GasAbsorberReport(AbsorberNum)%CoolingCapacity  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Part Load Ratio []', &
         GasAbsorberReport(AbsorberNum)%HeatPartLoadRatio  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Maximum Heating Rate [W]', &
         GasAbsorberReport(AbsorberNum)%HeatingCapacity  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Runtime Fraction []', &
         GasAbsorberReport(AbsorberNum)%FractionOfPeriodRunning  ,'System','Average',ChillerName)

  END DO
RETURN
END SUBROUTINE GetGasAbsorberInput

! End of Get Input subroutines for the Absorption Chiller Module
!******************************************************************************

SUBROUTINE InitGasAbsorber(ChillNum,RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of direct fired absorption chiller
          ! components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : TypeOf_Chiller_DFAbsorption, ScanPlantLoopsForObject, PlantLoop, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE Psychrometrics,  ONLY : RhoH2O

          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum           ! number of the current engine driven chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag            ! TRUE when chiller operating
                                             ! used to determine if heating side or cooling
                                             ! side of chiller-heater is being called

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:), SAVE :: MyPlantScanFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: HeatInletNode      ! node number of hot water inlet node
  INTEGER :: HeatOutletNode     ! node number of hot water outlet node
  LOGICAL :: errFlag
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: mdot ! lcoal fluid mass flow rate

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlantScanFlag(NumGasAbsorbers))
    ALLOCATE(MyEnvrnFlag(NumGasAbsorbers))
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
    MyPlantScanFlag = .TRUE.
  END IF

  ! Init more variables
  IF (MyPlantScanFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(GasAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_DFAbsorption, &
                                 GasAbsorber(ChillNum)%CWLoopNum, &
                                 GasAbsorber(ChillNum)%CWLoopSideNum, &
                                 GasAbsorber(ChillNum)%CWBranchNum, &
                                 GasAbsorber(ChillNum)%CWCompNum, &
                                 LowLimitTemp = GasAbsorber(ChillNum)%CHWLowLimitTemp, &
                                 InletNodeNumber = GasAbsorber(ChillNum)%ChillReturnNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitGasAbsorber: Program terminated due to previous condition(s).')
    ENDIF

    CALL ScanPlantLoopsForObject(GasAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_DFAbsorption, &
                                 GasAbsorber(ChillNum)%HWLoopNum, &
                                 GasAbsorber(ChillNum)%HWLoopSideNum, &
                                 GasAbsorber(ChillNum)%HWBranchNum, &
                                 GasAbsorber(ChillNum)%HWCompNum, &
                                 InletNodeNumber = GasAbsorber(ChillNum)%HeatReturnNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitGasAbsorber: Program terminated due to previous condition(s).')
    ENDIF

    IF (GasAbsorber(ChillNum)%isWaterCooled) THEN
      CALL ScanPlantLoopsForObject(GasAbsorber(ChillNum)%Name, &
                                   TypeOf_Chiller_DFAbsorption, &
                                   GasAbsorber(ChillNum)%CDLoopNum, &
                                   GasAbsorber(ChillNum)%CDLoopSideNum, &
                                   GasAbsorber(ChillNum)%CDBranchNum, &
                                   GasAbsorber(ChillNum)%CDCompNum, &
                                   InletNodeNumber = GasAbsorber(ChillNum)%CondReturnNodeNum,  &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitGasAbsorber: Program terminated due to previous condition(s).')
      ENDIF
      CALL InterConnectTwoPlantLoopSides( GasAbsorber(ChillNum)%CWLoopNum,      &
                                          GasAbsorber(ChillNum)%CWLoopSideNum,  &
                                          GasAbsorber(ChillNum)%CDLoopNum,      &
                                          GasAbsorber(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_DFAbsorption , .TRUE.)
      CALL InterConnectTwoPlantLoopSides( GasAbsorber(ChillNum)%HWLoopNum,      &
                                          GasAbsorber(ChillNum)%HWLoopSideNum,  &
                                          GasAbsorber(ChillNum)%CDLoopNum,      &
                                          GasAbsorber(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_DFAbsorption , .TRUE. )
    ENDIF

    CALL InterConnectTwoPlantLoopSides( GasAbsorber(ChillNum)%CWLoopNum,      &
                                        GasAbsorber(ChillNum)%CWLoopSideNum,  &
                                        GasAbsorber(ChillNum)%HWLoopNum,      &
                                        GasAbsorber(ChillNum)%HWLoopSideNum,  &
                                          TypeOf_Chiller_DFAbsorption, .TRUE. )

    ! check if outlet node of chilled water side has a setpoint.
    IF ((Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
      IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        IF (.NOT. GasAbsorber(ChillNum)%ChillSetpointErrDone) THEN
          CALL ShowWarningError('Missing temperature setpoint on cool side for chiller heater named ' // &
                                        TRIM(GasAbsorber(ChillNum)%Name) )
          CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller ' // &
                                           ', use a SetpointManager')
          CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
          GasAbsorber(ChillNum)%ChillSetpointErrDone = .TRUE.
        ENDIF
      ELSE
       ! need call to EMS to check node
        errFlag = .FALSE. ! but not really fatal yet, but should be.
        CALL CheckIfNodeSetpointManagedByEMS(GasAbsorber(ChillNum)%ChillSupplyNodeNum,iTemperatureSetpoint, errFlag)
        IF (errFlag) THEN
          IF (.NOT. GasAbsorber(ChillNum)%ChillSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on cool side for chiller heater named ' // &
                                        TRIM(GasAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller evaporator ')
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
            CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            GasAbsorber(ChillNum)%ChillSetpointErrDone = .TRUE.

          ENDIF
        ENDIF


      ENDIF
      GasAbsorber(ChillNum)%ChillSetpointSetToLoop = .TRUE.
      Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
    ! check if outlet node of hot water side has a setpoint.
    IF ((Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo == SensedNodeFlagValue)) THEN
      IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        IF (.NOT. GasAbsorber(ChillNum)%HeatSetpointErrDone) THEN
          CALL ShowWarningError('Missing temperature setpoint on heat side for chiller heater named ' // &
                                        TRIM(GasAbsorber(ChillNum)%Name) )
          CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller ' // &
                                           ', use a SetpointManager')
          CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
          GasAbsorber(ChillNum)%HeatSetpointErrDone = .TRUE.
        ENDIF
      ELSE
       ! need call to EMS to check node
        errFlag = .FALSE. ! but not really fatal yet, but should be.
        CALL CheckIfNodeSetpointManagedByEMS(GasAbsorber(ChillNum)%HeatSupplyNodeNum,iTemperatureSetpoint, errFlag)
        IF (errFlag) THEN
          IF (.NOT. GasAbsorber(ChillNum)%HeatSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on heat side for chiller heater named ' // &
                                        TRIM(GasAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller heater ')
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the heater side outlet node ')
            CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for heater side. The simulation continues ... ')
            GasAbsorber(ChillNum)%HeatSetpointErrDone = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      GasAbsorber(ChillNum)%HeatSetpointSetToLoop = .TRUE.
      Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPointLo
    ENDIF
    MyPlantScanFlag(ChillNum)=.FALSE.
  ENDIF



  CondInletNode  = GasAbsorber(ChillNum)%CondReturnNodeNum
  CondOutletNode = GasAbsorber(ChillNum)%CondSupplyNodeNum
  HeatInletNode  = GasAbsorber(ChillNum)%HeatReturnNodeNum
  HeatOutletNode = GasAbsorber(ChillNum)%HeatSupplyNodeNum

  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))THEN
    IF (PlantSizeNotComplete) CALL SizeGasAbsorber(ChillNum)
    IF (GasAbsorber(ChillNum)%isWaterCooled) THEN
      ! init max available condenser water flow rate
      IF (GasAbsorber(ChillNum)%CDLoopNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                'InitGasAbsorber')
      ELSE
        rho = RhoH2O(InitConvTemp)

      ENDIF

      GasAbsorber(ChillNum)%DesCondMassFlowRate = rho * GasAbsorber(ChillNum)%CondVolFlowRate
      CALL InitComponentNodes(0.d0, GasAbsorber(ChillNum)%DesCondMassFlowRate, &
                                    CondInletNode, CondOutletNode,      &
                                    GasAbsorber(ChillNum)%CDLoopNum,     &
                                    GasAbsorber(ChillNum)%CDLoopSideNum, &
                                    GasAbsorber(ChillNum)%CDBranchNum,   &
                                    GasAbsorber(ChillNum)%CDCompNum)
    ENDIF

    IF (GasAbsorber(ChillNum)%HWLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%FluidIndex, &
                                'InitGasAbsorber')
    ELSE
       rho = RhoH2O(InitConvTemp)
    ENDIF
    GasAbsorber(ChillNum)%DesHeatMassFlowRate = rho * GasAbsorber(ChillNum)%HeatVolFlowRate
    !init available hot water flow rate
    CALL InitComponentNodes(0.d0, GasAbsorber(ChillNum)%DesHeatMassFlowRate, &
                                  HeatInletNode, HeatOutletNode,      &
                                 GasAbsorber(ChillNum)%HWLoopNum, &
                                 GasAbsorber(ChillNum)%HWLoopSideNum, &
                                 GasAbsorber(ChillNum)%HWBranchNum, &
                                 GasAbsorber(ChillNum)%HWCompNum)

    IF (GasAbsorber(ChillNum)%CWLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                'InitGasAbsorber')
    ELSE
       rho = RhoH2O(InitConvTemp)
    ENDIF
    GasAbsorber(ChillNum)%DesEvapMassFlowRate = rho * GasAbsorber(ChillNum)%EvapVolFlowRate
    !init available hot water flow rate
    CALL InitComponentNodes(0.d0, GasAbsorber(ChillNum)%DesEvapMassFlowRate, &
                                  GasAbsorber(ChillNum)%ChillReturnNodeNum,   &
                                  GasAbsorber(ChillNum)%ChillSupplyNodeNum,   &
                                  GasAbsorber(ChillNum)%CWLoopNum,     &
                                  GasAbsorber(ChillNum)%CWLoopSideNum, &
                                  GasAbsorber(ChillNum)%CWBranchNum,   &
                                  GasAbsorber(ChillNum)%CWCompNum)

    MyEnvrnFlag(ChillNum) = .FALSE.

  END IF

  IF(.not. BeginEnvrnFlag)Then
    MyEnvrnFlag(ChillNum) = .TRUE.
  End IF

  !this component model works off setpoints on the leaving node
  ! fill from plant if needed
  IF (GasAbsorber(ChillNum)%ChillSetpointSetToLoop) THEN
    Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(GasAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ( GasAbsorber(ChillNum)%HeatSetpointSetToLoop ) THEN
    Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(GasAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo =  &
              Node(PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPointLo
  ENDIF

  IF ((GasAbsorber(ChillNum)%isWaterCooled) .AND. &
      ((GasAbsorber(ChillNum)%InHeatingMode) .OR. (GasAbsorber(ChillNum)%InCoolingMode)) &
        .AND. (.NOT. MyPlantScanFlag(ChillNum)) ) THEN
    mdot = GasAbsorber(ChillNum)%DesCondMassFlowRate
    !DSU removed, this has to have been wrong (?)  Node(CondInletNode)%Temp  = GasAbsorber(ChillNum)%TempDesCondReturn

    CALL SetComponentFlowRate(mdot, &
                              GasAbsorber(ChillNum)%CondReturnNodeNum,     &
                              GasAbsorber(ChillNum)%CondSupplyNodeNum,     &
                              GasAbsorber(ChillNum)%CDLoopNum,     &
                              GasAbsorber(ChillNum)%CDLoopSideNum, &
                              GasAbsorber(ChillNum)%CDBranchNum,   &
                              GasAbsorber(ChillNum)%CDCompNum)

  ELSE
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot, &
                              GasAbsorber(ChillNum)%CondReturnNodeNum,     &
                              GasAbsorber(ChillNum)%CondSupplyNodeNum,     &
                              GasAbsorber(ChillNum)%CDLoopNum,     &
                              GasAbsorber(ChillNum)%CDLoopSideNum, &
                              GasAbsorber(ChillNum)%CDBranchNum,   &
                              GasAbsorber(ChillNum)%CDCompNum)
  END IF

  RETURN

END SUBROUTINE InitGasAbsorber

SUBROUTINE SizeGasAbsorber(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing direct fired gas absorption chiller components for which
          ! capacities and flow rates have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,           ONLY: PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities,      ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
!  USE BranchInputManager,  ONLY: MyPlantSizingIndex
  USE FluidProperties,     ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER             :: PltSizCoolNum ! Plant Sizing index for cooling loop
  INTEGER             :: PltSizHeatNum ! Plant Sizing index for heating loop
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop

  LOGICAL             :: ErrorsFound   ! If errors detected in input
!  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)   :: Cp ! local fluid specific heat
  REAL(r64)   :: rho ! local fluid density
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  REAL(r64)           :: tmpHeatRecVolFlowRate ! local heat recovery design volume flow rate

  PltSizCoolNum = 0
  PltSizCondNum = 0
  PltSizHeatNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap             = GasAbsorber(ChillNum)%NomCoolingCap
  tmpEvapVolFlowRate    = GasAbsorber(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate    = GasAbsorber(ChillNum)%CondVolFlowRate
  tmpHeatRecVolFlowRate = GasAbsorber(ChillNum)%HeatVolFlowRate

  IF (GasAbsorber(ChillNum)%NomCoolingCap  == AutoSize .or. &
      GasAbsorber(ChillNum)%EvapVolFlowRate == AutoSize .or. &
      GasAbsorber(ChillNum)%HeatVolFlowRate == AutoSize .or. &
      GasAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN

    IF (GasAbsorber(ChillNum)%isWaterCooled) &
      PltSizCondNum = PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%PlantSizNum
    PltSizHeatNum = PlantLoop(GasAbsorber(ChillNum)%HWLoopNum)%PlantSizNum
    PltSizCoolNum = PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%PlantSizNum

  ENDIF

  IF (GasAbsorber(ChillNum)%NomCoolingCap  == AutoSize) THEN
    IF (PltSizCoolNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        Cp = GetSpecificHeatGlycol(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeGasAbsorber')
        rho = GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeGasAbsorber')
        tmpNomCap = Cp  * rho * PlantSizData(PltSizCoolNum)%DeltaT &
                                                    * PlantSizData(PltSizCoolNum)%DesVolFlowRate * GasAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%NomCoolingCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%NomCoolingCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeater:Absorption:DirectFired', GasAbsorber(ChillNum)%Name, &
                              'Nominal Cooling Capacity [W]', GasAbsorber(ChillNum)%NomCoolingCap)
    ELSE
      CALL ShowSevereError('SizeGasAbsorber: ChillerHeater:Absorption:DirectFired="'//trim(GasAbsorber(ChillNum)%Name)//  &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Direct Fired Absorption Chiller nominal cooling capacity requires')
      CALL ShowContinueError('a cooling loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (GasAbsorber(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizCoolNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizCoolNum)%DesVolFlowRate * GasAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeater:Absorption:DirectFired', GasAbsorber(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              GasAbsorber(ChillNum)%EvapVolFlowRate)

    ELSE
      CALL ShowSevereError('SizeGasAbsorber: ChillerHeater:Absorption:DirectFired="'//trim(GasAbsorber(ChillNum)%Name)//  &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Direct Fired Absorption Chiller evap flow rate requires')
      CALL ShowContinueError('a cooling loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(GasAbsorber(ChillNum)%ChillReturnNodeNum,tmpEvapVolFlowRate)

  IF (GasAbsorber(ChillNum)%HeatVolFlowRate == AutoSize) THEN
    IF (PltSizHeatNum > 0) THEN
      IF (PlantSizData(PltSizHeatNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpHeatRecVolFlowRate = PlantSizData(PltSizHeatNum)%DesVolFlowRate * GasAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%HeatVolFlowRate = tmpHeatRecVolFlowRate
      ELSE
        tmpHeatRecVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%HeatVolFlowRate = tmpHeatRecVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeater:Absorption:DirectFired', GasAbsorber(ChillNum)%Name, &
                              'Design Hot Water Flow Rate [m3/s]', &
                              GasAbsorber(ChillNum)%HeatVolFlowRate)
    ELSE
      CALL ShowSevereError('SizeGasAbsorber: ChillerHeater:Absorption:DirectFired="'//trim(GasAbsorber(ChillNum)%Name)//  &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Direct Fired Absorption Chiller hot water flow rate requires')
      CALL ShowContinueError('a heating loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(GasAbsorber(ChillNum)%HeatReturnNodeNum,tmpHeatRecVolFlowRate)

  IF ((GasAbsorber(ChillNum)%CondVolFlowRate == AutoSize) .AND. (GasAbsorber(ChillNum)%isWaterCooled)) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        Cp = GetSpecificHeatGlycol(PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   GasAbsorber(ChillNum)%TempDesCondReturn, &
                                   PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeGasAbsorber')
        rho = GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   GasAbsorber(ChillNum)%TempDesCondReturn, &
                                   PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeGasAbsorber')
        tmpCondVolFlowRate = tmpNomCap * &
                             (1.0d0 + GasAbsorber(ChillNum)%FuelCoolRatio) / &
                             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeater:Absorption:DirectFired', GasAbsorber(ChillNum)%Name, &
                                           'Design Condenser Water Flow Rate [m3/s]', &
                                           GasAbsorber(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('SizeGasAbsorber: ChillerHeater:Absorption:DirectFired="'//trim(GasAbsorber(ChillNum)%Name)//  &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Direct Fired Absorption Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (GasAbsorber(ChillNum)%isWaterCooled)  &
    CALL RegisterPlantCompDesignFlow(GasAbsorber(ChillNum)%CondReturnNodeNum,tmpCondVolFlowRate)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = GasAbsorber(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'ChillerHeater:Absorption:DirectFired')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,GasAbsorber(ChillNum)%FuelCoolRatio)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,GasAbsorber(ChillNum)%NomCoolingCap)
  ENDIF

  RETURN
END SUBROUTINE SizeGasAbsorber


! Beginning of Absorber model Subroutines
! *****************************************************************************

SUBROUTINE CalcGasAbsorberChillerModel(ChillNum,MyLoad,Runflag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a direct fired (gas consuming) absorption chiller using
          ! curves and inputs similar to DOE-2.1e

          ! METHODOLOGY EMPLOYED:
          ! Curve fit of performance data

          ! REFERENCES:
          ! 1.  DOE-2.1e Supplement and source code
          ! 2.  CoolTools GasMod work

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginFullSimFlag
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : DeltaTemptol, PlantLoop, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER              :: ChillNum        ! Absorber number
  REAL(r64)            :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)  :: RunFlag         ! TRUE when Absorber operating

! FlowLock = 0  if mass flow rates may be changed by loop components
! FlowLock = 1  if mass flow rates may not be changed by loop components

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! Local copies of GasAbsorberSpecs Type
          ! all variables that are local copies of data structure
          ! variables are prefaced with an "l" for local.
  REAL(r64)         :: lNomCoolingCap      ! W - design nominal capacity of Absorber
  REAL(r64)         :: lFuelCoolRatio      ! ratio of fuel input to cooling output
  REAL(r64)         :: lFuelHeatRatio      ! ratio of fuel input to heating output
  REAL(r64)         :: lElecCoolRatio      ! ratio of electricity input to cooling output
  INTEGER           :: lChillReturnNodeNum ! Node number on the inlet side of the plant
  INTEGER           :: lChillSupplyNodeNum ! Node number on the outlet side of the plant
  INTEGER           :: lCondReturnNodeNum  ! Node number on the inlet side of the condenser
  INTEGER           :: lCondSupplyNodeNum  ! Node number on the outlet side of the condenser
  REAL(r64)         :: lMinPartLoadRat     ! min allowed operating frac full load
  REAL(r64)         :: lMaxPartLoadRat     ! max allowed operating frac full load
  REAL(r64)         :: lOptPartLoadRat     ! optimal operating frac full load
  REAL(r64)         :: lTempDesCondReturn  ! design secondary loop fluid temperature at the Absorber condenser side inlet
  REAL(r64)         :: lTempDesCHWSupply   ! design chilled water supply temperature
  REAL(r64)         :: lCondVolFlowRate    ! m**3/s - design nominal water volumetric flow rate through the condenser
  INTEGER           :: lCoolCapFTCurve     ! cooling capacity as a function of temperature curve
  INTEGER           :: lFuelCoolFTCurve    ! Fuel-Input-to cooling output Ratio Function of Temperature Curve
  INTEGER           :: lFuelCoolFPLRCurve  ! Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
  INTEGER           :: lElecCoolFTCurve    ! Electric-Input-to cooling output Ratio Function of Temperature Curve
  INTEGER           :: lElecCoolFPLRCurve  ! Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
  LOGICAL           :: lIsEnterCondensTemp ! if using entering conderser water temperature is TRUE, exiting is FALSE
  LOGICAL           :: lIsWaterCooled      ! if water cooled it is TRUE
  REAL(r64)         :: lCHWLowLimitTemp    ! Chilled Water Lower Limit Temperature
  REAL(r64)         :: lFuelHeatingValue
            ! Local copies of GasAbsorberReportVars Type
  REAL(r64)    :: lCoolingLoad        ! cooling load on the chiller (previously called QEvap)
  REAL(r64)    :: lCoolingEnergy      ! variable to track total cooling load for period (was EvapEnergy)
  REAL(r64)    :: lTowerLoad          ! load on the cooling tower/condenser (previously called QCond)
  REAL(r64)    :: lTowerEnergy        ! variable to track total tower load for a period (was CondEnergy)
  REAL(r64)    :: lFuelUseRate        ! instantaneous use of gas for period
  REAL(r64)    :: lFuelEnergy         ! variable to track total fuel used for a period
  REAL(r64)    :: lCoolFuelUseRate    ! instantaneous use of gas for period for cooling
  REAL(r64)    :: lCoolFuelEnergy     ! variable to track total fuel used for a period for cooling
  REAL(r64)    :: lHeatFuelUseRate    ! instantaneous use of gas for period for heating
  REAL(r64)    :: lElectricPower      ! parasitic electric power used (was PumpingPower)
  REAL(r64)    :: lElectricEnergy     ! track the total electricity used for a period (was PumpingEnergy)
  REAL(r64)    :: lCoolElectricPower  ! parasitic electric power used  for cooling
  REAL(r64)    :: lCoolElectricEnergy ! track the total electricity used for a period for cooling
  REAL(r64)    :: lHeatElectricPower  ! parasitic electric power used  for heating
  REAL(r64)    :: lChillReturnTemp    ! reporting: evaporator inlet temperature (was EvapInletTemp)
  REAL(r64)    :: lChillSupplyTemp    ! reporting: evaporator outlet temperature (was EvapOutletTemp)
  REAL(r64)    :: lChillWaterMassFlowRate ! reporting: evaporator mass flow rate (was Evapmdot)
  REAL(r64)    :: lCondReturnTemp     ! reporting: condenser inlet temperature (was CondInletTemp)
  REAL(r64)    :: lCondSupplyTemp     ! reporting: condenser outlet temperature (was CondOutletTemp)
  REAL(r64)    :: lCondWaterMassFlowRate  ! reporting: condenser mass flow rate (was Condmdot)
  REAL(r64)    :: lCoolPartLoadRatio      ! operating part load ratio (load/capacity for cooling)
  REAL(r64)    :: lHeatPartLoadRatio      ! operating part load ratio (load/capacity for heating)
  REAL(r64)    :: lAvailableCoolingCapacity   ! current capacity after temperature adjustment
  REAL(r64)    :: lFractionOfPeriodRunning
  REAL(r64)    :: Partloadrat                 ! actual operating part load ratio of unit (ranges from minplr to 1)
  Real(r64)    :: lChillWaterMassflowratemax  ! Maximum flow rate through the evaporator

  ! other local variables
  REAL(r64)              :: ChillDeltaTemp      ! chilled water temperature difference
  REAL(r64)              :: ChillSupplySetPointTemp

  REAL(r64)              :: calcCondTemp          ! the condenser temperature used for curve calculation
                                                  ! either return or supply depending on user input
  REAL(r64), SAVE        :: oldCondSupplyTemp = 0.0d0 ! save the last iteration value of leaving condenser water temperature
  REAL(r64)              :: revisedEstimateAvailCap ! final estimate of available capacity if using leaving
                                                    ! condenser water temperature
  REAL(r64)              :: errorAvailCap          ! error fraction on final estimate of AvailableCoolingCapacity
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64)  :: rhoCW ! local fluid density for chilled water
  REAL(r64)  :: Cp_CW  ! local fluid specific heat for chilled water
  REAL(r64)  :: rhoCD ! local fluid density for condenser water
  REAL(r64)  :: Cp_CD  ! local fluid specific heat for condenser water

  !initialize all output variables to zero

  lCoolingLoad        = 0.0d0
  lCoolingEnergy      = 0.0d0
  lTowerLoad          = 0.0d0
  lTowerEnergy        = 0.0d0
  lFuelUseRate        = 0.0d0
  lFuelEnergy         = 0.0d0
  lCoolFuelUseRate    = 0.0d0
  lCoolFuelEnergy     = 0.0d0
  lHeatFuelUseRate    = 0.0d0
  lElectricPower      = 0.0d0
  lElectricEnergy     = 0.0d0
  lCoolElectricPower  = 0.0d0
  lCoolElectricEnergy = 0.0d0
  lHeatElectricPower  = 0.0d0
  lChillReturnTemp    = 0.0d0
  lChillSupplyTemp    = 0.0d0
  lChillWaterMassFlowRate = 0.0d0
  lCondReturnTemp     = 0.0d0
  lCondSupplyTemp     = 0.0d0
  lCondWaterMassFlowRate  = 0.0d0
  lCoolPartLoadRatio      = 0.0d0
  lHeatPartLoadRatio      = 0.0d0
  lAvailableCoolingCapacity    = 0.0d0
  lFractionOfPeriodRunning = 0.0d0
  PartloadRat         = 0.0d0

  ! set node values to data structure values for nodes

  lChillReturnNodeNum = GasAbsorber(ChillNum)%ChillReturnNodeNum
  lChillSupplyNodeNum = GasAbsorber(ChillNum)%ChillSupplyNodeNum
  lCondReturnNodeNum  = GasAbsorber(ChillNum)%CondReturnNodeNum
  lCondSupplyNodeNum  = GasAbsorber(ChillNum)%CondSupplyNodeNum

  ! set local copies of data from rest of input structure

  lNomCoolingCap      = GasAbsorber(ChillNum)%NomCoolingCap
  lFuelCoolRatio      = GasAbsorber(ChillNum)%FuelCoolRatio
  lFuelHeatRatio      = GasAbsorber(ChillNum)%FuelHeatRatio
  lElecCoolRatio      = GasAbsorber(ChillNum)%ElecCoolRatio
  lMinPartLoadRat     = GasAbsorber(ChillNum)%MinPartLoadRat
  lMaxPartLoadRat     = GasAbsorber(ChillNum)%MaxPartLoadRat
  lOptPartLoadRat     = GasAbsorber(ChillNum)%OptPartLoadRat
  lTempDesCondReturn  = GasAbsorber(ChillNum)%TempDesCondReturn
  lTempDesCHWSupply   = GasAbsorber(ChillNum)%TempDesCHWSupply
  lCondVolFlowRate    = GasAbsorber(ChillNum)%CondVolFlowRate
  lCoolCapFTCurve     = GasAbsorber(ChillNum)%CoolCapFTCurve
  lFuelCoolFTCurve    = GasAbsorber(ChillNum)%FuelCoolFTCurve
  lFuelCoolFPLRCurve  = GasAbsorber(ChillNum)%FuelCoolFPLRCurve
  lElecCoolFTCurve    = GasAbsorber(ChillNum)%ElecCoolFTCurve
  lElecCoolFPLRCurve  = GasAbsorber(ChillNum)%ElecCoolFPLRCurve
  lisEnterCondensTemp = GasAbsorber(ChillNum)%isEnterCondensTemp
  lisWaterCooled      = GasAbsorber(ChillNum)%isWaterCooled
  lCHWLowLimitTemp    = GasAbsorber(ChillNum)%CHWLowLimitTemp
  lFuelHeatingValue   = GasAbsorber(ChillNum)%FuelHeatingValue

  lHeatElectricPower  = GasAbsorberReport(ChillNum)%HeatElectricPower
  lHeatFuelUseRate    = GasAbsorberReport(ChillNum)%HeatFuelUseRate
  lHeatPartLoadRatio  = GasAbsorberReport(ChillNum)%HeatPartLoadRatio

! initialize entering conditions
  lChillReturnTemp  = Node(lChillReturnNodeNum)%Temp
  lChillWaterMassFlowRate  = Node(lChillReturnNodeNum)%MassFlowRate
  lCondReturnTemp  = Node(lCondReturnNodeNum)%Temp
  lCondWaterMassFlowRate  = Node(lCondReturnNodeNum)%MassFlowRate
  SELECT CASE (PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    ChillSupplySetPointTemp = Node(lChillSupplyNodeNum)%TempSetPoint
  CASE (DualSetpointDeadband)
    ChillSupplySetPointTemp = Node(lChillSupplyNodeNum)%TempSetPointHi
  END SELECT
  ChillDeltaTemp  = ABS(lChillReturnTemp - ChillSupplySetPointTemp)

  rhoCW =  GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                            'CalcGasAbsorberChillerModel')
  Cp_CW = GetSpecificHeatGlycol(PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(GasAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                            'CalcGasAbsorberChillerModel')
  rhoCD =  GetDensityGlycol(PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                            'CalcGasAbsorberChillerModel')
  Cp_CD = GetSpecificHeatGlycol(PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(GasAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                            'CalcGasAbsorberChillerModel')

          !If no loop demand or Absorber OFF, return
          ! will need to modify when absorber can act as a boiler
 IF (MyLoad>=0 .OR. .NOT. ((GasAbsorber(ChillNum)%InHeatingMode) .OR. (GasAbsorber(ChillNum)%InCoolingMode))) THEN
          !set node temperatures
    lChillSupplyTemp = lChillReturnTemp
    lCondSupplyTemp  = lCondReturnTemp
    lCondWaterMassFlowRate = 0.0d0
    IF (lisWaterCooled) THEN
      CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              GasAbsorber(ChillNum)%CondReturnNodeNum, &
                              GasAbsorber(ChillNum)%CondSupplyNodeNum, &
                              GasAbsorber(ChillNum)%CDLoopNum, &
                              GasAbsorber(ChillNum)%CDLoopSideNum, &
                              GasAbsorber(ChillNum)%CDBranchNum, &
                              GasAbsorber(ChillNum)%CDCompNum)

    ENDIF
    ChillDeltaTemp   = 0.0d0
    lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)
 ELSE

  ! if water cooled use the input node otherwise just use outside air temperature
  IF (lIsWaterCooled) THEN
    ! most manufacturers rate have tables of entering condenser water temperature
    ! but a few use leaving condenser water temperature so we have a flag
    ! when leaving is used it uses the previous iterations value of the value
    lCondReturnTemp  =  Node(lCondReturnNodeNum)%Temp
    IF (lIsEnterCondensTemp) THEN
      calcCondTemp = lCondReturnTemp
    ELSE
      IF (oldCondSupplyTemp == 0) THEN
        oldCondSupplyTemp = lCondReturnTemp + 8.0d0 ! if not previously estimated assume 8C greater than return
      END IF
      calcCondTemp = oldCondSupplyTemp
    END IF
          !Set mass flow rates
    lCondWaterMassFlowRate = GasAbsorber(ChillNum)%DesCondMassFlowRate
    CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              GasAbsorber(ChillNum)%CondReturnNodeNum, &
                              GasAbsorber(ChillNum)%CondSupplyNodeNum, &
                              GasAbsorber(ChillNum)%CDLoopNum, &
                              GasAbsorber(ChillNum)%CDLoopSideNum, &
                              GasAbsorber(ChillNum)%CDBranchNum, &
                              GasAbsorber(ChillNum)%CDCompNum)
  ELSE
    ! air cooled
    Node(lCondReturnNodeNum)%Temp=Node(lCondReturnNodeNum)%OutAirDryBulb
    lCondReturnTemp = Node(lCondReturnNodeNum)%Temp
    lCondWaterMassFlowRate = 0.d0
    CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              GasAbsorber(ChillNum)%CondReturnNodeNum, &
                              GasAbsorber(ChillNum)%CondSupplyNodeNum, &
                              GasAbsorber(ChillNum)%CDLoopNum, &
                              GasAbsorber(ChillNum)%CDLoopSideNum, &
                              GasAbsorber(ChillNum)%CDBranchNum, &
                              GasAbsorber(ChillNum)%CDCompNum)
  END IF

            !Determine available cooling capacity using the setpoint temperature
  lAvailableCoolingCapacity = lNomCoolingCap * CurveValue(lCoolCapFTCurve,ChillSupplySetPointTemp,calcCondTemp)

            !Calculate current load for cooling
  MyLoad = SIGN(MAX(ABS(MyLoad), lAvailableCoolingCapacity * lMinPartLoadRat), MyLoad )
  MyLoad = SIGN(MIN(ABS(MyLoad), lAvailableCoolingCapacity * lMaxPartLoadRat), MyLoad )

            ! Determine the following variables depending on if the flow has been set in
            ! the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
            !    chilled water flow,
            !    cooling load taken by the chiller, and
            !    supply temperature
  lChillWaterMassflowratemax = Gasabsorber(chillnum)%DesEvapMassFlowRate

  LoopNum = GasAbsorber(ChillNum)%CWLoopNum
  LoopSideNum = GasAbsorber(ChillNum)%CWLoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock)
    CASE (0) ! mass flow rates may be changed by loop components
      GasAbsorber(ChillNum)%Possiblesubcooling = .FALSE.
      lCoolingLoad = ABS(myLoad)
      IF (ChillDeltaTemp /= 0.0d0) THEN
        lChillWaterMassFlowRate = ABS(lCoolingLoad / (Cp_CW * ChillDeltaTemp))
        If(lChillWaterMassFlowRate -lChillWaterMassflowratemax.GT.MassFlowTolerance)   &
           GasAbsorber(ChillNum)%Possiblesubcooling = .TRUE.

        CALL SetComponentFlowRate(lChillWaterMassFlowRate, &
                              GasAbsorber(ChillNum)%ChillReturnNodeNum, &
                              GasAbsorber(ChillNum)%ChillSupplyNodeNum, &
                              GasAbsorber(ChillNum)%CWLoopNum, &
                              GasAbsorber(ChillNum)%CWLoopSideNum, &
                              GasAbsorber(ChillNum)%CWBranchNum, &
                              GasAbsorber(ChillNum)%CWCompNum)
        lChillSupplyTemp = ChillSupplySetPointTemp
      ELSE
        lChillWaterMassFlowRate = 0.0d0
        CALL ShowRecurringWarningErrorAtEnd('GasAbsorberChillerModel:Cooling"'//TRIM(GasAbsorber(ChillNum)%Name)//  &
             '", DeltaTemp = 0 in mass flow calculation',GasAbsorber(ChillNum)%DeltaTempCoolErrCount)
      END IF
      lChillSupplyTemp = ChillSupplySetPointTemp
    CASE (1) ! mass flow rates may not be changed by loop components
      lChillWatermassflowrate = Node(lChillreturnnodenum)%Massflowrate
      If (GasAbsorber(ChillNum)%Possiblesubcooling) then
        lCoolingload = ABS(myload)

        ChillDeltaTemp = lCoolingload/lChillWatermassflowrate/Cp_CW
        lChillSupplyTemp = Node(lChillReturnnodenum)%Temp - ChillDeltaTemp
      ELSE
        ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - ChillSupplySetPointTemp
        lCoolingload = ABS(lChillWatermassflowrate*Cp_CW*ChillDeltaTemp)
        lChillSupplyTemp = ChillSupplySetPointTemp
      END IF
      !Check that the Chiller Supply outlet temp honors both plant loop temp low limit and also the chiller low limit
      IF(lChillSupplyTemp .LT. lCHWLowLimitTemp ) THEN
        IF((Node(lChillReturnnodenum)%Temp - lCHWLowLimitTemp ) .GT. DeltaTemptol) THEN
          lChillSupplyTemp = lCHWLowLimitTemp
          ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - lChillSupplyTemp
          lCoolingload = lChillWatermassflowrate * Cp_CW * ChillDeltaTemp
        ELSE
          lChillSupplyTemp = Node(lChillReturnnodenum)%Temp
          ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - lChillSupplyTemp
          lCoolingload = lChillWatermassflowrate * Cp_CW * ChillDeltaTemp
        END IF
      END IF
      IF(lChillSupplyTemp .LT. Node(lChillSupplyNodenum)%TempMin) THEN
        IF((Node(lChillReturnnodenum)%Temp - Node(lChillSupplyNodenum)%TempMin) .GT. DeltaTemptol) THEN
          lChillSupplyTemp = Node(lChillSupplyNodenum)%TempMin
          ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - lChillSupplyTemp
          lCoolingload = lChillWatermassflowrate * Cp_CW * ChillDeltaTemp
        ELSE
          lChillSupplyTemp = Node(lChillReturnnodenum)%Temp
          ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - lChillSupplyTemp
          lCoolingload = lChillWatermassflowrate * Cp_CW * ChillDeltaTemp
        END IF
      END IF

                 ! Checks Coolingload on the basis of the machine limits.
      If(lCoolingload > ABS(MyLoad)) Then
        If(lChillwatermassflowrate > MassFlowTolerance) THEN
          lCoolingload = ABS(MyLoad)
          ChillDeltaTemp = lCoolingload/lChillwatermassflowrate/Cp_CW
          lChillSupplyTemp = Node(lChillReturnnodenum)%Temp - ChillDeltaTemp
        Else
          lChillSupplyTemp  = Node(lChillReturnnodenum)%Temp
          ChillDeltaTemp = Node(lChillReturnnodenum)%Temp - lChillSupplyTemp
          lCoolingload = lChillWatermassflowrate * Cp_CW * ChillDeltaTemp
        End If
      End If
  END SELECT

  !Calculate operating part load ratio for cooling
  Partloadrat = MIN(ABS(MyLoad)/lAvailableCoolingCapacity,lMaxPartLoadRat)
  Partloadrat = MAX(lMinPartLoadRat,Partloadrat)

  IF(lAvailableCoolingCapacity > 0.0d0) THEN
    IF(ABS(MyLoad)/lAvailableCoolingCapacity.LT.lMinPartLoadRat) THEN
      lCoolPartLoadRatio = myload/lAvailableCoolingCapacity
    ELSE
      lCoolPartLoadRatio = PartLoadRat
    ENDIF
  ELSE     !Else if AvailableCoolingCapacity < 0.0
    lCoolPartLoadRatio = 0.0d0
  ENDIF

  ! calculate the fraction of the time period that the chiller would be running
  ! use maximum from heating and cooling sides
  IF(lCoolPartLoadRatio.LT.lMinPartLoadRat.OR.lHeatPartLoadRatio.LT.lMinPartLoadRat) THEN
    lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)
  ELSE
    lFractionOfPeriodRunning = 1.0d0
  ENDIF

  !Calculate fuel consumption for cooling
  ! fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR
  lCoolFuelUseRate = lAvailableCoolingCapacity * lFuelCoolRatio  &
     * CurveValue(lFuelCoolFTCurve,lChillSupplyTemp,calcCondTemp) &
     * CurveValue(lFuelCoolFPLRCurve,lCoolPartLoadRatio)*lFractionOfPeriodRunning

  !Calculate electric parasitics used
  ! based on nominal capacity, not available capacity,
  ! electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
  lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning &
     * CurveValue(lElecCoolFTCurve,lChillSupplyTemp,calcCondTemp) &
     * CurveValue(lElecCoolFPLRCurve,lCoolPartLoadRatio)

  ! determine conderser load which is cooling load plus the
  ! fuel used for cooling times the burner efficiency plus
  ! the electricity used
  lTowerLoad = lCoolingLoad + lCoolFuelUseRate / lFuelHeatRatio + lCoolElectricPower

  ! for water cooled condenser make sure enough flow rate
  ! for air cooled condenser just set supply to return temperature
  IF (lIsWaterCooled) THEN
    IF (lCondWaterMassFlowRate > MassFlowTolerance) THEN
      lCondSupplyTemp = lCondReturnTemp + lTowerLoad / (lCondWaterMassFlowRate * Cp_CD )
    ELSE
      CALL ShowSevereError('CalcGasAbsorberChillerModel: Condenser flow = 0, for Gas Absorber Chiller='//  &
                           TRIM(GasAbsorber(ChillNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowFatalError('Program Terminates due to previous error condition.')
    END IF
  ELSE
    lCondSupplyTemp = lCondReturnTemp  !if air cooled condenser just set supply and return to same temperature
  END IF

  ! save the condenser water supply temperature for next iteration if that is used in lookup
  ! and if capacity is large enough error than report problem
  oldCondSupplyTemp = lCondSupplyTemp
  IF (.NOT. lIsEnterCondensTemp) THEN
    ! calculate the fraction of the estimated error between the capacity based on the previous
    ! iteration's value of condenser supply temperature and the actual calculated condenser supply
    ! temperature.  If this becomes too common then may need to iterate a solution instead of
    ! relying on previous iteration method.
    revisedEstimateAvailCap = lNomCoolingCap * CurveValue(lCoolCapFTCurve,ChillSupplySetPointTemp,lCondSupplyTemp)
    IF (revisedEstimateAvailCap > 0.0d0) THEN
      errorAvailCap = ABS((revisedEstimateAvailCap - lAvailableCoolingCapacity)/revisedEstimateAvailCap)
      IF (errorAvailCap > 0.05d0) THEN ! if more than 5% error in estimate
        CALL ShowRecurringWarningErrorAtEnd('GasAbsorberChillerModel:"'//TRIM(GasAbsorber(ChillNum)%Name)//  &
          '", poor Condenser Supply Estimate',GasAbsorber(ChillNum)%condErrCount,ReportMinOf=errorAvailCap,  &
          ReportMaxOf=errorAvailCap)
      ENDIF
    ENDIF
  ENDIF
 ENDIF ! IF(MyLoad>=0 .OR. .NOT. Runflag)
  ! Write into the Report Variables except for nodes
  GasAbsorberReport(ChillNum)%CoolingLoad             = lCoolingLoad
  GasAbsorberReport(ChillNum)%TowerLoad               = lTowerLoad
  GasAbsorberReport(ChillNum)%CoolFuelUseRate         = lCoolFuelUseRate
  GasAbsorberReport(ChillNum)%CoolElectricPower       = lCoolElectricPower
  GasAbsorberReport(ChillNum)%CondReturnTemp          = lCondReturnTemp
  GasAbsorberReport(ChillNum)%ChillReturnTemp         = lChillReturnTemp
  GasAbsorberReport(ChillNum)%CondSupplyTemp          = lCondSupplyTemp
  GasAbsorberReport(ChillNum)%ChillSupplyTemp         = lChillSupplyTemp
  GasAbsorberReport(ChillNum)%ChillWaterFlowRate      = lChillWaterMassFlowRate
  GasAbsorberReport(ChillNum)%CondWaterFlowRate       = lCondWaterMassFlowRate
  GasAbsorberReport(ChillNum)%CoolPartLoadRatio       = lCoolPartLoadRatio
  GasAbsorberReport(ChillNum)%CoolingCapacity         = lAvailableCoolingCapacity
  GasAbsorberReport(ChillNum)%FractionOfPeriodRunning = lFractionOfPeriodRunning

  ! write the combined heating and cooling fuel used and electric used
  GasAbsorberReport(ChillNum)%FuelUseRate             = lCoolFuelUseRate + lHeatFuelUseRate
  GasAbsorberReport(ChillNum)%ElectricPower           = lCoolElectricPower + lHeatElectricPower

END SUBROUTINE CalcGasAbsorberChillerModel

SUBROUTINE CalcGasAbsorberHeaterModel(ChillNum,MyLoad,Runflag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer and Michael J. Witte
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a direct fired (gas consuming) absorption chiller using
          ! curves and inputs similar to DOE-2.1e

          ! METHODOLOGY EMPLOYED:
          ! Curve fit of performance data

          ! REFERENCES:
          ! 1.  DOE-2.1e Supplement and source code
          ! 2.  CoolTools GasMod work

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginFullSimFlag
  USE DataEnvironment, ONLY : OutDryBulbTemp
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : PlantLoop, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE FluidProperties, ONLY : GetSpecificHeatGlycol, GetDensityGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER              :: ChillNum        ! Absorber number
  REAL(r64)            :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)  :: RunFlag         ! TRUE when Absorber operating
! FlowLock = 0  if mass flow rates may be changed by loop components
! FlowLock = 1  if mass flow rates may not be changed by loop components
! FlowLock = 2  if overloaded and mass flow rates has changed to a small amount and Tout drops
!                 below Setpoint

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! Local copies of GasAbsorberSpecs Type
          ! all variables that are local copies of data structure
          ! variables are prefaced with an "l" for local.
  REAL(r64)         :: lNomCoolingCap      ! W - design nominal capacity of Absorber
  REAL(r64)         :: lNomHeatCoolRatio   ! ratio of heating to cooling capacity
  REAL(r64)         :: lFuelHeatRatio      ! ratio of fuel input to heating output
  REAL(r64)         :: lElecHeatRatio      ! ratio of electricity input to heating output
  INTEGER           :: lHeatReturnNodeNum  ! absorber steam inlet node number, water side
  INTEGER           :: lHeatSupplyNodeNum  ! absorber steam outlet node number, water side
  REAL(r64)         :: lMinPartLoadRat     ! min allowed operating frac full load
  REAL(r64)         :: lMaxPartLoadRat     ! max allowed operating frac full load
  REAL(r64)         :: lOptPartLoadRat     ! optimal operating frac full load
  INTEGER           :: lHeatCapFCoolCurve  ! Heating Capacity Function of Cooling Capacity Curve
  INTEGER           :: lFuelHeatFHPLRCurve ! Fuel Input to heat output ratio during heating only function
  REAL(r64)         :: lFuelHeatingValue
            ! Local copies of GasAbsorberReportVars Type
  REAL(r64)    :: lHeatingLoad        ! heating load on the chiller
  REAL(r64)    :: lHeatingEnergy      ! heating energy
  REAL(r64)    :: lFuelUseRate        ! instantaneous use of gas for period
  REAL(r64)    :: lFuelEnergy         ! variable to track total fuel used for a period
  REAL(r64)    :: lCoolFuelUseRate    ! instantaneous use of gas for period for cooling
  REAL(r64)    :: lHeatFuelUseRate    ! instantaneous use of gas for period for heating
  REAL(r64)    :: lHeatFuelEnergy     ! variable to track total fuel used for a period for heating
  REAL(r64)    :: lElectricPower      ! parasitic electric power used (was PumpingPower)
  REAL(r64)    :: lElectricEnergy     ! track the total electricity used for a period (was PumpingEnergy)
  REAL(r64)    :: lCoolElectricPower  ! parasitic electric power used  for cooling
  REAL(r64)    :: lHeatElectricPower  ! parasitic electric power used  for heating
  REAL(r64)    :: lHeatElectricEnergy ! track the total electricity used for a period for heating
  REAL(r64)    :: lHotWaterReturnTemp ! reporting: hot water return (inlet) temperature
  REAL(r64)    :: lHotWaterSupplyTemp ! reporting: hot water supply (outlet) temperature
  REAL(r64)    :: lHotWaterMassFlowRate   ! reporting: hot water mass flow rate
  REAL(r64)    :: lCoolPartLoadRatio      ! operating part load ratio (load/capacity for cooling)
  REAL(r64)    :: lHeatPartLoadRatio      ! operating part load ratio (load/capacity for heating)
  REAL(r64)    :: lAvailableHeatingCapacity    ! current heating capacity
  REAL(r64)    :: lFractionOfPeriodRunning
  Real(r64)    :: lHotWaterMassFlowRateMax  ! Maximum flow rate through the evaporator
  ! other local variables
  REAL(r64)              :: HeatDeltaTemp       ! hot water temperature difference
  REAL(r64)              :: HeatSupplySetPointTemp
  INTEGER      :: LoopNum
  INTEGER      :: LoopSideNum
  REAL(r64)    :: Cp_HW  ! local fluid specific heat for hot water
  REAL(r64)    :: rhoHW  ! local fluid density for hot water

!  INTEGER, SAVE          :: ErrCount            ! error counter

  !initialize all output variables to zero

  lHeatingLoad        = 0.0d0
  lHeatingEnergy      = 0.0d0
  lFuelUseRate        = 0.0d0
  lFuelEnergy         = 0.0d0
  lCoolFuelUseRate    = 0.0d0
  lHeatFuelUseRate    = 0.0d0
  lHeatFuelEnergy     = 0.0d0
  lElectricPower      = 0.0d0
  lElectricEnergy     = 0.0d0
  lCoolElectricPower  = 0.0d0
  lHeatElectricPower  = 0.0d0
  lHeatElectricEnergy = 0.0d0
  lHotWaterReturnTemp = 0.0d0
  lHotWaterSupplyTemp = 0.0d0
  lHotWaterMassFlowRate   = 0.0d0
  lCoolPartLoadRatio      = 0.0d0
  lHeatPartLoadRatio      = 0.0d0
  lAvailableHeatingCapacity    = 0.0d0
  lFractionOfPeriodRunning = 0.0d0

  ! set node values to data structure values for nodes

  lHeatReturnNodeNum  = GasAbsorber(ChillNum)%HeatReturnNodeNum
  lHeatSupplyNodeNum  = GasAbsorber(ChillNum)%HeatSupplyNodeNum

  ! set local copies of data from rest of input structure

  lNomCoolingCap      = GasAbsorber(ChillNum)%NomCoolingCap
  lNomHeatCoolRatio   = GasAbsorber(ChillNum)%NomHeatCoolRatio
  lFuelHeatRatio      = GasAbsorber(ChillNum)%FuelHeatRatio
  lElecHeatRatio      = GasAbsorber(ChillNum)%ElecHeatRatio
  lMinPartLoadRat     = GasAbsorber(ChillNum)%MinPartLoadRat
  lMaxPartLoadRat     = GasAbsorber(ChillNum)%MaxPartLoadRat
  lOptPartLoadRat     = GasAbsorber(ChillNum)%OptPartLoadRat
  lHeatCapFCoolCurve  = GasAbsorber(ChillNum)%HeatCapFCoolCurve
  lFuelHeatFHPLRCurve = GasAbsorber(ChillNum)%FuelHeatFHPLRCurve
  lFuelHeatingValue   = GasAbsorber(ChillNum)%FuelHeatingValue
  lHotWaterMassFlowRateMax = Gasabsorber(chillnum)%DesHeatMassFlowRate
  LoopNum             = GasAbsorber(ChillNum)%HWLoopNum
  LoopSideNum         = GasAbsorber(ChillNum)%HWLoopSideNum

  Cp_HW               = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, &
                                               lHotWaterReturnTemp, &
                                               PlantLoop(LoopNum)%FluidIndex, &
                                               'CalcGasAbsorberHeaterModel')
  rhoHW               = GetDensityGlycol(PlantLoop(LoopNum)%FluidName, &
                                               lHotWaterReturnTemp, &
                                               PlantLoop(LoopNum)%FluidIndex, &
                                               'CalcGasAbsorberHeaterModel')

  lCoolElectricPower  = GasAbsorberReport(ChillNum)%CoolElectricPower
  lCoolFuelUseRate    = GasAbsorberReport(ChillNum)%CoolFuelUseRate
  lCoolPartLoadRatio  = GasAbsorberReport(ChillNum)%CoolPartLoadRatio

! initialize entering conditions
  lHotWaterReturnTemp  = Node(lHeatReturnNodeNum)%Temp
  lHotWaterMassFlowRate  = Node(lHeatReturnNodeNum)%MassFlowRate
  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    HeatSupplySetPointTemp = Node(lHeatSupplyNodeNum)%TempSetPoint
  CASE (DualSetpointDeadband)
    HeatSupplySetPointTemp = Node(lHeatSupplyNodeNum)%TempSetPointLo
  END SELECT
  HeatDeltaTemp  = ABS(lHotWaterReturnTemp - HeatSupplySetPointTemp)

          !If no loop demand or Absorber OFF, return
          ! will need to modify when absorber can act as a boiler
 IF (MyLoad<=0 .OR. .NOT. Runflag) THEN
          !set node temperatures
    lHotWaterSupplyTemp = lHotWaterReturnTemp
    HeatDeltaTemp = 0.0d0
    lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)
 ELSE

            !Determine available heating capacity using the current cooling load
         lAvailableHeatingCapacity = GasAbsorber(ChillNum)%NomHeatCoolRatio * &
           GasAbsorber(ChillNum)%NomCoolingCap * CurveValue(lHeatCapFCoolCurve, &
           (GasAbsorberReport(ChillNum)%CoolingLoad / GasAbsorber(ChillNum)%NomCoolingCap))

            !Calculate current load for heating
  MyLoad = SIGN(MAX(ABS(MyLoad), GasAbsorberReport(ChillNum)%HeatingCapacity * lMinPartLoadRat), MyLoad)
  MyLoad = SIGN(MIN(ABS(MyLoad), GasAbsorberReport(ChillNum)%HeatingCapacity * lMaxPartLoadRat), MyLoad)

            ! Determine the following variables depending on if the flow has been set in
            ! the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
            !    chilled water flow,
            !    cooling load taken by the chiller, and
            !    supply temperature
  SELECT CASE (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock)
    CASE (0) ! mass flow rates may be changed by loop components
        lHeatingLoad = ABS(MyLoad)
      IF (HeatDeltaTemp /= 0) THEN
        lHotWaterMassFlowRate = ABS(lHeatingLoad / (Cp_HW * HeatDeltaTemp))

        CALL SetComponentFlowRate(lHotWaterMassFlowRate, &
                              GasAbsorber(ChillNum)%HeatReturnNodeNum, &
                              GasAbsorber(ChillNum)%HeatSupplyNodeNum, &
                              GasAbsorber(ChillNum)%HWLoopNum, &
                              GasAbsorber(ChillNum)%HWLoopSideNum, &
                              GasAbsorber(ChillNum)%HWBranchNum, &
                              GasAbsorber(ChillNum)%HWCompNum)

      ELSE
        lHotWaterMassFlowRate = 0.0d0
        CALL ShowRecurringWarningErrorAtEnd('GasAbsorberChillerModel:Heating"'//TRIM(GasAbsorber(ChillNum)%Name)//  &
             '", DeltaTemp = 0 in mass flow calculation',GasAbsorber(ChillNum)%DeltaTempHeatErrCount)
      END IF
      lHotWaterSupplyTemp = HeatSupplySetPointTemp
    CASE (1) ! mass flow rates may not be changed by loop components
      lHotWaterSupplyTemp = HeatSupplySetPointTemp
      lHeatingLoad = ABS(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp)


!DSU this "2" is not a real state for flowLock
    CASE (2) ! chiller is underloaded and mass flow rates has changed to a small amount and Tout drops below Setpoint

!DSU? this component model needs a lot of work, does not honor limits, incomplete ...

! MJW Not sure what to do with this now
      ! Must make adjustment to supply temperature since load is greater than available capacity
      ! this also affects the available capacity itself since it is a function of supply temperature
      ! Since these curves are generally fairly flat just use an estimate (done above) and correction
      ! approach instead of iterating to a solution.
! MJW 07MAR01 Logic seems wrong here, because of misunderstanding of what "overload" means
!  "overload" means the chiller is overcooling the branch.  See SUBROUTINE DistributeLoad
!      IF (lChillWaterMassFlowRate > MassFlowTol) THEN
!        ChillDeltaTemp = MyLoad / (CPCW(lChillReturnTemp) * lChillWaterMassFlowRate)
!        lChillSupplyTemp = lChillReturnTemp - ChillDeltaTemp
!        lAvailableCoolingCapacity = lNomCoolingCap * CurveValue(lCoolCapFTCurve,lChillSupplyTemp,calcCondTemp)
!      ELSE
!        ErrCount = ErrCount + 1
!        IF (ErrCount < 10) THEN
!          CALL ShowWarningError('GasAbsorberModel:lChillWaterMassFlowRate near 0 in available capacity calculation')
!        END IF
!      END IF

! MJW 07MAR01 Borrow logic from steam absorption module
            ! The following conditional statements are made to avoid extremely small EvapMdot
            ! & unreasonable EvapOutletTemp due to overloading.
                ! Avoid 'divide by zero' due to small EvapMdot
      IF(lHotWaterMassFlowRate < MassFlowTolerance) THEN
        HeatDeltaTemp = 0.0d0
      ELSE
        HeatDeltaTemp = ABS(MyLoad) / (Cp_HW * lHotWaterMassFlowRate)
      END IF
      lHotWaterSupplyTemp = lHotWaterReturnTemp + HeatDeltaTemp

      lHeatingLoad = ABS(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp)
  END SELECT

            !Calculate operating part load ratio for cooling
  lHeatPartLoadRatio = lHeatingLoad / lAvailableHeatingCapacity

            !Calculate fuel consumption for cooling
            ! fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR

  lHeatFuelUseRate = lAvailableHeatingCapacity * lFuelHeatRatio  &
     * CurveValue(lFuelHeatFHPLRCurve,lHeatPartLoadRatio)

            ! calculate the fraction of the time period that the chiller would be running
            ! use maximum from heating and cooling sides
  lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)

            !Calculate electric parasitics used
            ! for heating based on nominal capacity not available capacity
  lHeatElectricPower = lNomCoolingCap * lNomHeatCoolRatio * lElecHeatRatio * lFractionOfPeriodRunning
            ! Coodinate electric parasitics for heating and cooling to avoid double counting
            ! Total electric is the max of heating electric or cooling electric
            ! If heating electric is greater, leave cooling electric and subtract if off of heating elec
            ! If cooling electric is greater, set heating electric to zero
  IF (lHeatElectricPower .LE. lCoolElectricPower) THEN
    lHeatElectricPower = 0.0d0
  ELSE
    lHeatElectricPower = lHeatElectricPower - lCoolElectricPower
  ENDIF


 ENDIF ! IF(MyLoad==0 .OR. .NOT. Runflag)
  ! Write into the Report Variables except for nodes
  GasAbsorberReport(ChillNum)%HeatingLoad             = lHeatingLoad
  GasAbsorberReport(ChillNum)%HeatFuelUseRate         = lHeatFuelUseRate
  GasAbsorberReport(ChillNum)%HeatElectricPower       = lHeatElectricPower
  GasAbsorberReport(ChillNum)%HotWaterReturnTemp      = lHotWaterReturnTemp
  GasAbsorberReport(ChillNum)%HotWaterSupplyTemp      = lHotWaterSupplyTemp
  GasAbsorberReport(ChillNum)%HotWaterFlowRate        = lHotWaterMassFlowRate
  GasAbsorberReport(ChillNum)%HeatPartLoadRatio       = lHeatPartLoadRatio
  GasAbsorberReport(ChillNum)%HeatingCapacity         = lAvailableHeatingCapacity
  GasAbsorberReport(ChillNum)%FractionOfPeriodRunning = lFractionOfPeriodRunning

  ! write the combined heating and cooling fuel used and electric used
  GasAbsorberReport(ChillNum)%FuelUseRate             = lCoolFuelUseRate + lHeatFuelUseRate
  GasAbsorberReport(ChillNum)%ElectricPower           = lCoolElectricPower + lHeatElectricPower

END SUBROUTINE CalcGasAbsorberHeaterModel

! End of Absorption Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************

SUBROUTINE UpdateGasAbsorberCoolRecords(MyLoad,RunFlag,ChillNum)
            ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2001

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataHVACGlobals, ONLY : TimeStepSys


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)          :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Absorber operating
  INTEGER, INTENT(IN)      :: ChillNum  ! Absorber number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: lChillReturnNodeNum ! Node number on the inlet side of the plant
  INTEGER           :: lChillSupplyNodeNum ! Node number on the outlet side of the plant
  INTEGER           :: lCondReturnNodeNum  ! Node number on the inlet side of the condenser
  INTEGER           :: lCondSupplyNodeNum  ! Node number on the outlet side of the condenser

           ! BEGIN ROUTINE

  lChillReturnNodeNum = GasAbsorber(ChillNum)%ChillReturnNodeNum
  lChillSupplyNodeNum = GasAbsorber(ChillNum)%ChillSupplyNodeNum
  lCondReturnNodeNum  = GasAbsorber(ChillNum)%CondReturnNodeNum
  lCondSupplyNodeNum  = GasAbsorber(ChillNum)%CondSupplyNodeNum


  IF (MyLoad==0 .OR. .NOT. RunFlag)THEN
          !set node temperatures


    Node(lChillSupplyNodeNum)%Temp     = Node(lChillReturnNodeNum)%Temp
    Node(lCondSupplyNodeNum)%Temp      = Node(lCondReturnNodeNum)%Temp

          !set node flow rates
          !Update Outlet Conditions so that same as Inlet, so component
          !can be bypassed if necessary
          !FlowResolver/EnforceSplitterContinuity will determine flow
          !received, whether component is running or not.
!
!    Node(lChillReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lChillSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lCondReturnNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
!    Node(lCondSupplyNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
  ELSE
          !set node temperatures
    Node(lChillSupplyNodeNum)%Temp                  = GasAbsorberReport(ChillNum)%ChillSupplyTemp
    Node(lCondSupplyNodeNum)%Temp                   = GasAbsorberReport(ChillNum)%CondSupplyTemp
          !set node flow rates;  for these load based models
          !assume that the sufficient evaporator flow rate available
!    Node(lChillReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lChillSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lCondReturnNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
!    Node(lCondSupplyNodeNum)%MassFlowRate           = GasAbsorberReport(ChillNum)%CondWaterFlowRate
  END IF

  ! convert power to energy and instantaneous use to use over the time step
  GasAbsorberReport(ChillNum)%CoolingEnergy      = GasAbsorberReport(ChillNum)%CoolingLoad * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%TowerEnergy        = GasAbsorberReport(ChillNum)%TowerLoad * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%FuelEnergy         = GasAbsorberReport(ChillNum)%FuelUseRate * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%CoolFuelEnergy     = GasAbsorberReport(ChillNum)%CoolFuelUseRate * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%ElectricEnergy     = GasAbsorberReport(ChillNum)%ElectricPower * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%CoolElectricEnergy = GasAbsorberReport(ChillNum)%CoolElectricPower * TimeStepSys * SecInHour
  IF (GasAbsorberReport(ChillNum)%CoolFuelUseRate .NE. 0.0d0) THEN
    GasAbsorberReport(ChillNum)%FuelCOP          = &
         GasAbsorberReport(ChillNum)%CoolingLoad/GasAbsorberReport(ChillNum)%CoolFuelUseRate
  ELSE
    GasAbsorberReport(ChillNum)%FuelCOP          = 0.0d0
  END IF
!  Node(lChillSupplyNodeNum)%MassFlowRateMaxAvail = Node(lChillReturnNodeNum)%MassFlowRateMaxAvail
!  Node(lChillSupplyNodeNum)%MassFlowRateMinAvail = Node(lChillReturnNodeNum)%MassFlowRateMinAvail
RETURN
END SUBROUTINE UpdateGasAbsorberCoolRecords

SUBROUTINE UpdateGasAbsorberHeatRecords(MyLoad,RunFlag,ChillNum)
            ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2001

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataHVACGlobals, ONLY : TimeStepSys


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)          :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Absorber operating
  INTEGER, INTENT(IN)      :: ChillNum  ! Absorber number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: lHeatReturnNodeNum  ! absorber steam inlet node number, water side
  INTEGER           :: lHeatSupplyNodeNum  ! absorber steam outlet node number, water side

           ! BEGIN ROUTINE

  lHeatReturnNodeNum  = GasAbsorber(ChillNum)%HeatReturnNodeNum
  lHeatSupplyNodeNum  = GasAbsorber(ChillNum)%HeatSupplyNodeNum


  IF (MyLoad==0 .OR. .NOT. RunFlag)THEN
          !set node temperatures
    Node(lHeatSupplyNodeNum)%Temp     = Node(lHeatReturnNodeNum)%Temp

          !set node flow rates
          !Update Outlet Conditions so that same as Inlet, so component
          !can be bypassed if necessary
          !FlowResolver/EnforceSplitterContinuity will determine flow
          !received, whether component is running or not.
!
!    Node(lHeatReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
!    Node(lHeatSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
  ELSE
          !set node temperatures
    Node(lHeatSupplyNodeNum)%Temp                  = GasAbsorberReport(ChillNum)%HotWaterSupplyTemp
!          !set node flow rates;  for these load based models
!          !assume that the sufficient evaporator flow rate available
!    Node(lHeatReturnNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
!    Node(lHeatSupplyNodeNum)%MassFlowRate          = GasAbsorberReport(ChillNum)%HotWaterFlowRate
  END IF

  ! convert power to energy and instantaneous use to use over the time step
  GasAbsorberReport(ChillNum)%HeatingEnergy      = GasAbsorberReport(ChillNum)%HeatingLoad * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%FuelEnergy         = GasAbsorberReport(ChillNum)%FuelUseRate * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%HeatFuelEnergy     = GasAbsorberReport(ChillNum)%HeatFuelUseRate * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%ElectricEnergy     = GasAbsorberReport(ChillNum)%ElectricPower * TimeStepSys * SecInHour
  GasAbsorberReport(ChillNum)%HeatElectricEnergy = GasAbsorberReport(ChillNum)%HeatElectricPower * TimeStepSys * SecInHour
!
!  Node(lHeatSupplyNodeNum)%MassFlowRateMaxAvail = Node(lHeatReturnNodeNum)%MassFlowRateMaxAvail
!  Node(lHeatSupplyNodeNum)%MassFlowRateMinAvail = Node(lHeatReturnNodeNum)%MassFlowRateMinAvail
RETURN
END SUBROUTINE UpdateGasAbsorberHeatRecords

! End of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************

!                                 COPYRIGHT NOTICE
!
!     Portions Copyright © Gas Research Institute 2001.  All rights reserved.
!
!     GRI LEGAL NOTICE
!     Neither GRI, members of GRI nor any person or organization acting on behalf
!     of either:
!
!     A. Makes any warranty of representation, express or implied with respect to
!        the accuracy, completness, or usefulness of the information contained in
!        in this program, including any warranty of merchantability or fitness of
!        any purpose with respoect to the program, or that the use of any
!        information disclosed in this program may not infringe privately-owned
!        rights, or
!
!     B.  Assumes any liability with respoct to the use of, or for any and all
!         damages resulting from the use of the program or any portion thereof or
!         any information disclosed therein.
!
!
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
END MODULE ChillerGasAbsorption

