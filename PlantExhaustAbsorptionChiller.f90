MODULE ChillerExhaustAbsorption  !Based on ChillerAbsorption

! MODULE INFORMATION:
!    AUTHOR         Jason Glazer of GARD Analytics, Inc.
!                   for Gas Research Institute (Original module GasAbsoptionChiller)
!    DATE WRITTEN   March 2001
!    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
!                   Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Absorption Chiller
!    RE-ENGINEERED  na
!
! PURPOSE OF THIS MODULE:
!    This module simulates the performance of the Exhaust fired double effect
!    absorption chiller.
!
! METHODOLOGY EMPLOYED:
!    Once the PlantLoopManager determines that the exhasut fired absorber chiller
!    is available to meet a loop cooling demand, it calls SimExhaustAbsorption
!    which in turn calls the appropriate Exhaut Fired Absorption Chiller model.
!
! REFERENCES:
!    DOE-2.1e Supplement
!    PG&E CoolToolsGas Mod
!    Performnace curves obtained from manufcaturer
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
!    Development of the original(GasAbsoptionChiller) module was funded by the Gas Research Institute.
!    (Please see copyright and disclaimer information at end of module)


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataInterfaces
USE DataGlobals ,   ONLY : MaxNameLength, BigNumber, InitConvTemp, SecInHour
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE General,         ONLY: TrimSigDigits
USE Psychrometrics,  ONLY: RhoH2O,CPCW,CPHW
USE DataGlobalConstants, ONLY: iGeneratorMicroturbine
USE MicroturbineElectricGenerator, ONLY: SimMTGenerator

IMPLICIT NONE

          !MODULE PARAMETER DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
PRIVATE
INTEGER, SAVE            :: NumExhaustAbsorbers =0 ! number of Absorption Chillers specified in input

TYPE ExhaustAbsorberSpecs
! Parts of Type that do not correspond with IDD definition
       LOGICAL           :: Available          =.false. ! need an array of logicals--load identifiers of available equipment
       LOGICAL           :: ON                 =.false. ! simulate the machine at it's operating part load ratio
       LOGICAL           :: InCoolingMode      = .FALSE.
       LOGICAL           :: InHeatingMode      = .FALSE.
! Part of Type that directly corresponds with IDD definition
       CHARACTER(len=MaxNameLength) :: Name    =' ' ! user identifier
       REAL(r64)         :: NomCoolingCap      =0.0d0 ! W - design nominal capacity of Absorber
       REAL(r64)         :: NomHeatCoolRatio   =0.0d0 ! ratio of heating to cooling capacity
       REAL(r64)         :: ThermalEnergyCoolRatio      =0.0d0 ! ratio of thermal energy input to cooling output
       REAL(r64)         :: ThermalEnergyHeatRatio      =0.0d0 ! ratio of thermal energy input to heating output
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
       INTEGER           :: ThermalEnergyCoolFTCurve    =0   ! Thermal Energy-Input-to cooling output Ratio Function of Temperature Curve (chilled
                                                    ! water temp, condenser water temp)
       INTEGER           :: ThermalEnergyCoolFPLRCurve  =0   ! Thermal Energy-Input-to cooling output Ratio Function of Part Load Ratio Curve
       INTEGER           :: ElecCoolFTCurve    =0   ! Electric-Input-to cooling output Ratio Function of Temperature Curve
                                                    ! (chilled water temp, condenser water temp)
       INTEGER           :: ElecCoolFPLRCurve  =0   ! Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
       INTEGER           :: HeatCapFCoolCurve  =0   ! Heating Capacity Function of Cooling Capacity Curve
       INTEGER           :: ThermalEnergyHeatFHPLRCurve =0   ! Thermal Energy Input to heat output ratio during heating only function
       LOGICAL           :: isEnterCondensTemp =.false. ! if using entering conderser water temperature is TRUE, exiting is FALSE
       LOGICAL           :: isWaterCooled      =.false. ! if water cooled it is TRUE
       REAL(r64)         :: CHWLowLimitTemp    =0.0d0 ! Chilled Water Lower Limit Temperature
       INTEGER           :: ExhaustAirInletNodeNum   ! Node number on Exhaust input from generator
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
       INTEGER           :: CompType_Num       = 0  ! Numeric designator for CompType (TypeOf)
       INTEGER           :: ExhTempLTAbsLeavingTempIndex      = 0 ! index for exhaust potentail less than thermal energy needed during cooling
       INTEGER           :: ExhTempLTAbsLeavingHeatingTempIndex = 0 ! index for exhaust potentail less than thermal energy needed during heating
       CHARACTER(len=MaxNameLength) :: TypeOf             = ' ' ! Generator type
       CHARACTER(len=MaxNameLength)  :: ExhuastSourceName   = ' ' ! Generator type Name
END TYPE ExhaustAbsorberSpecs

! This type holds the output from the algorithm i.e., the Report Variables
TYPE ReportVars
  REAL(r64)    :: CoolingLoad        =0.0d0 ! cooling load on the chiller (previously called QEvap)
  REAL(r64)    :: CoolingEnergy      =0.0d0 ! variable to track total cooling load for period (was EvapEnergy)
  REAL(r64)    :: HeatingLoad        =0.0d0 ! heating load on the chiller
  REAL(r64)    :: HeatingEnergy      =0.0d0 ! heating energy
  REAL(r64)    :: TowerLoad          =0.0d0 ! load on the cooling tower/condenser (previously called QCond)
  REAL(r64)    :: TowerEnergy        =0.0d0 ! variable to track total tower load for a period (was CondEnergy)
  REAL(r64)    :: ThermalEnergyUseRate        =0.0d0 ! instantaneous use of Exhaust for period
  REAL(r64)    :: ThermalEnergy         =0.0d0 ! variable to track total ThermalEnergy used for a period
  REAL(r64)    :: CoolThermalEnergyUseRate    =0.0d0 ! instantaneous use of Exhaust for period for cooling
  REAL(r64)    :: CoolThermalEnergy     =0.0d0 ! variable to track total ThermalEnergy used for a period for cooling
  REAL(r64)    :: HeatThermalEnergyUseRate    =0.0d0 ! instantaneous use of Exhaust for period for heating
  REAL(r64)    :: HeatThermalEnergy     =0.0d0 ! variable to track total ThermalEnergy used for a period for heating
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
  REAL(r64)    :: ThermalEnergyCOP            =0.0d0 ! reporting: cooling output/ThermalEnergy input = CoolingLoad/CoolThermalEnergyUseRate
  REAL(r64)    :: ExhaustInTemp      =0.0d0 ! reporting: Exhaust inlet temperature
  REAL(r64)    :: ExhaustInFlow      =0.0d0 ! reporting: Exhaust Inlet Flow rate
  REAL(r64)    :: ExhHeatRecPotentialHeat   =0.0d0 ! reporting: Heat Recovery Potential during heating
  REAL(r64)    :: ExhHeatRecPotentialCool   =0.0d0 ! reporting: Heat Recovery Potential during cooling
END TYPE ReportVars

TYPE (ExhaustAbsorberSpecs), ALLOCATABLE, DIMENSION(:)  :: ExhaustAbsorber  !dimension to number of machines
TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::ExhaustAbsorberReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcExhaustAbsorberChillerModel
PRIVATE    CalcExhaustAbsorberHeaterModel
PRIVATE    GetExhaustAbsorberInput
PRIVATE    InitExhaustAbsorber
PRIVATE    SizeExhaustAbsorber
PRIVATE    UpdateExhaustAbsorberCoolRecords
PRIVATE    UpdateExhaustAbsorberHeatRecords
PUBLIC     SimExhaustAbsorber


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Absorption Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimExhaustAbsorber(AbsorberType,AbsorberName,EquipFlowCtrl, CompIndex,RunFlag,FirstIteration,InitLoopEquip,&
                          MyLoad,BranchInletNodeNum,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
! SUBROUTINE INFORMATION:
!       AUTHOR         Jason Glazer
!       DATE WRITTEN   March 2001
!       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Double Effect Absorption Chiller
!       RE-ENGINEERED  na

! PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
! gets the input for the models, initializes simulation variables, call
! the appropriate model and sets up reporting variables.

! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : TypeOf_Chiller_ExhFiredAbsorption
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
  REAL(r64), INTENT(OUT) :: SizingFactor        ! sizing factor
         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)         :: HeatCap = 0.0d0       ! W - nominal heating capacity
  LOGICAL, SAVE     :: GetInput = .TRUE.   ! then TRUE, calls subroutine to read input file.
!unused  INTEGER           :: CondReturnNodeNum !holds the node number for the condenser side return
!unused  REAL(r64)         :: CondMassFlowRate !the rate of mass flow for the condenser (estimated)
  INTEGER :: ChillNum            ! Absorber number counter

          !Get Absorber data from input file
  IF (GetInput) THEN
    CALL GetExhaustAbsorberInput
    GetInput = .FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(AbsorberName,ExhaustAbsorber%Name,NumExhaustAbsorbers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimExhaustAbsorber: Unit not found='//TRIM(AbsorberName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumExhaustAbsorbers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimExhaustAbsorber:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumExhaustAbsorbers))//  &
                          ', Entered Unit name='//TRIM(AbsorberName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (AbsorberName /= ExhaustAbsorber(ChillNum)%Name) THEN
        CALL ShowFatalError('SimExhaustAbsorber: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(AbsorberName)//', stored Unit Name for that index='//  &
                            TRIM(ExhaustAbsorber(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF

          ! Check that this is a valid call
  IF (InitLoopEquip) THEN
    CALL InitExhaustAbsorber(ChillNum,RunFlag)
    CALL SizeExhaustAbsorber(ChillNum)
    ! Match inlet node name of calling branch to determine if this call is for heating or cooling
    IF (BranchInletNodeNum == ExhaustAbsorber(ChillNum)%ChillReturnNodeNum) THEN ! Operate as chiller
      MinCap = ExhaustAbsorber(ChillNum)%NomCoolingCap*ExhaustAbsorber(ChillNum)%MinPartLoadRat
      MaxCap = ExhaustAbsorber(ChillNum)%NomCoolingCap*ExhaustAbsorber(ChillNum)%MaxPartLoadRat
      OptCap = ExhaustAbsorber(ChillNum)%NomCoolingCap*ExhaustAbsorber(ChillNum)%OptPartLoadRat
    ELSEIF (BranchInletNodeNum == ExhaustAbsorber(ChillNum)%HeatReturnNodeNum) THEN ! Operate as heater
      HeatCap = ExhaustAbsorber(ChillNum)%NomCoolingCap*ExhaustAbsorber(ChillNum)%NomHeatCoolRatio
      MinCap  = HeatCap*ExhaustAbsorber(ChillNum)%MinPartLoadRat
      MaxCap  = HeatCap*ExhaustAbsorber(ChillNum)%MaxPartLoadRat
      OptCap  = HeatCap*ExhaustAbsorber(ChillNum)%OptPartLoadRat
    ELSEIF (BranchInletNodeNum == ExhaustAbsorber(ChillNum)%CondReturnNodeNum) THEN ! called from condenser loop
      HeatCap = 0.d0
      MinCap  = 0.d0
      MaxCap  = 0.d0
      OptCap  = 0.d0
    ELSE  ! Error, nodes do not match
      CALL ShowSevereError('SimExhaustAbsorber: Invalid call to Exhaust Absorbtion Chiller-Heater '//TRIM(AbsorberName))
      CALL ShowContinueError('Node connections in branch are not consistent with object nodes.')
      CALL ShowFatalError('Preceding conditions cause termination.')
    ENDIF ! Operate as Chiller or Heater
    IF (GetSizingFactor) THEN
      SizingFactor = ExhaustAbsorber(ChillNum)%SizFac
    END IF
    RETURN
  END IF

! Match inlet node name of calling branch to determine if this call is for heating or cooling
  IF (BranchInletNodeNum .EQ. ExhaustAbsorber(ChillNum)%ChillReturnNodeNum) THEN ! Operate as chiller
            ! Calculate Node Values
            ! Calculate Equipment and Update Variables
    IF (RunFlag) THEN
      ExhaustAbsorber(ChillNum)%InCoolingMode = .TRUE.
    ELSE
     ExhaustAbsorber(ChillNum)%InCoolingMode = .FALSE.
    ENDIF
    CALL InitExhaustAbsorber(ChillNum,RunFlag)
    CALL CalcExhaustAbsorberChillerModel(ChillNum,MyLoad,Runflag)
    CALL UpdateExhaustAbsorberCoolRecords(MyLoad,RunFlag,ChillNum)
  ELSEIF (BranchInletNodeNum .EQ. ExhaustAbsorber(ChillNum)%HeatReturnNodeNum) THEN ! Operate as heater
            ! Calculate Node Values
            ! Calculate Equipment and Update Variables
    IF (RunFlag) THEN
      ExhaustAbsorber(ChillNum)%InHeatingMode = .TRUE.
    ELSE
     ExhaustAbsorber(ChillNum)%InHeatingMode = .FALSE.
    ENDIF
    CALL InitExhaustAbsorber(ChillNum,RunFlag)
    CALL CalcExhaustAbsorberHeaterModel(ChillNum,MyLoad,Runflag)
    CALL UpdateExhaustAbsorberHeatRecords(MyLoad,RunFlag,ChillNum)
  ELSEIF (BranchInletNodeNum == ExhaustAbsorber(ChillNum)%CondReturnNodeNum) THEN ! called from condenser loop
      CALL UpdateChillerComponentCondenserSide(ExhaustAbsorber(ChillNum)%CDLoopNum, &
                                     ExhaustAbsorber(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_ExhFiredAbsorption,                     &
                                     ExhaustAbsorber(ChillNum)%CondReturnNodeNum, &
                                     ExhaustAbsorber(ChillNum)%CondSupplyNodeNum,  &
                                     ExhaustAbsorberReport(ChillNum)%TowerLoad,             &
                                     ExhaustAbsorberReport(ChillNum)%CondReturnTemp,    &
                                     ExhaustAbsorberReport(ChillNum)%CondSupplyTemp,     &
                                    ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate,          &
                                     FirstIteration)

  ELSE  ! Error, nodes do not match
    CALL ShowSevereError('Invalid call to Exhaust Absorber Chiller '//TRIM(AbsorberName))
    CALL ShowContinueError('Node connections in branch are not consistent with object nodes.')
    CALL ShowFatalError('Preceding conditions cause termination.')
  ENDIF

RETURN
END SUBROUTINE SimExhaustAbsorber

! End Absorption Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Absorption Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetExhaustAbsorberInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Jason Glazer
            !       DATE WRITTEN:    March 2001
            !       MODIFIED         Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate Exhaust Fired Double Effect Absorption Chiller
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Exhaust Fired Absorption chiller model in the object ChillerHeater:Absorption:DoubleEffect

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
  USE MicroturbineElectricGenerator, ONLY: GetMTGeneratorExhaustNode
  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: AbsorberNum !Absorber counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  INTEGER                     :: MTExhaustNodeNum  ! Exhaust node number passed from MicroTurbine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  CHARACTER(len=MaxNameLength)               :: ChillerName
  LOGICAL :: errflag
  LOGICAL :: Okay

         !FLOW
  cCurrentModuleObject = 'ChillerHeater:Absorption:DoubleEffect'
  NumExhaustAbsorbers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumExhaustAbsorbers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment found in input file')
    ErrorsFound=.true.
  ENDIF

  IF (ALLOCATED(ExhaustAbsorber)) RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (ExhaustAbsorber(NumExhaustAbsorbers))

  ALLOCATE (ExhaustAbsorberReport(NumExhaustAbsorbers))
  ALLOCATE(CheckEquipName(NumExhaustAbsorbers))
  CheckEquipName=.true.

         !LOAD ARRAYS

  DO AbsorberNum = 1 , NumExhaustAbsorbers
    CALL GetObjectItem(cCurrentModuleObject,AbsorberNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ExhaustAbsorber%Name,AbsorberNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    ExhaustAbsorber(AbsorberNum)%Name                = cAlphaArgs(1)
    ChillerName = TRIM(cCurrentModuleObject)//' Named ' // TRIM(ExhaustAbsorber(AbsorberNum)%Name)

            ! Assign capacities
    ExhaustAbsorber(AbsorberNum)%NomCoolingCap       = rNumericArgs(1)
    ExhaustAbsorber(AbsorberNum)%NomHeatCoolRatio    = rNumericArgs(2)
            ! Assign efficiencies
    ExhaustAbsorber(AbsorberNum)%ThermalEnergyCoolRatio       = rNumericArgs(3)
    ExhaustAbsorber(AbsorberNum)%ThermalEnergyHeatRatio       = rNumericArgs(4)
    ExhaustAbsorber(AbsorberNum)%ElecCoolRatio       = rNumericArgs(5)
    ExhaustAbsorber(AbsorberNum)%ElecHeatRatio       = rNumericArgs(6)

            ! Assign Node Numbers to specified nodes
    ExhaustAbsorber(AbsorberNum)%ChillReturnNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(2), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ExhaustAbsorber(AbsorberNum)%ChillSupplyNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(3), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')
            ! Condenser node processing depends on condenser type, see below
    ExhaustAbsorber(AbsorberNum)%HeatReturnNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(6), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 3, ObjectIsNotParent)
    ExhaustAbsorber(AbsorberNum)%HeatSupplyNodeNum  = &
        GetOnlySingleNode(cAlphaArgs(7), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 3, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Hot Water Nodes')
    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in processing node input for '// &
            TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .FALSE.
    END IF

            ! Assign Part Load Ratios
    ExhaustAbsorber(AbsorberNum)%MinPartLoadRat      = rNumericArgs(7)
    ExhaustAbsorber(AbsorberNum)%MaxPartLoadRat      = rNumericArgs(8)
    ExhaustAbsorber(AbsorberNum)%OptPartLoadRat      = rNumericArgs(9)
            ! Assign Design Conditions
    ExhaustAbsorber(AbsorberNum)%TempDesCondReturn   = rNumericArgs(10)
    ExhaustAbsorber(AbsorberNum)%TempDesCHWSupply    = rNumericArgs(11)
    ExhaustAbsorber(AbsorberNum)%EvapVolFlowRate     = rNumericArgs(12)
    IF (SameString(cAlphaArgs(16),'AirCooled') ) THEN
      ExhaustAbsorber(AbsorberNum)%CondVolFlowRate   = 0.0011d0 ! Condenser flow rate not used for this cond type
    ELSE
      ExhaustAbsorber(AbsorberNum)%CondVolFlowRate   = rNumericArgs(13)
    ENDIF
    ExhaustAbsorber(AbsorberNum)%HeatVolFlowRate     = rNumericArgs(14)
            ! Assign Curve Numbers
    ExhaustAbsorber(AbsorberNum)%CoolCapFTCurve      = GetCurveCheck(cAlphaArgs(8),  ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%ThermalEnergyCoolFTCurve     = GetCurveCheck(cAlphaArgs(9),  ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%ThermalEnergyCoolFPLRCurve   = GetCurveCheck(cAlphaArgs(10), ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%ElecCoolFTCurve     = GetCurveCheck(cAlphaArgs(11), ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%ElecCoolFPLRCurve   = GetCurveCheck(cAlphaArgs(12), ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%HeatCapFCoolCurve   = GetCurveCheck(cAlphaArgs(13), ErrorsFound, ChillerName)
    ExhaustAbsorber(AbsorberNum)%ThermalEnergyHeatFHPLRCurve  = GetCurveCheck(cAlphaArgs(14), ErrorsFound, ChillerName)
    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in processing curve input for '// &
            TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .FALSE.
    END IF
    IF (SameString(cAlphaArgs(15),'LeavingCondenser')) THEN
      ExhaustAbsorber(AbsorberNum)%isEnterCondensTemp = .FALSE.
    ELSEIF (SameString(cAlphaArgs(15),'EnteringCondenser')) THEN
      ExhaustAbsorber(AbsorberNum)%isEnterCondensTemp = .TRUE.
    ELSE
      ExhaustAbsorber(AbsorberNum)%isEnterCondensTemp = .TRUE.
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(15))//'='//TRIM(cAlphaArgs(15)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('resetting to ENTERING-CONDENSER, simulation continues')
    ENDIF
            ! Assign Other Paramters
    IF (SameString(cAlphaArgs(16),'AirCooled') ) THEN
      ExhaustAbsorber(AbsorberNum)%isWaterCooled     = .FALSE.
    ELSEIF (SameString(cAlphaArgs(16),'WaterCooled')) THEN
      ExhaustAbsorber(AbsorberNum)%isWaterCooled     = .TRUE.
    ELSE
      ExhaustAbsorber(AbsorberNum)%isWaterCooled     = .TRUE.
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(16))//'='//TRIM(cAlphaArgs(16)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('resetting to WATER-COOLED, simulation continues')
    ENDIF
    IF (ExhaustAbsorber(AbsorberNum)%isWaterCooled) THEN
      ExhaustAbsorber(AbsorberNum)%CondReturnNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(4), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
                 NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ExhaustAbsorber(AbsorberNum)%CondSupplyNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Water, &
                 NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser Water Nodes')
    ELSE
      ExhaustAbsorber(AbsorberNum)%CondReturnNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(4), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Air, &
                 NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      ExhaustAbsorber(AbsorberNum)%CondSupplyNodeNum  = &
          GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1),NodeType_Air, &
                 NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      ! Connection not required for air or evap cooled condenser so no call to TestCompSet here
      CALL CheckAndAddAirNodeNumber(ExhaustAbsorber(AbsorberNum)%CondReturnNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(4)))
      ENDIF
    ENDIF

    ExhaustAbsorber(AbsorberNum)%CHWLowLimitTemp     = rNumericArgs(15)
    ExhaustAbsorber(AbsorberNum)%SizFac              = rNumericArgs(16)
    ExhaustAbsorber(AbsorberNum)%TypeOf          = cAlphaArgs(17)

  IF (SameString(cAlphaArgs(17) , 'Generator:MicroTurbine') )  THEN
    ExhaustAbsorber(AbsorberNum)%CompType_Num  = iGeneratorMicroturbine
    ExhaustAbsorber(AbsorberNum)%ExhuastSourceName = cAlphaArgs(18)

   CALL GetMTGeneratorExhaustNode(ExhaustAbsorber(AbsorberNum)%CompType_Num,   &
                                  ExhaustAbsorber(AbsorberNum)%ExhuastSourceName, MTExhaustNodeNum)
   ExhaustAbsorber(AbsorberNum)%ExhaustAirInletNodeNum  = MTExhaustNodeNum
  ENDIF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO AbsorberNum = 1, NumExhaustAbsorbers
    ChillerName = ExhaustAbsorber(AbsorberNum)%Name

    CALL SetupOutputVariable('Chiller Heater Evaporator Cooling Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolingLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Cooling Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolingEnergy,'System','Sum',ChillerName,  &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater Heating Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatingLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatingEnergy,'System','Sum',ChillerName, &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BOILERS',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%TowerLoad  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%TowerEnergy,'System','Sum',ChillerName, &
                          ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')


     CALL SetupOutputVariable('Chiller Heater Cooling Source Heat COP [W/W]', &
         ExhaustAbsorberReport(AbsorberNum)%ThermalEnergyCOP ,'System','Average',ChillerName)


    CALL SetupOutputVariable('Chiller Heater Electric Power [W]', &
         ExhaustAbsorberReport(AbsorberNum)%ElectricPower  ,'System','Average',ChillerName)
         ! Do not include this on meters, this would duplicate the cool electric and heat electric
    CALL SetupOutputVariable('Chiller Heater Electric Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%ElectricEnergy,'System','Sum',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Cooling Electric Power [W]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolElectricPower  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Cooling Electric Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolElectricEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey='Electricity',GroupKey='Plant',EndUseKey='Cooling')

    CALL SetupOutputVariable('Chiller Heater Heating Electric Power [W]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatElectricPower  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Electric Energy [J]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatElectricEnergy,'System','Sum',ChillerName, &
         ResourceTypeKey='Electricity',GroupKey='Plant',EndUseKey='Heating')

    CALL SetupOutputVariable('Chiller Heater Evaporator Inlet Temperature [C]', &
         ExhaustAbsorberReport(AbsorberNum)%ChillReturnTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Outlet Temperature [C]', &
         ExhaustAbsorberReport(AbsorberNum)%ChillSupplyTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Evaporator Mass Flow Rate [kg/s]', &
         ExhaustAbsorberReport(AbsorberNum)%ChillWaterFlowRate  ,'System','Average',ChillerName)

    IF (ExhaustAbsorber(AbsorberNum)%isWaterCooled) THEN
      CALL SetupOutputVariable('Chiller Heater Condenser Inlet Temperature [C]', &
           ExhaustAbsorberReport(AbsorberNum)%CondReturnTemp  ,'System','Average',ChillerName)
      CALL SetupOutputVariable('Chiller Heater Condenser Outlet Temperature [C]', &
           ExhaustAbsorberReport(AbsorberNum)%CondSupplyTemp  ,'System','Average',ChillerName)
      CALL SetupOutputVariable('Chiller Heater Condenser Mass Flow Rate [kg/s]', &
           ExhaustAbsorberReport(AbsorberNum)%CondWaterFlowRate  ,'System','Average',ChillerName)
    ELSE
      CALL SetupOutputVariable('Chiller Heater Condenser Inlet Temperature [C]', &
           ExhaustAbsorberReport(AbsorberNum)%CondReturnTemp  ,'System','Average',ChillerName)
    ENDIF

    CALL SetupOutputVariable('Chiller Heater Heating Inlet Temperature [C]', &
         ExhaustAbsorberReport(AbsorberNum)%HotWaterReturnTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Outlet Temperature [C]', &
         ExhaustAbsorberReport(AbsorberNum)%HotWaterSupplyTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Mass Flow Rate [kg/s]', &
         ExhaustAbsorberReport(AbsorberNum)%HotWaterFlowRate  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Cooling Part Load Ratio []', &
         ExhaustAbsorberReport(AbsorberNum)%CoolPartLoadRatio  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Maximum Cooling Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolingCapacity  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Heating Part Load Ratio []', &
         ExhaustAbsorberReport(AbsorberNum)%HeatPartLoadRatio  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Maximum Heating Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatingCapacity  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Runtime Fraction []', &
         ExhaustAbsorberReport(AbsorberNum)%FractionOfPeriodRunning  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Source Exhaust Inlet Temperature [C]', &
         ExhaustAbsorberReport(AbsorberNum)%ExhaustInTemp  ,'System','Average',ChillerName)
    CALL SetupOutputVariable('Chiller Heater Source Exhaust Inlet Mass Flow Rate [kg/s]', &
         ExhaustAbsorberReport(AbsorberNum)%ExhaustInFlow  ,'System','Average',ChillerName)

    CALL SetupOutputVariable('Chiller Heater Heating Heat Recovery Potential Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%ExhHeatRecPotentialHeat  ,'System','Average',ChillerName)
   CALL SetupOutputVariable('Chiller Heater Cooling Heat Recovery Potential Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%ExhHeatRecPotentialCool  ,'System','Average',ChillerName)

   CALL SetupOutputVariable('Chiller Heater Cooling Source Heat Transfer Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%CoolThermalEnergyUseRate  ,'System','Average',ChillerName)
   CALL SetupOutputVariable('Chiller Heater Heating Source Heat Transfer Rate [W]', &
         ExhaustAbsorberReport(AbsorberNum)%HeatThermalEnergyUseRate  ,'System','Average',ChillerName)
  END DO
RETURN
END SUBROUTINE GetExhaustAbsorberInput

! End of Get Input subroutines for the Absorption Chiller Module
!******************************************************************************

SUBROUTINE InitExhaustAbsorber(ChillNum,RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of Exhaust Fired absorption chiller
          ! components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : TypeOf_Chiller_ExhFiredAbsorption, ScanPlantLoopsForObject, PlantLoop, &
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
    ALLOCATE(MyPlantScanFlag(NumExhaustAbsorbers))
    ALLOCATE(MyEnvrnFlag(NumExhaustAbsorbers))
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
    MyPlantScanFlag = .TRUE.
  END IF

  ! Init more variables
  IF (MyPlantScanFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ExhaustAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_ExhFiredAbsorption, &
                                 ExhaustAbsorber(ChillNum)%CWLoopNum, &
                                 ExhaustAbsorber(ChillNum)%CWLoopSideNum, &
                                 ExhaustAbsorber(ChillNum)%CWBranchNum, &
                                 ExhaustAbsorber(ChillNum)%CWCompNum, &
                                 LowLimitTemp = ExhaustAbsorber(ChillNum)%CHWLowLimitTemp, &
                                 InletNodeNumber = ExhaustAbsorber(ChillNum)%ChillReturnNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitExhaustAbsorber: Program terminated due to previous condition(s).')
    ENDIF

    CALL ScanPlantLoopsForObject(ExhaustAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_ExhFiredAbsorption, &
                                 ExhaustAbsorber(ChillNum)%HWLoopNum, &
                                 ExhaustAbsorber(ChillNum)%HWLoopSideNum, &
                                 ExhaustAbsorber(ChillNum)%HWBranchNum, &
                                 ExhaustAbsorber(ChillNum)%HWCompNum, &
                                 InletNodeNumber = ExhaustAbsorber(ChillNum)%HeatReturnNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitExhaustAbsorber: Program terminated due to previous condition(s).')
    ENDIF

    IF (ExhaustAbsorber(ChillNum)%isWaterCooled) THEN
      CALL ScanPlantLoopsForObject(ExhaustAbsorber(ChillNum)%Name, &
                                   TypeOf_Chiller_ExhFiredAbsorption, &
                                   ExhaustAbsorber(ChillNum)%CDLoopNum, &
                                   ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                                   ExhaustAbsorber(ChillNum)%CDBranchNum, &
                                   ExhaustAbsorber(ChillNum)%CDCompNum, &
                                   InletNodeNumber = ExhaustAbsorber(ChillNum)%CondReturnNodeNum,  &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitExhaustAbsorber: Program terminated due to previous condition(s).')
      ENDIF
      CALL InterConnectTwoPlantLoopSides( ExhaustAbsorber(ChillNum)%CWLoopNum,      &
                                          ExhaustAbsorber(ChillNum)%CWLoopSideNum,  &
                                          ExhaustAbsorber(ChillNum)%CDLoopNum,      &
                                          ExhaustAbsorber(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_ExhFiredAbsorption , .TRUE.)
      CALL InterConnectTwoPlantLoopSides( ExhaustAbsorber(ChillNum)%HWLoopNum,      &
                                          ExhaustAbsorber(ChillNum)%HWLoopSideNum,  &
                                          ExhaustAbsorber(ChillNum)%CDLoopNum,      &
                                          ExhaustAbsorber(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_ExhFiredAbsorption , .TRUE. )
    ENDIF

    CALL InterConnectTwoPlantLoopSides( ExhaustAbsorber(ChillNum)%CWLoopNum,      &
                                        ExhaustAbsorber(ChillNum)%CWLoopSideNum,  &
                                        ExhaustAbsorber(ChillNum)%HWLoopNum,      &
                                        ExhaustAbsorber(ChillNum)%HWLoopSideNum,  &
                                          TypeOf_Chiller_ExhFiredAbsorption, .TRUE. )

    ! check if outlet node of chilled water side has a setpoint.
    IF ((Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
      IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        IF (.NOT. ExhaustAbsorber(ChillNum)%ChillSetpointErrDone) THEN
          CALL ShowWarningError('Missing temperature setpoint on cool side for chiller heater named ' // &
                                        TRIM(ExhaustAbsorber(ChillNum)%Name) )
          CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller ' // &
                                           ', use a SetpointManager')
          CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
          ExhaustAbsorber(ChillNum)%ChillSetpointErrDone = .TRUE.
        ENDIF
      ELSE
       ! need call to EMS to check node
        errFlag = .FALSE. ! but not really fatal yet, but should be.
        CALL CheckIfNodeSetpointManagedByEMS(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum,iTemperatureSetpoint, errFlag)
        IF (errFlag) THEN
          IF (.NOT. ExhaustAbsorber(ChillNum)%ChillSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on cool side for chiller heater named ' // &
                                        TRIM(ExhaustAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller evaporator ')
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
            CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ExhaustAbsorber(ChillNum)%ChillSetpointErrDone = .TRUE.

          ENDIF
        ENDIF


      ENDIF
      ExhaustAbsorber(ChillNum)%ChillSetpointSetToLoop = .TRUE.
      Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
    ! check if outlet node of hot water side has a setpoint.
    IF ((Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo == SensedNodeFlagValue)) THEN
      IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        IF (.NOT. ExhaustAbsorber(ChillNum)%HeatSetpointErrDone) THEN
          CALL ShowWarningError('Missing temperature setpoint on heat side for chiller heater named ' // &
                                        TRIM(ExhaustAbsorber(ChillNum)%Name) )
          CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller ' // &
                                           ', use a SetpointManager')
          CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
          ExhaustAbsorber(ChillNum)%HeatSetpointErrDone = .TRUE.
        ENDIF
      ELSE
       ! need call to EMS to check node
        errFlag = .FALSE. ! but not really fatal yet, but should be.
        CALL CheckIfNodeSetpointManagedByEMS(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum,iTemperatureSetpoint, errFlag)
        IF (errFlag) THEN
          IF (.NOT. ExhaustAbsorber(ChillNum)%HeatSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on heat side for chiller heater named ' // &
                                        TRIM(ExhaustAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of this chiller heater ')
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the heater side outlet node ')
            CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for heater side. The simulation continues ... ')
            ExhaustAbsorber(ChillNum)%HeatSetpointErrDone = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      ExhaustAbsorber(ChillNum)%HeatSetpointSetToLoop = .TRUE.
      Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPointLo
    ENDIF
    MyPlantScanFlag(ChillNum)=.FALSE.
  ENDIF



  CondInletNode  = ExhaustAbsorber(ChillNum)%CondReturnNodeNum
  CondOutletNode = ExhaustAbsorber(ChillNum)%CondSupplyNodeNum
  HeatInletNode  = ExhaustAbsorber(ChillNum)%HeatReturnNodeNum
  HeatOutletNode = ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum

  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))THEN
    IF (PlantSizeNotComplete) CALL SizeExhaustAbsorber(ChillNum)
    IF (ExhaustAbsorber(ChillNum)%isWaterCooled) THEN
      ! init max available condenser water flow rate
      IF (ExhaustAbsorber(ChillNum)%CDLoopNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                'InitExhaustAbsorber')
      ELSE
        rho = RhoH2O(InitConvTemp)

      ENDIF

      ExhaustAbsorber(ChillNum)%DesCondMassFlowRate = rho * ExhaustAbsorber(ChillNum)%CondVolFlowRate
      CALL InitComponentNodes(0.d0, ExhaustAbsorber(ChillNum)%DesCondMassFlowRate, &
                                    CondInletNode, CondOutletNode,      &
                                    ExhaustAbsorber(ChillNum)%CDLoopNum,     &
                                    ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                                    ExhaustAbsorber(ChillNum)%CDBranchNum,   &
                                    ExhaustAbsorber(ChillNum)%CDCompNum)
    ENDIF

    IF (ExhaustAbsorber(ChillNum)%HWLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%FluidIndex, &
                                'InitExhaustAbsorber')
    ELSE
       rho = RhoH2O(InitConvTemp)
    ENDIF
    ExhaustAbsorber(ChillNum)%DesHeatMassFlowRate = rho * ExhaustAbsorber(ChillNum)%HeatVolFlowRate
    !init available hot water flow rate
    CALL InitComponentNodes(0.d0, ExhaustAbsorber(ChillNum)%DesHeatMassFlowRate, &
                                  HeatInletNode, HeatOutletNode,      &
                                 ExhaustAbsorber(ChillNum)%HWLoopNum, &
                                 ExhaustAbsorber(ChillNum)%HWLoopSideNum, &
                                 ExhaustAbsorber(ChillNum)%HWBranchNum, &
                                 ExhaustAbsorber(ChillNum)%HWCompNum)

    IF (ExhaustAbsorber(ChillNum)%CWLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                InitConvTemp,  &
                                PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                'InitExhaustAbsorber')
    ELSE
       rho = RhoH2O(InitConvTemp)
    ENDIF
    ExhaustAbsorber(ChillNum)%DesEvapMassFlowRate = rho * ExhaustAbsorber(ChillNum)%EvapVolFlowRate
    !init available hot water flow rate
    CALL InitComponentNodes(0.d0, ExhaustAbsorber(ChillNum)%DesEvapMassFlowRate, &
                                  ExhaustAbsorber(ChillNum)%ChillReturnNodeNum,   &
                                  ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum,   &
                                  ExhaustAbsorber(ChillNum)%CWLoopNum,     &
                                  ExhaustAbsorber(ChillNum)%CWLoopSideNum, &
                                  ExhaustAbsorber(ChillNum)%CWBranchNum,   &
                                  ExhaustAbsorber(ChillNum)%CWCompNum)

    MyEnvrnFlag(ChillNum) = .FALSE.

  END IF

  IF(.not. BeginEnvrnFlag)Then
    MyEnvrnFlag(ChillNum) = .TRUE.
  End IF

  !this component model works off setpoints on the leaving node
  ! fill from plant if needed
  IF (ExhaustAbsorber(ChillNum)%ChillSetpointSetToLoop) THEN
    Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum)%TempSetPointHi =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ( ExhaustAbsorber(ChillNum)%HeatSetpointSetToLoop ) THEN
    Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPoint =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum)%TempSetPointLo =  &
              Node(PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPointLo
  ENDIF

  IF ((ExhaustAbsorber(ChillNum)%isWaterCooled) .AND. &
      ((ExhaustAbsorber(ChillNum)%InHeatingMode) .OR. (ExhaustAbsorber(ChillNum)%InCoolingMode)) &
        .AND. (.NOT. MyPlantScanFlag(ChillNum)) ) THEN
    mdot = ExhaustAbsorber(ChillNum)%DesCondMassFlowRate

    CALL SetComponentFlowRate(mdot, &
                              ExhaustAbsorber(ChillNum)%CondReturnNodeNum,     &
                              ExhaustAbsorber(ChillNum)%CondSupplyNodeNum,     &
                              ExhaustAbsorber(ChillNum)%CDLoopNum,     &
                              ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CDBranchNum,   &
                              ExhaustAbsorber(ChillNum)%CDCompNum)

  ELSE
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot, &
                              ExhaustAbsorber(ChillNum)%CondReturnNodeNum,     &
                              ExhaustAbsorber(ChillNum)%CondSupplyNodeNum,     &
                              ExhaustAbsorber(ChillNum)%CDLoopNum,     &
                              ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CDBranchNum,   &
                              ExhaustAbsorber(ChillNum)%CDCompNum)
  END IF

  RETURN

END SUBROUTINE InitExhaustAbsorber

SUBROUTINE SizeExhaustAbsorber(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Exhaust Fired absorption chiller components for which
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
  tmpNomCap             = ExhaustAbsorber(ChillNum)%NomCoolingCap
  tmpEvapVolFlowRate    = ExhaustAbsorber(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate    = ExhaustAbsorber(ChillNum)%CondVolFlowRate
  tmpHeatRecVolFlowRate = ExhaustAbsorber(ChillNum)%HeatVolFlowRate

  IF (ExhaustAbsorber(ChillNum)%NomCoolingCap  == AutoSize .or. &
      ExhaustAbsorber(ChillNum)%EvapVolFlowRate == AutoSize .or. &
      ExhaustAbsorber(ChillNum)%HeatVolFlowRate == AutoSize .or. &
      ExhaustAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN

    IF (ExhaustAbsorber(ChillNum)%isWaterCooled) &
      PltSizCondNum = PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%PlantSizNum

    PltSizHeatNum = PlantLoop(ExhaustAbsorber(ChillNum)%HWLoopNum)%PlantSizNum

    PltSizCoolNum = PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%PlantSizNum

  ENDIF

  IF (ExhaustAbsorber(ChillNum)%NomCoolingCap  == AutoSize) THEN
    IF (PltSizCoolNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        Cp = GetSpecificHeatGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeExhaustAbsorber')
        rho = GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeExhaustAbsorber')
        tmpNomCap = Cp  * rho * PlantSizData(PltSizCoolNum)%DeltaT &
                                                    * PlantSizData(PltSizCoolNum)%DesVolFlowRate * ExhaustAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%NomCoolingCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%NomCoolingCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize)   &
           CALL ReportSizingOutput('ChillerHeater:Absorption:DoubleEffect', ExhaustAbsorber(ChillNum)%Name, &
                              'Nominal Cooling Capacity [W]', ExhaustAbsorber(ChillNum)%NomCoolingCap)
    ELSE
      CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect="'//trim(ExhaustAbsorber(ChillNum)%Name)// &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Exhaust Fired Absorption Chiller nominal cooling capacity requires')
      CALL ShowContinueError('a cooling loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ExhaustAbsorber(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizCoolNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizCoolNum)%DesVolFlowRate * ExhaustAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput('ChillerHeater:Absorption:DoubleEffect', ExhaustAbsorber(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              ExhaustAbsorber(ChillNum)%EvapVolFlowRate)

    ELSE
      CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect="'//trim(ExhaustAbsorber(ChillNum)%Name)// &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Exhaust Fired Absorption Chiller evap flow rate requires')
      CALL ShowContinueError('a cooling loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ExhaustAbsorber(ChillNum)%ChillReturnNodeNum,tmpEvapVolFlowRate)

  IF (ExhaustAbsorber(ChillNum)%HeatVolFlowRate == AutoSize) THEN
    IF (PltSizHeatNum > 0) THEN
      IF (PlantSizData(PltSizHeatNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpHeatRecVolFlowRate = PlantSizData(PltSizHeatNum)%DesVolFlowRate * ExhaustAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%HeatVolFlowRate = tmpHeatRecVolFlowRate
      ELSE
        tmpHeatRecVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%HeatVolFlowRate = tmpHeatRecVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput('ChillerHeater:Absorption:DoubleEffect', ExhaustAbsorber(ChillNum)%Name, &
                              'Design Hot Water Flow Rate [m3/s]', &
                              ExhaustAbsorber(ChillNum)%HeatVolFlowRate)
    ELSE
      CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect="'//trim(ExhaustAbsorber(ChillNum)%Name)// &
         '", autosize error.')
      CALL ShowContinueError('Autosizing of Exhaust Fired Absorption Chiller hot water flow rate requires')
      CALL ShowContinueError('a heating loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ExhaustAbsorber(ChillNum)%HeatReturnNodeNum,tmpHeatRecVolFlowRate)

  IF ((ExhaustAbsorber(ChillNum)%CondVolFlowRate == AutoSize) .AND. (ExhaustAbsorber(ChillNum)%isWaterCooled)) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCoolNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        Cp = GetSpecificHeatGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   ExhaustAbsorber(ChillNum)%TempDesCondReturn, &
                                   PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeExhaustAbsorber')
        rho = GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   ExhaustAbsorber(ChillNum)%TempDesCondReturn, &
                                   PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeExhaustAbsorber')
        tmpCondVolFlowRate = tmpNomCap * &
                             (1.0d0 + ExhaustAbsorber(ChillNum)%ThermalEnergyCoolRatio) / &
                             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ExhaustAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput('ChillerHeater:Absorption:DoubleEffect', ExhaustAbsorber(ChillNum)%Name, &
                                           'Design Condenser Water Flow Rate [m3/s]', &
                                           ExhaustAbsorber(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeater:Absorption:DoubleEffect="'//trim(ExhaustAbsorber(ChillNum)%Name)// &
         '", autosize error.')
      CALL ShowSevereError('Autosizing of Exhaust Fired Absorption Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object.')
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (ExhaustAbsorber(ChillNum)%isWaterCooled)  &
    CALL RegisterPlantCompDesignFlow(ExhaustAbsorber(ChillNum)%CondReturnNodeNum,tmpCondVolFlowRate)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = ExhaustAbsorber(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'ChillerHeater:Absorption:DoubleEffect')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ExhaustAbsorber(ChillNum)%ThermalEnergyCoolRatio)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ExhaustAbsorber(ChillNum)%NomCoolingCap)
  ENDIF

  RETURN
END SUBROUTINE SizeExhaustAbsorber


! Beginning of Absorber model Subroutines
! *****************************************************************************

SUBROUTINE CalcExhaustAbsorberChillerModel(ChillNum,MyLoad,Runflag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2001
          !       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired chiller
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
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
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb
  USE DataPlant,       ONLY : DeltaTemptol, PlantLoop, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate
  USE MicroturbineElectricGenerator, ONLY: SimMTGenerator

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER              :: ChillNum        ! Absorber number
  REAL(r64)            :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)  :: RunFlag         ! TRUE when Absorber operating

! FlowLock = 0  if mass flow rates may be changed by loop components
! FlowLock = 1  if mass flow rates may not be changed by loop components

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), parameter        :: WaterMassFlowTol=0.001d0      ! kg/s - minimum significan mass flow rate
  REAL(r64), parameter        :: AbsLeavingTemp = 176.667d0     ! C - Minimum temperature leaving the Chiller absorber (350 F)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! Local copies of ExhaustAbsorberSpecs Type
          ! all variables that are local copies of data structure
          ! variables are prefaced with an "l" for local.
  REAL(r64)         :: lNomCoolingCap      ! W - design nominal capacity of Absorber
  REAL(r64)         :: lThermalEnergyCoolRatio      ! ratio of ThermalEnergy input to cooling output
  REAL(r64)         :: lThermalEnergyHeatRatio      ! ratio of ThermalEnergy input to heating output
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
  INTEGER           :: lThermalEnergyCoolFTCurve    ! ThermalEnergy-Input-to cooling output Ratio Function of Temperature Curve
  INTEGER           :: lThermalEnergyCoolFPLRCurve  ! ThermalEnergy-Input-to cooling output Ratio Function of Part Load Ratio Curve
  INTEGER           :: lElecCoolFTCurve    ! Electric-Input-to cooling output Ratio Function of Temperature Curve
  INTEGER           :: lElecCoolFPLRCurve  ! Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
  LOGICAL           :: lIsEnterCondensTemp ! if using entering conderser water temperature is TRUE, exiting is FALSE
  LOGICAL           :: lIsWaterCooled      ! if water cooled it is TRUE
  REAL(r64)         :: lCHWLowLimitTemp    ! Chilled Water Lower Limit Temperature
  INTEGER           :: lExhaustAirInletNodeNum       ! Combustion Air Inlet Node number
  ! Local copies of ExhaustAbsorberReportVars Type
  REAL(r64)    :: lCoolingLoad        ! cooling load on the chiller (previously called QEvap)
  REAL(r64)    :: lCoolingEnergy      ! variable to track total cooling load for period (was EvapEnergy)
  REAL(r64)    :: lTowerLoad          ! load on the cooling tower/condenser (previously called QCond)
  REAL(r64)    :: lTowerEnergy        ! variable to track total tower load for a period (was CondEnergy)
  REAL(r64)    :: lThermalEnergyUseRate        ! instantaneous use of exhaust for period
  REAL(r64)    :: lThermalEnergy         ! variable to track total ThermalEnergy used for a period
  REAL(r64)    :: lCoolThermalEnergyUseRate    ! instantaneous use of exhaust for period for cooling
  REAL(r64)    :: lCoolThermalEnergy     ! variable to track total ThermalEnergy used for a period for cooling
  REAL(r64)    :: lHeatThermalEnergyUseRate    ! instantaneous use of exhaust for period for heating
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
  Real(r64)    :: lExhaustInTemp   ! Exhaust inlet temperature
  Real(r64)    :: lExhaustInFlow   ! Exhaust inlet flow rate
  Real(r64)    :: lExhHeatRecPotentialCool   ! Exhaust heat recovery potential during cooling
  Real(r64)    ::  lExhaustAirHumRat
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
 REAL(r64)  :: CpAir  ! specific heat of exhaust air

  !initialize all output variables to zero
  lCoolingLoad        = 0.0d0
  lCoolingEnergy      = 0.0d0
  lTowerLoad          = 0.0d0
  lTowerEnergy        = 0.0d0
  lThermalEnergyUseRate        = 0.0d0
  lThermalEnergy        = 0.0d0
  lCoolThermalEnergyUseRate    = 0.0d0
  lCoolThermalEnergy     = 0.0d0
  lHeatThermalEnergyUseRate    = 0.0d0
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
  lExhaustInTemp   = 0.0d0
  lExhaustInFlow   = 0.0d0
  lExhHeatRecPotentialCool  = 0.0d0
  lExhaustAirHumRat =0.0d0
! define constant values

  ! set node values to data structure values for nodes

  lChillReturnNodeNum = ExhaustAbsorber(ChillNum)%ChillReturnNodeNum
  lChillSupplyNodeNum = ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum
  lCondReturnNodeNum  = ExhaustAbsorber(ChillNum)%CondReturnNodeNum
  lCondSupplyNodeNum  = ExhaustAbsorber(ChillNum)%CondSupplyNodeNum
  lExhaustAirInletNodeNum   = ExhaustAbsorber(ChillNum)%ExhaustAirInletNodeNum

  ! set local copies of data from rest of input structure
  lNomCoolingCap      = ExhaustAbsorber(ChillNum)%NomCoolingCap
  lThermalEnergyCoolRatio      = ExhaustAbsorber(ChillNum)%ThermalEnergyCoolRatio
  lThermalEnergyHeatRatio      = ExhaustAbsorber(ChillNum)%ThermalEnergyHeatRatio
  lElecCoolRatio      = ExhaustAbsorber(ChillNum)%ElecCoolRatio
  lMinPartLoadRat     = ExhaustAbsorber(ChillNum)%MinPartLoadRat
  lMaxPartLoadRat     = ExhaustAbsorber(ChillNum)%MaxPartLoadRat
  lOptPartLoadRat     = ExhaustAbsorber(ChillNum)%OptPartLoadRat
  lTempDesCondReturn  = ExhaustAbsorber(ChillNum)%TempDesCondReturn
  lTempDesCHWSupply   = ExhaustAbsorber(ChillNum)%TempDesCHWSupply
  lCondVolFlowRate    = ExhaustAbsorber(ChillNum)%CondVolFlowRate
  lCoolCapFTCurve     = ExhaustAbsorber(ChillNum)%CoolCapFTCurve
  lThermalEnergyCoolFTCurve    = ExhaustAbsorber(ChillNum)%ThermalEnergyCoolFTCurve
  lThermalEnergyCoolFPLRCurve  = ExhaustAbsorber(ChillNum)%ThermalEnergyCoolFPLRCurve
  lElecCoolFTCurve    = ExhaustAbsorber(ChillNum)%ElecCoolFTCurve
  lElecCoolFPLRCurve  = ExhaustAbsorber(ChillNum)%ElecCoolFPLRCurve
  lisEnterCondensTemp = ExhaustAbsorber(ChillNum)%isEnterCondensTemp
  lisWaterCooled      = ExhaustAbsorber(ChillNum)%isWaterCooled
  lCHWLowLimitTemp    = ExhaustAbsorber(ChillNum)%CHWLowLimitTemp
  lHeatElectricPower  = ExhaustAbsorberReport(ChillNum)%HeatElectricPower
  lHeatThermalEnergyUseRate    = ExhaustAbsorberReport(ChillNum)%HeatThermalEnergyUseRate
  lHeatPartLoadRatio  = ExhaustAbsorberReport(ChillNum)%HeatPartLoadRatio

! initialize entering conditions
  lChillReturnTemp  = Node(lChillReturnNodeNum)%Temp
  lChillWaterMassFlowRate  = Node(lChillReturnNodeNum)%MassFlowRate
  lCondReturnTemp  = Node(lCondReturnNodeNum)%Temp
  lCondWaterMassFlowRate  = Node(lCondReturnNodeNum)%MassFlowRate
  SELECT CASE (PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    ChillSupplySetPointTemp = Node(lChillSupplyNodeNum)%TempSetPoint
  CASE (DualSetpointDeadband)
    ChillSupplySetPointTemp = Node(lChillSupplyNodeNum)%TempSetPointHi
  END SELECT
  ChillDeltaTemp  = ABS(lChillReturnTemp - ChillSupplySetPointTemp)
  lExhaustInTemp  = Node(lExhaustAirInletNodeNum)%Temp
  lExhaustInFlow  = Node(lExhaustAirInletNodeNum)%MassFlowRate
  lExhaustAirHumRat =Node(lExhaustAirInletNodeNum)%HumRat


  rhoCW =  GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                            'CalcExhaustAbsorberChillerModel')
  Cp_CW = GetSpecificHeatGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                            'CalcExhaustAbsorberChillerModel')
  rhoCD =  GetDensityGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                            'CalcExhaustAbsorberChillerModel')
  Cp_CD = GetSpecificHeatGlycol(PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                            lChillReturnTemp, &
                            PlantLoop(ExhaustAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                            'CalcExhaustAbsorberChillerModel')


          !If no loop demand or Absorber OFF, return
          ! will need to modify when absorber can act as a boiler
 IF (MyLoad>=0 .OR. .NOT. ((ExhaustAbsorber(ChillNum)%InHeatingMode) .OR. (ExhaustAbsorber(ChillNum)%InCoolingMode))) THEN
          !set node temperatures
    lChillSupplyTemp = lChillReturnTemp
    lCondSupplyTemp  = lCondReturnTemp
    lCondWaterMassFlowRate = 0.0d0
    IF (lisWaterCooled) THEN
      CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              ExhaustAbsorber(ChillNum)%CondReturnNodeNum, &
                              ExhaustAbsorber(ChillNum)%CondSupplyNodeNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CDBranchNum, &
                              ExhaustAbsorber(ChillNum)%CDCompNum)
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
    lCondWaterMassFlowRate = ExhaustAbsorber(ChillNum)%DesCondMassFlowRate
    CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              ExhaustAbsorber(ChillNum)%CondReturnNodeNum, &
                              ExhaustAbsorber(ChillNum)%CondSupplyNodeNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CDBranchNum, &
                              ExhaustAbsorber(ChillNum)%CDCompNum)
  ELSE
    ! air cooled
    Node(lCondReturnNodeNum)%Temp=Node(lCondReturnNodeNum)%OutAirDryBulb
    lCondReturnTemp = Node(lCondReturnNodeNum)%Temp
    lCondWaterMassFlowRate = 0.d0
    CALL SetComponentFlowRate(lCondWaterMassFlowRate, &
                              ExhaustAbsorber(ChillNum)%CondReturnNodeNum, &
                              ExhaustAbsorber(ChillNum)%CondSupplyNodeNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopNum, &
                              ExhaustAbsorber(ChillNum)%CDLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CDBranchNum, &
                              ExhaustAbsorber(ChillNum)%CDCompNum)
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
  lChillWaterMassflowratemax = Exhaustabsorber(chillnum)%DesEvapMassFlowRate

  LoopNum = ExhaustAbsorber(ChillNum)%CWLoopNum
  LoopSideNum = ExhaustAbsorber(ChillNum)%CWLoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock)
    CASE (0) ! mass flow rates may be changed by loop components
      ExhaustAbsorber(ChillNum)%Possiblesubcooling = .FALSE.
      lCoolingLoad = ABS(myLoad)
      IF (ChillDeltaTemp /= 0.0d0) THEN
        lChillWaterMassFlowRate = ABS(lCoolingLoad / (Cp_CW * ChillDeltaTemp))
        If(lChillWaterMassFlowRate -lChillWaterMassflowratemax.GT.MassFlowTolerance)   &
           ExhaustAbsorber(ChillNum)%Possiblesubcooling = .TRUE.

        CALL SetComponentFlowRate(lChillWaterMassFlowRate, &
                              ExhaustAbsorber(ChillNum)%ChillReturnNodeNum, &
                              ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum, &
                              ExhaustAbsorber(ChillNum)%CWLoopNum, &
                              ExhaustAbsorber(ChillNum)%CWLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%CWBranchNum, &
                              ExhaustAbsorber(ChillNum)%CWCompNum)
        SELECT CASE (PlantLoop(ExhaustAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          lChillSupplyTemp = Node(lChillSupplyNodeNum)%TempSetPoint
        CASE (DualSetpointDeadband)
          lChillSupplyTemp = Node(lChillSupplyNodeNum)%TempSetPointHi
        END SELECT
      ELSE
        lChillWaterMassFlowRate = 0.0d0
        CALL ShowRecurringWarningErrorAtEnd('ExhaustAbsorberChillerModel:Cooling"'//TRIM(ExhaustAbsorber(ChillNum)%Name)//  &
             '", DeltaTemp = 0 in mass flow calculation',ExhaustAbsorber(ChillNum)%DeltaTempCoolErrCount)
      END IF
      lChillSupplyTemp = ChillSupplySetPointTemp
    CASE (1) ! mass flow rates may not be changed by loop components
      lChillWatermassflowrate = Node(lChillreturnnodenum)%Massflowrate
      If (ExhaustAbsorber(ChillNum)%Possiblesubcooling) then
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

  !Calculate thermal energy consumption for cooling
  ! Thermal Energy used for cooling availCap * TeFIR * TeFIR-FT * TeFIR-FPLR
  lCoolThermalEnergyUseRate = lAvailableCoolingCapacity * lThermalEnergyCoolRatio  &
     * CurveValue(lThermalEnergyCoolFTCurve,lChillSupplyTemp,calcCondTemp) &
     * CurveValue(lThermalEnergyCoolFPLRCurve,lCoolPartLoadRatio)*lFractionOfPeriodRunning

  !Calculate electric parasitics used
  ! based on nominal capacity, not available capacity,
  ! electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
  lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning &
     * CurveValue(lElecCoolFTCurve,lChillSupplyTemp,calcCondTemp) &
     * CurveValue(lElecCoolFPLRCurve,lCoolPartLoadRatio)

  ! determine conderser load which is cooling load plus the
  ! ThermalEnergy used for cooling plus
  ! the electricity used
  lTowerLoad = lCoolingLoad + lCoolThermalEnergyUseRate / lThermalEnergyHeatRatio + lCoolElectricPower

   lExhaustInTemp  = Node(lExhaustAirInletNodeNum)%Temp
   lExhaustInFlow  = Node(lExhaustAirInletNodeNum)%MassFlowRate
   CpAir = PsyCpAirFnWTdb(lExhaustAirHumRat,lExhaustInTemp)
   lExhHeatRecPotentialCool =  lExhaustInFlow  * Cpair * (  lExhaustInTemp  - AbsLeavingTemp )
 ! If Microturbine Exhaust temperature and flow rate is not sufficient to run the chiller, then chiller will not run
 ! lCoolThermalEnergyUseRate , lTowerLoad and  lCoolElectricPower will be set to 0.0


  IF (lExhHeatRecPotentialCool .LT. lCoolThermalEnergyUseRate ) THEN
      IF(ExhaustAbsorber(ChillNum)%ExhTempLTAbsLeavingTempIndex == 0)THEN
         CALL ShowWarningError('ChillerHeater:Absorption:DoubleEffect "'//TRIM(ExhaustAbsorber(ChillNum)%Name)//'"')
          CALL ShowContinueError('...Exhaust temperature and flow input from Micro Turbine is not sufficient during cooling '&
                            //'to run the chiller ')
           CALL ShowContinueError('...Value of Exhaust air inlet temp ='//TRIM(TrimSigDigits(lExhaustInTemp,4))//' C.')
           CALL ShowContinueError('... and Exhaust air flow rate of '//TRIM(TrimSigDigits(lExhaustInFlow,2))//' kg/s.')
           CALL ShowContinueError('...Value of minimum absorber leaving temp ='//TRIM(TrimSigDigits(AbsLeavingTemp,4))//' C.')
          CALL ShowContinueError('...Either increase the Exhaust temperature (min required = 350 C )  '&
                            //'or flow or both of Micro Turbine to meet the min available potential criteria.')
          CALL ShowContinueErrorTimeStamp('... Simulation will continue.')
       ENDIF
          CALL ShowRecurringWarningErrorAtEnd('ChillerHeater:Absorption:DoubleEffect "'//  &
             TRIM(ExhaustAbsorber(ChillNum)%Name)//'":'// &
          ' Exhaust temperature from Micro Turbine is not sufficient to run the chiller during cooling warning continues...', &
           ExhaustAbsorber(ChillNum)%ExhTempLTAbsLeavingTempIndex,  lExhaustInTemp, AbsLeavingTemp)
! If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
    lCoolThermalEnergyUseRate = 0.0d0
    lTowerLoad = 0.0d0
    lCoolElectricPower =0.0d0
    lChillSupplyTemp = lChillReturnTemp
    lCondSupplyTemp  = lCondReturnTemp
    ChillDeltaTemp   = 0.0d0
    lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)
  END IF
  ! for water cooled condenser make sure enough flow rate
  ! for air cooled condenser just set supply to return temperature
  IF (lIsWaterCooled) THEN
    IF (lCondWaterMassFlowRate > MassFlowTolerance) THEN
      lCondSupplyTemp = lCondReturnTemp + lTowerLoad / (lCondWaterMassFlowRate * Cp_CD )
    ELSE
      CALL ShowSevereError('CalcExhaustAbsorberChillerModel: Condenser flow = 0, for Exhaust Absorber Chiller='//  &
                           TRIM(ExhaustAbsorber(ChillNum)%Name))
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
        CALL ShowRecurringWarningErrorAtEnd('ExhaustAbsorberChillerModel:"'//TRIM(ExhaustAbsorber(ChillNum)%Name)//  &
          '", poor Condenser Supply Estimate',ExhaustAbsorber(ChillNum)%condErrCount,ReportMinOf=errorAvailCap,  &
          ReportMaxOf=errorAvailCap)
      ENDIF
    ENDIF
  ENDIF
 ENDIF ! IF(MyLoad>=0 .OR. .NOT. Runflag)
  ! Write into the Report Variables except for nodes
  ExhaustAbsorberReport(ChillNum)%CoolingLoad             = lCoolingLoad
  ExhaustAbsorberReport(ChillNum)%TowerLoad               = lTowerLoad
  ExhaustAbsorberReport(ChillNum)%CoolThermalEnergyUseRate         = lCoolThermalEnergyUseRate
  ExhaustAbsorberReport(ChillNum)%CoolElectricPower       = lCoolElectricPower
  ExhaustAbsorberReport(ChillNum)%CondReturnTemp          = lCondReturnTemp
  ExhaustAbsorberReport(ChillNum)%ChillReturnTemp         = lChillReturnTemp
  ExhaustAbsorberReport(ChillNum)%CondSupplyTemp          = lCondSupplyTemp
  ExhaustAbsorberReport(ChillNum)%ChillSupplyTemp         = lChillSupplyTemp
  ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate      = lChillWaterMassFlowRate
  ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate       = lCondWaterMassFlowRate
  ExhaustAbsorberReport(ChillNum)%CoolPartLoadRatio       = lCoolPartLoadRatio
  ExhaustAbsorberReport(ChillNum)%CoolingCapacity         = lAvailableCoolingCapacity
  ExhaustAbsorberReport(ChillNum)%FractionOfPeriodRunning = lFractionOfPeriodRunning
  ExhaustAbsorberReport(ChillNum)%ExhaustInTemp         = lExhaustInTemp
  ExhaustAbsorberReport(ChillNum)%ExhaustInFlow         = lExhaustInFlow
  ExhaustAbsorberReport(ChillNum)%ExhHeatRecPotentialCool   = lExhHeatRecPotentialCool

  ! write the combined heating and cooling ThermalEnergy used and electric used
  ExhaustAbsorberReport(ChillNum)%ThermalEnergyUseRate    = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate
  ExhaustAbsorberReport(ChillNum)%ElectricPower           = lCoolElectricPower + lHeatElectricPower

END SUBROUTINE CalcExhaustAbsorberChillerModel

SUBROUTINE CalcExhaustAbsorberHeaterModel(ChillNum,MyLoad,Runflag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer and Michael J. Witte
          !       DATE WRITTEN   March 2001
          !       MODIFIED       Mahabir Bhandari, ORNL, Aug 2011, modified to accomodate exhaust fired double effect absorption chiller
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a Exhaust fired (Exhaust consuming) absorption chiller using
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
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE FluidProperties, ONLY : GetSpecificHeatGlycol, GetDensityGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb
  USE MicroturbineElectricGenerator, ONLY: SimMTGenerator

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
 REAL(r64), parameter        :: WaterMassFlowTol=0.001d0 ! kg/s - minimum significan mass flow rate
 REAL(r64), parameter        :: AbsLeavingTemp = 176.667d0     ! C - Minimum temperature leaving the Chiller absorber (350 F)
 !INTEGER    :: ExhTempLTAbsLeavingTempCount      = 0        ! Counter for exhaust temp < absorber leaving air temp warning messages
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! Local copies of ExhaustAbsorberSpecs Type
          ! all variables that are local copies of data structure
          ! variables are prefaced with an "l" for local.
  REAL(r64)         :: lNomCoolingCap      ! W - design nominal capacity of Absorber
  REAL(r64)         :: lNomHeatCoolRatio   ! ratio of heating to cooling capacity
  REAL(r64)         :: lThermalEnergyHeatRatio      ! ratio of ThermalEnergy input to heating output
  REAL(r64)         :: lElecHeatRatio      ! ratio of electricity input to heating output
  INTEGER           :: lHeatReturnNodeNum  ! absorber hot water inlet node number, water side
  INTEGER           :: lHeatSupplyNodeNum  ! absorber hot water outlet node number, water side
  REAL(r64)         :: lMinPartLoadRat     ! min allowed operating frac full load
  REAL(r64)         :: lMaxPartLoadRat     ! max allowed operating frac full load
  REAL(r64)         :: lOptPartLoadRat     ! optimal operating frac full load
  INTEGER           :: lHeatCapFCoolCurve  ! Heating Capacity Function of Cooling Capacity Curve
  INTEGER           :: lThermalEnergyHeatFHPLRCurve ! ThermalEnergy Input to heat output ratio during heating only function
  ! Local copies of ExhaustAbsorberReportVars Type
  REAL(r64)    :: lHeatingLoad        ! heating load on the chiller
  REAL(r64)    :: lHeatingEnergy      ! heating energy
  REAL(r64)    :: lThermalEnergyUseRate        ! instantaneous use of Thermal Energy for period
  REAL(r64)    :: lThermalEnergy         ! variable to track total Thermal Energy used for a period (reference only)
  REAL(r64)    :: lCoolThermalEnergyUseRate    ! instantaneous use of thermal energy for period for cooling
  REAL(r64)    :: lHeatThermalEnergyUseRate    ! instantaneous use of thermal energy for period for heating
  REAL(r64)    :: lHeatThermalEnergy     ! variable to track total ThermalEnergy used for a period for heating
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
  Real(r64)    :: lExhaustInTemp   ! Exhaust inlet temperature
  Real(r64)    :: lExhaustInFlow   ! Exhaust inlet flow rate
  Real(r64)    :: lExhHeatRecPotentialHeat   ! Exhaust heat recovery potential
  Real(r64)    ::  lExhaustAirHumRat
  ! other local variables
  REAL(r64)              :: HeatDeltaTemp       ! hot water temperature difference
  REAL(r64)              :: HeatSupplySetPointTemp
  INTEGER      :: LoopNum
  INTEGER      :: LoopSideNum
  REAL(r64)    :: Cp_HW  ! local fluid specific heat for hot water
  INTEGER  :: GeneratorType
  INTEGER :: GenIndex1
  REAL(r64)       ::CpAir
  REAL(r64)    :: rhoHW  ! local fluid density for hot water
  INTEGER      :: lExhaustAirInletNodeNum       ! Combustion Air Inlet Node number
  CHARACTER(len=MaxNameLength) :: GeneratorName

!  INTEGER      :: lExhaustAirOutletNodeNum      ! Combustion Air Outlet (Exhaust) Node number

  !initialize all output variables to zero

  lHeatingLoad        = 0.0d0
  lHeatingEnergy      = 0.0d0
  lThermalEnergyUseRate        = 0.0d0
  lThermalEnergy         = 0.0d0
  lCoolThermalEnergyUseRate    = 0.0d0
  lHeatThermalEnergyUseRate    = 0.0d0
  lHeatThermalEnergy     = 0.0d0
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
  lExhaustInTemp  = 0.0d0
   lExhaustInFlow  = 0.0d0
  lExhHeatRecPotentialHeat = 0.0d0
  lExhaustAirHumRat =0.0d0
  ! set node values to data structure values for nodes

  lHeatReturnNodeNum  = ExhaustAbsorber(ChillNum)%HeatReturnNodeNum
  lHeatSupplyNodeNum  = ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum
  lExhaustAirInletNodeNum   = ExhaustAbsorber(ChillNum)%ExhaustAirInletNodeNum

  ! set local copies of data from rest of input structure

  lNomCoolingCap      = ExhaustAbsorber(ChillNum)%NomCoolingCap
  lNomHeatCoolRatio   = ExhaustAbsorber(ChillNum)%NomHeatCoolRatio
  lThermalEnergyHeatRatio      = ExhaustAbsorber(ChillNum)%ThermalEnergyHeatRatio
  lElecHeatRatio      = ExhaustAbsorber(ChillNum)%ElecHeatRatio
  lMinPartLoadRat     = ExhaustAbsorber(ChillNum)%MinPartLoadRat
  lMaxPartLoadRat     = ExhaustAbsorber(ChillNum)%MaxPartLoadRat
  lOptPartLoadRat     = ExhaustAbsorber(ChillNum)%OptPartLoadRat
  lHeatCapFCoolCurve  = ExhaustAbsorber(ChillNum)%HeatCapFCoolCurve
  lThermalEnergyHeatFHPLRCurve = ExhaustAbsorber(ChillNum)%ThermalEnergyHeatFHPLRCurve
  lHotWaterMassFlowRateMax = Exhaustabsorber(chillnum)%DesHeatMassFlowRate
  LoopNum             = ExhaustAbsorber(ChillNum)%HWLoopNum
  LoopSideNum         = ExhaustAbsorber(ChillNum)%HWLoopSideNum

  Cp_HW               = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, &
                                               lHotWaterReturnTemp, &
                                               PlantLoop(LoopNum)%FluidIndex, &
                                               'CalcExhaustAbsorberHeaterModel')
  rhoHW               = GetDensityGlycol(PlantLoop(LoopNum)%FluidName, &
                                               lHotWaterReturnTemp, &
                                               PlantLoop(LoopNum)%FluidIndex, &
                                               'CalcExhaustAbsorberHeaterModel')

  lCoolElectricPower  = ExhaustAbsorberReport(ChillNum)%CoolElectricPower
  lCoolThermalEnergyUseRate    = ExhaustAbsorberReport(ChillNum)%CoolThermalEnergyUseRate
  lCoolPartLoadRatio  = ExhaustAbsorberReport(ChillNum)%CoolPartLoadRatio

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
         lAvailableHeatingCapacity = ExhaustAbsorber(ChillNum)%NomHeatCoolRatio * &
           ExhaustAbsorber(ChillNum)%NomCoolingCap * CurveValue(lHeatCapFCoolCurve, &
           (ExhaustAbsorberReport(ChillNum)%CoolingLoad / ExhaustAbsorber(ChillNum)%NomCoolingCap))

            !Calculate current load for heating
  MyLoad = SIGN(MAX(ABS(MyLoad), ExhaustAbsorberReport(ChillNum)%HeatingCapacity * lMinPartLoadRat), MyLoad)
  MyLoad = SIGN(MIN(ABS(MyLoad), ExhaustAbsorberReport(ChillNum)%HeatingCapacity * lMaxPartLoadRat), MyLoad)

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
                              ExhaustAbsorber(ChillNum)%HeatReturnNodeNum, &
                              ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum, &
                              ExhaustAbsorber(ChillNum)%HWLoopNum, &
                              ExhaustAbsorber(ChillNum)%HWLoopSideNum, &
                              ExhaustAbsorber(ChillNum)%HWBranchNum, &
                              ExhaustAbsorber(ChillNum)%HWCompNum)

      ELSE
        lHotWaterMassFlowRate = 0.0d0
        CALL ShowRecurringWarningErrorAtEnd('ExhaustAbsorberChillerModel:Heating"'//TRIM(ExhaustAbsorber(ChillNum)%Name)//  &
             '", DeltaTemp = 0 in mass flow calculation',ExhaustAbsorber(ChillNum)%DeltaTempHeatErrCount)
      END IF
      lHotWaterSupplyTemp = HeatSupplySetPointTemp
    CASE (1) ! mass flow rates may not be changed by loop components
      lHotWaterSupplyTemp = HeatSupplySetPointTemp
      lHeatingLoad = ABS(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp)


!DSU this "2" is not a real state for flowLock
    CASE (2) ! chiller is underloaded and mass flow rates has changed to a small amount and Tout drops below Setpoint

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

            !Calculate ThermalEnergy consumption for heating
            ! ThermalEnergy used for heating availCap * HIR * HIR-FT * HIR-FPLR

  lHeatThermalEnergyUseRate = lAvailableHeatingCapacity * lThermalEnergyHeatRatio  &
     * CurveValue(lThermalEnergyHeatFHPLRCurve,lHeatPartLoadRatio)

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

  lExhaustInTemp  = Node(lExhaustAirInletNodeNum)%Temp
  lExhaustInFlow  = Node(lExhaustAirInletNodeNum)%MassFlowRate
  CpAir = PsyCpAirFnWTdb(lExhaustAirHumRat,lExhaustInTemp)
  lExhHeatRecPotentialHeat =  lExhaustInFlow  * Cpair * (  lExhaustInTemp  - AbsLeavingTemp )
    IF (lExhHeatRecPotentialHeat .LT. lHeatThermalEnergyUseRate ) THEN
      IF(ExhaustAbsorber(ChillNum)%ExhTempLTAbsLeavingHeatingTempIndex  == 0)THEN
        CALL ShowWarningError('ChillerHeater:Absorption:DoubleEffect "'//TRIM(ExhaustAbsorber(ChillNum)%Name)//'"')
        CALL ShowContinueError('...Exhaust temperature and flow input from Micro Turbine is not sufficient '&
                          //'to run the chiller during heating .')
        CALL ShowContinueError('...Value of Exhaust air inlet temp ='//TRIM(TrimSigDigits(lExhaustInTemp,4))//' C.')
        CALL ShowContinueError('... and Exhaust air flow rate of '//TRIM(TrimSigDigits(lExhaustInFlow,2))//' kg/s.')
        CALL ShowContinueError('...Value of minimum absorber leaving temp ='//TRIM(TrimSigDigits(AbsLeavingTemp,4))//' C.')
        CALL ShowContinueError('...Either increase the Exhaust temperature (min required = 350 C  )  '&
                          //'or flow or both of Micro Turbine to meet the min available potential criteria.')
        CALL ShowContinueErrorTimeStamp('... Simulation will continue.')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('ChillerHeater:Absorption:DoubleEffect "'//  &
           TRIM(ExhaustAbsorber(ChillNum)%Name)//'":'// &
           ' Exhaust temperature from Micro Turbine is not sufficient to run the chiller during '//  &
             'heating warning continues...', &
           ExhaustAbsorber(ChillNum)%ExhTempLTAbsLeavingHeatingTempIndex,  lExhaustInTemp, AbsLeavingTemp)
! If exhaust is not available, it means the avilable thermal energy is 0.0 and Chiller is not available
   lHeatThermalEnergyUseRate = 0.0d0
   lHeatElectricPower =0.0d0
    lHotWaterSupplyTemp = lHotWaterReturnTemp
    HeatDeltaTemp = 0.0d0
    lFractionOfPeriodRunning = MIN(1.0d0,MAX(lHeatPartLoadRatio,lCoolPartLoadRatio)/lMinPartLoadRat)
   END IF

  IF (lHeatElectricPower .LE. lCoolElectricPower) THEN
    lHeatElectricPower = 0.0d0
  ELSE
    lHeatElectricPower = lHeatElectricPower - lCoolElectricPower
  ENDIF


 ENDIF ! IF(MyLoad==0 .OR. .NOT. Runflag)
  ! Write into the Report Variables except for nodes
  ExhaustAbsorberReport(ChillNum)%HeatingLoad             = lHeatingLoad
  ExhaustAbsorberReport(ChillNum)%HeatThermalEnergyUseRate = lHeatThermalEnergyUseRate
  ExhaustAbsorberReport(ChillNum)%HeatElectricPower        = lHeatElectricPower
  ExhaustAbsorberReport(ChillNum)%HotWaterReturnTemp       = lHotWaterReturnTemp
  ExhaustAbsorberReport(ChillNum)%HotWaterSupplyTemp       = lHotWaterSupplyTemp
  ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate         = lHotWaterMassFlowRate
  ExhaustAbsorberReport(ChillNum)%HeatPartLoadRatio        = lHeatPartLoadRatio
  ExhaustAbsorberReport(ChillNum)%HeatingCapacity          = lAvailableHeatingCapacity
  ExhaustAbsorberReport(ChillNum)%FractionOfPeriodRunning  = lFractionOfPeriodRunning

  ! write the combined heating and cooling ThermalEnergy used and electric used
  ExhaustAbsorberReport(ChillNum)%ThermalEnergyUseRate     = lCoolThermalEnergyUseRate + lHeatThermalEnergyUseRate
  ExhaustAbsorberReport(ChillNum)%ElectricPower            = lCoolElectricPower + lHeatElectricPower
  ExhaustAbsorberReport(ChillNum)%ExhaustInTemp            = lExhaustInTemp
  ExhaustAbsorberReport(ChillNum)%ExhaustInFlow            = lExhaustInFlow
  ExhaustAbsorberReport(ChillNum)%ExhHeatRecPotentialHeat  = lExhHeatRecPotentialHeat

END SUBROUTINE CalcExhaustAbsorberHeaterModel

! End of Absorption Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************

SUBROUTINE UpdateExhaustAbsorberCoolRecords(MyLoad,RunFlag,ChillNum)
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
 ! MBadded
  INTEGER           :: lExhaustAirInletNodeNum ! Node number on the inlet side of the plant
!  INTEGER           :: lExhaustAirOutletNodeNum ! Node number on the outlet side of the plant
  REAL(r64)         :: RptConstant

           ! BEGIN ROUTINE

  lChillReturnNodeNum = ExhaustAbsorber(ChillNum)%ChillReturnNodeNum
  lChillSupplyNodeNum = ExhaustAbsorber(ChillNum)%ChillSupplyNodeNum
  lCondReturnNodeNum  = ExhaustAbsorber(ChillNum)%CondReturnNodeNum
  lCondSupplyNodeNum  = ExhaustAbsorber(ChillNum)%CondSupplyNodeNum


  lExhaustAirInletNodeNum  = ExhaustAbsorber(ChillNum)%ExhaustAirInletNodeNum
  IF (MyLoad==0 .OR. .NOT. RunFlag)THEN
          !set node temperatures


    Node(lChillSupplyNodeNum)%Temp     = Node(lChillReturnNodeNum)%Temp
    Node(lCondSupplyNodeNum)%Temp      = Node(lCondReturnNodeNum)%Temp

    Node(lExhaustAirInletNodeNum)%Temp     = Node(lExhaustAirInletNodeNum)%Temp
          !set node flow rates
          !Update Outlet Conditions so that same as Inlet, so component
          !can be bypassed if necessary
          !FlowResolver/EnforceSplitterContinuity will determine flow
          !received, whether component is running or not.
!
!    Node(lChillReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lChillSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lCondReturnNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
!    Node(lCondSupplyNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
    Node(lExhaustAirInletNodeNum)%MassFlowRate       = ExhaustAbsorberReport(ChillNum)%ExhaustInFlow
  ELSE
          !set node temperatures
    Node(lChillSupplyNodeNum)%Temp                  = ExhaustAbsorberReport(ChillNum)%ChillSupplyTemp
    Node(lCondSupplyNodeNum)%Temp                   = ExhaustAbsorberReport(ChillNum)%CondSupplyTemp
          !set node flow rates;  for these load based models
          !assume that the sufficient evaporator flow rate available
!    Node(lChillReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lChillSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%ChillWaterFlowRate
!    Node(lCondReturnNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
!    Node(lCondSupplyNodeNum)%MassFlowRate           = ExhaustAbsorberReport(ChillNum)%CondWaterFlowRate
  Node(lExhaustAirInletNodeNum)%Temp               = ExhaustAbsorberReport(ChillNum)%ExhaustInTemp
  Node(lExhaustAirInletNodeNum)%MassFlowRate       = ExhaustAbsorberReport(ChillNum)%ExhaustInFlow
  END IF

  ! convert power to energy and instantaneous use to use over the time step
  RptConstant = TimeStepSys * SecInHour
  ExhaustAbsorberReport(ChillNum)%CoolingEnergy      = ExhaustAbsorberReport(ChillNum)%CoolingLoad * RptConstant
  ExhaustAbsorberReport(ChillNum)%TowerEnergy        = ExhaustAbsorberReport(ChillNum)%TowerLoad * RptConstant
  ExhaustAbsorberReport(ChillNum)%ThermalEnergy      = ExhaustAbsorberReport(ChillNum)%ThermalEnergyUseRate * RptConstant
  ExhaustAbsorberReport(ChillNum)%CoolThermalEnergy  = ExhaustAbsorberReport(ChillNum)%CoolThermalEnergyUseRate * RptConstant
  ExhaustAbsorberReport(ChillNum)%ElectricEnergy     = ExhaustAbsorberReport(ChillNum)%ElectricPower * RptConstant
  ExhaustAbsorberReport(ChillNum)%CoolElectricEnergy = ExhaustAbsorberReport(ChillNum)%CoolElectricPower * RptConstant
  IF (ExhaustAbsorberReport(ChillNum)%CoolThermalEnergyUseRate .NE. 0.0d0) THEN
    ExhaustAbsorberReport(ChillNum)%ThermalEnergyCOP          = &
         ExhaustAbsorberReport(ChillNum)%CoolingLoad/ExhaustAbsorberReport(ChillNum)%CoolThermalEnergyUseRate
  ELSE
    ExhaustAbsorberReport(ChillNum)%ThermalEnergyCOP          = 0.0d0
  END IF
!  Node(lChillSupplyNodeNum)%MassFlowRateMaxAvail = Node(lChillReturnNodeNum)%MassFlowRateMaxAvail
!  Node(lChillSupplyNodeNum)%MassFlowRateMinAvail = Node(lChillReturnNodeNum)%MassFlowRateMinAvail
RETURN
END SUBROUTINE UpdateExhaustAbsorberCoolRecords

SUBROUTINE UpdateExhaustAbsorberHeatRecords(MyLoad,RunFlag,ChillNum)
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
  REAL(r64)         :: RptConstant

           ! BEGIN ROUTINE

  lHeatReturnNodeNum  = ExhaustAbsorber(ChillNum)%HeatReturnNodeNum
  lHeatSupplyNodeNum  = ExhaustAbsorber(ChillNum)%HeatSupplyNodeNum


  IF (MyLoad==0 .OR. .NOT. RunFlag)THEN
          !set node temperatures
    Node(lHeatSupplyNodeNum)%Temp     = Node(lHeatReturnNodeNum)%Temp

          !set node flow rates
          !Update Outlet Conditions so that same as Inlet, so component
          !can be bypassed if necessary
          !FlowResolver/EnforceSplitterContinuity will determine flow
          !received, whether component is running or not.
!
!    Node(lHeatReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
!    Node(lHeatSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
  ELSE
          !set node temperatures
    Node(lHeatSupplyNodeNum)%Temp                  = ExhaustAbsorberReport(ChillNum)%HotWaterSupplyTemp
!          !set node flow rates;  for these load based models
!          !assume that the sufficient evaporator flow rate available
!    Node(lHeatReturnNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
!    Node(lHeatSupplyNodeNum)%MassFlowRate          = ExhaustAbsorberReport(ChillNum)%HotWaterFlowRate
  END IF

  ! convert power to energy and instantaneous use to use over the time step
  RptConstant = TimeStepSys * SecInHour
  ExhaustAbsorberReport(ChillNum)%HeatingEnergy      = ExhaustAbsorberReport(ChillNum)%HeatingLoad * RptConstant
  ExhaustAbsorberReport(ChillNum)%ThermalEnergy      = ExhaustAbsorberReport(ChillNum)%ThermalEnergyUseRate * RptConstant
  ExhaustAbsorberReport(ChillNum)%HeatThermalEnergy  = ExhaustAbsorberReport(ChillNum)%HeatThermalEnergyUseRate * RptConstant
  ExhaustAbsorberReport(ChillNum)%ElectricEnergy     = ExhaustAbsorberReport(ChillNum)%ElectricPower * RptConstant
  ExhaustAbsorberReport(ChillNum)%HeatElectricEnergy = ExhaustAbsorberReport(ChillNum)%HeatElectricPower * RptConstant

RETURN
END SUBROUTINE UpdateExhaustAbsorberHeatRecords

! End of Record Keeping subroutines for the Exhasut Fired Absorption Chiller Module
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
END MODULE ChillerExhaustAbsorption

