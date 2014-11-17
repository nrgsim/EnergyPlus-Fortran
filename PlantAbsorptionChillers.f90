!
!  NOTE: This file contains 2 modules:
!   ChillerAbsorption ( for Chiller:Absorption)
!   ChillerIndirectAbsorption (for Chiller:Absorption:Indirect)
!
MODULE ChillerAbsorption  !BLAST aborber Models

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Nov. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the BLAST
          ! absorbers.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the BLAST absorber
          ! is available to meet a loop cooling demand, it calls SimBLAST
          ! absorber which in turn calls the appropriate Absorption Chiller model.
          ! All Absorption Chiller models are based on a polynomial fit of Absorber
          ! performance data.

          ! REFERENCES:
          ! 1. BLAST Users Manual


          ! OTHER NOTES:
          ! The Absorber program from the BLAST family of software can be used
          ! to generate the coefficients for the model.


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataInterfaces
USE DataGlobals,     ONLY: MaxNameLength, InitConvTemp
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
!chiller flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203

          ! DERIVED TYPE DEFINITIONS:
TYPE BLASTAbsorberSpecs
    CHARACTER(len=MaxNameLength) :: Name     = ' '       ! user identifier
    LOGICAL           :: Available           = .false.   ! need an array of logicals--load identifiers of available equipment
    LOGICAL           :: ON                  = .false.   ! simulate the machine at it's operating part load ratio
    REAL(r64)         :: NomCap              = 0.0d0     ! W - design nominal capacity of Absorber
    REAL(r64)         :: NomPumpPower        = 0.0d0     ! W - design nominal capacity of Absorber
    INTEGER           :: FlowMode          = FlowModeNotSet ! one of 3 modes for componet flow during operation
    LOGICAL           :: ModulatedFlowSetToLoop  =.FALSE. ! True if the setpoint is missing at the outlet node
    LOGICAL           :: ModulatedFlowErrDone    =.FALSE. ! true if setpoint warning issued
    REAL(r64)         :: EvapVolFlowRate     = 0.0d0     ! m3/s - design water volumetric flow rate through the evaporator
    REAL(r64)         :: CondVolFlowRate     = 0.0d0     ! m3/s - design water volumetric flow rate through the condenser
    REAL(r64)         :: EvapMassFlowRateMax = 0.0d0     ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
    REAL(r64)         :: CondMassFlowRateMax = 0.0d0     ! Max Design Condeneser Mass Flow Rate [kg/s]
    REAL(r64)         :: GenMassFlowRateMax  = 0.0d0     ! Max Design Generator Mass Flow Rate converted from Volume Flow Rate
    REAL(r64)         :: SizFac              = 0.0d0     ! Sizing factor
    INTEGER           :: EvapInletNodeNum    = 0         ! Node number on the inlet side of the plant
    INTEGER           :: EvapOutletNodeNum   = 0         ! Node number on the outlet side of the plant
    INTEGER           :: CondInletNodeNum    = 0         ! Node number on the inlet side of the condenser
    INTEGER           :: CondOutletNodeNum   = 0         ! Node number on the outlet side of the condenser
    INTEGER           :: GeneratorInletNodeNum  = 0      ! absorber steam inlet node number, water side
    INTEGER           :: GeneratorOutletNodeNum = 0      ! absorber steam outlet node number, water side
    REAL(r64)         :: MinPartLoadRat      = 0.0d0     ! (BLAST MIN) min allowed operating frac full load
    REAL(r64)         :: MaxPartLoadRat      = 0.0d0     ! (BLAST MAX) max allowed operating frac full load
    REAL(r64)         :: OptPartLoadRat      = 0.0d0     ! (BLAST BEST) optimal operating frac full load
    REAL(r64)         :: TempDesCondIn       = 0.0d0     ! C - (BLAST ADJTC(1)The design secondary loop fluid
                                                         ! temperature at the Absorber condenser side inlet
    REAL(r64),DIMENSION(3) :: SteamLoadCoef  = 0.0d0     ! (BLAST RPWRC() ) coeff of full load poly. fit
    REAL(r64),DIMENSION(3) :: PumpPowerCoef  = 0.0d0     ! coeff of pumping power poly. fit
    REAL(r64)         :: TempLowLimitEvapOut = 0.0d0     ! C - low temperature shut off
    INTEGER           :: ErrCount2           = 0         ! error counter
    INTEGER           :: GenHeatSourceType   = 0         ! Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
    REAL(r64)         :: GeneratorVolFlowRate = 0.0d0    ! m3/s - hot water volumetric flow rate through generator
    REAL(r64)         :: GeneratorSubCool    = 0.0d0     ! amount of subcooling in steam generator
    INTEGER           :: SteamFluidIndex     = 0         ! index to generator fluid type
    REAL(r64)         :: GeneratorDeltaTemp  = -99999.d0 ! C - generator fluid temperature difference (water only)
    INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
    INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
    INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
    INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
    INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
    INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
    INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
    INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
    INTEGER           :: GenLoopNum     = 0  ! generator water plant loop index number
    INTEGER           :: GenLoopSideNum = 0  ! generator water plant loop side index
    INTEGER           :: GenBranchNum   = 0  ! generator water plant loop branch index
    INTEGER           :: GenCompNum     = 0  ! generator water plant loop component index
    LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE BLASTAbsorberSpecs

TYPE ReportVars
  REAL(r64)    :: PumpingPower        = 0.0d0  ! reporting: electric pumping power
  REAL(r64)    :: QGenerator          = 0.0d0  ! reporting: steam heat transfer rate
  REAL(r64)    :: QEvap               = 0.0d0  ! reporting: evaporator heat transfer rate
  REAL(r64)    :: QCond               = 0.0d0  ! reporting: condensor heat transfer rate
  REAL(r64)    :: PumpingEnergy       = 0.0d0  ! reporting: electric pumping power
  REAL(r64)    :: GeneratorEnergy     = 0.0d0  ! reporting: steam heat transfer rate
  REAL(r64)    :: EvapEnergy          = 0.0d0  ! reporting: evaporator heat transfer rate
  REAL(r64)    :: CondEnergy          = 0.0d0  ! reporting: condensor heat transfer rate
  REAL(r64)    :: CondInletTemp       = 0.0d0  ! reporting: condenser inlet temperature
  REAL(r64)    :: EvapInletTemp       = 0.0d0  ! reporting: evaporator inlet temperature
  REAL(r64)    :: CondOutletTemp      = 0.0d0  ! reporting: condenser outlet temperature
  REAL(r64)    :: EvapOutletTemp      = 0.0d0  ! reporting: evaporator outlet temperature
  REAL(r64)    :: Evapmdot            = 0.0d0  ! reporting: evaporator mass flow rate
  REAL(r64)    :: Condmdot            = 0.0d0  ! reporting: condenser mass flow rate
  REAL(r64)    :: Genmdot             = 0.0d0  ! reporting: generatore mass flow rate when connected to plant
  REAL(r64)    :: SteamMdot           = 0.0d0  ! reporting: steam mass flow rate
  REAL(r64)    :: ActualCOP           = 0.0d0  ! reporting: coefficient of performance = QEvap/QGenerator
END TYPE ReportVars

          ! MODULE VARIABLE DECLARATIONS:
INTEGER        :: NumBLASTAbsorbers   = 0      ! number of Absorption Chillers specified in input

REAL(r64)      :: CondMassFlowRate    = 0.0d0  ! Kg/s - condenser mass flow rate, water side
REAL(r64)      :: EvapMassFlowRate    = 0.0d0  ! Kg/s - evaporator mass flow rate, water side
REAL(r64)      :: SteamMassFlowRate   = 0.0d0  ! Kg/s - steam mass flow rate, water side
REAL(r64)      :: CondOutletTemp      = 0.0d0  ! C - condenser outlet temperature, water side
REAL(r64)      :: EvapOutletTemp      = 0.0d0  ! C - evaporator outlet temperature, water side
REAL(r64)      :: GenOutletTemp       = 0.0d0  ! C - generator fluid outlet temperature
REAL(r64)      :: SteamOutletEnthalpy = 0.0d0  ! J/kg - generator fluid outlet enthalpy
REAL(r64)      :: PumpingPower        = 0.0d0  ! W - rate of Absorber energy use
REAL(r64)      :: PumpingEnergy       = 0.0d0  ! J - Absorber energy use
REAL(r64)      :: QGenerator          = 0.0d0  ! W - rate of Absorber steam use
REAL(r64)      :: GeneratorEnergy     = 0.0d0  ! J - Absorber steam use
REAL(r64)      :: QEvaporator         = 0.0d0  ! W - rate of heat transfer to the evaporator coil
REAL(r64)      :: EvaporatorEnergy    = 0.0d0  ! J - heat transfer to the evaporator coil
REAL(r64)      :: QCondenser          = 0.0d0  ! W - rate of heat transfer to the condenser coil
REAL(r64)      :: CondenserEnergy     = 0.0d0  ! J - heat transfer to the condenser coil

TYPE (BLASTAbsorberSpecs), ALLOCATABLE, DIMENSION(:)  :: BLASTAbsorber  !dimension to number of machines

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::BLASTAbsorberReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:
PUBLIC  SimBLASTAbsorber
PRIVATE InitBLASTAbsorberModel
PRIVATE CalcBLASTAbsorberModel
PRIVATE GetBLASTAbsorberInput
PRIVATE UpdateBLASTAbsorberRecords
PRIVATE SizeAbsorpChiller

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Absorption Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimBLASTAbsorber(AbsorberType,AbsorberName,EquipFlowCtrl,LoopNum,LoopSide,CompIndex,RunFlag,FirstIteration, &
                            InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor, TempCondInDesign)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Nov. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide, UpdateAbsorberChillerComponentGeneratorSide
  USE DataPlant,      ONLY: TypeOf_Chiller_Absorption

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: AbsorberType  ! type of Absorber
  CHARACTER(len=*), INTENT(IN) :: AbsorberName  ! user specified name of Absorber
  INTEGER, INTENT(IN)    :: EquipFlowCtrl       ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum             ! Plant loop index for where called from
  INTEGER, INTENT(IN)    :: LoopSide            ! Plant loop side index for where called from
  LOGICAL , INTENT(IN)   :: RunFlag             ! simulate Absorber when TRUE
  LOGICAL , INTENT(IN)   :: FirstIteration      ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT) :: InitLoopEquip       ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)    :: MyLoad         ! loop demand component will meet
  REAL(r64), INTENT(INOUT)     :: MinCap           ! Minimum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: MaxCap           ! Maximum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: OptCap           ! Optimal operating capacity of chiller [W]
  INTEGER, INTENT(INOUT)       :: CompIndex        ! Chiller number pointer
  LOGICAL, INTENT(IN)          :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)     :: SizingFactor     ! sizing factor
  REAL(r64), INTENT(INOUT)     :: TempCondInDesign

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE     :: GetInput = .TRUE.! when TRUE, calls subroutine to read input file.
  INTEGER  :: ChillNum        ! Chiller number pointer

          !Get Absorber data from input file
  IF (GetInput) THEN
    CALL GetBLASTAbsorberInput
    GetInput = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(AbsorberName,BLASTAbsorber%Name,NumBLASTAbsorbers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimBLASTAbsorber: Specified Absorber not one of Valid Absorption Chillers='//TRIM(AbsorberName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumBLASTAbsorbers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimBLASTAbsorber:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumBLASTAbsorbers))//  &
                          ', Entered Unit name='//TRIM(AbsorberName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (AbsorberName /= BLASTAbsorber(ChillNum)%Name) THEN
        CALL ShowFatalError('SimBLASTAbsorber: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(AbsorberName)//', stored Unit Name for that index='//  &
                            TRIM(BLASTAbsorber(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF


   ! Initialize Loop Equipment
  IF (InitLoopEquip) THEN
    TempCondInDesign   = BLASTAbsorber(ChillNum)%TempDesCondIn
    CALL InitBLASTAbsorberModel(ChillNum,RunFlag,MyLoad)
    CALL SizeAbsorpChiller(ChillNum)
    IF (LoopNum == BLASTAbsorber(ChillNum)%CWLoopNum) THEN
      MinCap = BLASTAbsorber(ChillNum)%NomCap*BLASTAbsorber(ChillNum)%MinPartLoadRat
      MaxCap = BLASTAbsorber(ChillNum)%NomCap*BLASTAbsorber(ChillNum)%MaxPartLoadRat
      OptCap = BLASTAbsorber(ChillNum)%NomCap*BLASTAbsorber(ChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = BLASTAbsorber(ChillNum)%SizFac
    END IF
    RETURN
  END IF

  ! different actions depending on which loop the component was called from

  IF (LoopNum == BLASTAbsorber(ChillNum)%CWLoopNum) THEN
   ! called from dominant chilled water connection loop side

         !Calculate Load
    CALL InitBLASTAbsorberModel(ChillNum,RunFlag, MyLoad)
    CALL CalcBLASTAbsorberModel(ChillNum,MyLoad,Runflag,FirstIteration, EquipFlowCtrl)
    CALL UpdateBLASTAbsorberRecords(MyLoad,RunFlag,ChillNum)

  ELSEIF (LoopNum ==  BLASTAbsorber(ChillNum)%CDLoopNum ) THEN
    ! Called from non-dominant condenser water connection loop side
    CALL UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOf_Chiller_Absorption, &
                                            BLASTAbsorber(ChillNum)%CondInletNodeNum,      &
                                            BLASTAbsorber(ChillNum)%CondOutletNodeNum,     &
                                            BLASTAbsorberReport(ChillNum)%QCond,           &
                                            BLASTAbsorberReport(ChillNum)%CondInletTemp,   &
                                            BLASTAbsorberReport(ChillNum)%CondOutletTemp , &
                                            BLASTAbsorberReport(ChillNum)%Condmdot, FirstIteration )

  ELSEIF (LoopNum ==  BLASTAbsorber(ChillNum)%GenLoopNum) THEN
    ! Called from non-dominant generator hot water or steam connection loop side
    CALL UpdateAbsorberChillerComponentGeneratorSide(LoopNum, LoopSide, TypeOf_Chiller_Absorption, &
                                            BLASTAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                            BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                                            BLASTAbsorber(ChillNum)%GenHeatSourceType,      &
                                            BLASTAbsorberReport(ChillNum)%QGenerator,       &
                                            BLASTAbsorberReport(ChillNum)%SteamMdot, FirstIteration )

  ELSE
    CALL ShowFatalError('SimBLASTAbsorber: Invalid LoopNum passed=' // &
                                  TRIM(TrimSigDigits(LoopNum))// &
                                  ', Unit name='//TRIM(AbsorberName)// &
                                  ', stored chilled water loop='// &
                                  TRIM(TrimSigDigits(BLASTAbsorber(ChillNum)%CWLoopNum)) // &
                                  ', stored condenser water loop='// &
                                  TRIM(TrimSigDigits(BLASTAbsorber(ChillNum)%CDLoopNum)) // &
                                  ', stored generator loop='// &
                                  TRIM(TrimSigDigits(BLASTAbsorber(ChillNum)%GenLoopNum)) )
  ENDIF


RETURN
END SUBROUTINE SimBLASTAbsorber

! End Absorption Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Absorption Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetBLASTAbsorberInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 1998
            !       MODIFIED:        R. Raustad May 2008 - added generator nodes

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the BLAST Absorption chiller models as shown below:

            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutputReportPredefined
  USE FluidProperties,    ONLY: FindRefrigerant
  USE General, ONLY: RoundSigDigits
  USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE !

            ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: RoutineName='GetBLASTAbsorberInput: ' ! include trailing blank space

            !LOCAL VARIABLES
  INTEGER                     :: AbsorberNum !Absorber counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, ALLOCATABLE, DIMENSION(:) ::  GenInputOutputNodesUsed ! Used for SetupOutputVariable
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
!  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

         !FLOW
  cCurrentModuleObject = 'Chiller:Absorption'

  NumBLASTAbsorbers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumBLASTAbsorbers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
            !See if load distribution manager has already gotten the input
    ErrorsFound=.true.
  ENDIF

  IF (ALLOCATED(BLASTAbsorber))RETURN
         !ALLOCATE ARRAYS
  ALLOCATE (BLASTAbsorber(NumBLASTAbsorbers))
  ALLOCATE(CheckEquipName(NumBLASTAbsorbers))
  CheckEquipName=.TRUE.
  ALLOCATE(GenInputOutputNodesUsed(NumBLASTAbsorbers))
  GenInputOutputNodesUsed=.FALSE.

  ALLOCATE (BLASTAbsorberReport(NumBLASTAbsorbers))

         !LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
  DO AbsorberNum = 1 , NumBLASTAbsorbers
    CALL GetObjectItem(cCurrentModuleObject,AbsorberNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),BLASTAbsorber%Name,AbsorberNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    BLASTAbsorber(AbsorberNum)%Name                = cAlphaArgs(1)
    BLASTAbsorber(AbsorberNum)%NomCap              = rNumericArgs(1)
    BLASTAbsorber(AbsorberNum)%NomPumpPower        = rNumericArgs(2)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
            ! Assign Node Numbers to specified nodes
    BLASTAbsorber(AbsorberNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    BLASTAbsorber(AbsorberNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')

    BLASTAbsorber(AbsorberNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    BLASTAbsorber(AbsorberNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser (not tested) Nodes')

    IF(NumAlphas .GT. 8)THEN
      IF(SameString(cAlphaArgs(9),'HotWater') .OR. SameString(cAlphaArgs(9),'HotWater'))THEN
        BLASTAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Water
      ELSE IF(SameString(cAlphaArgs(9),'Steam') .OR. SameString(cAlphaArgs(9),Blank))THEN
        BLASTAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator heat source type must be Steam or Hot Water.')
        ErrorsFound=.true.
      END IF
    ELSE
      BLASTAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
    END IF

    IF(.NOT. lAlphaFieldBlanks(6)  .AND. .NOT. lAlphaFieldBlanks(7) )THEN
      GenInputOutputNodesUsed(AbsorberNum) = .TRUE.
      IF(BLASTAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
        BLASTAbsorber(AbsorberNum)%GeneratorInletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Inlet,3,ObjectIsNotParent)
        BLASTAbsorber(AbsorberNum)%GeneratorOutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Outlet,3,ObjectIsNotParent)
        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Hot Water Nodes')
      ELSE
        BLASTAbsorber(AbsorberNum)%SteamFluidIndex=FindRefrigerant('STEAM')
        BLASTAbsorber(AbsorberNum)%GeneratorInletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Steam,NodeConnectionType_Inlet,3,ObjectIsNotParent)
        BLASTAbsorber(AbsorberNum)%GeneratorOutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Steam,NodeConnectionType_Outlet,3,ObjectIsNotParent)
        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Steam Nodes')
      END IF
    ELSE IF( (lAlphaFieldBlanks(6) .AND. .NOT. lAlphaFieldBlanks(7)) .OR. &
            (.NOT. lAlphaFieldBlanks(6) .AND. lAlphaFieldBlanks(7)))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator fluid nodes must both be entered (or both left blank).')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)))
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)))
        ErrorsFound=.true.
    ELSE
      IF(BLASTAbsorber(AbsorberNum)%GenHeatSourceType .EQ. NodeType_Water)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.')
        CALL ShowContinueError('...Generator fluid type is set to Steam and the simulation continues.')
        BLASTAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
      END IF
    END IF

            ! Get remaining data
    BLASTAbsorber(AbsorberNum)%MinPartLoadRat      = rNumericArgs(3)
    BLASTAbsorber(AbsorberNum)%MaxPartLoadRat      = rNumericArgs(4)
    BLASTAbsorber(AbsorberNum)%OptPartLoadRat      = rNumericArgs(5)
    BLASTAbsorber(AbsorberNum)%TempDesCondIn       = rNumericArgs(6)
    BLASTAbsorber(AbsorberNum)%EvapVolFlowRate     = rNumericArgs(7)
    BLASTAbsorber(AbsorberNum)%CondVolFlowRate     = rNumericArgs(8)
    BLASTAbsorber(AbsorberNum)%SteamLoadCoef(1)    = rNumericArgs(9)
    BLASTAbsorber(AbsorberNum)%SteamLoadCoef(2)    = rNumericArgs(10)
    BLASTAbsorber(AbsorberNum)%SteamLoadCoef(3)    = rNumericArgs(11)
    BLASTAbsorber(AbsorberNum)%PumpPowerCoef(1)    = rNumericArgs(12)
    BLASTAbsorber(AbsorberNum)%PumpPowerCoef(2)    = rNumericArgs(13)
    BLASTAbsorber(AbsorberNum)%PumpPowerCoef(3)    = rNumericArgs(14)
    BLASTAbsorber(AbsorberNum)%TempLowLimitEvapOut = rNumericArgs(15)

    SELECT CASE (TRIM(cAlphaArgs(8)))
    CASE ( 'CONSTANTFLOW' )
      BLASTAbsorber(AbsorberNum)%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      BLASTAbsorber(AbsorberNum)%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      BLASTAbsorber(AbsorberNum)%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      BLASTAbsorber(AbsorberNum)%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      BLASTAbsorber(AbsorberNum)%FlowMode = NotModulated
    END SELECT

    IF(NumNums .GT. 15)THEN
      BLASTAbsorber(AbsorberNum)%GeneratorVolFlowRate = rNumericArgs(16)
    END IF

    IF(BLASTAbsorber(AbsorberNum)%GeneratorVolFlowRate == 0.0d0 .AND. &
       BLASTAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
       CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(16))//'='//TRIM(RoundSigDigits(rNumericArgs(16),2)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
       CALL ShowContinueError('...Generator water flow rate must be greater than 0'// &
                              ' when absorber generator fluid type is hot water.')
       ErrorsFound=.true.
    END IF

    IF(NumNums .GT. 16)THEN
      BLASTAbsorber(AbsorberNum)%GeneratorSubCool = rNumericArgs(17)
    ELSE
      BLASTAbsorber(AbsorberNum)%GeneratorSubCool = 1.0d0
    END IF

    IF(NumNums .GT. 17)THEN
      BLASTAbsorber(AbsorberNum)%SizFac = rNumericArgs(18)
    ELSE
      BLASTAbsorber(AbsorberNum)%SizFac = 1.0d0
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO AbsorberNum = 1, NumBLASTAbsorbers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          BLASTAbsorberReport(AbsorberNum)%PumpingPower,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          BLASTAbsorberReport(AbsorberNum)%PumpingEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          BLASTAbsorberReport(AbsorberNum)%QEvap,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          BLASTAbsorberReport(AbsorberNum)%EvapEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          BLASTAbsorberReport(AbsorberNum)%EvapInletTemp,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          BLASTAbsorberReport(AbsorberNum)%EvapOutletTemp,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          BLASTAbsorberReport(AbsorberNum)%Evapmdot,'System','Average',BLASTAbsorber(AbsorberNum)%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          BLASTAbsorberReport(AbsorberNum)%QCond,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          BLASTAbsorberReport(AbsorberNum)%CondEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
          BLASTAbsorberReport(AbsorberNum)%CondInletTemp,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
          BLASTAbsorberReport(AbsorberNum)%CondOutletTemp,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
          BLASTAbsorberReport(AbsorberNum)%Condmdot,'System','Average',BLASTAbsorber(AbsorberNum)%Name)

     IF(BLASTAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
       CALL SetupOutputVariable('Chiller Hot Water Consumption Rate [W]', &
            BLASTAbsorberReport(AbsorberNum)%QGenerator,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
       CALL SetupOutputVariable('Chiller Source Hot Water Energy [J]', &
            BLASTAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='CHILLERS',GroupKey='Plant')
     ELSE
       IF(GenInputOutputNodesUsed(AbsorberNum))THEN
         CALL SetupOutputVariable('Chiller Source Steam Rate [W]', &
              BLASTAbsorberReport(AbsorberNum)%QGenerator,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
         CALL SetupOutputVariable('Chiller Source Steam Energy [J]', &
              BLASTAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='CHILLERS',GroupKey='Plant')
       ELSE
         CALL SetupOutputVariable('Chiller Source Steam Rate [W]', &
              BLASTAbsorberReport(AbsorberNum)%QGenerator,'System','Average',BLASTAbsorber(AbsorberNum)%Name)
         CALL SetupOutputVariable('Chiller Source Steam Energy [J]', &
              BLASTAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',BLASTAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='Steam',EndUseKey='Cooling',GroupKey='Plant')
       END IF
     END IF

     CALL SetupOutputVariable('Chiller COP [W/W]', &
          BLASTAbsorberReport(AbsorberNum)%ActualCOP,'System','Average',BLASTAbsorber(AbsorberNum)%Name)

     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', BLASTAbsorber(AbsorberNum)%Name, '[W]', &
                                     BLASTAbsorber(AbsorberNum)%NomCap  )
     ENDIF

  END DO

IF(ALLOCATED(GenInputOutputNodesUsed)) DEALLOCATE(GenInputOutputNodesUsed)

RETURN
END SUBROUTINE GetBLASTAbsorberInput

! End of Get Input subroutines for the Absorption Chiller Module
!******************************************************************************

SUBROUTINE InitBLASTAbsorberModel(ChillNum,RunFlag, MyLoad)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Electric Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_Absorption, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE InputProcessor,  ONLY : SameString
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  Use FluidProperties, ONLY : GetDensityGlycol, GetSatEnthalpyRefrig, GetSatDensityRefrig
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  INTEGER   :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER   :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER   :: LoopCtr            ! Plant loop counter
  INTEGER   :: LoopSideCtr        ! Loop side counter
  INTEGER   :: BranchCtr          ! Plant branch counter
  INTEGER   :: CompCtr            ! Component counter
  LOGICAL   :: errFlag
  LOGICAL   :: FatalError
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: CpWater ! local specific heat
  REAL(r64) :: SteamDensity        ! density of generator steam (when connected to a steam loop)
  REAL(r64) :: EnthSteamOutDry     ! dry enthalpy of steam (quality = 1)
  REAL(r64) :: EnthSteamOutWet     ! wet enthalpy of steam (quality = 0)
  REAL(r64) :: HfgSteam            ! latent heat of steam at constant pressure
  REAL(r64) :: SteamDeltaT         ! amount of sub-cooling of steam condensate
  INTEGER   :: GeneratorInletNode      ! generator inlet node number, steam/water side
  REAL(r64) :: SteamOutletTemp
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: mdotEvap ! local fluid mass flow rate thru evaporator
  REAL(r64) :: mdotCond ! local fluid mass flow rate thru condenser
  REAL(r64) :: mdotGen ! local fluid mass flow rate thru generator

          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumBLASTAbsorbers))
    ALLOCATE(MyEnvrnFlag(NumBLASTAbsorbers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(BLASTAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_Absorption, &
                                 BLASTAbsorber(ChillNum)%CWLoopNum, &
                                 BLASTAbsorber(ChillNum)%CWLoopSideNum, &
                                 BLASTAbsorber(ChillNum)%CWBranchNum, &
                                 BLASTAbsorber(ChillNum)%CWCompNum, &
                                 LowLimitTemp = BLASTAbsorber(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = BLASTAbsorber(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (BLASTAbsorber(ChillNum)%CondInletNodeNum > 0) THEN
      CALL ScanPlantLoopsForObject(BLASTAbsorber(ChillNum)%Name, &
                                   TypeOf_Chiller_Absorption, &
                                   BLASTAbsorber(ChillNum)%CDLoopNum, &
                                   BLASTAbsorber(ChillNum)%CDLoopSideNum, &
                                   BLASTAbsorber(ChillNum)%CDBranchNum, &
                                   BLASTAbsorber(ChillNum)%CDCompNum, &
                                   InletNodeNumber = BLASTAbsorber(ChillNum)%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( BLASTAbsorber(ChillNum)%CWLoopNum,      &
                                          BLASTAbsorber(ChillNum)%CWLoopSideNum,  &
                                          BLASTAbsorber(ChillNum)%CDLoopNum,      &
                                          BLASTAbsorber(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_Absorption, .TRUE. )
    ENDIF
    IF (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) THEN
      CALL ScanPlantLoopsForObject(BLASTAbsorber(ChillNum)%Name,         &
                                   TypeOf_Chiller_Absorption,            &
                                   BLASTAbsorber(ChillNum)%GenLoopNum,   &
                                   BLASTAbsorber(ChillNum)%GenLoopSideNum, &
                                   BLASTAbsorber(ChillNum)%GenBranchNum,   &
                                   BLASTAbsorber(ChillNum)%GenCompNum,     &
                                   InletNodeNumber = BLASTAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( BLASTAbsorber(ChillNum)%CWLoopNum,      &
                                          BLASTAbsorber(ChillNum)%CWLoopSideNum,  &
                                          BLASTAbsorber(ChillNum)%GenLoopNum,     &
                                          BLASTAbsorber(ChillNum)%GenCompNum,     &
                                          TypeOf_Chiller_Absorption, .TRUE. )
    ENDIF

    !Fill in connection data
    IF ( (BLASTAbsorber(ChillNum)%CondInletNodeNum > 0)  .AND. &
         (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) ) THEN
      CALL InterConnectTwoPlantLoopSides( BLASTAbsorber(ChillNum)%CDLoopNum,     &
                                          BLASTAbsorber(ChillNum)%CDLoopSideNum, &
                                          BLASTAbsorber(ChillNum)%GenLoopNum,    &
                                          BLASTAbsorber(ChillNum)%GenCompNum,     &
                                          TypeOf_Chiller_Absorption, .FALSE. )
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitBLASTAbsorberModel: Program terminated due to previous condition(s).')
    ENDIF

    IF (BLASTAbsorber(ChillNum)%FlowMode == ConstantFlow) THEN
      PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%LoopSide(BLASTAbsorber(ChillNum)%CWLoopSideNum)% &
          Branch(BLASTAbsorber(ChillNum)%CWBranchNum)%Comp(BLASTAbsorber(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (BLASTAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) THEN
      PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%LoopSide(BLASTAbsorber(ChillNum)%CWLoopSideNum)% &
          Branch(BLASTAbsorber(ChillNum)%CWBranchNum)%Comp(BLASTAbsorber(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

      IF ((Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. BLASTAbsorber(ChillNum)%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(BLASTAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            BLASTAbsorber(ChillNum)%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(BLASTAbsorber(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. BLASTAbsorber(ChillNum)%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(BLASTAbsorber(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              BLASTAbsorber(ChillNum)%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF

        BLASTAbsorber(ChillNum)%ModulatedFlowSetToLoop = .TRUE.
        Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint = &
           Node(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi = &
           Node(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF

    MyFlag(ChillNum)=.FALSE.
  ENDIF

  CondInletNode  = BLASTAbsorber(ChillNum)%CondInletNodeNum
  CondOutletNode = BLASTAbsorber(ChillNum)%CondOutletNodeNum

          !Initialize critical Demand Side Variables
!  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
!     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN

  IF (MyEnvrnFlag(ChillNum) .AND. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeAbsorpChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                             InitConvTemp, &
                             PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                             'InitBLASTAbsorberModel')

    BLASTAbsorber(ChillNum)%EvapMassFlowRateMax = BLASTAbsorber(ChillNum)%EvapVolFlowRate * rho

    CALL InitComponentNodes(0.d0, BLASTAbsorber(ChillNum)%EvapMassFlowRateMax, &
                              BLASTAbsorber(ChillNum)%EvapInletNodeNum, &
                              BLASTAbsorber(ChillNum)%EvapOutletNodeNum, &
                              BLASTAbsorber(ChillNum)%CWLoopNum, &
                              BLASTAbsorber(ChillNum)%CWLoopSideNum, &
                              BLASTAbsorber(ChillNum)%CWBranchNum, &
                              BLASTAbsorber(ChillNum)%CWCompNum)

    rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                           InitConvTemp, &
                           PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                           'InitBLASTAbsorberModel')

    BLASTAbsorber(ChillNum)%CondMassFlowRateMax = rho * BLASTAbsorber(ChillNum)%CondVolFlowRate

    CALL InitComponentNodes(0.d0, BLASTAbsorber(ChillNum)%CondMassFlowRateMax, &
                            CondInletNode, CondOutletNode, &
                            BLASTAbsorber(ChillNum)%CDLoopNum, &
                            BLASTAbsorber(ChillNum)%CDLoopSideNum, &
                            BLASTAbsorber(ChillNum)%CDBranchNum, &
                            BLASTAbsorber(ChillNum)%CDCompNum)
    Node(CondInletNode)%Temp = BLASTAbsorber(ChillNum)%TempDesCondIn


    IF (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) THEN

      IF(BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
        rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                               'InitBLASTAbsorberModel')

        BLASTAbsorber(ChillNum)%GenMassFlowRateMax = rho * BLASTAbsorber(ChillNum)%GeneratorVolFlowRate
      ELSEIF (BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Steam ) THEN

        QGenerator = (BLASTAbsorber(ChillNum)%SteamLoadCoef(1) + BLASTAbsorber(ChillNum)%SteamLoadCoef(2) + &
                      BLASTAbsorber(ChillNum)%SteamLoadCoef(3)) * BLASTAbsorber(ChillNum)%NomCap
        GeneratorInletNode = BLASTAbsorber(ChillNum)%GeneratorInletNodeNum
        EnthSteamOutDry   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,1.0d0, &
                                                 BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                                 'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
        EnthSteamOutWet   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,0.0d0, &
                                                 BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                                 'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
        SteamDeltaT       = BLASTAbsorber(ChillNum)%GeneratorSubCool
        SteamOutletTemp   = Node(GeneratorInletNode)%Temp - SteamDeltaT
        HfgSteam          = EnthSteamOutDry - EnthSteamOutWet
        SteamDensity      = GetSatDensityRefrig('STEAM',Node(GeneratorInletNode)%Temp,1.0d0, &
                                                 BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                                'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
        CpWater           = GetDensityGlycol('WATER', SteamOutletTemp, DummyWaterIndex,  &
                            'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
        BLASTAbsorber(ChillNum)%GenMassFlowRateMax = QGenerator/(HfgSteam+CpWater*SteamDeltaT)
      ENDIF


      CALL InitComponentNodes(0.d0, BLASTAbsorber(ChillNum)%GenMassFlowRateMax, &
                              BLASTAbsorber(ChillNum)%GeneratorInletNodeNum, &
                              BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                              BLASTAbsorber(ChillNum)%GenLoopNum, &
                              BLASTAbsorber(ChillNum)%GenLoopSideNum, &
                              BLASTAbsorber(ChillNum)%GenBranchNum, &
                              BLASTAbsorber(ChillNum)%GenCompNum)
    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  ! every time inits

  IF ((BLASTAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated)  &
      .AND. BLASTAbsorber(ChillNum)%ModulatedFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
    mdotEvap = BLASTAbsorber(ChillNum)%EvapMassFlowRateMax
    mdotCond = BLASTAbsorber(ChillNum)%CondMassFlowRateMax
    mdotGen  = BLASTAbsorber(ChillNum)%GenMassFlowRateMax
  ELSE
    mdotEvap = 0.d0
    mdotCond = 0.d0
    mdotGen  = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdotEvap, &
                              BLASTAbsorber(ChillNum)%EvapInletNodeNum, &
                              BLASTAbsorber(ChillNum)%EvapOutletNodeNum,&
                              BLASTAbsorber(ChillNum)%CWLoopNum,     &
                              BLASTAbsorber(ChillNum)%CWLoopSideNum, &
                              BLASTAbsorber(ChillNum)%CWBranchNum,   &
                              BLASTAbsorber(ChillNum)%CWCompNum)

  CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,  &
                                BLASTAbsorber(ChillNum)%CDLoopNum,     &
                                BLASTAbsorber(ChillNum)%CDLoopSideNum, &
                                BLASTAbsorber(ChillNum)%CDBranchNum,   &
                                BLASTAbsorber(ChillNum)%CDCompNum)

  IF (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) THEN

    CALL SetComponentFlowRate( mdotGen, &
                                BLASTAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                                BLASTAbsorber(ChillNum)%GenLoopNum,     &
                                BLASTAbsorber(ChillNum)%GenLoopSideNum, &
                                BLASTAbsorber(ChillNum)%GenBranchNum,   &
                                BLASTAbsorber(ChillNum)%GenCompNum)

  ENDIF


  RETURN

END SUBROUTINE InitBLASTAbsorberModel

SUBROUTINE SizeAbsorpChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2008
          !       MODIFIED:      R. Raustad May 2008 - added generator node sizing
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize, MyPlantSizingIndex
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties
!  USE BranchInputManager, ONLY: MyPlantSizingIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizIndex         ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum           ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum       ! Plant Sizing index for condenser loop
  INTEGER             :: PltSizSteamNum      ! Plant Sizing index for steam heating loop
  INTEGER             :: PltSizHeatingNum    ! Plant Sizing index for how water heating loop
  REAL(r64)           :: SteamInputRatNom    ! nominal energy input ratio (steam or hot water)
  REAL(r64)           :: SteamDensity        ! density of generator steam (when connected to a steam loop)
  REAL(r64)           :: EnthSteamOutDry     ! dry enthalpy of steam (quality = 1)
  REAL(r64)           :: EnthSteamOutWet     ! wet enthalpy of steam (quality = 0)
  REAL(r64)           :: HfgSteam            ! latent heat of steam at constant pressure
  REAL(r64)           :: SteamDeltaT         ! amount of sub-cooling of steam condensate
  REAL(r64)           :: SteamMassFlowRate   ! steam mass flow rate through generator
  REAL(r64)           :: CpWater             ! specific heat of generator fluid (when connected to a hot water loop)
  REAL(r64)           :: RhoWater            ! density of water
  REAL(r64)           :: GeneratorOutletTemp ! outlet temperature of generator
  LOGICAL             :: ErrorsFound         ! If errors detected in input
  LOGICAL             :: LoopErrorsFound     !
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: rho ! local fluid density
  REAL(r64)           :: Cp  ! local specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpNomPumpPower ! local nominal pump power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  REAL(r64)           :: tmpGeneratorVolFlowRate ! local generator design volume flow rate
  INTEGER             :: DummWaterIndex = 1

  PltSizNum = 0
  PltSizCondNum = 0
  PltSizHeatingNum = 0
  PltSizSteamNum = 0
  ErrorsFound = .FALSE.
  SteamInputRatNom = BLASTAbsorber(ChillNum)%SteamLoadCoef(1) + BLASTAbsorber(ChillNum)%SteamLoadCoef(2) +   &
                               BLASTAbsorber(ChillNum)%SteamLoadCoef(3)
  ! init local temporary version in case of partial/mixed autosizing
  tmpNomCap          = BLASTAbsorber(ChillNum)%NomCap
  tmpNomPumpPower    = BLASTAbsorber(ChillNum)%NomPumpPower
  tmpEvapVolFlowRate = BLASTAbsorber(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = BLASTAbsorber(ChillNum)%CondVolFlowRate
  tmpGeneratorVolFlowRate = BLASTAbsorber(ChillNum)%GeneratorVolFlowRate

  ! find the appropriate Plant Sizing object
  PltSizNum = PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%PlantSizNum

  IF (BLASTAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
    PltSizCondNum = PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%PlantSizNum
!    PltSizCondNum = MyCondPlantSizingIndex('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
!                                         BLASTAbsorber(ChillNum)%CondInletNodeNum, &
!                                         BLASTAbsorber(ChillNum)%CondOutletNodeNum, LoopErrorsFound)
  ENDIF

  IF (BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Steam) THEN
    IF (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 .AND. BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum > 0) THEN
      PltSizSteamNum = MyPlantSizingIndex('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                                          BLASTAbsorber(ChillNum)%GeneratorInletNodeNum, &
                                          BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum, LoopErrorsFound)
    ELSE
      DO PltSizIndex=1,NumPltSizInput
        IF(PlantSizData(PltSizIndex)%LoopType == SteamLoop)THEN
          PltSizSteamNum = PltSizIndex
        END IF
      END DO
    END IF
  ELSE
    IF (BLASTAbsorber(ChillNum)%GeneratorInletNodeNum > 0 .AND. BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum > 0) THEN
      PltSizHeatingNum = MyPlantSizingIndex('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                                          BLASTAbsorber(ChillNum)%GeneratorInletNodeNum, &
                                          BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum, LoopErrorsFound)
    ELSE
      DO PltSizIndex=1,NumPltSizInput
        IF(PlantSizData(PltSizIndex)%LoopType == HeatingLoop)THEN
          PltSizHeatingNum = PltSizIndex
        END IF
      END DO
    END IF
  END IF

  IF (BLASTAbsorber(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        Cp = GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')

        rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate * BLASTAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize)  BLASTAbsorber(ChillNum)%NomCap = tmpNomCap

      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize)  BLASTAbsorber(ChillNum)%NomCap = tmpNomCap

      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                              'Nominal Capacity [W]', BLASTAbsorber(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:Absorption object='//TRIM(BLASTAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (BLASTAbsorber(ChillNum)%NomPumpPower  == AutoSize) THEN
    IF (PlantSizesOkayToFinalize) THEN
     ! the DOE-2 EIR for single stage absorption chiller
      tmpNomPumpPower = 0.0045d0 * BLASTAbsorber(ChillNum)%NomCap
      BLASTAbsorber(ChillNum)%NomPumpPower = tmpNomPumpPower

      CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                              'Nominal Pumping Power [W]', BLASTAbsorber(ChillNum)%NomPumpPower)
    ELSE
      tmpNomPumpPower = 0.0045d0 * tmpNomCap
    ENDIF
  END IF

  IF (BLASTAbsorber(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * BLASTAbsorber(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) BLASTAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)   BLASTAbsorber(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              BLASTAbsorber(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in CHILLER:ABSORPTION object='//TRIM(BLASTAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (BLASTAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (BLASTAbsorber(ChillNum)%EvapVolFlowRate >= SmallWaterVolFlow) THEN
!       QCondenser = QEvaporator + QGenerator + PumpingPower

        Cp = GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   BLASTAbsorber(ChillNum)%TempDesCondIn, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')

        rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')
        tmpCondVolFlowRate = tmpNomCap * &
                    (1.0d0 + SteamInputRatNom + tmpNomPumpPower/tmpNomCap) / &
                    ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize)  BLASTAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize)  BLASTAbsorber(ChillNum)%CondVolFlowRate = 0.0d0
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                                                             'Design Condenser Water Flow Rate [m3/s]', &
                                                              BLASTAbsorber(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in CHILLER:ABSORPTION object='//TRIM(BLASTAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  CALL RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum)%CondInletNodeNum, tmpCondVolFlowRate)

  IF (BLASTAbsorber(ChillNum)%GeneratorVolFlowRate == AutoSize) THEN
    IF (PltSizSteamNum > 0 .AND. BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Steam .OR. &
        PltSizHeatingNum > 0 .AND. BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water) THEN
      IF (BLASTAbsorber(ChillNum)%EvapVolFlowRate >= SmallWaterVolFlow) THEN
        IF(BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
          CpWater =  GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   PlantSizData(PltSizHeatingNum)%ExitTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')
          SteamDeltaT = MAX(0.5d0,PlantSizData(PltSizHeatingNum)%DeltaT)
          RhoWater = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   (PlantSizData(PltSizHeatingNum)%ExitTemp - SteamDeltaT), &
                                   PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')

          IF (PlantSizesOkayToFinalize) Then
            BLASTAbsorber(ChillNum)%GeneratorVolFlowRate = (BLASTAbsorber(ChillNum)%NomCap * SteamInputRatNom)/ &
                                                      (CpWater * SteamDeltaT * RhoWater)

            CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                              'Design Generator Fluid Flow Rate [m3/s]', &
                              BLASTAbsorber(ChillNum)%GeneratorVolFlowRate)
          ELSE
            tmpGeneratorVolFlowRate = (tmpNomCap * SteamInputRatNom)/ &
                                                      (CpWater * SteamDeltaT * RhoWater)

          ENDIF

        ELSE
          SteamDensity = GetSatDensityRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,1.0d0, &
                                              BLASTAbsorber(ChillNum)%SteamFluidIndex,'SizeAbsorptionChiller')
          SteamDeltaT         = PlantSizData(PltSizSteamNum)%DeltaT
          GeneratorOutletTemp = PlantSizData(PltSizSteamNum)%ExitTemp - SteamDeltaT

          EnthSteamOutDry   = GetSatEnthalpyRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,1.0d0, &
                                                   BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                                  'Chiller:Absorption'//TRIM(BLASTAbsorber(ChillNum)%Name))
          EnthSteamOutWet   = GetSatEnthalpyRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,0.0d0, &
                                                   BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                                  'Chiller:Absorption'//TRIM(BLASTAbsorber(ChillNum)%Name))
          CpWater =  GetSpecificHeatGlycol('WATER', GeneratorOutletTemp, DummWaterIndex,  'SizeAbsorpChiller')
          HfgSteam          = EnthSteamOutDry - EnthSteamOutWet
          IF (PlantSizesOkayToFinalize) THEN
            SteamMassFlowRate = (BLASTAbsorber(ChillNum)%NomCap * SteamInputRatNom) / &
                              ( (HfgSteam) + (SteamDeltaT * CpWater) )


            BLASTAbsorber(ChillNum)%GeneratorVolFlowRate = SteamMassFlowRate / SteamDensity

            CALL ReportSizingOutput('Chiller:Absorption', BLASTAbsorber(ChillNum)%Name, &
                                'Design Generator Fluid Flow Rate [m3/s]', &
                                BLASTAbsorber(ChillNum)%GeneratorVolFlowRate)
          ELSE
            SteamMassFlowRate = (tmpNomCap * SteamInputRatNom) / &
                              ( (HfgSteam) + (SteamDeltaT * CpWater) )
            tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity
          ENDIF
        END IF
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          BLASTAbsorber(ChillNum)%GeneratorVolFlowRate = 0.0d0
        ELSE
          tmpGeneratorVolFlowRate = 0.d0
        ENDIF
      END IF
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.')
      CALL ShowContinueError(' For steam loops, use a steam Sizing:Plant object.')
      CALL ShowContinueError(' For hot water loops, use a heating Sizing:Plant object.')
      CALL ShowContinueError('Occurs in Chiller:Absorption object='//TRIM(BLASTAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
  IF (PlantSizesOkayToFinalize) THEN
    CALL RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum)%GeneratorInletNodeNum,BLASTAbsorber(ChillNum)%GeneratorVolFlowRate)
  ELSE
    CALL RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum)%GeneratorInletNodeNum,tmpGeneratorVolFlowRate)
  ENDIF


  IF(BLASTAbsorber(ChillNum)%GeneratorDeltaTemp == AutoSize)THEN
    IF(PltSizHeatingNum > 0 .AND. BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water) THEN
      BLASTAbsorber(ChillNum)%GeneratorDeltaTemp = MAX(0.5d0,PlantSizData(PltSizHeatingNum)%DeltaT)
    ELSE IF(BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
      Cp =  GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')
      rho = GetDensityGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeAbsorpChiller')

      IF (PlantSizesOkayToFinalize) THEN
        BLASTAbsorber(ChillNum)%GeneratorDeltaTemp = (SteamInputRatNom * BLASTAbsorber(ChillNum)%NomCap)/ &
                (Cp * rho * BLASTAbsorber(ChillNum)%GeneratorVolFlowRate)
      ENDIF
    END IF
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) THEN
   !create predefined report
    equipName = BLASTAbsorber(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Absorption')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,'n/a')
    CALL PreDefTableEntry(pdchMechNomCap,equipName,BLASTAbsorber(ChillNum)%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeAbsorpChiller

! Beginning of Absorber model Subroutines
! *****************************************************************************

SUBROUTINE CalcBLASTAbsorberModel(ChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       April 1999, May 2000- Taecheol Kim
          !                      May   2008 - R. Raustad, added generator nodes
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression Absorber using the BLAST model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1.  BLAST User Manual
          ! 2.  Absorber User Manual

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, WarmupFlag
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys
  USE DataPlant,       ONLY : DeltaTemptol, PlantLoop, CompSetPtBasedSchemeType, &
                              SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE FluidProperties
  USE General,         ONLY : TrimSigDigits
  USE PlantUtilities,  ONLY : SetComponentFlowRate

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillNum        ! Absorber number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep !unused1208
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when Absorber operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl   ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: SteamLoadFactor     ! coefficients to poly curve fit
  REAL(r64), DIMENSION(3)     :: ElectricLoadFactor  ! coefficients to poly curve fit
  REAL(r64)              :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn          ! C - (BLAST ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign    ! C - (BLAST ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: EvapInletTemp       ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp       ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut         ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: AbsorberNomCap      ! Absorber nominal capacity
  REAL(r64)              :: NomPumpPower        ! Absorber nominal pumping power
  REAL(r64)              :: PartLoadRat         ! part load ratio for efficiency calc
  REAL(r64)              :: OperPartLoadRat     ! Operating part load ratio
  REAL(r64)              :: EvapDeltaTemp       ! C - evaporator temperature difference, water side
  REAL(r64)              :: TempLowLimitEout    ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: SteamInputRat       ! energy input ratio
  REAL(r64)              :: ElectricInputRat    ! energy input ratio
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER                :: GeneratorInletNode      ! generator inlet node number, steam/water side
  INTEGER                :: GeneratorOutletNode     ! generator outlet node number, steam/water side
  REAL(r64)              :: EnthSteamOutDry     ! enthalpy of dry steam at generator inlet
  REAL(r64)              :: EnthSteamOutWet     ! enthalpy of wet steam at generator inlet
  REAL(r64)              :: HfgSteam            ! heat of vaporization of steam
  LOGICAL,ALLOCATABLE,DIMENSION(:),SAVE  :: MyEnvironFlag
  LOGICAL,ALLOCATABLE,DIMENSION(:),SAVE  :: MyEnvironSteamFlag
  LOGICAL, SAVE :: OneTimeFlag = .true.
  REAL(r64)              :: FRAC
!  LOGICAL,SAVE           :: PossibleSubCooling
  REAL(r64)              :: CpFluid             ! local specific heat of fluid
  REAL(r64)              :: SteamDeltaT
  REAL(r64)              :: SteamDensity
  REAL(r64)              :: SteamOutletTemp
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: DummyWaterIndex = 1


          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  SteamMassFlowRate          = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  QGenerator                 = 0.0d0
  PumpingEnergy              = 0.0d0
  CondenserEnergy            = 0.0d0
  EvaporatorEnergy           = 0.0d0
  GeneratorEnergy            = 0.0d0
  PumpingPower               = 0.0d0
  FRAC                       = 1.0d0
  EvapInletNode  = BLASTAbsorber(ChillNum)%EvapInletNodeNum
  EvapOutletNode = BLASTAbsorber(ChillNum)%EvapOutletNodeNum
  CondInletNode  = BLASTAbsorber(ChillNum)%CondInletNodeNum
  CondOutletNode = BLASTAbsorber(ChillNum)%CondOutletNodeNum
  GeneratorInletNode  = BLASTAbsorber(ChillNum)%GeneratorInletNodeNum
  GeneratorOutletNode = BLASTAbsorber(ChillNum)%GeneratorOutletNodeNum

          !If no loop demand or Absorber OFF, return
  IF(MyLoad >= 0.0d0 .OR. .NOT. RunFlag) THEN !off or heating
    IF(EquipFlowCtrl == ControlType_SeriesActive) EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    RETURN
  END IF

         !set module level Absorber inlet and temperature variables
  EvapInletTemp  = Node(EvapInletNode)%Temp
  CondInletTemp  = Node(CondInletNode)%Temp

        !Set the condenser mass flow rates
  CondMassFlowRate = Node(CondInletNode)%MassFlowRate


      !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  SteamLoadFactor    = BLASTAbsorber(ChillNum)%SteamLoadCoef
  ElectricLoadFactor = BLASTAbsorber(ChillNum)%PumpPowerCoef
  MinPartLoadRat     = BLASTAbsorber(ChillNum)%MinPartLoadRat
  MaxPartLoadRat     = BLASTAbsorber(ChillNum)%MaxPartLoadRat
  TempCondInDesign   = BLASTAbsorber(ChillNum)%TempDesCondIn
  AbsorberNomCap     = BLASTAbsorber(ChillNum)%NomCap
  NomPumpPower       = BLASTAbsorber(ChillNum)%NomPumpPower
  TempCondIn         = Node(BLASTAbsorber(ChillNum)%CondInletNodeNum)%Temp
  TempEvapOut        = Node(BLASTAbsorber(ChillNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = BLASTAbsorber(ChillNum)%TempLowLimitEvapOut
  LoopNum            = BLASTAbsorber(ChillNum)%CWLoopNum
  LoopSideNum        = BLASTAbsorber(ChillNum)%CWLoopSideNum

  CpFluid            = GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                          EvapInletTemp, &
                                          PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                          'CalcBLASTAbsorberModel')

          ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
          ! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==0) THEN
    BLASTAbsorber(ChillNum)%PossibleSubCooling = .FALSE.
    QEvaporator = ABS(MyLoad)
     ! limit by max capacity
    QEvaporator = MIN(QEvaporator, (BLASTAbsorber(ChillNum)%MaxPartLoadRat *  BLASTAbsorber(ChillNum)%NomCap) )

     ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    IF ((BLASTAbsorber(ChillNum)%FlowMode == ConstantFlow) &
        .OR. (BLASTAbsorber(ChillNum)%FlowMode == NotModulated))Then
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate

      IF (EvapMassFlowRate /= 0.0D0) THEN

        EvapDeltaTemp = QEvaporator/EvapMassFlowRate / CpFluid
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      EvapOutletTemp = EvapInletTemp - EvapDeltaTemp

    ELSE IF (BLASTAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) THEN
        ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      SELECT CASE (PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
      CASE (DualSetpointDeadband)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
      END SELECT
      IF (EvapDeltaTemp /= 0) THEN

        EvapMassFlowRate = ABS(QEvaporator/CpFluid/EvapDeltaTemp)
        IF((EvapMassFlowRate - BLASTAbsorber(ChillNum)%EvapMassFlowRateMax) .GT. MassFlowTolerance)   &
                       BLASTAbsorber(ChillNum)%PossibleSubCooling = .TRUE.
          !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(BLASTAbsorber(ChillNum)%EvapMassFlowRateMax, EvapMassFlowRate)
        CALL SetComponentFlowRate(EvapMassFlowRate, &
                                  BLASTAbsorber(ChillNum)%EvapInletNodeNum, &
                                  BLASTAbsorber(ChillNum)%EvapOutletNodeNum,&
                                  BLASTAbsorber(ChillNum)%CWLoopNum,     &
                                  BLASTAbsorber(ChillNum)%CWLoopSideNum, &
                                  BLASTAbsorber(ChillNum)%CWBranchNum,   &
                                  BLASTAbsorber(ChillNum)%CWCompNum)
        SELECT CASE (PlantLoop(BLASTAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme )
        CASE (SingleSetpoint)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
        CASE (DualSetpointDeadband)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
        END SELECT
      ELSE
        EvapMassFlowRate=0.0d0

        EvapOutletTemp = Node(EvapInletNode)%Temp

        CALL ShowRecurringWarningErrorAtEnd('CalcBLASTAbsorberModel: Name="'//  &
            TRIM(BLASTAbsorber(ChillNum)%Name)//  &
           '" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.',  &
           BLASTAbsorber(ChillNum)%ErrCount2)
      END IF
    END IF  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    IF(BLASTAbsorber(ChillNum)%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CpFluid
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE
      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        IF ((BLASTAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BLASTAbsorber(ChillNum)%CWBranchNum) &
              %Comp(BLASTAbsorber(ChillNum)%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
        ENDIF
      CASE (DualSetpointDeadband)
        IF ((BLASTAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BLASTAbsorber(ChillNum)%CWBranchNum) &
              %Comp(BLASTAbsorber(ChillNum)%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
        ENDIF
      END SELECT
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*CpFluid*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
      !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
      END IF
    END IF
      ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CpFluid
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

  END IF  !This is the end of the FlowLock Block

      !Calculate part load ratio for efficiency calcs. If this part load ratio is greater than
      !Min PLR it will be used for calculations too.
  PartLoadRat = MAX(MinPartLoadRat,MIN(QEvaporator / AbsorberNomCap,MaxPartLoadRat))

      !In case myload is less than the Min PLR load, the power and steam input should be adjusted
      !for cycling. The ratios used however are based on MinPLR.
  OperPartLoadRat = QEvaporator/AbsorberNomCap

  IF(OperPartLoadRat .LT. PartLoadRat) THEN
    FRAC = MIN(1.0d0,OperPartLoadRat/MinPartLoadRat)
  ELSE
    FRAC = 1.0d0
  END IF

        !Calculate steam input ratio
  SteamInputRat = SteamLoadFactor(1)/PartLoadRat + SteamLoadFactor(2) + SteamLoadFactor(3) * PartLoadRat

        !Calculate electric input ratio
  ElectricInputRat = ElectricLoadFactor(1) + ElectricLoadFactor(2)*PartLoadRat + ElectricLoadFactor(3) * PartLoadRat**2

        !Calculate electric energy input
  PumpingPower = ElectricInputRat * NomPumpPower * FRAC

        !Calculate steam load
  QGenerator = SteamInputRat * QEvaporator * FRAC

  IF(EvapMassFlowRate == 0.0d0) THEN
    QGenerator = 0.0d0
    EvapOutletTemp = Node(EvapInletNode)%Temp
    PumpingPower = 0.0d0
  END IF

  QCondenser = QEvaporator + QGenerator + PumpingPower

  CpFluid    = GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                          CondInletTemp, &
                                          PlantLoop(BLASTAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                          'CalcBLASTAbsorberModel')

  IF (CondMassFlowRate > MassFlowTolerance) THEN
    CondOutletTemp = QCondenser/CondMassFlowRate/CpFluid + CondInletTemp
  ELSE

    CondOutletTemp = CondInletTemp
    CondMassFlowRate = 0.d0
    QCondenser = 0.d0
    RETURN
    ! V7 plant upgrade, no longer fatal here anymore, set some things and return
  END IF

  IF (GeneratorInletNode .GT. 0) THEN
!         Hot water plant is used for the generator
    IF(BLASTAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
      CpFluid    = GetSpecificHeatGlycol(PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                          Node(GeneratorInletNode)%Temp, &
                                          PlantLoop(BLASTAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                          'CalcBLASTAbsorberModel')
      IF ((BLASTAbsorber(ChillNum)%FlowMode == ConstantFlow) &
          .OR. (BLASTAbsorber(ChillNum)%FlowMode == NotModulated )) THEN
        SteamMassFlowRate = BLASTAbsorber(ChillNum)%GenMassFlowRateMax
      ELSE
        SteamMassFlowRate = QGenerator/CpFluid/BLASTAbsorber(ChillNum)%GeneratorDeltaTemp
      END IF

      Call SetComponentFlowRate(SteamMassFlowRate, &
                                GeneratorInletNode, GeneratorOutletNode, &
                                BLASTAbsorber(ChillNum)%GenLoopNum,     &
                                BLASTAbsorber(ChillNum)%GenLoopSideNum, &
                                BLASTAbsorber(ChillNum)%GenBranchNum,   &
                                BLASTAbsorber(ChillNum)%GenCompNum)

      IF(SteamMassFlowRate .LE. 0.0d0)THEN
        GenOutletTemp       = Node(GeneratorInletNode)%Temp
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy
      ELSE
        GenOutletTemp       = Node(GeneratorInletNode)%Temp - QGenerator/(CpFluid*SteamMassFlowRate)
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy - QGenerator/SteamMassFlowRate
      END IF

    ELSE ! using a steam plant for the generator

      EnthSteamOutDry   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,1.0d0, &
                                             BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                            'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
      EnthSteamOutWet   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,0.0d0, &
                                             BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                            'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name))
      SteamDeltaT       = BLASTAbsorber(ChillNum)%GeneratorSubCool
      SteamOutletTemp   = Node(GeneratorInletNode)%Temp - SteamDeltaT
      HfgSteam          = EnthSteamOutDry - EnthSteamOutWet
      CpFluid           = GetSpecificHeatGlycol('WATER', SteamOutletTemp, DummyWaterIndex, &
                                                'CALC Chiller:Absorption '//TRIM(BLASTAbsorber(ChillNum)%Name) )
      SteamMassFlowRate = QGenerator/(HfgSteam+CpFluid *SteamDeltaT)
      Call SetComponentFlowRate(SteamMassFlowRate, &
                                GeneratorInletNode, GeneratorOutletNode, &
                                BLASTAbsorber(ChillNum)%GenLoopNum,     &
                                BLASTAbsorber(ChillNum)%GenLoopSideNum, &
                                BLASTAbsorber(ChillNum)%GenBranchNum,   &
                                BLASTAbsorber(ChillNum)%GenCompNum)

      IF(SteamMassFlowRate .LE. 0.0d0)THEN
        GenOutletTemp = Node(GeneratorInletNode)%Temp
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy
      ELSE
        GenOutletTemp = Node(GeneratorInletNode)%Temp - SteamDeltaT
        SteamOutletEnthalpy = GetSatEnthalpyRefrig('STEAM',GenOutletTemp,0.0d0, &
                                            BLASTAbsorber(ChillNum)%SteamFluidIndex, &
                                         'Chiller:Absorption'//TRIM(BLASTAbsorber(ChillNum)%Name))
        SteamOutletEnthalpy = SteamOutletEnthalpy - CpFluid*SteamDeltaT
      END IF

    END IF
  END IF ! IF(GeneratorInletNode .GT. 0)THEN

  !convert power to energy
  GeneratorEnergy     = QGenerator*TimeStepSys*SecInHour
  EvaporatorEnergy    = QEvaporator*TimeStepSys*SecInHour
  CondenserEnergy     = QCondenser*TimeStepSys*SecInHour
  PumpingEnergy       = PumpingPower*TimeStepSys*SecInHour

  RETURN
END SUBROUTINE CalcBLASTAbsorberModel

! End of Absorption Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************

SUBROUTINE UpdateBLASTAbsorberRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE PlantUtilities, ONLY: SafeCopyPlantNode

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)      :: MyLoad    ! current load
  LOGICAL,   INTENT(IN)      :: RunFlag   ! TRUE if Absorber operating
  INTEGER,   INTENT(IN)      :: Num       ! Absorber number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER                :: GeneratorInletNode      ! generator inlet node number, steam/water side
  INTEGER                :: GeneratorOutletNode     ! generator outlet node number, steam/water side


  EvapInletNode       = BLASTAbsorber(Num)%EvapInletNodeNum
  EvapOutletNode      = BLASTAbsorber(Num)%EvapOutletNodeNum
  CondInletNode       = BLASTAbsorber(Num)%CondInletNodeNum
  CondOutletNode      = BLASTAbsorber(Num)%CondOutletNodeNum
  GeneratorInletNode  = BLASTAbsorber(Num)%GeneratorInletNodeNum
  GeneratorOutletNode = BLASTAbsorber(Num)%GeneratorOutletNodeNum


  IF (MyLoad>=0 .OR. .NOT. RunFlag)THEN
   !set node conditions
    CALL SafeCopyPlantNode(EvapInletNode , EvapOutletNode)
    CALL SafeCopyPlantNode(CondInletNode , CondOutletNode)

    BLASTAbsorberReport(Num)%PumpingPower     = 0.0d0
    BLASTAbsorberReport(Num)%QEvap            = 0.0d0
    BLASTAbsorberReport(Num)%QCond            = 0.0d0
    BLASTAbsorberReport(Num)%QGenerator       = 0.0d0
    BLASTAbsorberReport(Num)%PumpingEnergy    = 0.0d0
    BLASTAbsorberReport(Num)%EvapEnergy       = 0.0d0
    BLASTAbsorberReport(Num)%CondEnergy       = 0.0d0
    BLASTAbsorberReport(Num)%GeneratorEnergy  = 0.0d0
    BLASTAbsorberReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    BLASTAbsorberReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    BLASTAbsorberReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    BLASTAbsorberReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    BLASTAbsorberReport(Num)%Evapmdot         = 0.0d0
    BLASTAbsorberReport(Num)%Condmdot         = 0.0d0
    BLASTAbsorberReport(Num)%Genmdot         = 0.0d0
    BLASTAbsorberReport(Num)%ActualCOP        = 0.0d0

    IF(GeneratorInletNode .GT. 0)THEN
      CALL SafeCopyPlantNode(GeneratorInletNode , GeneratorOutletNode)
    END IF

  ELSE
          !set node conditions
    CALL SafeCopyPlantNode(EvapInletNode , EvapOutletNode)
    CALL SafeCopyPlantNode(CondInletNode , CondOutletNode)
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    BLASTAbsorberReport(Num)%PumpingPower     = PumpingPower
    BLASTAbsorberReport(Num)%QEvap            = QEvaporator
    BLASTAbsorberReport(Num)%QCond            = QCondenser
    BLASTAbsorberReport(Num)%QGenerator       = QGenerator
    BLASTAbsorberReport(Num)%PumpingEnergy    = PumpingEnergy
    BLASTAbsorberReport(Num)%EvapEnergy       = EvaporatorEnergy
    BLASTAbsorberReport(Num)%CondEnergy       = CondenserEnergy
    BLASTAbsorberReport(Num)%GeneratorEnergy  = GeneratorEnergy
    BLASTAbsorberReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    BLASTAbsorberReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    BLASTAbsorberReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    BLASTAbsorberReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    BLASTAbsorberReport(Num)%Evapmdot         = EvapMassFlowRate
    BLASTAbsorberReport(Num)%Condmdot         = CondMassFlowRate
    BLASTAbsorberReport(Num)%Genmdot          = SteamMassFlowRate
    IF (QGenerator .NE. 0.0d0) THEN
      BLASTAbsorberReport(Num)%ActualCOP      = QEvaporator/QGenerator
    ELSE
      BLASTAbsorberReport(Num)%ActualCOP      = 0.0d0
    END IF

    IF(GeneratorInletNode .GT. 0)THEN
      CALL SafeCopyPlantNode(GeneratorInletNode , GeneratorOutletNode)
      Node(GeneratorOutletNode)%Temp          = GenOutletTemp
    END IF

  END IF

RETURN
END SUBROUTINE UpdateBLASTAbsorberRecords

! End of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************


END MODULE ChillerAbsorption
!***************************************************************************************************************************
!       New Module in File
!***************************************************************************************************************************
MODULE ChillerIndirectAbsorption  !revised BLAST aborber Model

          ! MODULE INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the revised BLAST
          ! absorbers. New curve objects are included.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the revised BLAST absorber
          ! is available to meet a loop cooling demand, it calls SimIndirectAbsorber
          ! which in turn calls the appropriate Indirect Absorption Chiller model.
          ! All Absorption Chiller models are based on a polynomial fit of Absorber
          ! performance data.

          ! REFERENCES:
          ! 1. BLAST Users Manual


          ! OTHER NOTES:
          ! Manufacturers performance data can be used to generate the coefficients for the model.


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataInterfaces
USE DataGlobals ,   ONLY : MaxNameLength, InitConvTemp
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
!chiller flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203

          ! DERIVED TYPE DEFINITIONS:
TYPE IndirectAbsorberSpecs
    CHARACTER(len=MaxNameLength) :: Name = ' '    ! user identifier
    REAL(r64)  :: NomCap                 = 0.0d0  ! W - design nominal capacity of Absorber
    REAL(r64)  :: NomPumpPower           = 0.0d0  ! W - design nominal capacity of Absorber
    REAL(r64)  :: EvapVolFlowRate        = 0.0d0  ! m3/s - design nominal water volumetric flow rate through the evaporator
    REAL(r64)  :: CondVolFlowRate        = 0.0d0  ! m3/s - design nominal water volumetric flow rate through the condenser
    REAL(r64)  :: EvapMassFlowRateMax    = 0.0d0  ! kg/s - Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
    REAL(r64)  :: CondMassFlowRateMax    = 0.0d0     ! Max Design Condeneser Mass Flow Rate [kg/s]
    REAL(r64)  :: GenMassFlowRateMax     = 0.0d0  ! kg/s - Max Design Generator Mass Flow Rate converted from Volume Flow Rate
    REAL(r64)  :: MinPartLoadRat         = 0.0d0  ! (BLAST MIN) min allowed operating frac full load
    REAL(r64)  :: MaxPartLoadRat         = 0.0d0  ! (BLAST MAX) max allowed operating frac full load
    REAL(r64)  :: OptPartLoadRat         = 0.0d0  ! (BLAST BEST) optimal operating frac full load
    REAL(r64)  :: TempDesCondIn          = 0.0d0  ! C - (BLAST ADJTC(1)The design secondary loop fluid
                                                  ! temperature at the Absorber condenser side inlet
    REAL(r64)  :: MinCondInletTemp       = 0.0d0  ! C - minimum condenser inlet temperature for chiller operation
    REAL(r64)  :: MinGeneratorInletTemp  = 0.0d0  ! C - minimum generator inlet temperature for chiller operation
    REAL(r64)  :: TempLowLimitEvapOut    = 0.0d0  ! C - low temperature shut off
    REAL(r64)  :: GeneratorVolFlowRate   = 0.0d0  ! m3/s - hot water volumetric flow rate through generator
    REAL(r64)  :: GeneratorSubCool       = 0.0d0  ! C - amount of subcooling in steam generator
    REAL(r64)  :: LoopSubCool            = 0.0d0  ! C - amount of subcooling in steam generator
    REAL(r64)  :: GeneratorDeltaTemp     = -99999.d0 ! C - generator fluid temperature difference (water only)
    REAL(r64)  :: SizFac                 = 0.0d0     ! Sizing factor
    INTEGER    :: EvapInletNodeNum       = 0      ! Node number on the inlet side of the plant
    INTEGER    :: EvapOutletNodeNum      = 0      ! Node number on the outlet side of the plant
    INTEGER    :: CondInletNodeNum       = 0      ! Node number on the inlet side of the condenser
    INTEGER    :: CondOutletNodeNum      = 0      ! Node number on the outlet side of the condenser
    INTEGER    :: GeneratorInletNodeNum  = 0      ! Generator inlet node number, steam/water side
    INTEGER    :: GeneratorOutletNodeNum = 0      ! Generator outlet node number, steam/water side
    INTEGER    :: GeneratorInputCurvePtr = 0      ! Index to steam use curve as a function of PLR
    INTEGER    :: PumpPowerCurvePtr      = 0      ! Index to pump power curve as a function of PLR
    INTEGER    :: CapFCondenserTempPtr   = 0      ! Index to capacity as a function of absorber temp curve
    INTEGER    :: CapFEvaporatorTempPtr  = 0      ! Index to capacity as a function of evaporator temp curve
    INTEGER    :: CapFGeneratorTempPtr   = 0      ! Index to capacity as a function of generator temp curve
    INTEGER    :: HeatInputFCondTempPtr  = 0      ! Index to generator heat input as a function of absorber temp
    INTEGER    :: HeatInputFEvapTempPtr  = 0      ! Index to generator heat input as a function of absorber temp
    INTEGER    :: ErrCount2              = 0      ! error counter
    INTEGER    :: GenHeatSourceType      = 0      ! Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
    INTEGER    :: SteamFluidIndex        = 0      ! index to generator fluid type
    LOGICAL    :: Available              =.false. ! need an array of logicals--load identifiers of available equipment
    LOGICAL    :: ON                     =.false. ! simulate the machine at it's operating part load ratio
    INTEGER    :: FlowMode               = FlowModeNotSet ! one of 3 modes for componet flow during operation
    LOGICAL    :: ModulatedFlowSetToLoop =.FALSE. ! True if the setpoint is missing at the outlet node
    LOGICAL    :: ModulatedFlowErrDone   =.FALSE. ! true if setpoint warning issued
    INTEGER    :: MinCondInletTempCtr    = 0      ! Low condenser temp warning message counter
    INTEGER    :: MinCondInletTempIndex  = 0      ! Low condenser temp warning message index
    INTEGER    :: MinGenInletTempCtr     = 0      ! Low generator temp warning message counter
    INTEGER    :: MinGenInletTempIndex   = 0      ! Low generator temp warning message index
    INTEGER    :: CWLoopNum     = 0  ! chilled water plant loop index number
    INTEGER    :: CWLoopSideNum = 0  ! chilled water plant loop side index
    INTEGER    :: CWBranchNum   = 0  ! chilled water plant loop branch index
    INTEGER    :: CWCompNum     = 0  ! chilled water plant loop component index
    INTEGER    :: CDLoopNum     = 0  ! condenser water plant loop index number
    INTEGER    :: CDLoopSideNum = 0  ! condenser water plant loop side index
    INTEGER    :: CDBranchNum   = 0  ! condenser water plant loop branch index
    INTEGER    :: CDCompNum     = 0  ! condenser water plant loop component index
    INTEGER    :: GenLoopNum     = 0  ! generator plant loop index number
    INTEGER    :: GenLoopSideNum = 0  ! generator plant loop side index
    INTEGER    :: GenBranchNum   = 0  ! generator plant loop branch index
    INTEGER    :: GenCompNum     = 0  ! generator plant loop component index
    LOGICAL    :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE IndirectAbsorberSpecs

TYPE ReportVars
  REAL(r64)    :: PumpingPower         = 0.0d0    ! reporting: W - electric pumping power
  REAL(r64)    :: QGenerator           = 0.0d0    ! reporting: W - steam heat transfer rate
  REAL(r64)    :: QEvap                = 0.0d0    ! reporting: W - evaporator heat transfer rate
  REAL(r64)    :: QCond                = 0.0d0    ! reporting: W - condensor heat transfer rate
  REAL(r64)    :: PumpingEnergy        = 0.0d0    ! reporting: J - electric pumping power
  REAL(r64)    :: GeneratorEnergy      = 0.0d0    ! reporting: J - steam heat transfer rate
  REAL(r64)    :: EvapEnergy           = 0.0d0    ! reporting: J - evaporator heat transfer rate
  REAL(r64)    :: CondEnergy           = 0.0d0    ! reporting: J - condensor heat transfer rate
  REAL(r64)    :: CondInletTemp        = 0.0d0    ! reporting: C - condenser inlet temperature
  REAL(r64)    :: EvapInletTemp        = 0.0d0    ! reporting: C - evaporator inlet temperature
  REAL(r64)    :: CondOutletTemp       = 0.0d0    ! reporting: C - condenser outlet temperature
  REAL(r64)    :: EvapOutletTemp       = 0.0d0    ! reporting: C - evaporator outlet temperature
  REAL(r64)    :: Evapmdot             = 0.0d0    ! reporting: kg/ - evaporator mass flow rate
  REAL(r64)    :: Condmdot             = 0.0d0    ! reporting: kg/ - condenser mass flow rate
  REAL(r64)    :: Genmdot              = 0.0d0    ! reporting: generatore mass flow rate when connected to plant
  REAL(r64)    :: SteamMdot            = 0.0d0    ! reporting: kg/s - steam mass flow rate
  REAL(r64)    :: ActualCOP            = 0.0d0    ! reporting: coefficient of performance = QEvap/QGenerator
  REAL(r64)    :: ChillerPartLoadRatio = 0.0d0    ! reporting: part-load ratio
  REAL(r64)    :: ChillerCyclingFrac   = 0.0d0    ! reporting: chiller on/off cycling fraction
  REAL(r64)    :: LoopLoss             = 0.0d0    ! reporting: W - loop loss from absorber outlet to condensate pump inlet
END TYPE ReportVars

          ! MODULE VARIABLE DECLARATIONS:
INTEGER        :: NumIndirectAbsorbers = 0        ! number of Absorption Chillers specified in input
REAL(r64)      :: CondMassFlowRate     = 0.0d0    ! Kg/s - condenser mass flow rate, water side
REAL(r64)      :: EvapMassFlowRate     = 0.0d0    ! Kg/s - evaporator mass flow rate, water side
REAL(r64)      :: GenMassFlowRate      = 0.0d0    ! Kg/s - steam mass flow rate, water side
REAL(r64)      :: CondOutletTemp       = 0.0d0    ! C - condenser outlet temperature, water side
REAL(r64)      :: EvapOutletTemp       = 0.0d0    ! C - evaporator outlet temperature, water side
REAL(r64)      :: GenOutletTemp        = 0.0d0    ! C - generator fluid outlet temperature
REAL(r64)      :: SteamOutletEnthalpy  = 0.0d0    ! J/kg - generator fluid outlet enthalpy
REAL(r64)      :: PumpingPower         = 0.0d0    ! W - rate of Absorber energy use
REAL(r64)      :: PumpingEnergy        = 0.0d0    ! J - Absorber energy use
REAL(r64)      :: QGenerator           = 0.0d0    ! W - rate of Absorber steam use
REAL(r64)      :: GeneratorEnergy      = 0.0d0    ! J - Absorber steam use
REAL(r64)      :: QEvaporator          = 0.0d0    ! W - rate of heat transfer to the evaporator coil
REAL(r64)      :: EvaporatorEnergy     = 0.0d0    ! J - heat transfer to the evaporator coil
REAL(r64)      :: QCondenser           = 0.0d0    ! W - rate of heat transfer to the condenser coil
REAL(r64)      :: CondenserEnergy      = 0.0d0    ! J - heat transfer to the condenser coil
REAL(r64)      :: EnergyLossToEnvironment = 0.0d0 ! J - piping energy loss from generator outlet to pump inlet
REAL(r64)      :: ChillerONOFFCyclingFrac = 0.0d0 ! fraction of time chiller is on

TYPE (IndirectAbsorberSpecs), ALLOCATABLE, DIMENSION(:)  :: IndirectAbsorber  !dimension to number of machines

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::IndirectAbsorberReport

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:
PUBLIC  SimIndirectAbsorber
PRIVATE InitIndirectAbsorpChiller
PRIVATE CalcIndirectAbsorberModel
PRIVATE GetIndirectAbsorberInput
PRIVATE UpdateIndirectAbsorberRecords
PRIVATE SizeIndirectAbsorpChiller

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Absorption Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimIndirectAbsorber(AbsorberType,AbsorberName,EquipFlowCtrl,LoopNum,LoopSide,CompIndex,RunFlag,FirstIteration, &
                            InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor,TempCondInDesign)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the Indirect Absorption Chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide, UpdateAbsorberChillerComponentGeneratorSide
  USE DataPlant,      ONLY: TypeOf_Chiller_Indirect_Absorption

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: AbsorberType   ! type of Absorber
  CHARACTER(len=*), INTENT(IN)    :: AbsorberName   ! user specified name of Absorber
  INTEGER,          INTENT(IN)    :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER,          INTENT(IN)    :: LoopNum             ! Plant loop index for where called from
  INTEGER,          INTENT(IN)    :: LoopSide            ! Plant loop side index for where called from
  LOGICAL,          INTENT(IN)    :: RunFlag        ! simulate Absorber when TRUE
  LOGICAL,          INTENT(IN)    :: FirstIteration ! initialize variables when TRUE
  LOGICAL,          INTENT(INOUT) :: InitLoopEquip  ! If not zero, calculate the max load for operating conditions
  REAL(r64),        INTENT(INOUT) :: MyLoad         ! loop demand component will meet
  REAL(r64),        INTENT(INOUT) :: MinCap         ! W - minimum operating capacity of Absorber
  REAL(r64),        INTENT(INOUT) :: MaxCap         ! W - maximum operating capacity of Absorber
  REAL(r64),        INTENT(INOUT) :: OptCap         ! W - optimal operating capacity of Absorber
  INTEGER,          INTENT(INOUT) :: CompIndex        ! Chiller number pointer
  LOGICAL,          INTENT(IN)    :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64),        INTENT(OUT)   :: SizingFactor     ! sizing factor
  REAL(r64),        INTENT(OUT)   :: TempCondInDesign

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE     :: GetInput = .TRUE. ! when TRUE, calls subroutine to read input file.
  INTEGER  :: ChillNum        ! Chiller number pointer

  IF ( CompIndex .NE. 0 ) THEN
     TempCondInDesign   = IndirectAbsorber(CompIndex)%TempDesCondIn
  END IF

          !Get Absorber data from input file
  IF (GetInput) THEN
    CALL GetIndirectAbsorberInput
    GetInput = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(AbsorberName,IndirectAbsorber%Name,NumIndirectAbsorbers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimIndirectAbsorber: Specified chiller not one of Valid Absorption Chillers='//TRIM(AbsorberName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumIndirectAbsorbers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimIndirectAbsorber:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumIndirectAbsorbers))//  &
                          ', Entered Unit name='//TRIM(AbsorberName))
    ENDIF
    IF (AbsorberName /= IndirectAbsorber(ChillNum)%Name) THEN
      CALL ShowFatalError('SimIndirectAbsorber: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Unit name='//TRIM(AbsorberName)//', stored Unit Name for that index='//  &
                          TRIM(IndirectAbsorber(ChillNum)%Name))
    ENDIF
  ENDIF




   ! Initialize Loop Equipment
  IF (InitLoopEquip) THEN
    CALL InitIndirectAbsorpChiller(ChillNum,RunFlag, MyLoad)
    CALL SizeIndirectAbsorpChiller(ChillNum)
    IF (LoopNum == IndirectAbsorber(ChillNum)%CWLoopNum ) THEN
      MinCap = IndirectAbsorber(ChillNum)%NomCap*IndirectAbsorber(ChillNum)%MinPartLoadRat
      MaxCap = IndirectAbsorber(ChillNum)%NomCap*IndirectAbsorber(ChillNum)%MaxPartLoadRat
      OptCap = IndirectAbsorber(ChillNum)%NomCap*IndirectAbsorber(ChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      ChillNum = FindItemInList(AbsorberName,IndirectAbsorber%Name,NumIndirectAbsorbers)
      IF (ChillNum /= 0) THEN
        SizingFactor = IndirectAbsorber(ChillNum)%SizFac
      END IF
    END IF
    RETURN
  END IF

  IF (LoopNum == IndirectAbsorber(ChillNum)%CWLoopNum ) THEN

    CALL InitIndirectAbsorpChiller(ChillNum,RunFlag, MyLoad)
    CALL CalcIndirectAbsorberModel(ChillNum,MyLoad,Runflag,FirstIteration, EquipFlowCtrl)
    CALL UpdateIndirectAbsorberRecords(MyLoad,RunFlag,ChillNum)

  ELSEIF (LoopNum == IndirectAbsorber(ChillNum)%CDLoopNum ) THEN
    ! Called from non-dominant condenser water connection loop side
    CALL UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOf_Chiller_Indirect_Absorption, &
                                            IndirectAbsorber(ChillNum)%CondInletNodeNum,      &
                                            IndirectAbsorber(ChillNum)%CondOutletNodeNum,     &
                                            IndirectAbsorberReport(ChillNum)%QCond,           &
                                            IndirectAbsorberReport(ChillNum)%CondInletTemp,   &
                                            IndirectAbsorberReport(ChillNum)%CondOutletTemp , &
                                            IndirectAbsorberReport(ChillNum)%Condmdot , FirstIteration)

  ELSEIF (LoopNum == IndirectAbsorber(ChillNum)%GenLoopNum ) THEN
    ! Called from non-dominant generator hot water or steam connection loop side
    CALL UpdateAbsorberChillerComponentGeneratorSide(LoopNum, LoopSide, TypeOf_Chiller_Indirect_Absorption, &
                                            IndirectAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                            IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                                            IndirectAbsorber(ChillNum)%GenHeatSourceType,      &
                                            IndirectAbsorberReport(ChillNum)%QGenerator,       &
                                            IndirectAbsorberReport(ChillNum)%SteamMdot , FirstIteration)

  ELSE
    CALL ShowFatalError('SimIndirectAbsorber: Invalid LoopNum passed=' // &
                                  TRIM(TrimSigDigits(LoopNum))// &
                                  ', Unit name='//TRIM(AbsorberName)// &
                                  ', stored chilled water loop='// &
                                  TRIM(TrimSigDigits(IndirectAbsorber(ChillNum)%CWLoopNum)) // &
                                  ', stored condenser water loop='// &
                                  TRIM(TrimSigDigits(IndirectAbsorber(ChillNum)%CDLoopNum)) // &
                                  ', stored generator loop='// &
                                  TRIM(TrimSigDigits(IndirectAbsorber(ChillNum)%GenLoopNum)) )
  ENDIF

RETURN
END SUBROUTINE SimIndirectAbsorber

! End Absorption Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Absorption Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetIndirectAbsorberInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          R. Raustad (FSEC)
            !       DATE WRITTEN:    May 2008

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Indirect Absorption chiller models as shown below:


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE DataIPShortCuts
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE GlobalNames,           ONLY: VerifyUniqueChillerName
  USE OutputReportPredefined
  USE FluidProperties,       ONLY: FindRefrigerant
  USE CurveManager,          ONLY: GetCurveIndex, GetCurveType, CurveValue
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE !

            ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: RoutineName='GetIndirectAbsorberInput: ' ! include trailing blank space

            !LOCAL VARIABLES
  INTEGER                     :: AbsorberNum ! Absorber counter
  INTEGER                     :: NumAlphas   ! Number of elements in the alpha array
  INTEGER                     :: NumNums     ! Number of elements in the numeric array
  INTEGER                     :: IOStat      ! IO Status when calling get input subroutine
  LOGICAL, SAVE               :: ErrorsFound=.false.
  LOGICAL                     :: IsNotOK     ! Flag to verify name
  LOGICAL                     :: IsBlank     ! Flag for blank name
  LOGICAL                     :: errflag     ! GetInput error flag
  LOGICAL, ALLOCATABLE, DIMENSION(:) ::  GenInputOutputNodesUsed ! Used for SetupOutputVariable

         !FLOW
  cCurrentModuleObject = 'Chiller:Absorption:Indirect'
  NumIndirectAbsorbers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumIndirectAbsorbers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
            !See if load distribution manager has already gotten the input
    ErrorsFound=.true.
  ENDIF

  IF (ALLOCATED(IndirectAbsorber))RETURN
         !ALLOCATE ARRAYS
  ALLOCATE (IndirectAbsorber(NumIndirectAbsorbers))

  ALLOCATE (IndirectAbsorberReport(NumIndirectAbsorbers))

  ALLOCATE(GenInputOutputNodesUsed(NumIndirectAbsorbers))
  GenInputOutputNodesUsed=.FALSE.

         !LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
  DO AbsorberNum = 1 , NumIndirectAbsorbers
    CALL GetObjectItem(cCurrentModuleObject,AbsorberNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT, &
                    NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),IndirectAbsorber%Name,AbsorberNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    IndirectAbsorber(AbsorberNum)%Name                = cAlphaArgs(1)
    IndirectAbsorber(AbsorberNum)%NomCap              = rNumericArgs(1)
    IndirectAbsorber(AbsorberNum)%NomPumpPower        = rNumericArgs(2)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
            ! Assign Node Numbers to specified nodes
    IndirectAbsorber(AbsorberNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    IndirectAbsorber(AbsorberNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')

    IndirectAbsorber(AbsorberNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    IndirectAbsorber(AbsorberNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser (not tested) Nodes')

    IndirectAbsorber(AbsorberNum)%GeneratorInputCurvePtr = GetCurveIndex(cAlphaArgs(7))
    IF (IndirectAbsorber(AbsorberNum)%GeneratorInputCurvePtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%GeneratorInputCurvePtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Generator Heat Input function of part-load ratio curve type for this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%GeneratorInputCurvePtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

    IndirectAbsorber(AbsorberNum)%PumpPowerCurvePtr      = GetCurveIndex(cAlphaArgs(8))
    IF (IndirectAbsorber(AbsorberNum)%PumpPowerCurvePtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%PumpPowerCurvePtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Pump Electric Input function of part-load ratio curve type for this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%PumpPowerCurvePtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

    IF(NumAlphas .GT. 15)THEN
      IF(SameString(cAlphaArgs(16),'HotWater') .OR. SameString(cAlphaArgs(16),'HotWater'))THEN
        IndirectAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Water
!       Default to Steam if left blank
      ELSE IF(SameString(cAlphaArgs(16),'Steam') .OR. SameString(cAlphaArgs(16),Blank))THEN
        IndirectAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
      ELSE
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator heat source type must be Steam or Hot Water.')
        CALL ShowContinueError('...Entered generator heat source type = '//TRIM(cAlphaArgs(16)))
        ErrorsFound=.true.
      END IF
    ELSE
!     Default to Steam if not entered as input
      IndirectAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
    END IF

    IF(.NOT. SameString(cAlphaArgs(9),Blank) .AND. .NOT. SameString(cAlphaArgs(10),Blank))THEN
      GenInputOutputNodesUsed(AbsorberNum) = .TRUE.
      IF(IndirectAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
        IndirectAbsorber(AbsorberNum)%GeneratorInletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Inlet,3,ObjectIsNotParent)
        IndirectAbsorber(AbsorberNum)%GeneratorOutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Outlet,3,ObjectIsNotParent)
        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(9),cAlphaArgs(10),'Hot Water Nodes')
      ELSE
        IndirectAbsorber(AbsorberNum)%SteamFluidIndex=FindRefrigerant('Steam')
        IndirectAbsorber(AbsorberNum)%GeneratorInletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Steam,NodeConnectionType_Inlet,3,ObjectIsNotParent)
        IndirectAbsorber(AbsorberNum)%GeneratorOutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Steam,NodeConnectionType_Outlet,3,ObjectIsNotParent)
        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(9),cAlphaArgs(10),'Steam Nodes')
      END IF
    ELSE IF((SameString(cAlphaArgs(9),Blank) .AND. .NOT. SameString(cAlphaArgs(10),Blank)) .OR. &
            (.NOT. SameString(cAlphaArgs(9),Blank) .AND. SameString(cAlphaArgs(10),Blank)))THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator fluid nodes must both be entered (or both left blank).')
        CALL ShowContinueError('...Generator fluid inlet node  = '//TRIM(cAlphaArgs(9)))
        CALL ShowContinueError('...Generator fluid outlet node = '//TRIM(cAlphaArgs(10)))
        ErrorsFound=.true.
    ELSE
!     Generator fluid type must be steam if generator inlet/outlet nodes are not used
      IF(IndirectAbsorber(AbsorberNum)%GenHeatSourceType .EQ. NodeType_Water)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.')
        CALL ShowContinueError('...Generator fluid type is set to Steam and the simulation continues.')
        IndirectAbsorber(AbsorberNum)%GenHeatSourceType = NodeType_Steam
      END IF
    END IF

    SELECT CASE (TRIM(cAlphaArgs(6)))
    CASE ( 'CONSTANTFLOW' )
      IndirectAbsorber(AbsorberNum)%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      IndirectAbsorber(AbsorberNum)%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      IndirectAbsorber(AbsorberNum)%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      IndirectAbsorber(AbsorberNum)%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      IndirectAbsorber(AbsorberNum)%FlowMode = NotModulated
    END SELECT

    IndirectAbsorber(AbsorberNum)%CapFCondenserTempPtr  = GetCurveIndex(cAlphaArgs(11))
    IF (IndirectAbsorber(AbsorberNum)%CapFCondenserTempPtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFCondenserTempPtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Capacity Correction function of condenser temperature curve type for this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFCondenserTempPtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

    IndirectAbsorber(AbsorberNum)%CapFEvaporatorTempPtr = GetCurveIndex(cAlphaArgs(12))
    IF (IndirectAbsorber(AbsorberNum)%CapFEvaporatorTempPtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFEvaporatorTempPtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Capacity Correction function of evaporator temperature curve type for this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFCondenserTempPtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

    IndirectAbsorber(AbsorberNum)%CapFGeneratorTempPtr  = GetCurveIndex(cAlphaArgs(13))
    IF (IndirectAbsorber(AbsorberNum)%CapFGeneratorTempPtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFGeneratorTempPtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        IF(IndirectAbsorber(AbsorberNum)%GenHeatSourceType .EQ. NodeType_Water)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
          CALL ShowContinueError('...illegal Capacity Correction function of generator temperature curve type for this object.')
          CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%CapFGeneratorTempPtr)))
          ErrorsFound=.true.
        END IF
      END SELECT
    END IF

    IndirectAbsorber(AbsorberNum)%HeatInputFCondTempPtr = GetCurveIndex(cAlphaArgs(14))
    IF (IndirectAbsorber(AbsorberNum)%HeatInputFCondTempPtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%HeatInputFCondTempPtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Generator Heat Input Correction function of condenser temperature curve type for '// &
                               'this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%HeatInputFCondTempPtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

    IndirectAbsorber(AbsorberNum)%HeatInputFEvapTempPtr = GetCurveIndex(cAlphaArgs(15))
    IF (IndirectAbsorber(AbsorberNum)%HeatInputFEvapTempPtr .GT. 0) THEN
      ! Verify Curve Object, only legal types are Quadratic or Cubic
      SELECT CASE(GetCurveType(IndirectAbsorber(AbsorberNum)%HeatInputFEvapTempPtr))
      CASE('QUADRATIC', 'CUBIC')
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(IndirectAbsorber(AbsorberNum)%Name)//'"')
        CALL ShowContinueError('...illegal Generator Heat Input Correction function of evaporator temperature curve type for '// &
                               'this object.')
        CALL ShowContinueError('...Curve type = '//TRIM(GetCurveType(IndirectAbsorber(AbsorberNum)%HeatInputFEvapTempPtr)))
        ErrorsFound=.true.
      END SELECT
    END IF

            ! Get remaining data
    IndirectAbsorber(AbsorberNum)%MinPartLoadRat      = rNumericArgs(3)
    IndirectAbsorber(AbsorberNum)%MaxPartLoadRat      = rNumericArgs(4)
    IndirectAbsorber(AbsorberNum)%OptPartLoadRat      = rNumericArgs(5)
    IndirectAbsorber(AbsorberNum)%TempDesCondIn       = rNumericArgs(6)
    IndirectAbsorber(AbsorberNum)%MinCondInletTemp    = rNumericArgs(7)
    IndirectAbsorber(AbsorberNum)%TempLowLimitEvapOut = rNumericArgs(8)
    IndirectAbsorber(AbsorberNum)%EvapVolFlowRate     = rNumericArgs(9)
    IndirectAbsorber(AbsorberNum)%CondVolFlowRate     = rNumericArgs(10)

    IF(NumNums .GT. 10)THEN
      IndirectAbsorber(AbsorberNum)%GeneratorVolFlowRate = rNumericArgs(11)
    END IF

    IF(IndirectAbsorber(AbsorberNum)%GeneratorVolFlowRate == 0.0d0 .AND. &
       IndirectAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
       CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Name='//TRIM(cAlphaArgs(1)))
       CALL ShowContinueError('...Generator water flow rate must be greater than 0'// &
                              ' when absorber generator fluid type is hot water.')
       ErrorsFound=.true.
    END IF

    IF(NumNums .GT. 11)THEN
      IndirectAbsorber(AbsorberNum)%MinGeneratorInletTemp = rNumericArgs(12)
    ELSE
      IndirectAbsorber(AbsorberNum)%MinGeneratorInletTemp = 0.0d0
    END IF

    IF(NumNums .GT. 12)THEN
      IndirectAbsorber(AbsorberNum)%GeneratorSubCool = rNumericArgs(13)
    ELSE
      IndirectAbsorber(AbsorberNum)%GeneratorSubCool = 0.0d0
    END IF

    IF(NumNums .GT. 13)THEN
      IndirectAbsorber(AbsorberNum)%LoopSubCool = rNumericArgs(14)
    ELSE
      IndirectAbsorber(AbsorberNum)%LoopSubCool = 0.0d0
    END IF

    IF(NumNums .GT. 14)THEN
      IndirectAbsorber(AbsorberNum)%SizFac = rNumericArgs(15)
    ELSE
      IndirectAbsorber(AbsorberNum)%SizFac = 1.0d0
    END IF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting Chiller:Absorption:Indirect')
  ENDIF

  DO AbsorberNum = 1, NumIndirectAbsorbers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          IndirectAbsorberReport(AbsorberNum)%PumpingPower,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          IndirectAbsorberReport(AbsorberNum)%PumpingEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          IndirectAbsorberReport(AbsorberNum)%QEvap,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          IndirectAbsorberReport(AbsorberNum)%EvapEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          IndirectAbsorberReport(AbsorberNum)%EvapInletTemp,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          IndirectAbsorberReport(AbsorberNum)%EvapOutletTemp,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          IndirectAbsorberReport(AbsorberNum)%Evapmdot,'System','Average',IndirectAbsorber(AbsorberNum)%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          IndirectAbsorberReport(AbsorberNum)%QCond,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          IndirectAbsorberReport(AbsorberNum)%CondEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
          IndirectAbsorberReport(AbsorberNum)%CondInletTemp,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
          IndirectAbsorberReport(AbsorberNum)%CondOutletTemp,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
          IndirectAbsorberReport(AbsorberNum)%Condmdot,'System','Average',IndirectAbsorber(AbsorberNum)%Name)

     IF(IndirectAbsorber(AbsorberNum)%GenHeatSourceType == NodeType_Water)THEN
       CALL SetupOutputVariable('Chiller Hot Water Consumption Rate [W]', &
            IndirectAbsorberReport(AbsorberNum)%QGenerator,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
       CALL SetupOutputVariable('Chiller Source Hot Water Energy [J]', &
            IndirectAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='EnergyTransfer',EndUseKey='Cooling',GroupKey='Plant')
     ELSE
       IF(GenInputOutputNodesUsed(AbsorberNum))THEN
         CALL SetupOutputVariable('Chiller Source Steam Rate [W]', &
              IndirectAbsorberReport(AbsorberNum)%QGenerator,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
         CALL SetupOutputVariable('Chiller Source Steam Energy [J]', &
              IndirectAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='CHILLERS',GroupKey='Plant')
       ELSE
         CALL SetupOutputVariable('Chiller Source Steam Rate [W]', &
              IndirectAbsorberReport(AbsorberNum)%QGenerator,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
         CALL SetupOutputVariable('Chiller Source Steam Energy [J]', &
              IndirectAbsorberReport(AbsorberNum)%GeneratorEnergy,'System','Sum',IndirectAbsorber(AbsorberNum)%Name,  &
                              ResourceTypeKey='Steam',EndUseKey='Cooling',GroupKey='Plant')
       END IF
     END IF

     CALL SetupOutputVariable('Chiller COP [W/W]', &
          IndirectAbsorberReport(AbsorberNum)%ActualCOP,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Part Load Ratio []', &
          IndirectAbsorberReport(AbsorberNum)%ChillerPartLoadRatio,'System','Average',IndirectAbsorber(AbsorberNum)%Name)
     CALL SetupOutputVariable('Chiller Cycling Ratio []', &
          IndirectAbsorberReport(AbsorberNum)%ChillerCyclingFrac,'System','Average',IndirectAbsorber(AbsorberNum)%Name)

     CALL SetupOutputVariable('Chiller Steam Heat Loss Rate [W]', IndirectAbsorberReport(AbsorberNum)%LoopLoss, &
                  'System','Average',IndirectAbsorber(AbsorberNum)%Name)

     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', IndirectAbsorber(AbsorberNum)%Name, '[W]', &
                                     IndirectAbsorber(AbsorberNum)%NomCap  )
     ENDIF
  END DO

IF(ALLOCATED(GenInputOutputNodesUsed)) DEALLOCATE(GenInputOutputNodesUsed)

RETURN
END SUBROUTINE GetIndirectAbsorberInput

! End of Get Input subroutines for the Absorption Chiller Module
!******************************************************************************

SUBROUTINE InitIndirectAbsorpChiller(ChillNum,RunFlag, MyLoad)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Indirect Absorption Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_Indirect_Absorption, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE InputProcessor,  ONLY : SameString
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE FluidProperties, ONLY : GetDensityGlycol, GetSatDensityRefrig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad ! requested load

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: LoopCtr            ! Plant loop counter
  INTEGER :: LoopSideCtr        ! Loop side counter
  INTEGER :: BranchCtr          ! Plant branch counter
  INTEGER :: CompCtr            ! Component counter
  LOGICAL :: errFlag
  LOGICAL   :: FatalError
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: CpWater ! local specific heat
  REAL(r64) :: SteamDensity        ! density of generator steam (when connected to a steam loop)
  REAL(r64) :: EnthSteamOutDry     ! dry enthalpy of steam (quality = 1)
  REAL(r64) :: EnthSteamOutWet     ! wet enthalpy of steam (quality = 0)
  REAL(r64) :: HfgSteam            ! latent heat of steam at constant pressure
  REAL(r64) :: SteamDeltaT         ! amount of sub-cooling of steam condensate
  INTEGER   :: GeneratorInletNode      ! generator inlet node number, steam/water side
  REAL(r64) :: SteamOutletTemp
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: mdotEvap ! local fluid mass flow rate thru evaporator
  REAL(r64) :: mdotCond ! local fluid mass flow rate thru condenser
  REAL(r64) :: mdotGen ! local fluid mass flow rate thru generator

          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumIndirectAbsorbers))
    ALLOCATE(MyEnvrnFlag(NumIndirectAbsorbers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF
  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(IndirectAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_Indirect_Absorption, &
                                 IndirectAbsorber(ChillNum)%CWLoopNum, &
                                 IndirectAbsorber(ChillNum)%CWLoopSideNum, &
                                 IndirectAbsorber(ChillNum)%CWBranchNum, &
                                 IndirectAbsorber(ChillNum)%CWCompNum, &
                                 LowLimitTemp = IndirectAbsorber(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = IndirectAbsorber(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)

    CALL ScanPlantLoopsForObject(IndirectAbsorber(ChillNum)%Name, &
                                 TypeOf_Chiller_Indirect_Absorption, &
                                 IndirectAbsorber(ChillNum)%CDLoopNum, &
                                 IndirectAbsorber(ChillNum)%CDLoopSideNum, &
                                 IndirectAbsorber(ChillNum)%CDBranchNum, &
                                 IndirectAbsorber(ChillNum)%CDCompNum, &
                                 InletNodeNumber = IndirectAbsorber(ChillNum)%CondInletNodeNum,  &
                                 errFlag=errFlag)
    CALL InterConnectTwoPlantLoopSides( IndirectAbsorber(ChillNum)%CWLoopNum,      &
                                        IndirectAbsorber(ChillNum)%CWLoopSideNum,  &
                                        IndirectAbsorber(ChillNum)%CDLoopNum,      &
                                        IndirectAbsorber(ChillNum)%CDLoopSideNum,  &
                                        TypeOf_Chiller_Indirect_Absorption, .TRUE. )


    IF (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 )  THEN
      CALL ScanPlantLoopsForObject(IndirectAbsorber(ChillNum)%Name, &
                                   TypeOf_Chiller_Indirect_Absorption, &
                                   IndirectAbsorber(ChillNum)%GenLoopNum, &
                                   IndirectAbsorber(ChillNum)%GenLoopSideNum, &
                                   IndirectAbsorber(ChillNum)%GenBranchNum, &
                                   IndirectAbsorber(ChillNum)%GenCompNum, &
                                   InletNodeNumber = IndirectAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( IndirectAbsorber(ChillNum)%CWLoopNum,      &
                                          IndirectAbsorber(ChillNum)%CWLoopSideNum,  &
                                          IndirectAbsorber(ChillNum)%GenLoopNum,     &
                                          IndirectAbsorber(ChillNum)%GenCompNum,  &
                                          TypeOf_Chiller_Indirect_Absorption, .TRUE. )

    ENDIF


   IF ((IndirectAbsorber(ChillNum)%CondInletNodeNum > 0 ) .AND. &
       (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) )  THEN
     CALL InterConnectTwoPlantLoopSides( IndirectAbsorber(ChillNum)%CDLoopNum,      &
                                        IndirectAbsorber(ChillNum)%CDLoopSideNum,  &
                                        IndirectAbsorber(ChillNum)%GenLoopNum,     &
                                        IndirectAbsorber(ChillNum)%GenCompNum,  &
                                        TypeOf_Chiller_Indirect_Absorption , .FALSE.)

    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitIndirectAbsorpChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (IndirectAbsorber(ChillNum)%FlowMode == ConstantFlow ) THEN
      ! reset flow priority
      PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%LoopSide(IndirectAbsorber(ChillNum)%CWLoopSideNum)% &
          Branch(IndirectAbsorber(ChillNum)%CWBranchNum)%Comp(IndirectAbsorber(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (IndirectAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated ) THEN
      ! reset flow priority
      PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%LoopSide(IndirectAbsorber(ChillNum)%CWLoopSideNum)% &
          Branch(IndirectAbsorber(ChillNum)%CWBranchNum)%Comp(IndirectAbsorber(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

      IF ((Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. IndirectAbsorber(ChillNum)%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(IndirectAbsorber(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            IndirectAbsorber(ChillNum)%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(IndirectAbsorber(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. IndirectAbsorber(ChillNum)%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(IndirectAbsorber(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              IndirectAbsorber(ChillNum)%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF

        IndirectAbsorber(ChillNum)%ModulatedFlowSetToLoop = .TRUE.
        Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint = &
           Node(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi = &
           Node(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF



    MyFlag(ChillNum)=.FALSE.
  ENDIF

  CondInletNode  = IndirectAbsorber(ChillNum)%CondInletNodeNum
  CondOutletNode = IndirectAbsorber(ChillNum)%CondOutletNodeNum

           !Initialize Supply Side Variables
  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize) )Then
    IF (PlantSizeNotComplete) CALL SizeIndirectAbsorpChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                             InitConvTemp, &
                             PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                             'InitIndirectAbsorpChiller')

    IndirectAbsorber(ChillNum)%EvapMassFlowRateMax = IndirectAbsorber(ChillNum)%EvapVolFlowRate * rho

    CALL InitComponentNodes(0.d0, IndirectAbsorber(ChillNum)%EvapMassFlowRateMax, &
                              IndirectAbsorber(ChillNum)%EvapInletNodeNum, &
                              IndirectAbsorber(ChillNum)%EvapOutletNodeNum, &
                              IndirectAbsorber(ChillNum)%CWLoopNum, &
                              IndirectAbsorber(ChillNum)%CWLoopSideNum, &
                              IndirectAbsorber(ChillNum)%CWBranchNum, &
                              IndirectAbsorber(ChillNum)%CWCompNum)

    rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                             InitConvTemp, &
                             PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                             'InitIndirectAbsorpChiller')

    IndirectAbsorber(ChillNum)%CondMassFlowRateMax = rho * IndirectAbsorber(ChillNum)%CondVolFlowRate

    CALL InitComponentNodes(0.d0, IndirectAbsorber(ChillNum)%CondMassFlowRateMax, &
                            CondInletNode, CondOutletNode, &
                            IndirectAbsorber(ChillNum)%CDLoopNum, &
                            IndirectAbsorber(ChillNum)%CDLoopSideNum, &
                            IndirectAbsorber(ChillNum)%CDBranchNum, &
                            IndirectAbsorber(ChillNum)%CDCompNum)

    Node(CondInletNode)%Temp = IndirectAbsorber(ChillNum)%TempDesCondIn

    IF (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) THEN

      IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN

        rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                               'InitIndirectAbsorpChiller')
        IndirectAbsorber(ChillNum)%GenMassFlowRateMax   = rho * IndirectAbsorber(ChillNum)%GeneratorVolFlowRate

      ELSE
        SteamDensity      = GetSatDensityRefrig('STEAM',Node(IndirectAbsorber(ChillNum)%GeneratorInletNodeNum)%Temp,1.0d0, &
                                                 IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                                'CALC Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name))
        IndirectAbsorber(ChillNum)%GenMassFlowRateMax   = SteamDensity*IndirectAbsorber(ChillNum)%GeneratorVolFlowRate
      END IF

      CALL InitComponentNodes(0.d0, IndirectAbsorber(ChillNum)%GenMassFlowRateMax, &
                              IndirectAbsorber(ChillNum)%GeneratorInletNodeNum, &
                              IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                              IndirectAbsorber(ChillNum)%GenLoopNum, &
                              IndirectAbsorber(ChillNum)%GenLoopSideNum, &
                              IndirectAbsorber(ChillNum)%GenBranchNum, &
                              IndirectAbsorber(ChillNum)%GenCompNum)
    ENDIF
    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.TRUE.
  ENDIF

  IF ((IndirectAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) &
      .AND. IndirectAbsorber(ChillNum)%ModulatedFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
    mdotEvap = IndirectAbsorber(ChillNum)%EvapMassFlowRateMax
    mdotCond = IndirectAbsorber(ChillNum)%CondMassFlowRateMax
    mdotGen  = IndirectAbsorber(ChillNum)%GenMassFlowRateMax
  ELSE
    mdotEvap = 0.d0
    mdotCond = 0.d0
    mdotGen  = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdotEvap, &
                              IndirectAbsorber(ChillNum)%EvapInletNodeNum, &
                              IndirectAbsorber(ChillNum)%EvapOutletNodeNum,&
                              IndirectAbsorber(ChillNum)%CWLoopNum,     &
                              IndirectAbsorber(ChillNum)%CWLoopSideNum, &
                              IndirectAbsorber(ChillNum)%CWBranchNum,   &
                              IndirectAbsorber(ChillNum)%CWCompNum)

  CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,  &
                                IndirectAbsorber(ChillNum)%CDLoopNum,     &
                                IndirectAbsorber(ChillNum)%CDLoopSideNum, &
                                IndirectAbsorber(ChillNum)%CDBranchNum,   &
                                IndirectAbsorber(ChillNum)%CDCompNum)

  IF (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 ) THEN

    CALL SetComponentFlowRate( mdotGen, &
                                IndirectAbsorber(ChillNum)%GeneratorInletNodeNum,  &
                                IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum, &
                                IndirectAbsorber(ChillNum)%GenLoopNum,     &
                                IndirectAbsorber(ChillNum)%GenLoopSideNum, &
                                IndirectAbsorber(ChillNum)%GenBranchNum,   &
                                IndirectAbsorber(ChillNum)%GenCompNum)

  ENDIF


  RETURN

END SUBROUTINE InitIndirectAbsorpChiller

SUBROUTINE SizeIndirectAbsorpChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Indirect Absorption Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,    ONLY : PlantLoop, PlantSizesOkayToFinalize, MyPlantSizingIndex
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE CurveManager, ONLY : CurveValue
  USE OutputReportPredefined
  USE FluidProperties
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput

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
  INTEGER             :: PltSizIndex         ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum           ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum       ! Plant Sizing index for condenser loop
  INTEGER             :: PltSizSteamNum      ! Plant Sizing index for steam heating loop
  INTEGER             :: PltSizHeatingNum    ! Plant Sizing index for how water heating loop
  REAL(r64)           :: SteamInputRatNom    ! nominal energy input ratio (steam or hot water)
  REAL(r64)           :: SteamDensity        ! density of generator steam (when connected to a steam loop)
  REAL(r64)           :: EnthSteamOutDry     ! dry enthalpy of steam (quality = 1)
  REAL(r64)           :: EnthSteamOutWet     ! wet enthalpy of steam (quality = 0)
  REAL(r64)           :: HfgSteam            ! latent heat of steam at constant pressure
  REAL(r64)           :: SteamDeltaT         ! amount of sub-cooling of steam condensate
  REAL(r64)           :: SteamMassFlowRate   ! steam mass flow rate through generator
  REAL(r64)           :: CpWater             ! specific heat of generator fluid (when connected to a hot water loop)
  REAL(r64)           :: RhoWater            ! density of water (kg/m3)
  REAL(r64)           :: GeneratorOutletTemp ! outlet temperature of generator
  LOGICAL             :: ErrorsFound         ! If errors detected in input
  LOGICAL             :: LoopErrorsFound     !
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: rho ! local fluid density
  REAL(r64)           :: Cp  ! local specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpNomPumpPower ! local nominal pump power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  REAL(r64)           :: tmpGeneratorVolFlowRate ! local generator design volume flow rate
  INTEGER             :: DummWaterIndex = 1

  PltSizNum = 0
  PltSizCondNum = 0
  PltSizHeatingNum = 0
  PltSizSteamNum = 0
  ErrorsFound = .FALSE.
    ! init local temporary version in case of partial/mixed autosizing
  tmpNomCap          = IndirectAbsorber(ChillNum)%NomCap
  tmpNomPumpPower    = IndirectAbsorber(ChillNum)%NomPumpPower
  tmpEvapVolFlowRate = IndirectAbsorber(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = IndirectAbsorber(ChillNum)%CondVolFlowRate
  tmpGeneratorVolFlowRate = IndirectAbsorber(ChillNum)%GeneratorVolFlowRate

  IF(IndirectAbsorber(ChillNum)%GeneratorInputCurvePtr .GT. 0)THEN
    SteamInputRatNom = CurveValue(IndirectAbsorber(ChillNum)%GeneratorInputCurvePtr,1.0d0)
  ELSE
    SteamInputRatNom = 1.0d0
  END IF

  ! find the appropriate Plant Sizing object
  IF (CurLoopNum > 0) THEN
    PltSizNum = PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%PlantSizNum
  END IF

  IF (IndirectAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
    PltSizCondNum = MyPlantSizingIndex('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                                         IndirectAbsorber(ChillNum)%CondInletNodeNum, &
                                         IndirectAbsorber(ChillNum)%CondOutletNodeNum, LoopErrorsFound)
  END IF

  IF (IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Steam) THEN
    IF (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 .AND. IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum > 0) THEN
      PltSizSteamNum = MyPlantSizingIndex('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                                          IndirectAbsorber(ChillNum)%GeneratorInletNodeNum, &
                                          IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum, LoopErrorsFound)
    ELSE
      DO PltSizIndex=1,NumPltSizInput
        IF(PlantSizData(PltSizIndex)%LoopType == SteamLoop)THEN
          PltSizSteamNum = PltSizIndex
        END IF
      END DO
    END IF
  ELSE
    IF (IndirectAbsorber(ChillNum)%GeneratorInletNodeNum > 0 .AND. IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum > 0) THEN
      PltSizHeatingNum = MyPlantSizingIndex('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                                          IndirectAbsorber(ChillNum)%GeneratorInletNodeNum, &
                                          IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum, LoopErrorsFound)
    ELSE
      DO PltSizIndex=1,NumPltSizInput
        IF(PlantSizData(PltSizIndex)%LoopType == HeatingLoop)THEN
          PltSizHeatingNum = PltSizIndex
        END IF
      END DO
    END IF
  END IF

  IF (IndirectAbsorber(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        Cp = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')

        rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%NomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate * IndirectAbsorber(ChillNum)%SizFac
        ELSE
          tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate * IndirectAbsorber(ChillNum)%SizFac
        ENDIF
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%NomCap = 0.d0
        ELSE
          tmpNomCap = 0.d0
        ENDIF
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                              'Nominal Capacity [W]', IndirectAbsorber(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:Absorption:Indirect object='//TRIM(IndirectAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (IndirectAbsorber(ChillNum)%NomPumpPower  == AutoSize) THEN
    IF (PlantSizesOkayToFinalize) THEN
     ! the DOE-2 EIR for single stage absorption chiller
      IndirectAbsorber(ChillNum)%NomPumpPower = 0.0045d0 * IndirectAbsorber(ChillNum)%NomCap

      CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                              'Nominal Pumping Power [W]', IndirectAbsorber(ChillNum)%NomPumpPower)
    ELSE
      tmpNomPumpPower = 0.0045d0 * tmpNomCap
    ENDIF
  END IF

  IF (IndirectAbsorber(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%EvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * IndirectAbsorber(ChillNum)%SizFac
        ELSE
          tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * IndirectAbsorber(ChillNum)%SizFac
        ENDIF
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%EvapVolFlowRate = 0.d0
        ELSE
          tmpEvapVolFlowRate = 0.d0
        ENDIF
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              IndirectAbsorber(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:Absorption:Indirect object='//TRIM(IndirectAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF
  IF (PlantSizesOkayToFinalize) THEN
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%EvapInletNodeNum,IndirectAbsorber(ChillNum)%EvapVolFlowRate)
  ELSE
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)
  ENDIF

  IF (IndirectAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (IndirectAbsorber(ChillNum)%EvapVolFlowRate >= SmallWaterVolFlow) THEN
!       QCondenser = QEvaporator + QGenerator + PumpingPower

        Cp = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')

        rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%CondVolFlowRate = IndirectAbsorber(ChillNum)%NomCap * &
            (1.0d0 + SteamInputRatNom + IndirectAbsorber(ChillNum)%NomPumpPower/IndirectAbsorber(ChillNum)%NomCap) / &
            ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        ELSE
          tmpCondVolFlowRate = tmpNomCap * &
             (1.0d0 + SteamInputRatNom + tmpNomPumpPower/tmpNomCap) / &
             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        ENDIF
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%CondVolFlowRate = 0.0d0
        ELSE
          tmpCondVolFlowRate = 0.d0
        ENDIF
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              IndirectAbsorber(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:Absorption:Indirect object='//TRIM(IndirectAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (PlantSizesOkayToFinalize) Then
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%CondInletNodeNum,IndirectAbsorber(ChillNum)%CondVolFlowRate)
  ELSE
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%CondInletNodeNum,tmpCondVolFlowRate)
  ENDIF

  IF (IndirectAbsorber(ChillNum)%GeneratorVolFlowRate == AutoSize) THEN
    IF (PltSizSteamNum > 0 .AND. IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Steam .OR. &
        PltSizHeatingNum > 0 .AND. IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water) THEN
      IF (IndirectAbsorber(ChillNum)%EvapVolFlowRate >= SmallWaterVolFlow) THEN
        IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
          CpWater = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   PlantSizData(PltSizHeatingNum)%ExitTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
          SteamDeltaT = MAX(0.5d0,PlantSizData(PltSizHeatingNum)%DeltaT)

          RhoWater = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   (PlantSizData(PltSizHeatingNum)%ExitTemp - SteamDeltaT), &
                                   PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
          IF (PlantSizesOkayToFinalize) Then
            IndirectAbsorber(ChillNum)%GeneratorVolFlowRate = (IndirectAbsorber(ChillNum)%NomCap * SteamInputRatNom)/ &
                                                      (CpWater * SteamDeltaT * RhoWater)

            CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                              'Design Generator Fluid Flow Rate [m3/s]', &
                              IndirectAbsorber(ChillNum)%GeneratorVolFlowRate)
          ELSE
            tmpGeneratorVolFlowRate = (tmpNomCap * SteamInputRatNom)/ &
                                                      (CpWater * SteamDeltaT * RhoWater)
          ENDIF

        ELSE
          SteamDensity = GetSatDensityRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,1.0d0, &
                                              IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                                  'SIZE Chiller:Absorption:Indirect'//TRIM(IndirectAbsorber(ChillNum)%Name))
          SteamDeltaT         = PlantSizData(PltSizSteamNum)%DeltaT
          GeneratorOutletTemp = PlantSizData(PltSizSteamNum)%ExitTemp - SteamDeltaT

          EnthSteamOutDry   = GetSatEnthalpyRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,1.0d0, &
                                                   IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                                  'SIZE Chiller:Absorption:Indirect'//TRIM(IndirectAbsorber(ChillNum)%Name))
          EnthSteamOutWet   = GetSatEnthalpyRefrig('STEAM',PlantSizData(PltSizSteamNum)%ExitTemp,0.0d0, &
                                                   IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                                  'SIZE Chiller:Absorption:Indirect'//TRIM(IndirectAbsorber(ChillNum)%Name))
          CpWater =  GetSpecificHeatGlycol('WATER', GeneratorOutletTemp, DummWaterIndex,  'SizeIndirectAbsorpChiller')
          HfgSteam          = EnthSteamOutDry - EnthSteamOutWet
          IF (PlantSizesOkayToFinalize) THEN
  !         calculate the mass flow rate through the generator
            SteamMassFlowRate = (IndirectAbsorber(ChillNum)%NomCap * SteamInputRatNom) / &
                                ( (HfgSteam) + (SteamDeltaT * CpWater) )

  !         calculate the steam volumetric flow rate
            IndirectAbsorber(ChillNum)%GeneratorVolFlowRate = SteamMassFlowRate / SteamDensity

            CALL ReportSizingOutput('Chiller:Absorption:Indirect', IndirectAbsorber(ChillNum)%Name, &
                                'Design Generator Fluid Flow Rate [m3/s]', &
                                IndirectAbsorber(ChillNum)%GeneratorVolFlowRate)
          ELSE
            SteamMassFlowRate = (tmpNomCap * SteamInputRatNom) / &
                                ( (HfgSteam) + (SteamDeltaT * CpWater) )

  !         calculate the steam volumetric flow rate
            tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity
          ENDIF
        END IF
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          IndirectAbsorber(ChillNum)%GeneratorVolFlowRate = 0.0d0
        ELSE
          tmpGeneratorVolFlowRate = 0.d0
        ENDIF
      END IF
    ELSE
      CALL ShowSevereError('Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.')
      CALL ShowContinueError(' For steam loops, use a steam Sizing:Plant object.')
      CALL ShowContinueError(' For hot water loops, use a heating Sizing:Plant object.')
      CALL ShowContinueError('Occurs in Chiller:Absorption:Indirect object='//TRIM(IndirectAbsorber(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
  IF (PlantSizesOkayToFinalize) THEN
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%GeneratorInletNodeNum,  &
       IndirectAbsorber(ChillNum)%GeneratorVolFlowRate)
  ELSE
    CALL RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum)%GeneratorInletNodeNum,tmpGeneratorVolFlowRate)
  ENDIF

  IF(IndirectAbsorber(ChillNum)%GeneratorDeltaTemp == AutoSize)THEN
    IF(PltSizHeatingNum > 0 .AND. IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water) THEN
      IndirectAbsorber(ChillNum)%GeneratorDeltaTemp = MAX(0.5d0,PlantSizData(PltSizHeatingNum)%DeltaT)
    ELSE IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
      rho = GetDensityGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
      CpWater = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                   PlantSizData(PltSizHeatingNum)%ExitTemp, &
                                   PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                   'SizeIndirectAbsorpChiller')
      IF (PlantSizesOkayToFinalize) THEN
        IndirectAbsorber(ChillNum)%GeneratorDeltaTemp = (SteamInputRatNom * IndirectAbsorber(ChillNum)%NomCap)/ &
                (CpWater * rho * IndirectAbsorber(ChillNum)%GeneratorVolFlowRate)
      ENDIF
    END IF
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) THEN
      !create predefined report
    equipName = IndirectAbsorber(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Absorption:Indirect')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,'n/a')
    CALL PreDefTableEntry(pdchMechNomCap,equipName,IndirectAbsorber(ChillNum)%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeIndirectAbsorpChiller

! Beginning of Absorber model Subroutines
! *****************************************************************************

SUBROUTINE CalcIndirectAbsorberModel(ChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression Absorber using a revised BLAST model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1.  BLAST User Manual
          ! 2.  Absorber User Manual

          ! USE STATEMENTS:
  USE FluidProperties
  USE General,         ONLY : TrimSigDigits, RoundSigDigits
  USE DataPlant,       ONLY : DeltaTemptol,PlantLoop,CompSetPtBasedSchemeType, &
                              SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, WarmupFlag
  USE CurveManager,    ONLY : CurveValue
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys
  USE DataEnvironment, ONLY : OutBaroPress
  USE PlantUtilities,  ONLY : SetComponentFlowRate, RegisterPlantCompDesignFlow

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(IN)   :: ChillNum        ! Absorber number
  REAL(r64),INTENT(IN)   :: MyLoad          ! operating load
  LOGICAL,  INTENT(IN)   :: RunFlag         ! TRUE when Absorber operating
  LOGICAL,  INTENT(IN)   :: FirstIteration  ! TRUE when first iteration of timestep !unused1208
  INTEGER,  INTENT(IN)   :: EquipFlowCtrl   ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn          ! C - (BLAST ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: EvapInletTemp       ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp       ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut         ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: AbsorberNomCap      ! Absorber nominal capacity
  REAL(r64)              :: NomPumpPower        ! Absorber nominal pumping power
  REAL(r64)              :: PartLoadRat         ! part load ratio for efficiency calc
  REAL(r64)              :: OperPartLoadRat     ! Operating part load ratio
  REAL(r64)              :: EvapDeltaTemp       ! C - evaporator temperature difference, water side
  REAL(r64)              :: TempLowLimitEout    ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: HeatInputRat        ! genertaor heat input ratio
  REAL(r64)              :: ElectricInputRat    ! energy input ratio
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER                :: GeneratorInletNode  ! generator inlet node number, steam/water side
  INTEGER                :: GeneratorOutletNode ! generator outlet node number, steam/water side
  REAL(r64)              :: EnthSteamOutDry     ! enthalpy of dry steam at generator inlet
  REAL(r64)              :: EnthSteamOutWet     ! enthalpy of wet steam at generator inlet
  REAL(r64)              :: HfgSteam            ! heat of vaporization of steam
  LOGICAL,ALLOCATABLE,DIMENSION(:),SAVE  :: MyEnvironFlag
  LOGICAL,ALLOCATABLE,DIMENSION(:),SAVE  :: MyEnvironSteamFlag
  LOGICAL, SAVE :: OneTimeFlag = .true.
  REAL(r64)              :: FRAC                ! fraction of time step chiller cycles
  LOGICAL,SAVE           :: PossibleSubCooling  ! flag to determine if supply water temperature is below setpoint
  REAL(r64)              :: CpFluid             ! specific heat of generator fluid
  REAL(r64)              :: SteamDeltaT         ! temperature difference of fluid through generator
  REAL(r64)              :: SteamDensity        ! density of steam
  REAL(r64)              :: SteamOutletTemp     ! generator outlet temperature
  REAL(r64)              :: CapacityfAbsorberTemp   ! performance curve output
  REAL(r64)              :: CapacityfEvaporatorTemp ! performance curve output
  REAL(r64)              :: CapacityfGeneratorTemp  ! performance curve output
  REAL(r64)              :: HeatInputfCondTemp      ! performance curve output
  REAL(r64)              :: HeatInputfEvapTemp      ! performance curve output
  REAL(r64)              :: TempWaterAtmPress   ! temperature of condensed steam leaving generator (after condensate trap)
  REAL(r64)              :: TempLoopOutToPump   ! temperature of condensed steam entering pump (includes loop losses)
  REAL(r64)              :: EnthAtAtmPress      ! enthalpy  of condensed steam leaving generator (after condensate trap)
  REAL(r64)              :: EnthPumpInlet       ! enthalpy of condensed steam entering pump (includes loop losses)
  INTEGER :: LoopSideNum
  INTEGER :: LoopNum
  INTEGER :: DummyWaterIndex = 1

  IF (OneTimeFlag) THEN
    ALLOCATE(MyEnvironFlag(NumIndirectAbsorbers))
    ALLOCATE(MyEnvironSteamFlag(NumIndirectAbsorbers))
    MyEnvironFlag=.true.
    MyEnvironSteamFlag=.true.
    OneTimeFlag=.false.
  ENDIF

          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  GenMassFlowRate            = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  QGenerator                 = 0.0d0
  PumpingEnergy              = 0.0d0
  CondenserEnergy            = 0.0d0
  EvaporatorEnergy           = 0.0d0
  GeneratorEnergy            = 0.0d0
  PumpingPower               = 0.0d0
  FRAC                       = 1.0d0
  ChillerONOFFCyclingFrac    = 0.0d0
  EvapInletNode       = IndirectAbsorber(ChillNum)%EvapInletNodeNum
  EvapOutletNode      = IndirectAbsorber(ChillNum)%EvapOutletNodeNum
  CondInletNode       = IndirectAbsorber(ChillNum)%CondInletNodeNum
  CondOutletNode      = IndirectAbsorber(ChillNum)%CondOutletNodeNum
  GeneratorInletNode  = IndirectAbsorber(ChillNum)%GeneratorInletNodeNum
  GeneratorOutletNode = IndirectAbsorber(ChillNum)%GeneratorOutletNodeNum


!  If no loop demand or Absorber OFF, return
  IF(MyLoad >= 0.0d0 .OR. .NOT. RunFlag) THEN
   IF(EquipFlowCtrl == ControlType_SeriesActive) EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
   RETURN
  END IF

! Warn if entering condenser water temperature is below minimum
  IF(Node(CondInletNode)%Temp .LT. IndirectAbsorber(ChillNum)%MinCondInletTemp)THEN
    IF(.NOT. WarmupFlag)THEN
      IF(IndirectAbsorber(ChillNum)%MinCondInletTempCtr .LT. 1)THEN
        IndirectAbsorber(ChillNum)%MinCondInletTempCtr = IndirectAbsorber(ChillNum)%MinCondInletTempCtr + 1
        CALL ShowWarningError('Chiller:Absorption:Indirect "'//TRIM(IndirectAbsorber(ChillNum)%Name)//'"')
        CALL ShowContinueError('...Entering condenser water temperature below specified minimum ('// &
                                TRIM(RoundSigDigits(IndirectAbsorber(ChillNum)%MinCondInletTemp,3))//' C).')
        CALL ShowContinueError('...Entering condenser water temperature = ' &
                                //TRIM(RoundSigDigits(Node(CondInletNode)%Temp,3))//' C.')
        CALL ShowContinueErrorTimeStamp('...simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd('Entering condenser water temperature below specified minimum error continues.', &
                                             IndirectAbsorber(ChillNum)%MinCondInletTempIndex,Node(CondInletNode)%Temp, &
                                             Node(CondInletNode)%Temp)
      END IF
    END IF
  END IF

! Warn if entering generator fluid temperature is below minimum
  IF(GeneratorInletNode .GT. 0)THEN
    IF(Node(GeneratorInletNode)%Temp .LT. IndirectAbsorber(ChillNum)%MinGeneratorInletTemp)THEN
      IF(.NOT. WarmupFlag)THEN
        IF(IndirectAbsorber(ChillNum)%MinGenInletTempCtr .LT. 1)THEN
          IndirectAbsorber(ChillNum)%MinGenInletTempCtr = IndirectAbsorber(ChillNum)%MinGenInletTempCtr + 1
          CALL ShowWarningError('Chiller:Absorption:Indirect "'//TRIM(IndirectAbsorber(ChillNum)%Name)//'"')
          CALL ShowContinueError('...Entering generator fluid temperature below specified minimum ('// &
                                TRIM(RoundSigDigits(IndirectAbsorber(ChillNum)%MinGeneratorInletTemp,3))//' C).')
          CALL ShowContinueError('...Entering generator fluid temperature = ' &
                                 //TRIM(RoundSigDigits(Node(GeneratorInletNode)%Temp,3))//' C.')
          CALL ShowContinueErrorTimeStamp('...simulation continues.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd('Entering generator fluid temperature below specified minimum error continues.', &
                                             IndirectAbsorber(ChillNum)%MinGenInletTempIndex,Node(GeneratorInletNode)%Temp, &
                                             Node(GeneratorInletNode)%Temp)
        END IF
      END IF
    END IF
  END IF

! Set module level Absorber inlet and temperature variables
  EvapInletTemp  = Node(EvapInletNode)%Temp
  CondInletTemp  = Node(CondInletNode)%Temp

! Set the condenser mass flow rates
  CondMassFlowRate = Node(CondInletNode)%MassFlowRate

! LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  MinPartLoadRat     = IndirectAbsorber(ChillNum)%MinPartLoadRat
  MaxPartLoadRat     = IndirectAbsorber(ChillNum)%MaxPartLoadRat
  AbsorberNomCap     = IndirectAbsorber(ChillNum)%NomCap
  NomPumpPower       = IndirectAbsorber(ChillNum)%NomPumpPower
  TempCondIn         = Node(IndirectAbsorber(ChillNum)%CondInletNodeNum)%Temp
  TempEvapOut        = Node(IndirectAbsorber(ChillNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = IndirectAbsorber(ChillNum)%TempLowLimitEvapOut
  LoopNum            = IndirectAbsorber(ChillNum)%CWLoopNum
  LoopSideNum        = IndirectAbsorber(ChillNum)%CWLoopSideNum

  CpFluid            = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidName, &
                                          EvapInletTemp, &
                                          PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%FluidIndex, &
                                          'CalcIndirectAbsorberModel')

  IF(IndirectAbsorber(ChillNum)%CapFCondenserTempPtr .GT. 0)THEN
    CapacityfAbsorberTemp = CurveValue(IndirectAbsorber(ChillNum)%CapFCondenserTempPtr, TempCondIn)
  ELSE
    CapacityfAbsorberTemp = 1.0d0
  END IF
  IF(IndirectAbsorber(ChillNum)%CapFEvaporatorTempPtr .GT. 0)THEN
    CapacityfEvaporatorTemp = CurveValue(IndirectAbsorber(ChillNum)%CapFEvaporatorTempPtr, TempEvapOut)
  ELSE
    CapacityfEvaporatorTemp = 1.0d0
  END IF
  IF(IndirectAbsorber(ChillNum)%CapFGeneratorTempPtr .GT. 0)THEN
    IF(GeneratorInletNode .GT. 0)THEN
      IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN
        CapacityfGeneratorTemp = CurveValue(IndirectAbsorber(ChillNum)%CapFGeneratorTempPtr, Node(GeneratorInletNode)%Temp)
      ELSE
        CapacityfGeneratorTemp = 1.0d0
      END IF
    ELSE
      CapacityfGeneratorTemp = 1.0d0
    END IF
  ELSE
    CapacityfGeneratorTemp = 1.0d0
  END IF

  AbsorberNomCap = AbsorberNomCap * CapacityfAbsorberTemp * CapacityfEvaporatorTemp * CapacityfGeneratorTemp

! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==0) THEN
     IndirectAbsorber(ChillNum)%PossibleSubCooling = .FALSE.
     QEvaporator = ABS(MyLoad)

     ! Either set the flow to the Constant value or caluclate the flow for the variable volume
     IF((IndirectAbsorber(ChillNum)%FlowMode == ConstantFlow)  &
        .OR. (IndirectAbsorber(ChillNum)%FlowMode == NotModulated ))THEN
        EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate

        IF (EvapMassFlowRate /= 0.0D0) THEN
          EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CpFluid
        ELSE
          EvapDeltaTemp = 0.0D0
        ENDIF
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

     ELSE IF(IndirectAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated)THEN
        ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
        SELECT CASE (PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
        CASE (DualSetpointDeadband)
          EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
        END SELECT

        IF (EvapDeltaTemp /= 0) THEN
          EvapMassFlowRate = ABS(QEvaporator/CpFluid/EvapDeltaTemp)
          IF((EvapMassFlowRate - IndirectAbsorber(ChillNum)%EvapMassFlowRateMax) .GT. MassFlowTolerance)   &
                       PossibleSubCooling = .TRUE.
          !Check to see if the Maximum is exceeded, if so set to maximum
          EvapMassFlowRate = MIN(IndirectAbsorber(ChillNum)%EvapMassFlowRateMax, EvapMassFlowRate)
          CALL SetComponentFlowRate(EvapMassFlowRate, &
                                  IndirectAbsorber(ChillNum)%EvapInletNodeNum, &
                                  IndirectAbsorber(ChillNum)%EvapOutletNodeNum,&
                                  IndirectAbsorber(ChillNum)%CWLoopNum,     &
                                  IndirectAbsorber(ChillNum)%CWLoopSideNum, &
                                  IndirectAbsorber(ChillNum)%CWBranchNum,   &
                                  IndirectAbsorber(ChillNum)%CWCompNum)
          SELECT CASE (PlantLoop(IndirectAbsorber(ChillNum)%CWLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetpoint)
            EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
          CASE (DualSetpointDeadband)
            EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
          END SELECT
        ELSE
          EvapMassFlowRate=0.0d0
          EvapOutletTemp = Node(EvapInletNode)%Temp

          CALL ShowRecurringWarningErrorAtEnd('CalcIndirectAbsorberModel: Name="'//  &
             TRIM(IndirectAbsorber(ChillNum)%Name)//  &
             '" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.',  &
             IndirectAbsorber(ChillNum)%ErrCount2)
        END IF
     END IF  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

     EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
     IF(PossibleSubCooling) THEN
       QEvaporator = ABS(MyLoad)
       EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CpFluid
       EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
     ELSE
       SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
       CASE (SingleSetpoint)
         IF ((IndirectAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(IndirectAbsorber(ChillNum)%CWBranchNum) &
              %Comp(IndirectAbsorber(ChillNum)%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
           TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
         ELSE
           TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
         ENDIF
       CASE (DualSetpointDeadband)
         IF ((IndirectAbsorber(ChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(IndirectAbsorber(ChillNum)%CWBranchNum) &
              %Comp(IndirectAbsorber(ChillNum)%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
           TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
         ELSE
           TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
         ENDIF
       END SELECT
       EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
       QEvaporator = ABS(EvapMassFlowRate*CpFluid*EvapDeltaTemp)
       EvapOutletTemp = TempEvapOutSetpoint
     END IF
     !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
     IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
       IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
         EvapOutletTemp = TempLowLimitEout
         EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
         QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
       ELSE
         EvapOutletTemp = Node(EvapInletNode)%Temp
         EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
         QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
       END IF
     END IF
     IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
       IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
         EvapOutletTemp = Node(EvapOutletNode)%TempMin
         EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
         QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
       ELSE
         EvapOutletTemp = Node(EvapInletNode)%Temp
         EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
         QEvaporator = EvapMassFlowRate*CpFluid*EvapDeltaTemp
       END IF
     END IF
     ! Checks QEvaporator on the basis of the machine limits.
     IF(QEvaporator > ABS(MyLoad)) Then
       IF(EvapMassFlowRate > MassFlowTolerance) THEN
          QEvaporator = ABS(MyLoad)
          EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CpFluid
          EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       ELSE
          QEvaporator = 0.0d0
          EvapOutletTemp = Node(EvapInletNode)%Temp
       END IF
     END IF

  END IF  !This is the end of the FlowLock Block

  OperPartLoadRat = QEvaporator/AbsorberNomCap
  PartLoadRat = MAX(MinPartLoadRat,OperPartLoadRat)
  IndirectAbsorberReport(ChillNum)%ChillerPartLoadRatio = OperPartLoadRat

  IF(OperPartLoadRat .LT. PartLoadRat) THEN
    FRAC = MIN(1.0d0,OperPartLoadRat/MinPartLoadRat)
  ELSE
    FRAC = 1.0d0
  END IF

  ChillerONOFFCyclingFrac = FRAC

  IF(GeneratorInletNode .GT. 0)THEN
    IF(IndirectAbsorber(ChillNum)%HeatInputFCondTempPtr .GT. 0)THEN
      HeatInputfCondTemp = CurveValue(IndirectAbsorber(ChillNum)%HeatInputFCondTempPtr, Node(GeneratorInletNode)%Temp)
    ELSE
      HeatInputfCondTemp = 1.0d0
    END IF
  ELSE
    HeatInputfCondTemp  = 1.0d0
  END IF
  IF(IndirectAbsorber(ChillNum)%HeatInputFEvapTempPtr .GT. 0)THEN
    HeatInputfEvapTemp = CurveValue(IndirectAbsorber(ChillNum)%HeatInputFEvapTempPtr, Node(EvapOutletNode)%Temp)
  ELSE
    HeatInputfEvapTemp = 1.0d0
  END IF

  !Calculate steam input ratio. Inlcude impact of generator and evaporator temperatures
  IF(IndirectAbsorber(ChillNum)%GeneratorInputCurvePtr .GT. 0)THEN
    HeatInputRat    = CurveValue(IndirectAbsorber(ChillNum)%GeneratorInputCurvePtr,PartLoadRat) * &
                        HeatInputfCondTemp * HeatInputfEvapTemp
  ELSE
    HeatInputRat    = HeatInputfCondTemp * HeatInputfEvapTemp
  END IF

  !Calculate electric input ratio
  IF(IndirectAbsorber(ChillNum)%PumpPowerCurvePtr .GT. 0)THEN
    ElectricInputRat = CurveValue(IndirectAbsorber(ChillNum)%PumpPowerCurvePtr,PartLoadRat)
  ELSE
    ElectricInputRat = 1.0d0
  END IF

  QGenerator   = HeatInputRat * AbsorberNomCap * FRAC
  PumpingPower = ElectricInputRat * NomPumpPower * FRAC

  IF(EvapMassFlowRate == 0.0d0) THEN
    QGenerator     = 0.0d0
    EvapOutletTemp = Node(EvapInletNode)%Temp
    PumpingPower   = 0.0d0
  END IF

  QCondenser = QEvaporator + QGenerator + PumpingPower

  CpFluid    = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidName, &
                                          CondInletTemp, &
                                          PlantLoop(IndirectAbsorber(ChillNum)%CDLoopNum)%FluidIndex, &
                                          'CalcIndirectAbsorberModel')

  IF (CondMassFlowRate > MassFlowTolerance) THEN
     CondOutletTemp = QCondenser/CondMassFlowRate/CpFluid + CondInletTemp
  ELSE
    CondOutletTemp = CondInletTemp
    CondMassFlowRate = 0.d0
    QCondenser = 0.d0
    RETURN
    !V7 plant upgrade, no longer fatal here anymore... set some things and return
  END IF

  IF(GeneratorInletNode .GT. 0)THEN
!   Hot water plant is used for the generator
    IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN

      CpFluid   = GetSpecificHeatGlycol(PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidName, &
                                          Node(GeneratorInletNode)%Temp, &
                                          PlantLoop(IndirectAbsorber(ChillNum)%GenLoopNum)%FluidIndex, &
                                          'CalcIndirectAbsorberModel')
      IF (( IndirectAbsorber(ChillNum)%FlowMode == ConstantFlow)   &
          .OR. (IndirectAbsorber(ChillNum)%FlowMode == NotModulated ) ) THEN
        GenMassFlowRate = IndirectAbsorber(ChillNum)%GenMassFlowRateMax
      ELSE
        GenMassFlowRate = QGenerator/CpFluid/IndirectAbsorber(ChillNum)%GeneratorDeltaTemp
      END IF

      Call SetComponentFlowRate(GenMassFlowRate, &
                                GeneratorInletNode, GeneratorOutletNode, &
                                IndirectAbsorber(ChillNum)%GenLoopNum,     &
                                IndirectAbsorber(ChillNum)%GenLoopSideNum, &
                                IndirectAbsorber(ChillNum)%GenBranchNum,   &
                                IndirectAbsorber(ChillNum)%GenCompNum)

      IF(GenMassFlowRate .LE. 0.0d0)THEN
        GenOutletTemp       = Node(GeneratorInletNode)%Temp
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy
      ELSE
        GenOutletTemp       = Node(GeneratorInletNode)%Temp - QGenerator/(CpFluid*GenMassFlowRate)
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy - QGenerator/GenMassFlowRate
      END IF

    ELSE ! using a steam plant for the generator

      EnthSteamOutDry   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,1.0d0, &
                                                IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                               'CALC Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name))
      EnthSteamOutWet   = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,0.0d0, &
                                                IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                               'CALC Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name))
      SteamDeltaT       = IndirectAbsorber(ChillNum)%GeneratorSubCool
      SteamOutletTemp   = Node(GeneratorInletNode)%Temp - SteamDeltaT
      HfgSteam          = EnthSteamOutDry - EnthSteamOutWet
      CpFluid           = GetSpecificHeatGlycol('WATER', SteamOutletTemp, DummyWaterIndex, &
                                                'CALC Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name) )
      GenMassFlowRate = QGenerator/(HfgSteam+CpFluid*SteamDeltaT)
      Call SetComponentFlowRate(GenMassFlowRate, &
                                GeneratorInletNode, GeneratorOutletNode, &
                                IndirectAbsorber(ChillNum)%GenLoopNum,     &
                                IndirectAbsorber(ChillNum)%GenLoopSideNum, &
                                IndirectAbsorber(ChillNum)%GenBranchNum,   &
                                IndirectAbsorber(ChillNum)%GenCompNum)

      IF(GenMassFlowRate .LE. 0.0d0)THEN
        GenOutletTemp       = Node(GeneratorInletNode)%Temp
        SteamOutletEnthalpy = Node(GeneratorInletNode)%Enthalpy
      ELSE
        GenOutletTemp       = Node(GeneratorInletNode)%Temp - SteamDeltaT
        SteamOutletEnthalpy = GetSatEnthalpyRefrig('STEAM',Node(GeneratorInletNode)%Temp,0.0d0, &
                                                IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                               'Loop Losses: Chiller:Absorption:Indirect'//TRIM(IndirectAbsorber(ChillNum)%Name))
        CpFluid           = GetSpecificHeatGlycol('WATER', Node(GeneratorInletNode)%Temp, DummyWaterIndex, &
                                                'CALC Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name) )

        SteamOutletEnthalpy = SteamOutletEnthalpy - CpFluid*SteamDeltaT

        !************************* Loop Losses *****************************
        TempWaterAtmPress = GetSatTemperatureRefrig('Steam',OutBaroPress, &
                                               IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                               'Loop Losses: Chiller:Absorption:Indirect'//TRIM(IndirectAbsorber(ChillNum)%Name))

        EnthAtAtmPress = GetSatEnthalpyRefrig('STEAM',TempWaterAtmPress,0.0d0, &
                                               IndirectAbsorber(ChillNum)%SteamFluidIndex, &
                                              'Loop Losses: Chiller:Absorption:Indirect '//TRIM(IndirectAbsorber(ChillNum)%Name))

        ! Point 4 at atm - loop delta subcool during return journery back to pump
        TempLoopOutToPump = TempWaterAtmPress - IndirectAbsorber(ChillNum)%LoopSubCool

        ! Reported value of coil outlet enthalpy at the node to match the node outlet temperature
        EnthPumpInlet = EnthAtAtmPress - CpFluid * IndirectAbsorber(ChillNum)%LoopSubCool

        ! Point 3-Point 5,
        EnergyLossToEnvironment=GenMassFlowRate*(SteamOutletEnthalpy-EnthPumpInlet)

        !************************* Loop Losses *****************************

        GenOutletTemp       = TempLoopOutToPump
        SteamOutletEnthalpy = EnthPumpInlet

      END IF ! IF(GenMassFlowRate .LE. 0.0d0)THEN

    END IF ! IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN

  END IF ! IF(GeneratorInletNode .GT. 0)THEN

        !convert power to energy
        GeneratorEnergy     = QGenerator   * TimeStepSys * SecInHour
        EvaporatorEnergy    = QEvaporator  * TimeStepSys * SecInHour
        CondenserEnergy     = QCondenser   * TimeStepSys * SecInHour
        PumpingEnergy       = PumpingPower * TimeStepSys * SecInHour

  RETURN

!                              ------
!                            /        \
!                          /           |
!                       6/-------------1 - Boiler Outlet Temp/Enthalpy/Pressure
!                    /  /             /.
!                 /    /             / . \
!               /    /              /  .   pressure drop (PD) across steam pressure regulator
! P           /     /              /   . /
! r         5      /              /    .
! e        /    3-2'-------------2------ - Generator Inlet Temp/Enthalpy/Pressure
! s       /     |/              /
! s      /      |  PD across   /      2-2' latent heat of vaporization (neglecting amount of superheat due to PD)
! u     /      /| condensate  /       1-3  delta H in generator
! r    /      / |   trap     /        2'-3 subcooling of hot water in generator
! e   4------/--3'          /         3-3' pressure drop at generator hot-water condensate trap
!           /              /          3-4  loop subcooling back to loop pump
!          /              /           4-5  pressure/temp/enthalpy increase due to loop condensate pump
!         /              /            5-6  heat addition in boiler to return condensate
!        /              /             6-1  heat of vaporization in boiler of return condensate to steam
!____________________________________
!         Enthalpy (H)

END SUBROUTINE CalcIndirectAbsorberModel

! End of Absorption Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************

SUBROUTINE UpdateIndirectAbsorberRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          R. Raustad (FSEC)
            !       DATE WRITTEN:    May 2008

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE PlantUtilities, ONLY: SafeCopyPlantNode

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)          :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Absorber operating
  INTEGER, INTENT(IN)      :: Num       ! Absorber number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER :: CondInletNode       ! condenser inlet node number, water side
  INTEGER :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER :: GeneratorInletNode  ! generator inlet node number, steam/water side
  INTEGER :: GeneratorOutletNode ! generator outlet node number, steam/water side


  EvapInletNode       = IndirectAbsorber(Num)%EvapInletNodeNum
  EvapOutletNode      = IndirectAbsorber(Num)%EvapOutletNodeNum
  CondInletNode       = IndirectAbsorber(Num)%CondInletNodeNum
  CondOutletNode      = IndirectAbsorber(Num)%CondOutletNodeNum
  GeneratorInletNode  = IndirectAbsorber(Num)%GeneratorInletNodeNum
  GeneratorOutletNode = IndirectAbsorber(Num)%GeneratorOutletNodeNum


  IF (MyLoad>=0 .OR. .NOT. RunFlag)THEN
          !set node temperature
    CALL SafeCopyPlantNode(EvapInletNode , EvapOutletNode)
    CALL SafeCopyPlantNode(CondInletNode , CondOutletNode)

    IndirectAbsorberReport(Num)%PumpingPower     = 0.0d0
    IndirectAbsorberReport(Num)%QEvap            = 0.0d0
    IndirectAbsorberReport(Num)%QCond            = 0.0d0
    IndirectAbsorberReport(Num)%QGenerator       = 0.0d0
    IndirectAbsorberReport(Num)%PumpingEnergy    = 0.0d0
    IndirectAbsorberReport(Num)%EvapEnergy       = 0.0d0
    IndirectAbsorberReport(Num)%CondEnergy       = 0.0d0
    IndirectAbsorberReport(Num)%GeneratorEnergy  = 0.0d0
    IndirectAbsorberReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    IndirectAbsorberReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    IndirectAbsorberReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    IndirectAbsorberReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    IndirectAbsorberReport(Num)%Evapmdot         = 0.0d0
    IndirectAbsorberReport(Num)%Condmdot         = 0.0d0
    IndirectAbsorberReport(Num)%Genmdot         = 0.0d0
    IndirectAbsorberReport(Num)%ActualCOP        = 0.0d0
    IndirectAbsorberReport(Num)%ChillerPartLoadRatio = 0.0d0
    IndirectAbsorberReport(Num)%LoopLoss         = 0.0d0
    IndirectAbsorberReport(Num)%ChillerCyclingFrac = 0.0d0

    IF(GeneratorInletNode .GT. 0)THEN
      CALL SafeCopyPlantNode(GeneratorInletNode , GeneratorOutletNode)
    END IF

  ELSE
          !set node temperatures
    CALL SafeCopyPlantNode(EvapInletNode , EvapOutletNode)
    CALL SafeCopyPlantNode(CondInletNode , CondOutletNode)
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    IndirectAbsorberReport(Num)%PumpingPower     = PumpingPower
    IndirectAbsorberReport(Num)%QEvap            = QEvaporator
    IndirectAbsorberReport(Num)%QCond            = QCondenser
    IndirectAbsorberReport(Num)%QGenerator       = QGenerator
    IndirectAbsorberReport(Num)%PumpingEnergy    = PumpingEnergy
    IndirectAbsorberReport(Num)%EvapEnergy       = EvaporatorEnergy
    IndirectAbsorberReport(Num)%CondEnergy       = CondenserEnergy
    IndirectAbsorberReport(Num)%GeneratorEnergy  = GeneratorEnergy
    IndirectAbsorberReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    IndirectAbsorberReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    IndirectAbsorberReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    IndirectAbsorberReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    IndirectAbsorberReport(Num)%Evapmdot         = EvapMassFlowRate
    IndirectAbsorberReport(Num)%Condmdot         = CondMassFlowRate
    IndirectAbsorberReport(Num)%Genmdot          = GenMassFlowRate
    IndirectAbsorberReport(Num)%LoopLoss         = EnergyLossToEnvironment
    IndirectAbsorberReport(Num)%ChillerCyclingFrac = ChillerONOFFCyclingFrac

    IF (QGenerator .NE. 0.0d0) THEN
      IndirectAbsorberReport(Num)%ActualCOP      = QEvaporator/QGenerator
    ELSE
      IndirectAbsorberReport(Num)%ActualCOP      = 0.0d0
    END IF

    IF(GeneratorInletNode .GT. 0)THEN
      CALL SafeCopyPlantNode(GeneratorInletNode , GeneratorOutletNode)
      Node(GeneratorOutletNode)%Temp          = GenOutletTemp
    END IF

  END IF

RETURN
END SUBROUTINE UpdateIndirectAbsorberRecords

! End of Record Keeping subroutines for the Absorption Chiller Module
! *****************************************************************************


END MODULE ChillerIndirectAbsorption

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


