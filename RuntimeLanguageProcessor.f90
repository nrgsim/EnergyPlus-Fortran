MODULE RuntimeLanguageProcessor

        ! MODULE INFORMATION:
        !       AUTHOR         Peter Graham Ellis
        !       DATE WRITTEN   June 2006
        !       MODIFIED       Brent Griffith, May - August 2009
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS MODULE:
        !

        ! METHODOLOGY EMPLOYED:
        !

        ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, OutputFileDebug
USE DataRuntimeLanguage

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER, PUBLIC :: MaxErrors = 20

! keyword parameters for types of Erl statements
INTEGER, PARAMETER :: KeywordNone   = 0 ! statement type not set
INTEGER, PARAMETER :: KeywordReturn = 1 ! Return statement, as in leave program
INTEGER, PARAMETER :: KeywordGoto   = 2 ! Goto statement, used in parsing to manage IF-ElseIf-Else-EndIf and nesting
INTEGER, PARAMETER :: KeywordSet    = 3 ! Set statement, as in assign RHS to LHS
INTEGER, PARAMETER :: KeywordRun    = 4 ! Run statement, used to call a subroutine from a main program
INTEGER, PARAMETER :: KeywordIf     = 5 ! If statement, begins an IF-ElseIf-Else-EndIf logic block
INTEGER, PARAMETER :: KeywordElseIf = 6 ! ElseIf statement, begins an ElseIf block
INTEGER, PARAMETER :: KeywordElse   = 7 ! Else statement, begins an Else block
INTEGER, PARAMETER :: KeywordEndIf  = 8 ! EndIf statement, terminates an IF-ElseIf-Else-EndIf logic block
INTEGER, PARAMETER :: KeywordWhile  = 9 ! While statement, begins a While block
INTEGER, PARAMETER :: KeywordEndWhile = 10 ! EndWhile statement, terminates a While block

! token type parameters for Erl code parsing
INTEGER, PARAMETER :: TokenNumber      = 1 ! matches the ValueNumber
INTEGER, PARAMETER :: TokenVariable    = 4 ! matches the ValueVariable
INTEGER, PARAMETER :: TokenExpression  = 5 ! matches the ValueExpression
INTEGER, PARAMETER :: TokenOperator    = 7 ! includes basic operators and built-in functions.

INTEGER, PARAMETER :: TokenParenthesis = 9  ! parenthesis token

INTEGER, PARAMETER :: ParenthesisLeft  = 10 ! indicates left side parenthesis found in parsing
INTEGER, PARAMETER :: ParenthesisRight = 11 ! indicates right side parenthesis found in parsing

          ! DERIVED TYPE DEFINITIONS:
TYPE TokenType
  ! structure for token information for parsing Erl code
  INTEGER                        :: Type        = 0   ! token type, eg. TokenNumber
  REAL(r64)                      :: Number      = 0.0D0 ! May want to store all literals as a variable?
  CHARACTER(len=2*MaxNameLength) :: String      = ''  ! Serves double duty, also saves string version of token for easy debugging
  INTEGER                        :: Operator    = 0   ! indentifies operator or function 1..64
  INTEGER                        :: Variable    = 0   ! points to a variable in ErlVariable structure
  INTEGER                        :: Parenthesis = 0   ! identifes if token is left or right parenthesis
  INTEGER                        :: Expression  = 0   ! points to an expression in ErlExpression structure
  CHARACTER(len=2*MaxNameLength) :: Error = ''        ! holds token processing error message content
END TYPE TokenType

TYPE RuntimeReportVarType
  CHARACTER(len=MaxNameLength) :: Name        = ''  ! name of custom Erl report variable
  INTEGER                      :: VariableNum = 0   ! pointer to Erl variable associated with custom report variable
  REAL(r64)                    :: Value       = 0.0D0 ! Value registered with output processor for report variable
END TYPE RuntimeReportVarType

          ! MODULE VARIABLE TYPE DECLARATIONS:

TYPE(RuntimeReportVarType), DIMENSION(:), ALLOCATABLE :: RuntimeReportVar

          ! INTERFACE BLOCK SPECIFICATIONS: na

          ! MODULE VARIABLE DECLARATIONS:

LOGICAL :: GetInput       = .TRUE.
LOGICAL :: InitializeOnce = .TRUE.
LOGICAL :: MyEnvrnFlag    = .TRUE.

! index pointer references to dynamic built-in variables
INTEGER :: NullVariableNum = 0
INTEGER :: FalseVariableNum = 0
INTEGER :: TrueVariableNum = 0
INTEGER :: OffVariableNum = 0
INTEGER :: OnVariableNum = 0
INTEGER :: PiVariableNum = 0
INTEGER, DIMENSION(:), ALLOCATABLE :: CurveIndexVariableNums
INTEGER, DIMENSION(:), ALLOCATABLE :: ConstructionIndexVariableNums
INTEGER :: YearVariableNum = 0
INTEGER :: MonthVariableNum = 0
INTEGER :: DayOfMonthVariableNum = 0
INTEGER :: DayOfWeekVariableNum = 0
INTEGER :: DayOfYearVariableNum = 0
INTEGER :: HourVariableNum = 0
INTEGER :: MinuteVariableNum = 0
INTEGER :: HolidayVariableNum = 0
INTEGER :: DSTVariableNum = 0
INTEGER :: CurrentTimeVariableNum = 0
INTEGER :: SunIsUpVariableNum = 0
INTEGER :: IsRainingVariableNum = 0
INTEGER :: SystemTimeStepVariableNum = 0
INTEGER :: ZoneTimeStepVariableNum = 0
INTEGER :: CurrentEnvironmentPeriodNum = 0
INTEGER :: ActualDateAndTimeNum = 0
INTEGER :: ActualTimeNum = 0

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC InitializeRuntimeLanguage
PUBLIC BeginEnvrnInitializeRuntimeLanguage
PRIVATE ParseStack
PRIVATE AddInstruction
PRIVATE AddError
PUBLIC EvaluateStack
PRIVATE WriteTrace
PRIVATE ParseExpression
PRIVATE ProcessTokens
PRIVATE NewExpression
PRIVATE EvaluateExpression
PRIVATE GetRuntimeLanguageUserInput
PUBLIC ReportRuntimeLanguage
PUBLIC FindEMSVariable
PUBLIC NewEMSVariable
PUBLIC SetErlValueNumber
PUBLIC ExternalInterfaceSetErlVariable
PUBLIC ExternalInterfaceInitializeErlVariable
PUBLIC isExternalInterfaceErlVariable
CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE InitializeRuntimeLanguage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Rui Zhang February 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! One time run.  Must be run BEFORE anything gets parsed.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi, HourOfDay, OutputFileDebug, CurrentTime,TimeStepZone
  USE DataEnvironment, ONLY: Year, Month, DayOfMonth, DayOfWeek, DayOfYear, &
                             SunIsUp, IsRain, HolidayIndex, DSTIndicator, CurEnvirNum
  USE DataHVACGlobals, ONLY: TimeStepSys, SysTimeElapsed

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: tmpCurrentTime = 0.d0
  REAL(r64) :: tmpMinutes     = 0.0D0
  REAL(r64) :: tmpHours       = 0.0D0
  REAL(r64) :: tmpCurEnvirNum = 0.0D0
  integer, dimension(8) :: datevalues
!value(1)   Current year
!value(2)   Current month
!value(3)   Current day
!value(4)   Time difference with respect to UTC in minutes (0-59)
!value(5)   Hour of the day (0-23)
!value(6)   Minutes (0-59)
!value(7)   Seconds (0-59)
!value(8)   Milliseconds (0-999)

  character(len=15) datestring  ! supposedly returns blank when no date available.

          ! FLOW:
  IF (InitializeOnce) THEN

    False = SetErlValueNumber(0.0d0)
    True = SetErlValueNumber(1.0d0)

    ! Create constant built-in variables
    NullVariableNum  = NewEMSVariable('NULL', 0)
    ErlVariable(NullVariableNum)%Value%Type = ValueNull
    FalseVariableNum = NewEMSVariable('FALSE', 0, False)
    TrueVariableNum  = NewEMSVariable('TRUE', 0, True)
    OffVariableNum   = NewEMSVariable('OFF', 0, False)
    OnVariableNum    = NewEMSVariable('ON', 0, True)
    PiVariableNum    = NewEMSVariable('PI', 0, SetErlValueNumber(Pi))

    ! Create dynamic built-in variables
    YearVariableNum           = NewEMSVariable('YEAR', 0)
    MonthVariableNum          = NewEMSVariable('MONTH', 0)
    DayOfMonthVariableNum     = NewEMSVariable('DAYOFMONTH', 0)  ! 'DAYOFMONTH'?
    DayOfWeekVariableNum      = NewEMSVariable('DAYOFWEEK', 0)
    DayOfYearVariableNum      = NewEMSVariable('DAYOFYEAR', 0)
    HourVariableNum           = NewEMSVariable('HOUR', 0)
    MinuteVariableNum         = NewEMSVariable('MINUTE', 0)
    HolidayVariableNum        = NewEMSVariable('HOLIDAY', 0)
    DSTVariableNum            = NewEMSVariable('DAYLIGHTSAVINGS',0)
    CurrentTimeVariableNum    = NewEMSVariable('CURRENTTIME',0)
    SunIsUpVariableNum        = NewEMSVariable('SUNISUP', 0)
    IsRainingVariableNum      = NewEMSVariable('ISRAINING', 0)
    SystemTimeStepVariableNum = NewEMSVariable('SYSTEMTIMESTEP', 0)
    ZoneTimeStepVariableNum   = NewEMSVariable('ZONETIMESTEP', 0)
    ErlVariable(ZoneTimeStepVariableNum)%Value     = SetErlValueNumber(TimeStepZone)
    CurrentEnvironmentPeriodNum = NewEMSVariable('CURRENTENVIRONMENT', 0)
    ActualDateAndTimeNum = NewEMSVariable('ACTUALDATEANDTIME', 0)
    ActualTimeNum = NewEMSVariable('ACTUALTIME', 0)

    CALL GetRuntimeLanguageUserInput  ! Load and parse all runtime language objects

    CALL DATE_AND_TIME(date=datestring,values=datevalues)
    IF (datestring /= ' ') THEN
      ErlVariable(ActualDateAndTimeNum)%Value  = SetErlValueNumber(REAL(SUM(datevalues),r64))
        !datevalues(1)+datevalues(2)+datevalues(3)+  &
        !datevalues(5)+datevalues(6)+datevalues(7)+datevalues(8)
      ErlVariable(ActualTimeNum)%Value  =  SetErlValueNumber(REAL(SUM(datevalues(5:8)),r64))
      !datevalues(5)+datevalues(6)+datevalues(7)+datevalues(8)
!    ELSE
!      ErlVariable(ActualDateAndTimeNum)%Value  = SetErlValueNumber(real(RANDOM_NUMBER(X=509),r64))
!      ErlVariable(ActualTimeNum)%Value  = SetErlValueNumber(real(RANDOM_NUMBER(X=400),r64))
    ENDIF

    InitializeOnce = .FALSE.
  END IF

  ! Update built-in variables
  ErlVariable(YearVariableNum)%Value       = SetErlValueNumber(REAL(Year,r64))
  ErlVariable(MonthVariableNum)%Value      = SetErlValueNumber(REAL(Month,r64))
  ErlVariable(DayOfMonthVariableNum)%Value = SetErlValueNumber(REAL(DayOfMonth,r64))
  ErlVariable(DayOfWeekVariableNum)%Value  = SetErlValueNumber(REAL(DayOfWeek, r64))
  ErlVariable(DayOfYearVariableNum)%Value  = SetErlValueNumber(REAL(DayOfYear, r64))

  ErlVariable(DSTVariableNum)%Value        = SetErlValueNumber(REAL(DSTIndicator, r64))
   !DSTadjust = REAL(DSTIndicator, r64)
  tmpHours  = REAL(HourOfDay-1,r64) ! no, just stay on 0..23+ DSTadjust ! offset by 1 and daylight savings time
  ErlVariable(HourVariableNum)%Value     = SetErlValueNumber(tmpHours)

  IF (TimeStepSys < TimeStepZone) THEN
    !CurrentTime is for end of zone timestep, need to account for system timestep
    tmpCurrentTime = CurrentTime - TimeStepZone + SysTimeElapsed + TimeStepSys
  ELSE
    tmpCurrentTime = CurrentTime
  ENDIF
  ErlVariable(CurrentTimeVariableNum)%Value   = SetErlValueNumber(tmpCurrentTime)
  tmpMinutes = ((tmpCurrentTime - REAL(HourOfDay-1,r64)) * 60.0D0) !- 1.d0 ! off by 1
  ErlVariable(MinuteVariableNum)%Value   = SetErlValueNumber(tmpMinutes)
  ErlVariable(HolidayVariableNum)%Value  = SetErlValueNumber(REAL(HolidayIndex,r64))
  IF (SunIsUp) THEN
    ErlVariable(SunIsUpVariableNum)%Value = SetErlValueNumber(1.0D0)
  ELSE
    ErlVariable(SunIsUpVariableNum)%Value = SetErlValueNumber(0.0D0)
  ENDIF
  IF (IsRain) THEN
    ErlVariable(IsRainingVariableNum)%Value = SetErlValueNumber(1.0D0)
  ELSE
    ErlVariable(IsRainingVariableNum)%Value = SetErlValueNumber(0.0D0)
  ENDIF
  ErlVariable(SystemTimeStepVariableNum)%Value   = SetErlValueNumber(TimeStepSys)

  tmpCurEnvirNum = REAL(CurEnvirNum, r64)
  ErlVariable(CurrentEnvironmentPeriodNum)%Value = SetErlValueNumber(tmpCurEnvirNum)

  RETURN

END SUBROUTINE InitializeRuntimeLanguage

SUBROUTINE BeginEnvrnInitializeRuntimeLanguage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2010
          !       MODIFIED       B. Griffith, added Sensor initialation
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! re initialize Erl for new simulation environment period

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use OutputProcessor , ONLY : SetInternalVariableValue

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
  INTEGER :: ActuatorUsedLoop
  INTEGER :: EMSActuatorVariableNum
  INTEGER :: ErlVariableNum
  INTEGER :: TrendVarNum
  INTEGER :: SensorNum
  INTEGER :: TrendDepth
  INTEGER :: Loop
  LOGICAL :: CycleThisVariable

  !reinitialize state of Erl variable values to zero, this gets sensors and internal variables used
  DO ErlVariableNum = 1, NumErlVariables
    !but skip constant built-in variables so don't overwrite them
    IF (ErlVariableNum == NullVariableNum)  CYCLE
    IF (ErlVariableNum == FalseVariableNum) CYCLE
    IF (ErlVariableNum == TrueVariableNum)  CYCLE
    IF (ErlVariableNum == OffVariableNum)   CYCLE
    IF (ErlVariableNum == OnVariableNum)    CYCLE
    IF (ErlVariableNum == PiVariableNum)    CYCLE
    IF (ErlVariableNum == ZoneTimeStepVariableNum) CYCLE
    IF (ErlVariableNum == ActualDateAndTimeNum) CYCLE
    IF (ErlVariableNum == ActualTimeNum) CYCLE

    ! need to preserve curve index variables
    CycleThisVariable = .FALSE.
    DO Loop = 1, NumEMSCurveIndices
      IF ( ErlVariableNum == CurveIndexVariableNums(Loop)) CycleThisVariable = .TRUE.
    ENDDO
    IF (CycleThisVariable) CYCLE
    CycleThisVariable = .FALSE.
    DO Loop = 1, NumEMSConstructionIndices
      IF ( ErlVariableNum == ConstructionIndexVariableNums(Loop)) CycleThisVariable = .TRUE.
    ENDDO
    IF (CycleThisVariable) CYCLE

    ErlVariable(ErlVariableNum)%Value = SetErlValueNumber(0.0D0, &
                                         OrigValue = ErlVariable(ErlVariableNum)%Value)

  ENDDO
  !reinitialize state of actuators
  DO ActuatorUsedLoop = 1, NumActuatorsUsed + NumExternalInterfaceActuatorsUsed
    EMSActuatorVariableNum = EMSActuatorUsed(ActuatorUsedLoop)%ActuatorVariableNum
    ErlVariableNum = EMSActuatorUsed(ActuatorUsedLoop)%ErlVariableNum
    ErlVariable(ErlVariableNum)%Value%TYPE = ValueNull
    EMSActuatorAvailable(EMSActuatorVariableNum)%Actuated  = .FALSE.
    SELECT CASE (EMSActuatorAvailable(EMSActuatorVariableNum)%PntrVarTypeUsed)
    CASE (PntrReal)
      EMSActuatorAvailable(EMSActuatorVariableNum)%RealValue = 0.0D0
    CASE (PntrInteger)
      EMSActuatorAvailable(EMSActuatorVariableNum)%IntValue  = 0
    CASE (PntrLogical)
      EMSActuatorAvailable(EMSActuatorVariableNum)%LogValue  = .FALSE.
    END SELECT
  ENDDO

  !reinitialize trend variables so old data are purged
  DO TrendVarNum = 1, NumErlTrendVariables
    TrendDepth = TrendVariable(TrendVarNum)%LogDepth
    TrendVariable(TrendVarNum)%TrendValARR(1:TrendDepth) = 0.0D0
  ENDDO

  ! reinitilize sensors
  DO SensorNum = 1, NumSensors
    CALL SetInternalVariableValue(Sensor(SensorNum)%Type, Sensor(SensorNum)%Index, 0.d0,0)
  ENDDO

  RETURN

END SUBROUTINE BeginEnvrnInitializeRuntimeLanguage


SUBROUTINE ParseStack(StackNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith June 2009
          !                      Brent Griffith March 2012, add WHILE loops
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Parsing a block of text creates a program stack in DataRuntimeLanguage.
          ! This routine only executes once for each Erl program.

          ! METHODOLOGY EMPLOYED:
          ! Loop over each line of Erl code and parse based on statement keyword

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUpperCase, ProcessNumber, FindItemInList
  USE DataSystemVariables, ONLY: DeveloperFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: StackNum  !


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: IfDepthAllowed = 5 ! depth of IF block nesting
  INTEGER, PARAMETER :: ELSEIFLengthAllowed = 200 ! number of ELSEIFs allowed
  INTEGER, PARAMETER :: WhileDepthAllowed = 1 ! depth of While block nesting
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: LineNum
  INTEGER :: StackNum2
  INTEGER :: Pos
  INTEGER :: ExpressionNum
  INTEGER :: VariableNum
  CHARACTER(len=MaxNameLength) :: Line  ! local copy of a single line of Erl program code
  CHARACTER(len=MaxNameLength) :: Keyword ! local copy of statement keyword parsed from line (Run, Set, If, etc)
  CHARACTER(len=MaxNameLength) :: Remainder ! local copy of what is left for text in the line after keyword
  CHARACTER(len=MaxNameLength) :: Expression !
  CHARACTER(len=MaxNameLength) :: Variable
  INTEGER :: NestedIFDepth ! indicates depth into If statement,
  INTEGER :: NestedWhileDepth ! indicates depth into While statement
  INTEGER :: InstructionNum
  INTEGER :: InstructionNum2
  INTEGER :: GotoNum
  INTEGER, DIMENSION(IfDepthAllowed) :: SavedIfInstructionNum ! index is depth of If statements
  INTEGER, DIMENSION(IfDepthAllowed, ELSEIFLengthAllowed) :: SavedGotoInstructionNum
  INTEGER, DIMENSION(IfDepthAllowed) :: NumGotos       !  index is depth of If statements,
  INTEGER :: SavedWhileInstructionNum
  INTEGER :: SavedWhileExpressionNum
  INTEGER :: NumWhileGotos
  LOGICAL, DIMENSION(IfDepthAllowed) :: ReadyForElse
  LOGICAL, DIMENSION(IfDepthAllowed) :: ReadyForEndif

!  CHARACTER(len=2*MaxNameLength), DIMENSION(:), ALLOCATABLE :: DummyError

          ! FLOW:
  LineNum = 1
  NestedIFDepth = 0
  ReadyForElse=.false.
  ReadyForEndif=.false.
  SavedIfInstructionNum = 0
  SavedGotoInstructionNum = 0
  NumGotos = 0
  NestedWhileDepth = 0
  SavedWhileInstructionNum =0
  SavedWhileExpressionNum = 0
  NumWhileGotos = 0

  DO WHILE (LineNum <= ErlStack(StackNum)%NumLines)

    Line = ADJUSTL(ErlStack(StackNum)%Line(LineNum))
    IF (LEN_TRIM(Line) == 0) THEN
      LineNum = LineNum + 1
      CYCLE  ! Blank lines can be skipped
    END IF

    Pos = SCAN(TRIM(Line), ' ')
    IF (Pos == 0) Pos = LEN_TRIM(Line) + 1
!    Keyword = MakeUpperCase(Line(1:Pos-1))
    Keyword = Line(1:Pos-1)
    Remainder = ADJUSTL(Line(Pos+1:))

    SELECT CASE (Keyword)

      CASE ('RETURN')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'RETURN "'//trim(Line)//'"'
        IF (LEN_TRIM(Remainder) == 0) THEN
          InstructionNum = AddInstruction(StackNum, LineNum, KeywordReturn)
        ELSE
          CALL ParseExpression(Remainder, StackNum, ExpressionNum, Line)
          InstructionNum = AddInstruction(StackNum, LineNum, KeywordReturn, ExpressionNum)
        END IF

      CASE ('SET')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'SET "'//trim(Line)//'"'
        Pos = SCAN(Remainder, '=')
        IF (Pos == 0) THEN
          CALL AddError(StackNum, LineNum, 'Equal sign missing for the SET instruction.')
        ELSE IF (Pos == 1) THEN
          CALL AddError(StackNum, LineNum, 'Variable name missing for the SET instruction.')
        ELSE
          Variable = ADJUSTL(Remainder(1:Pos-1))  ! VariableName would be more expressive
          VariableNum = NewEMSVariable(Variable, StackNum)
          ! Check for invalid variable name

          Expression = ADJUSTL(Remainder(Pos+1:))
          IF (LEN_TRIM(Expression) == 0) THEN
            CALL AddError(StackNum, LineNum, 'Expression missing for the SET instruction.')
          ELSE
            CALL ParseExpression(Expression, StackNum, ExpressionNum, Line)
            InstructionNum = AddInstruction(StackNum, LineNum, KeywordSet, VariableNum, ExpressionNum)
          END IF
        END IF

      CASE ('RUN')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'RUN "'//trim(Line)//'"'
        IF (LEN_TRIM(Remainder) == 0) THEN
          CALL AddError(StackNum, LineNum, 'Program or Subroutine name missing for the RUN instruction.')
        ELSE
          Pos = SCAN(Remainder, ' ')
          Variable = MakeUpperCase(ADJUSTL(Remainder(1:Pos-1)))  ! really the subroutine, or reference to instruction set
          StackNum2 = FindItemInList(Variable, ErlStack%Name, NumErlStacks)
          IF (StackNum2 == 0) THEN
            CALL AddError(StackNum, LineNum,'Program or Subroutine name ['//TRIM(Variable)//'] not found for the RUN instruction.')
          ELSE
            InstructionNum = AddInstruction(StackNum, LineNum, KeywordRun, StackNum2)
          END IF
        END IF

      CASE ('IF')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'IF "'//trim(Line)//'"'
        IF (DeveloperFlag) write(OutputFileDebug,*) 'NestedIf=',NestedIFDepth
        IF (LEN_TRIM(Remainder) == 0) THEN
          CALL AddError(StackNum, LineNum, 'Expression missing for the IF instruction.')
          ExpressionNum = 0
        ELSE
          Expression = ADJUSTL(Remainder)
          CALL ParseExpression(Expression, StackNum, ExpressionNum, Line)
        END IF

        NestedIFDepth = NestedIFDepth + 1
        ReadyForElse(NestedIFDepth)=.true.
        ReadyForEndif(NestedIFDepth)=.true.
        IF (NestedIFDepth > IfDepthAllowed) THEN
          CALL AddError(StackNum,LineNum, 'Detected IF nested deeper than is allowed; need to terminate an earlier IF instruction.')
          EXIT
        ELSE
          InstructionNum = AddInstruction(StackNum, LineNum, KeywordIf, ExpressionNum)  ! Arg2 added at next ELSEIF, ELSE, ENDIF
          SavedIfInstructionNum(NestedIFDepth) = InstructionNum
        ENDIF

      CASE ('ELSEIF')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ELSEIF "'//trim(Line)//'"'
        IF (DeveloperFlag) write(OutputFileDebug,*) 'NestedIf=',NestedIFDepth
        IF (NestedIFDepth == 0) THEN
          CALL AddError(StackNum, LineNum, 'Starting IF instruction missing for the ELSEIF instruction.')
          EXIT  ! Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
        END IF

        ! Complete the preceding block with a GOTO instruction
        InstructionNum = AddInstruction(StackNum, 0, KeywordGoto)  ! Arg2 is added at the ENDIF
        NumGotos(NestedIFDepth) = NumGotos(NestedIFDepth) + 1
        IF (NumGotos(NestedIFDepth) > ELSEIFLengthAllowed) THEN
          CALL AddError(StackNum,LineNum, 'Detected ELSEIF series that is longer than allowed; terminate earlier IF instruction.')
          EXIT
        ELSE
          SavedGotoInstructionNum(NestedIFDepth, NumGotos(NestedIFDepth)) = InstructionNum
        ENDIF

        IF (LEN_TRIM(Remainder) == 0) THEN
          CALL AddError(StackNum, LineNum, 'Expression missing for the ELSEIF instruction.')
          ExpressionNum = 0
        ELSE
          Expression = ADJUSTL(Remainder)
          CALL ParseExpression(Expression, StackNum, ExpressionNum, Line)
        END IF

        InstructionNum = AddInstruction(StackNum, LineNum, KeywordIf, ExpressionNum)  ! Arg2 added at next ELSEIF, ELSE, ENDIF
        ErlStack(StackNum)%Instruction(SavedIfInstructionNum(NestedIFDepth))%Argument2 = InstructionNum
        SavedIfInstructionNum(NestedIFDepth) = InstructionNum

      CASE ('ELSE')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ELSE "'//trim(Line)//'"'
        IF (DeveloperFlag) write(OutputFileDebug,*) 'NestedIf=',NestedIFDepth
        IF (NestedIFDepth == 0) THEN
          CALL AddError(StackNum, LineNum, 'Starting IF instruction missing for the ELSE instruction.')
          EXIT  ! Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
        END IF
        IF (.not. ReadyForElse(NestedIfDepth)) THEN
          CALL AddError(StackNum, LineNum, 'ELSE statement without corresponding IF stetement.')
        ENDIF
        ReadyForElse(NestedIfDepth)=.false.

        ! Complete the preceding block with a GOTO instruction
        InstructionNum = AddInstruction(StackNum, 0, KeywordGoto)  ! Arg2 is added at the ENDIF
        NumGotos(NestedIFDepth) = NumGotos(NestedIFDepth) + 1
        IF (NumGotos(NestedIFDepth) > ELSEIFLengthAllowed) THEN
          CALL AddError(StackNum,LineNum, 'Detected ELSEIF-ELSE series that is longer than allowed.')
          EXIT
        ELSE
          SavedGotoInstructionNum(NestedIFDepth, NumGotos(NestedIFDepth)) = InstructionNum
        ENDIF

        IF (LEN_TRIM(Remainder) > 0) THEN
          CALL AddError(StackNum, LineNum, 'Nothing is allowed to follow the ELSE instruction.')
        END IF

        InstructionNum = AddInstruction(StackNum, LineNum, KeywordElse)  ! can make this into a KeywordIf?
        ErlStack(StackNum)%Instruction(SavedIfInstructionNum(NestedIFDepth))%Argument2 = InstructionNum
        SavedIfInstructionNum(NestedIFDepth) = InstructionNum

      CASE ('ENDIF')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ENDIF "'//trim(Line)//'"'
        IF (DeveloperFlag) write(OutputFileDebug,*) 'NestedIf=',NestedIFDepth
        IF (NestedIFDepth == 0) THEN
          CALL AddError(StackNum, LineNum, 'Starting IF instruction missing for the ENDIF instruction.')
          EXIT  ! PE Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
        END IF

        IF (.not. ReadyForEndif(NestedIfDepth)) THEN
          CALL AddError(StackNum, LineNum, 'ENDIF statement without corresponding IF stetement.')
        ENDIF
        ReadyForEndif(NestedIfDepth)=.false.
        ReadyForElse(NestedIfDepth)=.false.

        IF (LEN_TRIM(Remainder) > 0) THEN
          CALL AddError(StackNum, LineNum, 'Nothing is allowed to follow the ENDIF instruction.')
        END IF

        InstructionNum = AddInstruction(StackNum, LineNum, KeywordEndIf)
        ErlStack(StackNum)%Instruction(SavedIfInstructionNum(NestedIFDepth))%Argument2 = InstructionNum

        ! Go back and complete all of the GOTOs that terminate each IF and ELSEIF block
        DO GotoNum = 1, NumGotos(NestedIFDepth)
          InstructionNum2 = SavedGotoInstructionNum(NestedIFDepth, GotoNum)
          ErlStack(StackNum)%Instruction(InstructionNum2)%Argument1 = InstructionNum
          SavedGotoInstructionNum(NestedIFDepth, GotoNum) = 0
        END DO

        NumGotos(NestedIFDepth) = 0
        SavedIfInstructionNum(NestedIFDepth) = 0
        NestedIFDepth = NestedIFDepth - 1

      CASE ('WHILE')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'WHILE "'//trim(Line)//'"'
        IF (LEN_TRIM(Remainder) == 0) THEN
          CALL AddError(StackNum, LineNum, 'Expression missing for the WHILE instruction.')
          ExpressionNum = 0
        ELSE
          Expression = ADJUSTL(Remainder)
          CALL ParseExpression(Expression, StackNum, ExpressionNum, Line)
        END IF

        NestedWhileDepth = NestedWhileDepth + 1
        IF (NestedWhileDepth > WhileDepthAllowed) THEN
          CALL AddError(StackNum,LineNum,   &
             'Detected WHILE nested deeper than is allowed; need to terminate an earlier WHILE instruction.')
          EXIT
        ELSE
          InstructionNum = AddInstruction(StackNum, LineNum, KeywordWhile, ExpressionNum)
          SavedWhileInstructionNum = InstructionNum
          SavedWhileExpressionNum  = ExpressionNum
        ENDIF

      CASE ('ENDWHILE')
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ENDWHILE "'//trim(Line)//'"'
        IF (NestedWhileDepth == 0) THEN
          CALL AddError(StackNum, LineNum, 'Starting WHILE instruction missing for the ENDWHILE instruction.')
          EXIT
        END IF
        IF (LEN_TRIM(Remainder) > 0) THEN
          CALL AddError(StackNum, LineNum, 'Nothing is allowed to follow the ENDWHILE instruction.')
        END IF

        InstructionNum = AddInstruction(StackNum, LineNum, KeywordEndWhile)
        ErlStack(StackNum)%Instruction(SavedWhileInstructionNum)%Argument2 = InstructionNum
        ErlStack(StackNum)%Instruction(InstructionNum)%Argument1 = SavedWhileExpressionNum
        ErlStack(StackNum)%Instruction(InstructionNum)%Argument2 = SavedWhileInstructionNum

        NestedWhileDepth = 0
        SavedWhileInstructionNum =0
        SavedWhileExpressionNum = 0

      CASE DEFAULT
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ERROR "'//trim(Line)//'"'
        CALL AddError(StackNum, LineNum, 'Unknown keyword ['//TRIM(Keyword)//'].')

    END SELECT

    LineNum = LineNum + 1
  END DO  ! LineNum

  IF (NestedIFDepth == 1) THEN
    CALL AddError(StackNum, 0, 'Missing an ENDIF instruction needed to terminate an earlier IF instruction.')
  ELSE IF (NestedIFDepth > 1) THEN
    CALL AddError(StackNum, 0, &
      'Missing '//TRIM(IntegerToString(NestedIFDepth))//' ENDIF instructions needed to terminate earlier IF instructions.')
  END IF

!  ALLOCATE(DummyError(ErlStack(StackNum)%NumErrors))
!  DummyError = ErlStack(StackNum)%Error

  RETURN

END SUBROUTINE ParseStack

FUNCTION AddInstruction(StackNum, LineNum, Keyword, Argument1, Argument2) RESULT(InstructionNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Adds an instruction to a stack.

          ! METHODOLOGY EMPLOYED:
          !

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                   :: StackNum
  INTEGER, INTENT(IN)                   :: LineNum
  INTEGER, INTENT(IN)                   :: Keyword
  INTEGER, INTENT(IN), OPTIONAL         :: Argument1 ! Erl variable index
  INTEGER, INTENT(IN), OPTIONAL         :: Argument2
  INTEGER                               :: InstructionNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(ErlStackType)        :: TempStack

          ! FLOW:
  IF (ErlStack(StackNum)%NumInstructions == 0) THEN
    ALLOCATE(ErlStack(StackNum)%Instruction(1))
    ErlStack(StackNum)%NumInstructions = 1
  ELSE
    TempStack = ErlStack(StackNum)
    DEALLOCATE(ErlStack(StackNum)%Instruction)
    ALLOCATE(ErlStack(StackNum)%Instruction(ErlStack(StackNum)%NumInstructions + 1))
    ErlStack(StackNum)%Instruction(1:ErlStack(StackNum)%NumInstructions) =   &
       TempStack%Instruction(1:ErlStack(StackNum)%NumInstructions)
    ErlStack(StackNum)%NumInstructions = ErlStack(StackNum)%NumInstructions + 1
  END IF

  InstructionNum = ErlStack(StackNum)%NumInstructions
  ErlStack(StackNum)%Instruction(InstructionNum)%LineNum = LineNum
  ErlStack(StackNum)%Instruction(InstructionNum)%Keyword = Keyword

  IF (Present(Argument1)) ErlStack(StackNum)%Instruction(InstructionNum)%Argument1 = Argument1
  IF (Present(Argument2)) ErlStack(StackNum)%Instruction(InstructionNum)%Argument2 = Argument2

  RETURN

END FUNCTION AddInstruction

SUBROUTINE AddError(StackNum, LineNum, Error)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Adds an error message to a stack.

          ! METHODOLOGY EMPLOYED:
          !

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                   :: StackNum ! index pointer to location in ErlStack structure
  INTEGER, INTENT(IN)                   :: LineNum ! Erl program line number
  CHARACTER(len=*), INTENT(IN)          :: Error ! error message to be added to ErlStack

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(ErlStackType)     :: TempStack ! temporary copy of single ErlStack
  INTEGER                :: ErrorNum  ! local count of errors for this ErlStack

          ! FLOW:
  IF (ErlStack(StackNum)%NumErrors == 0) THEN
    ALLOCATE(ErlStack(StackNum)%Error(1))
    ErlStack(StackNum)%NumErrors = 1
  ELSE
    TempStack = ErlStack(StackNum)
    DEALLOCATE(ErlStack(StackNum)%Error)
    ALLOCATE(ErlStack(StackNum)%Error(ErlStack(StackNum)%NumErrors + 1))
    ErlStack(StackNum)%Error(1:ErlStack(StackNum)%NumErrors) = TempStack%Error(1:ErlStack(StackNum)%NumErrors)
    ErlStack(StackNum)%NumErrors = ErlStack(StackNum)%NumErrors + 1
  END IF

  ErrorNum = ErlStack(StackNum)%NumErrors
  IF (LineNum > 0) THEN
    ErlStack(StackNum)%Error(ErrorNum) = &
      'Line '//TRIM(IntegerToString(LineNum))//':  '//Error//' "'//TRIM(ErlStack(StackNum)%Line(LineNum))//'"'
  ELSE
    ErlStack(StackNum)%Error(ErrorNum) = Error
  END IF

  RETURN

END SUBROUTINE AddError


RECURSIVE FUNCTION EvaluateStack(StackNum) RESULT(ReturnValue)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith, May 2009
          !                      Brent Griffith, March 2012, add While loop support
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Runs a stack with the interpreter.

          ! METHODOLOGY EMPLOYED:
          !
          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowFatalError


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: StackNum
  TYPE(ErlValueType) :: ReturnValue

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InstructionNum
  INTEGER :: InstructionNum2
  INTEGER :: ExpressionNum
  REAL(r64) :: ReturnValueActual  ! for testing
  INTEGER , SAVE :: VariableNum
  INTEGER :: WhileLoopExitCounter ! to avoid infinite loop in While loop

  WhileLoopExitCounter = 0
  ReturnValue%Type = ValueNumber
  ReturnValue%Number = 0.0d0

  InstructionNum = 1
  DO WHILE (InstructionNum <= ErlStack(StackNum)%NumInstructions)

    SELECT CASE (ErlStack(StackNum)%Instruction(InstructionNum)%Keyword)

      CASE (KeywordNone)
        ! There probably shouldn't be any of these

      CASE (KeywordReturn)
        IF (ErlStack(StackNum)%Instruction(InstructionNum)%Argument1 > 0) &
          ReturnValue = EvaluateExpression(ErlStack(StackNum)%Instruction(InstructionNum)%Argument1)

        CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
        EXIT  ! RETURN always terminates an instruction stack

      CASE (KeywordSet)


        ReturnValue = EvaluateExpression(ErlStack(StackNum)%Instruction(InstructionNum)%Argument2)
        VariableNum = ErlStack(StackNum)%Instruction(InstructionNum)%Argument1
        IF ((.NOT. ErlVariable(VariableNum)%ReadOnly) .AND. (.NOT. ErlVariable(VariableNum)%Value%TrendVariable)) THEN
           ErlVariable(VariableNum)%Value = ReturnValue
        ELSEIF (ErlVariable(VariableNum)%Value%TrendVariable) THEN
           ErlVariable(VariableNum)%Value%Number = ReturnValue%Number
           ErlVariable(VariableNum)%Value%Error  = ReturnValue%Error
        ENDIF

        CALL WriteTrace(StackNum, InstructionNum, ReturnValue)

      CASE (KeywordRun)
        ReturnValue%Type = ValueString
        ReturnValue%String = ''
        CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
        ReturnValue = EvaluateStack(ErlStack(StackNum)%Instruction(InstructionNum)%Argument1)

      CASE (KeywordIf, KeywordElse)  ! same???
        ExpressionNum = ErlStack(StackNum)%Instruction(InstructionNum)%Argument1
        InstructionNum2 = ErlStack(StackNum)%Instruction(InstructionNum)%Argument2

        IF (ExpressionNum > 0) THEN   ! could be 0 if this was an ELSE
          ReturnValue = EvaluateExpression(ExpressionNum)
          CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
          IF (ReturnValue%Number == 0.0d0) THEN  !  This is the FALSE case
               ! Eventually should handle strings and arrays too
            InstructionNum = InstructionNum2
            CYCLE
          END IF
        ELSE
          ! KeywordELSE  -- kind of a kludge
          ReturnValue%Type = ValueNumber
          ReturnValue%Number = 1.0d0
          CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
        END IF

      CASE (KeywordGoto)
        InstructionNum = ErlStack(StackNum)%Instruction(InstructionNum)%Argument1

        ! For debug purposes only...
        ReturnValue%Type = ValueString
        ReturnValue%String = '' !IntegerToString(InstructionNum)

        CYCLE
        ! PE if this ever went out of bounds, would the DO loop save it?  or need check here?

      CASE (KeywordEndIf)
        ReturnValue%Type = ValueString
        ReturnValue%String = ''
        CALL WriteTrace(StackNum, InstructionNum, ReturnValue)

      CASE (KeywordWhile)
        ! evaluate expresssion at while, skip to past endwhile if not true
        ExpressionNum = ErlStack(StackNum)%Instruction(InstructionNum)%Argument1
        InstructionNum2 = ErlStack(StackNum)%Instruction(InstructionNum)%Argument2
        ReturnValue = EvaluateExpression(ExpressionNum)
        CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
        IF (ReturnValue%Number == 0.0d0) THEN  !  This is the FALSE case
             ! Eventually should handle strings and arrays too
          InstructionNum = InstructionNum2
         ! CYCLE
        END IF
      CASE (KeywordEndWhile)

        ! reevaluate expression at While and goto there if true, otherwise continue
        ExpressionNum = ErlStack(StackNum)%Instruction(InstructionNum)%Argument1
        InstructionNum2 =ErlStack(StackNum)%Instruction(InstructionNum)%Argument2
        ReturnValue = EvaluateExpression(ExpressionNum)
        IF ((ReturnValue%Number /= 0.0d0) .AND. (WhileLoopExitCounter <= MaxWhileLoopIterations)) THEN  !  This is the True case
             ! Eventually should handle strings and arrays too
          CALL WriteTrace(StackNum, InstructionNum, ReturnValue) ! duplicative?
          InstructionNum = InstructionNum2
          WhileLoopExitCounter = WhileLoopExitCounter + 1

          CYCLE
        ELSE ! false, leave while block
          IF (WhileLoopExitCounter > MaxWhileLoopIterations) THEN
            WhileLoopExitCounter = 0
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Maximum WHILE loop iteration limit reached'
            CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
          ELSE
            ReturnValue%Type = ValueNumber
            ReturnValue%Number = 0.0d0
            CALL WriteTrace(StackNum, InstructionNum, ReturnValue)
            WhileLoopExitCounter = 0
          ENDIF
        END IF
      CASE DEFAULT
        CALL ShowFatalError('Fatal error in RunStack:  Unknown keyword.')

    END SELECT

    InstructionNum = InstructionNum + 1
  END DO  ! InstructionNum

  ReturnValueActual = (4.91d0 + 632.d0) / (32.d0 * (4.d0 - 10.2d0))  ! must have extra periods

  RETURN

END FUNCTION EvaluateStack


SUBROUTINE WriteTrace(StackNum, InstructionNum, ReturnValue)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith, May 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE General,                         ONLY : CreateSysTimeIntervalString
  USE DataEnvironment,                 ONLY : EnvironmentName,CurMnDy
  USE DataGlobals,                     ONLY : WarmupFlag,DoingSizing
  USE InputProcessor, ONLY: ProcessNumber
  USE DataGlobals, ONLY: OutputFileDebug
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)             :: StackNum
  INTEGER, INTENT(IN)             :: InstructionNum
  TYPE(ErlValueType), INTENT(IN)     :: ReturnValue

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=2*MaxNameLength) :: NameString
  CHARACTER(len=2*MaxNameLength) :: LineNumString
  CHARACTER(len=2*MaxNameLength) :: LineString
  CHARACTER(len=2*MaxNameLength) :: cValueString
  CHARACTER(len=2*MaxNameLength) :: TimeString
  INTEGER :: LineNum
  CHARACTER(len=45) :: DuringWarmup
  LOGICAL, SAVE :: MyOneTimeFlag = .FALSE.

          ! FLOW:
  IF ((.NOT. OutputFullEMSTrace) .AND. (.NOT. OutputEMSErrors)) RETURN

  IF ((OutputEMSErrors) .AND. (.NOT. OutputFullEMSTrace)) THEN
    !see if error needs to be reported.
    IF (ReturnValue%Type /= ValueError) RETURN

  ENDIF

  IF (.NOT. MyOneTimeFlag) THEN
    WRITE(OutputEMSFileUnitNum, '(A)') '****  Begin EMS Language Processor Error and Trace Output  *** '
    WRITE(OutputEMSFileUnitNum, '(A)') '<Erl program name, line #, line text, result, occurance timing information ... >'
    MyOneTimeFlag = .TRUE.
  ENDIF
  ! if have not return'd yet then write out full trace

  NameString = ErlStack(StackNum)%Name
  LineNum = ErlStack(StackNum)%Instruction(InstructionNum)%LineNum
  LineNumString = IntegerToString(LineNum)
  LineString = ErlStack(StackNum)%Line(LineNum)
  cValueString = ValueToString(ReturnValue)

  ! put together timestamp info
  IF (WarmupFlag) THEN
    IF (.not. DoingSizing) THEN
      DuringWarmup=' During Warmup, Occurrence info='
    ELSE
      DuringWarmup=' During Warmup & Sizing, Occurrence info='
    ENDIF
  ELSE
    IF (.not. DoingSizing) THEN
      DuringWarmup=' Occurrence info='
    ELSE
      DuringWarmup=' During Sizing, Occurrence info='
    ENDIF
  ENDIF
  TimeString = TRIM(DuringWarmup)//TRIM(EnvironmentName)//', '//  &
       TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString())

  WRITE(OutputEMSFileUnitNum, '(A)') TRIM(NameString)//',Line '//TRIM(LineNumString)//','//TRIM(LineString) &
                //','//TRIM(cValueString)//','//Trim(TimeString)

END SUBROUTINE WriteTrace


!******************************************************************************************

!  Expression Processor

!******************************************************************************************


SUBROUTINE ParseExpression(InString, StackNum, ExpressionNum, Line)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith, May 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Parsing string into a series of tokens

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUpperCase, ProcessNumber, SameString
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError
  USE DataSystemVariables, ONLY: DeveloperFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxDoLoopCounts = 500
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InString     ! String of expression text written in the Runtime Language
  INTEGER, INTENT(IN)          :: StackNum   ! Parent StackNum??
  INTEGER, INTENT(OUT)         :: ExpressionNum ! index of expression in structure
  CHARACTER(len=*), INTENT(IN) :: Line ! Actual line from string

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  CHARACTER(len=120), DIMENSION(MaxErrors) :: Error  ! Errors should be stored with the stack
  INTEGER                      :: NumErrors  !
  INTEGER :: Pos
  INTEGER :: LastPos
  INTEGER :: NumTokens
  CHARACTER(len=MaxNameLength) :: StringToken  ! MaxNameLength won't do here
  CHARACTER(len=MaxNameLength+10) :: String
  TYPE(TokenType), DIMENSION(:), ALLOCATABLE,SAVE :: Token
  TYPE(TokenType), DIMENSION(:), ALLOCATABLE :: TempToken
  CHARACTER :: NextChar
  LOGICAL :: PeriodFound
  LOGICAL :: MinusFound
  LOGICAL :: PlusFound
  LOGICAL :: ErrorFlag
  LOGICAL :: OperatorProcessing
  INTEGER :: CountDoLooping
  LOGICAL, SAVE :: firsttime=.true.
  INTEGER :: i
  LOGICAL :: LastED  ! last character in a numeric was an E or D


          ! FLOW:
  CountDoLooping = 0
  NumErrors = 0
!  Error = 'No errors.'

  ! Break the string into tokens
  if (firsttime) then
    ALLOCATE(Token(1))
    firsttime=.false.
  endif
  NumTokens = 0
  String=InString

  ! Following is a workaround to parse unitary operators as first value in the expression.
  ! i.e. Set X = -1
  ! this creates Set X = 0-1
  ! and seems to work.

  IF (String(1:1) == '-') THEN
    String='0'//trim(String)
  ELSEIF (String(1:1) == '+') THEN
    String='0'//trim(String)
  ENDIF
  LastPos = LEN_TRIM(String)
  Pos = 1
  DO WHILE (Pos <= LastPos)
    CountDoLooping = CountDoLooping + 1
    IF (CountDoLooping > MaxDoLoopCounts) THEN
      CALL ShowSevereError('EMS ParseExpression: Entity='//trim(ErlStack(StackNum)%Name))
      CALL ShowContinueError('...Line='//trim(Line))
      CALL ShowContinueError('...Failed to process String="'//trim(String)//'".')
      CALL ShowFatalError('...program terminates due to preceding condition.')
    ENDIF
    NextChar = String(Pos:Pos)
    IF (SCAN(NextChar, ' ') /= 0) THEN
      Pos = Pos + 1
      CYCLE
    END IF

    ! Extend the token array
    ! code replaced with a bit safer code below.
!    ALLOCATE(TempToken(NumTokens))
!    TempToken = Token
!    DEALLOCATE(Token)
!    ALLOCATE(Token(NumTokens + 1))
!    Token(1:NumTokens) = TempToken(1:NumTokens)
!    DEALLOCATE(TempToken)

    ! Extend the token array
    if (NumTokens > 0) then  !noel -- add to prevent deallocate of 0 -- but not sure if needed
       ALLOCATE(TempToken(NumTokens))

       !noel
       !TempToken = Token
       do i=1,NumTokens
          TempToken(i) = Token(i)
       enddo
       DEALLOCATE(Token)
       ALLOCATE(Token(NumTokens + 1))

       !noel
       !Token(1:NumTokens) = TempToken(1:NumTokens)
       do i=1,NumTokens
          Token(i) = TempToken(i)
       enddo
       DEALLOCATE(TempToken)
    endif

    ! Get the next token
    NumTokens = NumTokens + 1
    StringToken = ''
    PeriodFound = .FALSE.
    MinusFound=.false.
    PlusFound=.false.
    OperatorProcessing=.false.  ! true when an operator is found until terminated by non-operator
    ErrorFlag = .FALSE.
    LastED=.false.
    IF (SCAN(NextChar, '0123456789.') /= 0) THEN
      ! Parse a number literal token
      Pos = Pos + 1
      StringToken = TRIM(StringToken)//NextChar
      IF (SCAN(NextChar, '.') /= 0) PeriodFound = .TRUE.

      DO WHILE (LEN_TRIM(String) > 0)
        NextChar = String(Pos:Pos)
        IF (SCAN(NextChar, '0123456789.eEdD') /= 0) THEN
          Pos = Pos + 1
          IF (SCAN(NextChar, '.') /= 0) THEN
            IF (PeriodFound) THEN
              ! ERROR:  two periods appearing in a number literal!
              CALL ShowSevereError('EMS Parse Expression, for "'//trim(ErlStack(StackNum)%Name)//'".')
              CALL ShowContinueError('...Line="'//trim(Line)//'".')
              CALL ShowContinueError('...Bad String="'//trim(String)//'".')
              CALL ShowContinueError('...Two decimal points detected in String.')
              NumErrors=NumErrors+1
              ErrorFlag=.true.
              EXIT
            ELSE
              PeriodFound = .TRUE.
            END IF
          END IF
          IF (SCAN(NextChar, 'eEdD') /= 0) THEN
            StringToken = TRIM(StringToken)//NextChar
            IF (LastED) THEN
              CALL ShowSevereError('EMS Parse Expression, for "'//trim(ErlStack(StackNum)%Name)//'".')
              CALL ShowContinueError('...Line="'//trim(Line)//'".')
              CALL ShowContinueError('...Bad String="'//trim(String)//'".')
              CALL ShowContinueError('...Two D/E in numeric String.')
              NumErrors=NumErrors+1
              ErrorFlag=.true.
              ! error
              EXIT
            ELSE
              LastED=.true.
            ENDIF
          ELSE
            StringToken = TRIM(StringToken)//NextChar
          END IF
        ELSE IF (SCAN(NextChar, '+-') /= 0) THEN  ! +/- following an ED is okay.
          IF (LastED) THEN
            StringToken = TRIM(StringToken)//NextChar
            Pos = Pos + 1
            LastED=.false.
          ELSE
            EXIT
          ENDIF
        ELSE IF (SCAN(NextChar, ' +-*/^=<>)') /= 0) THEN  ! Any binary operator is okay
          EXIT  ! End of token
        ELSE
          ! Error: strange sequence of characters:  return TokenString//NextChar   e.g.,  234.44a or 234.44%
          StringToken = TRIM(StringToken)//NextChar
          EXIT
        END IF
      END DO

      ! Save the number token
      IF (.not. ErrorFlag) THEN
        Token(NumTokens)%Type = TokenNumber
        Token(NumTokens)%String = StringToken
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'Number="'//trim(StringToken)//'"'
        Token(NumTokens)%Number = ProcessNumber(StringToken, ErrorFlag)
        IF (DeveloperFlag .and. ErrorFlag) write(OutputFileDebug,fmta) 'Numeric error flagged'
        IF (MinusFound) Token(NumTokens)%Number = -Token(NumTokens)%Number
        IF (ErrorFlag) THEN
          ! Error: something wrong with this number!
          CALL ShowSevereError('EMS Parse Expression, for "'//trim(ErlStack(StackNum)%Name)//'".')
          CALL ShowContinueError('...Line="'//trim(Line)//'".')
          CALL ShowContinueError('...Bad String="'//trim(String)//'".')
          CALL ShowContinueError('Invalid numeric="'//trim(StringToken)//'".')
          NumErrors=NumErrors+1
        END IF
      ENDIF

    ELSE IF (SCAN(NextChar, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') /= 0) THEN
      ! Parse an undetermined string token (could be a variable, subroutine, or named operator)
      Pos = Pos + 1
      StringToken = TRIM(StringToken)//NextChar
      OperatorProcessing=.false.

      DO WHILE (LEN_TRIM(String) > 0)
        NextChar = String(Pos:Pos)
        IF (SCAN(NextChar, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789') /= 0) THEN
          Pos = Pos + 1
          StringToken = TRIM(StringToken)//NextChar
        ELSE IF (SCAN(NextChar, ' +-*/^=<>()') /= 0) THEN
          EXIT  ! End of token
        ELSE
          ! Error: bad syntax:  return TokenString//NextChar   e.g.,  var1$ or b%
          EXIT
        END IF
      END DO

      ! Save the variable token
      Token(NumTokens)%Type = TokenVariable
      Token(NumTokens)%String = StringToken
      IF (DeveloperFlag) write(OutputFileDebug,fmta) 'Variable="'//trim(StringToken)//'"'
      Token(NumTokens)%Variable = NewEMSVariable(StringToken, StackNum)

    ELSE IF (SCAN(NextChar, '+-*/^=<>@|&') /= 0) THEN
      ! Parse an operator token
      StringToken = NextChar

      Token(NumTokens)%Type = TokenOperator

      ! First check for two character operators:  == <> <= >=
      IF (String(Pos:Pos + 1) == '==') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatorEqual
        Token(NumTokens)%String = String(Pos:Pos + 1)
        Pos = Pos + 1
      ELSE IF (String(Pos:Pos + 1) == '<>') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatorNotEqual
        Token(NumTokens)%String = String(Pos:Pos + 1)
        Pos = Pos + 1
      ELSE IF (String(Pos:Pos + 1) == '<=') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatorLessOrEqual
        Token(NumTokens)%String = String(Pos:Pos + 1)
        Pos = Pos + 1
      ELSE IF (String(Pos:Pos + 1) == '>=') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatorGreaterOrEqual
        Token(NumTokens)%String = String(Pos:Pos + 1)
        Pos = Pos + 1
      ELSE IF (String(Pos:Pos +1) == '||') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatiorLogicalOR
        Token(NumTokens)%String = String(Pos:Pos +1)
        Pos = Pos + 1
      ELSE IF (String(Pos:Pos +1) == '&&') THEN
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+1))//'"'
        Token(NumTokens)%Operator = OperatorLogicalAND
        Token(NumTokens)%String = String(Pos:Pos +1)
        Pos = Pos + 1
      ! next check for builtin functions signaled by "@"
      ELSE IF (String(Pos:Pos) == '@') THEN

        IF (SameString(String(Pos:Pos + 5) , '@Round')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+5))//'"'
          Token(NumTokens)%Operator = FuncRound
          Token(NumTokens)%String = String(Pos:Pos + 5)
          Pos = Pos + 5
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Mod')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncMod
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Sin')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncSin
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Cos')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncCos
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 6) , '@ArcCos')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+6))//'"'
          Token(NumTokens)%Operator = FuncArcCos
          Token(NumTokens)%String = String(Pos:Pos + 6)
          Pos = Pos + 6
        ELSEIF (SameString(String(Pos:Pos + 6) , '@ArcSin')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+6))//'"'
          Token(NumTokens)%Operator = FuncArcSin
          Token(NumTokens)%String = String(Pos:Pos + 6)
          Pos = Pos + 6
        ELSEIF (SameString(String(Pos:Pos + 8) , '@DegToRad')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncDegToRad
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 8) , '@RadToDeg')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncRadToDeg
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Exp')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncExp
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 2) , '@Ln')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+2))//'"'
          Token(NumTokens)%Operator = FuncLn
          Token(NumTokens)%String = String(Pos:Pos + 2)
          Pos = Pos + 2
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Max')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncMax
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Min')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncMin
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 3) , '@Abs')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+3))//'"'
          Token(NumTokens)%Operator = FuncABS
          Token(NumTokens)%String = String(Pos:Pos + 3)
          Pos = Pos + 3
        ELSEIF (SameString(String(Pos:Pos + 13 ) , '@RANDOMUNIFORM')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+13))//'"'
          Token(NumTokens)%Operator = FuncRandU
          Token(NumTokens)%String = String(Pos:Pos + 13)
          Pos = Pos + 13
        ELSEIF (SameString(String(Pos:Pos + 12 ) , '@RANDOMNORMAL')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+12))//'"'
          Token(NumTokens)%Operator = FuncRandG
          Token(NumTokens)%String = String(Pos:Pos + 12)
          Pos = Pos + 12
        ELSEIF (SameString(String(Pos:Pos + 10 ) , '@SEEDRANDOM')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncRandSeed
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 14) , '@RhoAirFnPbTdbW')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+14))//'"'
          Token(NumTokens)%Operator = FuncRhoAirFnPbTdbW
          Token(NumTokens)%String = String(Pos:Pos + 14)
          Pos = Pos + 14
        ELSEIF (SameString(String(Pos:Pos + 11) , '@CpAirFnWTdb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncCpAirFnWTdb
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 12) , '@HfgAirFnWTdb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+12))//'"'
          Token(NumTokens)%Operator = FuncHfgAirFnWTdb
          Token(NumTokens)%String = String(Pos:Pos + 12)
          Pos = Pos + 12
        ELSEIF (SameString(String(Pos:Pos + 11) , '@HgAirFnWTdb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncHgAirFnWTdb
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 13) , '@TdpFnTdbTwbPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+13))//'"'
          Token(NumTokens)%Operator = FuncTdpFnTdbTwbPb
          Token(NumTokens)%String = String(Pos:Pos + 13)
          Pos = Pos + 13
        ELSEIF (SameString(String(Pos:Pos + 8) , '@TdpFnWPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncTdpFnWPb
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 7) , '@HFnTdbW')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+7))//'"'
          Token(NumTokens)%Operator = FuncHFnTdbW
          Token(NumTokens)%String = String(Pos:Pos + 7)
          Pos = Pos + 7
        ELSEIF (SameString(String(Pos:Pos + 10) , '@HFnTdbRhPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncHFnTdbRhPb
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 7) , '@TdbFnHW')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+7))//'"'
          Token(NumTokens)%Operator = FuncTdbFnHW
          Token(NumTokens)%String = String(Pos:Pos + 7)
          Pos = Pos + 7
        ELSEIF (SameString(String(Pos:Pos + 11) , '@RhovFnTdbRh')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncRhovFnTdbRh
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 17) , '@RhovFnTdbRhLBnd0C')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+17))//'"'
          Token(NumTokens)%Operator = FuncRhovFnTdbRhLBnd0C
          Token(NumTokens)%String = String(Pos:Pos + 17)
          Pos = Pos + 17
        ELSEIF (SameString(String(Pos:Pos + 12) , '@RhovFnTdbWPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+12))//'"'
          Token(NumTokens)%Operator = FuncRhovFnTdbWPb
          Token(NumTokens)%String = String(Pos:Pos + 12)
          Pos = Pos + 12
        ELSEIF (SameString(String(Pos:Pos + 11) , '@RhFnTdbRhov')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncRhFnTdbRhov
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 17) , '@RhFnTdbRhovLBnd0C')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+17))//'"'
          Token(NumTokens)%Operator = FuncRhFnTdbRhovLBnd0C
          Token(NumTokens)%String = String(Pos:Pos + 17)
          Pos = Pos + 17
        ELSEIF (SameString(String(Pos:Pos + 10) , '@RhFnTdbWPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncRhFnTdbWPb
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 11) , '@TwbFnTdbWPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncTwbFnTdbWPb
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 9) , '@VFnTdbWPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+9))//'"'
          Token(NumTokens)%Operator = FuncVFnTdbWPb
          Token(NumTokens)%String = String(Pos:Pos + 9)
          Pos = Pos + 9
        ELSEIF (SameString(String(Pos:Pos + 8) , '@WFnTdpPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncWFnTdpPb
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 7) , '@WFnTdbH')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+7))//'"'
          Token(NumTokens)%Operator = FuncWFnTdbH
          Token(NumTokens)%String = String(Pos:Pos + 7)
          Pos = Pos + 7
        ELSEIF (SameString(String(Pos:Pos + 11) , '@WFnTdbTwbPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncWFnTdbTwbPb
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 10) , '@WFnTdbRhPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncWFnTdbRhPb
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 10) , '@PsatFnTemp')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncPsatFnTemp
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 9) , '@TsatFnHPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+9))//'"'
          Token(NumTokens)%Operator = FuncTsatFnHPb
          Token(NumTokens)%String = String(Pos:Pos + 9)
          Pos = Pos + 9
        ELSEIF (SameString(String(Pos:Pos + 8) , '@TsatFnPb')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncTsatFnPb
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 4) , '@CpCW')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+4))//'"'
          Token(NumTokens)%Operator = FuncCpCW
          Token(NumTokens)%String = String(Pos:Pos + 4)
          Pos = Pos + 4
        ELSEIF (SameString(String(Pos:Pos + 4) , '@CpHW')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+4))//'"'
          Token(NumTokens)%Operator = FuncCpHW
          Token(NumTokens)%String = String(Pos:Pos + 4)
          Pos = Pos + 4
        ELSEIF (SameString(String(Pos:Pos + 6) , '@RhoH2O')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+6))//'"'
          Token(NumTokens)%Operator = FuncRhoH2O
          Token(NumTokens)%String = String(Pos:Pos + 6)
          Pos = Pos + 6
        ELSEIF (SameString(String(Pos:Pos + 11) , '@FATALHALTEP')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+11))//'"'
          Token(NumTokens)%Operator = FuncFatalHaltEp
          Token(NumTokens)%String = String(Pos:Pos + 11)
          Pos = Pos + 11
        ELSEIF (SameString(String(Pos:Pos + 12) , '@SEVEREWARNEP')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+12))//'"'
          Token(NumTokens)%Operator = FuncSevereWarnEp
          Token(NumTokens)%String = String(Pos:Pos + 12)
          Pos = Pos + 12
        ELSEIF (SameString(String(Pos:Pos + 6) , '@WARNEP')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+6))//'"'
          Token(NumTokens)%Operator = FuncWarnEp
          Token(NumTokens)%String = String(Pos:Pos + 6)
          Pos = Pos + 6
        ELSEIF (SameString(String(Pos:Pos + 10) , '@TRENDVALUE')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncTrendValue
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSEIF (SameString(String(Pos:Pos + 12) , '@TRENDAVERAGE')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+12))//'"'
          Token(NumTokens)%Operator = FuncTrendAverage
          Token(NumTokens)%String = String(Pos:Pos + 12)
          Pos = Pos + 12
        ELSEIF (SameString(String(Pos:Pos + 8) , '@TRENDMAX')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncTrendMax
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 8) , '@TRENDMIN')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncTrendMin
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 14) , '@TRENDDIRECTION')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+14))//'"'
          Token(NumTokens)%Operator = FuncTrendDirection
          Token(NumTokens)%String = String(Pos:Pos + 14)
          Pos = Pos + 14
        ELSEIF (SameString(String(Pos:Pos + 8) , '@TRENDSUM')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+8))//'"'
          Token(NumTokens)%Operator = FuncTrendSum
          Token(NumTokens)%String = String(Pos:Pos + 8)
          Pos = Pos + 8
        ELSEIF (SameString(String(Pos:Pos + 10) , '@CURVEVALUE')) THEN
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'FUNCTION "'//trim(String(Pos:Pos+10))//'"'
          Token(NumTokens)%Operator = FuncCurveValue
          Token(NumTokens)%String = String(Pos:Pos + 10)
          Pos = Pos + 10
        ELSE  ! throw error
          IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ERROR "'//trim(String)//'"'
          CALL ShowFatalError('EMS Runtime Language: did not find valid input for built-in function ='//Trim(String))

        ENDIF
      ELSE
        ! Check for remaining single character operators
        Token(NumTokens)%String = StringToken
        IF (DeveloperFlag) write(OutputFileDebug,fmta) 'OPERATOR "'//trim(StringToken)//'"'

        SELECT CASE (StringToken)
          CASE ('+')
            IF (.not. OperatorProcessing) THEN
              Token(NumTokens)%Operator = OperatorAdd
              OperatorProcessing=.true.
            ELSE
              PlusFound=.true.
              OperatorProcessing=.false.
            ENDIF
          CASE ('-')
            IF (.not. OperatorProcessing) THEN
              Token(NumTokens)%Operator = OperatorSubtract
              OperatorProcessing=.true.
            ELSE
              MinusFound=.true.
              OperatorProcessing=.false.
            ENDIF
          CASE ('*')
            Token(NumTokens)%Operator = OperatorMultiply
            OperatorProcessing=.true.
          CASE ('/')
            Token(NumTokens)%Operator = OperatorDivide
            OperatorProcessing=.true.
          CASE ('<')
            Token(NumTokens)%Operator = OperatorLessThan
            OperatorProcessing=.true.
          CASE ('>')
            Token(NumTokens)%Operator = OperatorGreaterThan
            OperatorProcessing=.true.
          CASE ('^')
            Token(NumTokens)%Operator = OperatorRaiseToPower
            OperatorProcessing=.true.
          CASE DEFAULT
            ! Uh OH, this should never happen! throw error
            IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ERROR "'//trim(StringToken)//'"'
            CAll ShowFatalError('EMS, caught unexpected token = "'//Trim(StringToken)//'" ; while parsing string='//Trim(String))

        END SELECT
      END IF

      Pos = Pos + 1

    ELSE IF (SCAN(NextChar, '()') /= 0) THEN
      ! Parse a parenthesis token
      Pos = Pos + 1
      StringToken = NextChar
      IF (DeveloperFlag) write(OutputFileDebug,fmta) 'PAREN "'//trim(StringToken)//'"'
      Token(NumTokens)%Type = TokenParenthesis
      Token(NumTokens)%String = StringToken
      IF (NextChar == '(') Token(NumTokens)%Parenthesis = ParenthesisLeft
      IF (NextChar == ')') Token(NumTokens)%Parenthesis = ParenthesisRight

    ELSE IF (SCAN(NextChar, '"') /= 0) THEN
      ! Parse a string literal token
      IF (DeveloperFlag) write(OutputFileDebug,fmta) 'LITERAL STRING'
      Pos = Pos + 1

    ELSE
      ! Error: bad start to the token

    END IF
  END DO

  IF (NumErrors > 0) THEN
    IF (DeveloperFlag) write(OutputFileDebug,fmta) 'ERROR OUT'
    CALL ShowFatalError('EMS, previous errors cause termination.')
  ENDIF

  ExpressionNum = ProcessTokens(Token, NumTokens, StackNum, String)

  RETURN

END SUBROUTINE ParseExpression

RECURSIVE FUNCTION ProcessTokens(TokenIN, NumTokensIN, StackNum, ParsingString) RESULT(ExpressionNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Processes tokens into expressions.

          ! METHODOLOGY EMPLOYED:
          ! Uses recursion to handle tokens with compound expressions

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUpperCase, ProcessNumber
  USE DataInterfaces, ONLY: ShowSevereError, ShowFatalError, ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(TokenType), DIMENSION(:), INTENT(IN) :: TokenIN
  INTEGER, INTENT(IN) :: NumTokensIN
  INTEGER, INTENT(IN) :: StackNum
  INTEGER             :: ExpressionNum
  CHARACTER(len=*), INTENT(IN) :: ParsingString

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(TokenType), DIMENSION(:), ALLOCATABLE :: Token
  TYPE(TokenType), DIMENSION(:), ALLOCATABLE :: TempToken
  TYPE(TokenType), DIMENSION(:), ALLOCATABLE :: SubTokenList
  INTEGER :: Pos
  INTEGER :: LastPos
  INTEGER :: TokenNum
  INTEGER :: NumTokens
  INTEGER :: Depth
  INTEGER :: NumSubTokens
  INTEGER :: NewNumTokens
  INTEGER :: OperatorNum
  INTEGER :: NumOperands
  INTEGER :: ParenthWhileCounter ! used to trap for unbalanced parentheses
  INTEGER :: i

          ! FLOW:
  ExpressionNum = 0
  NumTokens = NumTokensIN
  ALLOCATE(Token(NumTokens))
! safer code below
!  Token = TokenIN

  do i=1,NumTokens
    Token(i)= TokenIN(i)
  enddo

  ! Process parentheses
  Pos = 0
  DO TokenNum = 1, NumTokens
    IF (Token(TokenNum)%Type == TokenParenthesis) THEN
      Pos = TokenNum
      EXIT
    END IF
  END DO


  ParenthWhileCounter = 0

  DO WHILE((Pos > 0) .AND. (ParenthWhileCounter < 50))
    ParenthWhileCounter = ParenthWhileCounter + 1
    Depth = 0
    DO TokenNum = 1, NumTokens
      IF (Token(TokenNum)%Type == TokenParenthesis) THEN
        IF (Token(TokenNum)%Parenthesis == ParenthesisLeft) THEN
          IF (Depth == 0) Pos = TokenNum  ! Record position of first left parenthesis
          Depth = Depth + 1
        END IF
        IF (Token(TokenNum)%Parenthesis == ParenthesisRight) THEN
          Depth = Depth - 1
          IF (Depth == 0) THEN
            LastPos = TokenNum
            NumSubTokens = LastPos - Pos - 1
            ALLOCATE(SubTokenList(NumSubTokens))
            SubTokenList(1:NumSubTokens) = Token(Pos + 1:LastPos - 1)  ! Need to check that these don't exceed bounds
            ExpressionNum = ProcessTokens(SubTokenList, NumSubTokens, StackNum, ParsingString)
            DEALLOCATE(SubTokenList)

            ! Replace the parenthetical tokens with one expression token
            NewNumTokens = NumTokens - NumSubTokens - 1
            IF (NewNumTokens > 0) THEN
              ALLOCATE(TempToken(NewNumTokens))
              IF (Pos - 1 > 0) THEN
                TempToken(1:Pos - 1) = Token(1:Pos - 1)
              END IF
              IF (LastPos + 1 <= NumTokens) THEN
                TempToken(Pos + 1:) = Token(LastPos + 1:)
              END IF
              TempToken(Pos)%Type = TokenExpression
              TempToken(Pos)%Expression = ExpressionNum
              TempToken(Pos)%String = 'Expr'
              DEALLOCATE(Token)
              ALLOCATE(Token(NewNumTokens))
              Token = TempToken
              DEALLOCATE(TempToken)
              NumTokens = NewNumTokens
            END IF

            ! Reset loop for next parenthetical set
            EXIT
          END IF
        END IF
      END IF

    END DO

    ! This repeats code again...  Just checks to see if there are any more parentheses to be found
    Pos = 0
    DO TokenNum = 1, NumTokens
      IF (Token(TokenNum)%Type == TokenParenthesis) THEN
        Pos = TokenNum
        EXIT
      END IF
    END DO

  END DO

  IF (ParenthWhileCounter == 50) THEN ! symptom of mismatched parenthesis
    CALL ShowSevereError('EMS error parsing parentheses, check that parentheses are balanced')
    CALL ShowContinueError('String being parsed="'//trim(ParsingString)//'".')
    CALL ShowFatalError('Program terminates due to preceding error.')
  ENDIF

  Call SetupPossibleOperators ! includes built-in functions

  ! Process operators and builtin functions
  ! Loop thru all operators and group expressions in the order of precedence
  DO OperatorNum = 1, NumPossibleOperators

    ! Find the next occurrence of the operator
    Pos = 0 !  position in sequence of tokens
    DO TokenNum = 1, NumTokens
      IF ((Token(TokenNum)%Type == TokenOperator) &
        .AND. (Token(TokenNum)%Operator == OperatorNum)) THEN
        Pos = TokenNum
        EXIT
      END IF
    END DO

    DO WHILE (Pos > 0)
      IF (Pos == 1) THEN
        !if first token is for a built-in function starting with "@" then okay, otherwise the operator needs a LHS
        IF (Token(TokenNum)%Operator > OperatiorLogicalOR) Then ! we have a function expression to set up
          ExpressionNum = NewExpression()
          ErlExpression(ExpressionNum)%Operator = OperatorNum
          NumOperands = PossibleOperators(OperatorNum)%NumOperands
          ErlExpression(ExpressionNum)%NumOperands = NumOperands
          ALLOCATE(ErlExpression(ExpressionNum)%Operand(NumOperands))

          ErlExpression(ExpressionNum)%Operand(1)%Type        = Token(Pos + 1)%Type
          ErlExpression(ExpressionNum)%Operand(1)%Number      = Token(Pos + 1)%Number
          ErlExpression(ExpressionNum)%Operand(1)%Expression  = Token(Pos + 1)%Expression
          ErlExpression(ExpressionNum)%Operand(1)%Variable    = Token(Pos + 1)%Variable
          IF (Token(Pos + 1)%Variable > 0 ) THEN
            ErlExpression(ExpressionNum)%Operand(1)%TrendVariable  = ErlVariable(Token(Pos + 1)%Variable)%Value%TrendVariable
            ErlExpression(ExpressionNum)%Operand(1)%TrendVarPointer = ErlVariable(Token(Pos + 1)%Variable)%Value%TrendVarPointer
          ENDIF
          IF ((NumOperands >= 2) .AND. (NumTokens >= 3)) THEN
            ErlExpression(ExpressionNum)%Operand(2)%Type       = Token(Pos + 2)%Type
            ErlExpression(ExpressionNum)%Operand(2)%Number     = Token(Pos + 2)%Number
            ErlExpression(ExpressionNum)%Operand(2)%Expression = Token(Pos + 2)%Expression
            ErlExpression(ExpressionNum)%Operand(2)%Variable   = Token(Pos + 2)%Variable
          ENDIF

          IF ((NumOperands >= 3) .AND. (NumTokens >= 4))  THEN
            ErlExpression(ExpressionNum)%Operand(3)%Type       = Token(Pos + 3)%Type
            ErlExpression(ExpressionNum)%Operand(3)%Number     = Token(Pos + 3)%Number
            ErlExpression(ExpressionNum)%Operand(3)%Expression = Token(Pos + 3)%Expression
            ErlExpression(ExpressionNum)%Operand(3)%Variable   = Token(Pos + 3)%Variable
            IF ((NumOperands == 3) .AND. (NumTokens - 4 > 0)) THEN  ! too many tokens for this non-binary operator
              CALL showfatalError('EMS error parsing tokens, too many for built-in function')
            ENDIF
          ENDIF

          IF ((NumOperands >= 4) .AND. (NumTokens >= 5)) THEN
            ErlExpression(ExpressionNum)%Operand(4)%Type       = Token(Pos + 4)%Type
            ErlExpression(ExpressionNum)%Operand(4)%Number     = Token(Pos + 4)%Number
            ErlExpression(ExpressionNum)%Operand(4)%Expression = Token(Pos + 4)%Expression
            ErlExpression(ExpressionNum)%Operand(4)%Variable   = Token(Pos + 4)%Variable
            IF ((NumOperands == 4) .AND. (NumTokens - 5 > 0)) THEN  ! too many tokens for this non-binary operator
              CALL showfatalError('EMS error parsing tokens, too many for built-in function')
            ENDIF
          ENDIF

          IF ((NumOperands == 5) .AND. (NumTokens >= 6)) THEN
            ErlExpression(ExpressionNum)%Operand(5)%Type       = Token(Pos + 5)%Type
            ErlExpression(ExpressionNum)%Operand(5)%Number     = Token(Pos + 5)%Number
            ErlExpression(ExpressionNum)%Operand(5)%Expression = Token(Pos + 5)%Expression
            ErlExpression(ExpressionNum)%Operand(5)%Variable   = Token(Pos + 5)%Variable
            IF ((NumOperands == 5) .AND. (NumTokens - 6 > 0)) THEN  ! too many tokens for this non-binary operator
              CALL showfatalError('EMS error parsing tokens, too many for  built-in function')
            ENDIF
          ENDIF
          EXIT
        ELSE
          CALL ShowSevereError('The operator "'//TRIM(PossibleOperators(OperatorNum)%Symbol)//'" is missing the left-hand operand!')
          CALL ShowContinueError('String being parsed="'//trim(ParsingString)//'".')
          EXIT
        ENDIF
      ELSE IF (Pos == NumTokens) THEN
        CALL ShowSevereError('The operator "'//TRIM(PossibleOperators(OperatorNum)%Symbol)//'" is missing the right-hand operand!')
        CALL ShowContinueError('String being parsed="'//trim(ParsingString)//'".')
        EXIT
      ELSE

        ExpressionNum = NewExpression()
        ErlExpression(ExpressionNum)%Operator = OperatorNum
        NumOperands = PossibleOperators(OperatorNum)%NumOperands
        ErlExpression(ExpressionNum)%NumOperands = NumOperands
        ALLOCATE(ErlExpression(ExpressionNum)%Operand(NumOperands))

        ! PE commment: Need a right-hand and left-hand check for these, not just number of operators
        ! Unification of TYPEs would turn these into one-liners

        ErlExpression(ExpressionNum)%Operand(1)%Type        = Token(Pos - 1)%Type
        ErlExpression(ExpressionNum)%Operand(1)%Number      = Token(Pos - 1)%Number
        ErlExpression(ExpressionNum)%Operand(1)%Expression  = Token(Pos - 1)%Expression
        ErlExpression(ExpressionNum)%Operand(1)%Variable    = Token(Pos - 1)%Variable

        IF (NumOperands >= 2) THEN
          ErlExpression(ExpressionNum)%Operand(2)%Type       = Token(Pos + 1)%Type
          ErlExpression(ExpressionNum)%Operand(2)%Number     = Token(Pos + 1)%Number
          ErlExpression(ExpressionNum)%Operand(2)%Expression = Token(Pos + 1)%Expression
          ErlExpression(ExpressionNum)%Operand(2)%Variable   = Token(Pos + 1)%Variable
        ENDIF

        ! Replace the three tokens with one expression token
        IF ((NumOperands == 2) .AND. (NumTokens - 2 > 0)) THEN
          ALLOCATE(TempToken(NumTokens - 2))
          IF (Pos - 2 > 0) THEN
            TempToken(1:Pos - 2) = Token(1:Pos - 2)
          END IF
          IF (Pos + 2 <= NumTokens) THEN
            TempToken(Pos:NumTokens - 2) = Token(Pos + 2:)
          END IF
          TempToken(Pos - 1)%Type = TokenExpression
          TempToken(Pos - 1)%Expression = ExpressionNum
          TempToken(Pos - 1)%String = 'Expr'
          DEALLOCATE(Token)
          ALLOCATE(Token(NumTokens - 2))
          Token = TempToken
          DEALLOCATE(TempToken)
          NumTokens = NumTokens - 2
        END IF
      END IF

      ! Find the next occurrence of the operator  (this repeats code, but don't have better idea)
      Pos = 0
      DO TokenNum = 1, NumTokens
        IF ((Token(TokenNum)%Type == TokenOperator) &
          .AND. (Token(TokenNum)%Operator == OperatorNum)) THEN
          Pos = TokenNum
          EXIT
        END IF
      END DO

    END DO

  END DO

  ! Should be down to just one token now
  IF (Token(1)%Type == TokenNumber) THEN
    ExpressionNum = NewExpression()
    ErlExpression(ExpressionNum)%Operator = OperatorLiteral
    ErlExpression(ExpressionNum)%NumOperands = 1
    ALLOCATE(ErlExpression(ExpressionNum)%Operand(1))
    ErlExpression(ExpressionNum)%Operand(1)%Type = Token(1)%Type
    ErlExpression(ExpressionNum)%Operand(1)%Number = Token(1)%Number
  ELSE IF (Token(1)%Type == TokenVariable) THEN
    ExpressionNum = NewExpression()
    ErlExpression(ExpressionNum)%Operator = OperatorLiteral
    ErlExpression(ExpressionNum)%NumOperands = 1
    ALLOCATE(ErlExpression(ExpressionNum)%Operand(1))
    ErlExpression(ExpressionNum)%Operand(1)%Type = Token(1)%Type
    ErlExpression(ExpressionNum)%Operand(1)%Variable = Token(1)%Variable
  END IF

  DEALLOCATE(Token)

  RETURN

END FUNCTION ProcessTokens

FUNCTION NewExpression() RESULT(ExpressionNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Creates a new expression.

          ! METHODOLOGY EMPLOYED:
          !

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: ExpressionNum

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  TYPE(ErlExpressionType), DIMENSION(:), ALLOCATABLE :: TempExpression

          ! FLOW:
  IF (NumExpressions == 0) THEN
    ALLOCATE(ErlExpression(1))
    NumExpressions = 1
  ELSE
    ALLOCATE(TempExpression(NumExpressions))
    TempExpression = ErlExpression
    DEALLOCATE(ErlExpression)
    ALLOCATE(ErlExpression(NumExpressions + 1))
    ErlExpression(1:NumExpressions) = TempExpression(1:NumExpressions)
    NumExpressions = NumExpressions + 1
    DEALLOCATE(TempExpression)
  END IF

  ExpressionNum = NumExpressions

  RETURN

END FUNCTION NewExpression

RECURSIVE FUNCTION EvaluateExpression(ExpressionNum) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith, May 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Evaluates an expression.

          ! METHODOLOGY EMPLOYED:

  !USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it
  USE DataGlobals, ONLY: DegToRadians !unused, TimeStepZone
  USE DataInterfaces, ONLY: ShowFatalError, ShowContinueErrorTimeStamp, &
                         ShowSevereError, ShowWarningError
  USE Psychrometrics
  USE General, ONLY: TrimSigDigits, RoundSigDigits
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ExpressionNum
  TYPE(ErlValueType) :: ReturnValue

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: thisTrend ! local temporary
  INTEGER :: thisIndex ! local temporary
  REAL(r64) :: thisAverage ! local temporary
  INTEGER :: loop ! local temporary
  REAL(r64) :: thisSlope ! local temporary
  REAL(r64) :: thisMax ! local temporary
  REAL(r64) :: thisMin ! local temporary
  INTEGER :: OperandNum
  CHARACTER(len = 1) :: SeedElementChar
  INTEGER            :: SeedElementInt
  INTEGER :: SeedN  ! number of digits in the number used to seed the generator
  CHARACTER(len=MaxNameLength) :: SeedChar ! local temporary for random seed
  INTEGER, DIMENSION(:), ALLOCATABLE :: SeedIntARR ! local temporary for random seed
  INTEGER  :: Pos ! local temporary for string position.
  REAL(r64) :: tmpRANDU1 ! local temporary for uniform random number
  REAL(r64) :: tmpRANDU2 ! local temporary for uniform random number
  REAL(r64) :: tmpRANDG ! local temporary for gaussian random number
  REAL(r64) :: UnitCircleTest ! local temporary for Box-Muller algo
  REAL(R64) :: TestValue ! local temporary
  TYPE(ErlValueType), ALLOCATABLE, DIMENSION(:) :: Operand

          ! FLOW:
  ReturnValue%Type = ValueNumber
  ReturnValue%Number = 0.0d0

  IF (ExpressionNum > 0) THEN
  ! is there a way to keep these and not allocate and deallocate all the time?
    ALLOCATE(Operand(ErlExpression(ExpressionNum)%NumOperands))
  ! Reduce operands down to literals
    DO OperandNum = 1, ErlExpression(ExpressionNum)%NumOperands
      Operand(OperandNum) = ErlExpression(ExpressionNum)%Operand(OperandNum)
      IF (Operand(OperandNum)%Type == ValueExpression) THEN
        Operand(OperandNum) = EvaluateExpression(Operand(OperandNum)%Expression) !recursive call
      ELSE IF (Operand(OperandNum)%Type == ValueVariable) THEN
        Operand(OperandNum) = ErlVariable(Operand(OperandNum)%Variable)%Value
      END IF
    END DO

    ! Perform the operation
    SELECT CASE (ErlExpression(ExpressionNum)%Operator)

      CASE (OperatorLiteral)
        ReturnValue = Operand(1)
      CASE (OperatorNegative)  ! unary minus sign.  parsing does not work yet
        ReturnValue = SetErlValueNumber(-1.0D0 * Operand(1)%Number)
      CASE (OperatorDivide)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(2)%Number == 0.0d0) THEN
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Divide by zero!'
          ELSE
            ReturnValue = SetErlValueNumber(Operand(1)%Number / Operand(2)%Number)
          END IF
        END IF

      CASE (OperatorMultiply)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          ReturnValue = SetErlValueNumber(Operand(1)%Number * Operand(2)%Number)
        END IF

      CASE (OperatorSubtract)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          ReturnValue = SetErlValueNumber(Operand(1)%Number - Operand(2)%Number)
        END IF

      CASE (OperatorAdd)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          ReturnValue = SetErlValueNumber(Operand(1)%Number + Operand(2)%Number)
        END IF

      CASE (OperatorEqual)
        IF (Operand(1)%Type == Operand(2)%Type) THEN
          IF (Operand(1)%Type == ValueNull) THEN
            ReturnValue = True
          ELSE IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(1)%Number == Operand(2)%Number)) THEN
            ReturnValue = True
          ELSE
            ReturnValue = False
          END IF
        ELSE
          ReturnValue = False
        END IF

      CASE (OperatorNotEqual)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(1)%Number /= Operand(2)%Number) Then
            ReturnValue = True
          ELSE
            ReturnValue = False
          ENDIF
        ENDIF

      CASE (OperatorLessOrEqual)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(1)%Number <= Operand(2)%Number) Then
            ReturnValue = True
          ELSE
            ReturnValue = False
          ENDIF
        ENDIF

      CASE (OperatorGreaterOrEqual)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(1)%Number >= Operand(2)%Number) Then
            ReturnValue = True
          ELSE
            ReturnValue = False
          ENDIF
        ENDIF
      CASE (OperatorLessThan)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(1)%Number < Operand(2)%Number) Then
            ReturnValue = True
          ELSE
            ReturnValue = False
          ENDIF
        ENDIF
      CASE (OperatorGreaterThan)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          IF (Operand(1)%Number > Operand(2)%Number) Then
            ReturnValue = True
          ELSE
            ReturnValue = False
          ENDIF
        ENDIF

      CASE (OperatorRaiseToPower)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
          TestValue = Operand(1)%Number**Operand(2)%Number
          IF (ISNAN(TestValue)) THEN ! Use IEEE_IS_NAN when GFortran supports it
          ! throw Error
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Attempted to raise to power with incompatible numbers: '  &
                                //TRIM(TrimSigDigits(Operand(1)%Number, 6))//' raised to ' &
                                //TRIM(TrimSigDigits(Operand(2)%Number, 6))
          ELSE
            ReturnValue = SetErlValueNumber(TestValue)
          ENDIF

        ENDIF
      CASE (OperatorLogicalAND)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
           IF ((Operand(1)%Number == True%Number) .AND. (Operand(2)%Number == True%Number)) THEN
             ReturnValue = True
           ELSE
             ReturnValue = False
           ENDIF
        ENDIF
      CASE (OperatiorLogicalOR)
        IF ((Operand(1)%Type == ValueNumber) .AND. (Operand(2)%Type == ValueNumber)) THEN
           IF ((Operand(1)%Number == True%Number) .OR. (Operand(2)%Number == True%Number)) THEN
             ReturnValue = True
           ELSE
             ReturnValue = False
           ENDIF
        ENDIF
      CASE (FuncRound)
        ReturnValue = SetErlValueNumber(DNINT(Operand(1)%Number))
      CASE (FuncMod)
        ReturnValue = SetErlValueNumber(MOD(Operand(1)%Number, Operand(2)%Number))
      CASE (FuncSin)
        ReturnValue = SetErlValueNumber(SIN(Operand(1)%Number))
      CASE (FuncCos)
        ReturnValue = SetErlValueNumber(COS(Operand(1)%Number))
      CASE (FuncArcSin)
        ReturnValue = SetErlValueNumber(ASIN(Operand(1)%Number))
      CASE (FuncArcCos)
        ReturnValue = SetErlValueNumber(ACOS(Operand(1)%Number))
      CASE (FuncDegToRad)
        ReturnValue = SetErlValueNumber(Operand(1)%Number * DegToRadians)
      CASE (FuncRadToDeg)
        ReturnValue = SetErlValueNumber(Operand(1)%Number / DegToRadians)
      CASE (FuncExp)
        IF (Operand(1)%Number < 700.0D0) THEN
          ReturnValue = SetErlValueNumber(EXP(Operand(1)%Number))
        ELSE
          ! throw Error
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Attempted to calculate exponential value of too large a number: '  &
                                //Trim(TrimSigDigits(Operand(1)%Number, 4))
        ENDIF
      CASE (FuncLn)
        IF (Operand(1)%Number > 0.0D0 ) THEN
          ReturnValue = SetErlValueNumber(LOG(Operand(1)%Number))
        ELSE
          ! throw error,
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Natural Log of zero or less!'
        ENDIF
      CASE (FuncMax)
        ReturnValue = SetErlValueNumber(MAX(Operand(1)%Number, Operand(2)%Number))
      CASE (FuncMin)
        ReturnValue = SetErlValueNumber(MIN(Operand(1)%Number, Operand(2)%Number))

      CASE (FuncABS)
        ReturnValue = SetErlValueNumber(ABS(Operand(1)%Number))
      CASE (FuncRandU)
        CALL RANDOM_NUMBER(tmpRANDU1 )
        tmpRANDU1 = Operand(1)%Number + (Operand(2)%Number - Operand(1)%Number) * tmpRANDU1
        ReturnValue = SetErlValueNumber(tmpRANDU1)
      CASE (FuncRandG)
        DO ! Box-Muller algorithm
          CALL RANDOM_NUMBER( tmpRANDU1 )
          CALL RANDOM_NUMBER( tmpRANDU2 )
          tmpRANDU1 = 2.d0*tmpRANDU1 - 1.d0
          tmpRANDU2 = 2.d0*tmpRANDU2 - 1.d0
          UnitCircleTest = tmpRANDU1**2  + tmpRANDU2**2
          IF (UnitCircleTest > 0.d0 .AND. UnitCircleTest < 1.0d0) EXIT
        ENDDO
        tmpRANDG = SQRT(-2.d0 * LOG(UnitCircleTest)/UnitCircleTest)
        tmpRANDG = tmpRANDG * tmpRANDU1 ! standard normal ran
        !  x     = ran      * sigma             + mean
        tmpRANDG = tmpRANDG * Operand(2)%Number + Operand(1)%Number
        tmpRANDG = MAX(tmpRANDG, Operand(3)%Number) ! min limit
        tmpRANDG = MIN(tmpRANDG, Operand(4)%Number) ! max limit
        ReturnValue = SetErlValueNumber(tmpRANDG)
      CASE (FuncRandSeed)
        ! convert arg to an integer array for the seed.
        CALL RANDOM_SEED(size = SeedN) ! obtains processor's use size as output
        ALLOCATE(SeedIntARR(SeedN))
        Do loop = 1, SeedN
          IF (loop == 1) THEN
            SeedIntARR(loop)  = FLOOR(Operand(1)%Number,i64)
          ELSE
            SeedIntARR(loop)  = FLOOR(Operand(1)%Number,i64)*loop
          ENDIF
        ENDDO
        CALL RANDOM_SEED(put = SeedIntARR)
        ReturnValue = SetErlValueNumber(REAL(SeedIntARR(1),r64)) !just return first number pass as seed
        DEALLOCATE(SeedIntARR)
      CASE (FuncRhoAirFnPbTdbW)
        ReturnValue = SetErlValueNumber( &             ! result =>   density of moist air (kg/m3)
                         PsyRhoAirFnPbTdbW(Operand(1)%Number, & ! pressure (Pa)
                                           Operand(2)%Number, & ! drybulb (C)
                                           Operand(3)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                           'EMS Built-In Function') ) ! called from
      CASE (FuncCpAirFnWTdb)
        ReturnValue = SetErlValueNumber( &       ! result =>   heat capacity of air {J/kg-C}
                         PsyCpAirFnWTdb(Operand(1)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                        Operand(2)%Number, & ! drybulb (C)
                                       'EMS Built-In Function') )
      CASE (FuncHfgAirFnWTdb)
        !BG comment these two psych funct seems confusing (?) is this the enthalpy of water in the air?
        ReturnValue = SetErlValueNumber( &             ! result =>   heat of vaporization for moist air {J/kg}
                         PsyHfgAirFnWTdb(Operand(1)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                         Operand(2)%Number, & ! drybulb (C)
                                        'EMS Built-In Function') )
      CASE (FuncHgAirFnWTdb)
        ! confusing ?  seems like this is really classical Hfg, heat of vaporization
        ReturnValue = SetErlValueNumber( &       ! result =>   enthalpy of the gas {units?}
                         PsyHgAirFnWTdb(Operand(1)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                        Operand(2)%Number, & ! drybulb (C)
                                        'EMS Built-In Function') )
      CASE (FuncTdpFnTdbTwbPb)
        ReturnValue = SetErlValueNumber( &         ! result =>   dew-point temperature {C}
                         PsyTdpFnTdbTwbPb(Operand(1)%Number, & ! drybulb (C)
                                          Operand(2)%Number, & ! wetbulb (C)
                                          Operand(3)%Number, & ! pressure (Pa)
                                         'EMS Built-In Function') )
      CASE (FuncTdpFnWPb)
        ReturnValue = SetErlValueNumber( &    ! result =>  dew-point temperature {C}
                         PsyTdpFnWPb( Operand(1)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                      Operand(2)%Number, & ! pressure (Pa)
                                      'EMS Built-In Function') )
      CASE (FuncHFnTdbW)
        ReturnValue = SetErlValueNumber( &    ! result =>  enthalpy (J/kg)
                         PsyHFnTdbW(Operand(1)%Number, & ! drybulb (C)
                                    Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                    'EMS Built-In Function') )
      CASE (FuncHFnTdbRhPb)
        ReturnValue =  SetErlValueNumber( &          ! result =>  enthalpy (J/kg)
                         PsyHFnTdbRhPb(Operand(1)%Number, & ! drybulb (C)
                                       Operand(2)%Number, & ! relative humidity value (0.0 - 1.0)
                                       Operand(3)%Number, & ! pressure (Pa)
                                       'EMS Built-In Function') )
      CASE (FuncTdbFnHW)
        ReturnValue =  SetErlValueNumber( &       ! result =>  dry-bulb temperature {C}
                         PsyTdbFnHW(Operand(1)%Number, & !  enthalpy (J/kg)
                                    Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                    'EMS Built-In Function') )
      CASE (FuncRhovFnTdbRh)
        ReturnValue =  SetErlValueNumber( &     ! result =>  Vapor density in air (kg/m3)
                         PsyRhovFnTdbRh(Operand(1)%Number, & ! drybulb (C)
                                        Operand(2)%Number, & ! relative humidity value (0.0 - 1.0)
                                        'EMS Built-In Function') )
      CASE (FuncRhovFnTdbRhLBnd0C)
        ReturnValue =  SetErlValueNumber( &   ! result =>  Vapor density in air (kg/m3)
                         PsyRhovFnTdbRhLBnd0C(Operand(1)%Number, & ! drybulb (C)
                                              Operand(2)%Number, & ! relative humidity value (0.0 - 1.0)
                                             'EMS Built-In Function') )
      CASE (FuncRhovFnTdbWPb)
        ReturnValue = SetErlValueNumber( &   ! result =>  Vapor density in air (kg/m3)
                        PsyRhovFnTdbWPb(Operand(1)%Number, & ! drybulb (C)
                                        Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                        Operand(3)%Number, & ! pressure (Pa)
                                       'EMS Built-In Function') )
      CASE (FuncRhFnTdbRhov)
        ReturnValue = SetErlValueNumber( &              ! result => relative humidity value (0.0-1.0)
                        PsyRhFnTdbRhov(Operand(1)%Number, & ! drybulb (C)
                                       Operand(2)%Number, & ! vapor density in air (kg/m3)
                                       'EMS Built-In Function') )
      CASE (FuncRhFnTdbRhovLBnd0C)
        ReturnValue = SetErlValueNumber( &                              ! relative humidity value (0.0-1.0)
                        PsyRhFnTdbRhovLBnd0C(Operand(1)%Number, & ! drybulb (C)
                                             Operand(2)%Number, & ! vapor density in air (kg/m3)
                                            'EMS Built-In Function') )
      CASE (FuncRhFnTdbWPb)
        ReturnValue = SetErlValueNumber( &   ! result =>  relative humidity value (0.0-1.0)
                        PsyRhFnTdbWPb(Operand(1)%Number, & ! drybulb (C)
                                      Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                      Operand(3)%Number, & ! pressure (Pa)
                                      'EMS Built-In Function') )
      CASE (FuncTwbFnTdbWPb)
        ReturnValue = SetErlValueNumber( &   ! result=> Temperature Wet-Bulb {C}
                        PsyTwbFnTdbWPb(Operand(1)%Number, & ! drybulb (C)
                                       Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                       Operand(3)%Number, & ! pressure (Pa)
                                      'EMS Built-In Function') )
      CASE (FuncVFnTdbWPb)
        ReturnValue = SetErlValueNumber( &                      ! result=> specific volume {m3/kg}
                        PsyVFnTdbWPb(Operand(1)%Number, & ! drybulb (C)
                                     Operand(2)%Number, & ! Humidity ratio (kg water vapor/kg dry air)
                                     Operand(3)%Number, & ! pressure (Pa)
                                     'EMS Built-In Function') )
      CASE (FuncWFnTdpPb)
        ReturnValue = SetErlValueNumber( &    ! result=> humidity ratio  (kg water vapor/kg dry air)
                        PsyWFnTdpPb(Operand(1)%Number, & ! dew point temperature (C)
                                    Operand(2)%Number, & ! pressure (Pa)
                                    'EMS Built-In Function') )
      CASE (FuncWFnTdbH)
        ReturnValue = SetErlValueNumber( &    ! result=> humidity ratio  (kg water vapor/kg dry air)
                        PsyWFnTdbH(Operand(1)%Number, & ! drybulb (C)
                                   Operand(2)%Number, & ! enthalpy (J/kg)
                                   'EMS Built-In Function') )
      CASE (FuncWFnTdbTwbPb)
        ReturnValue = SetErlValueNumber( &    ! result=> humidity ratio  (kg water vapor/kg dry air)
                        PsyWFnTdbTwbPb(Operand(1)%Number, & ! drybulb (C)
                                       Operand(2)%Number, & ! wet-bulb temperature {C}
                                       Operand(3)%Number, & ! pressure (Pa)
                                      'EMS Built-In Function') )
      CASE (FuncWFnTdbRhPb)
        ReturnValue = SetErlValueNumber( &   ! result=> humidity ratio  (kg water vapor/kg dry air)
                        PsyWFnTdbRhPb(Operand(1)%Number, & ! drybulb (C)
                                      Operand(2)%Number, & ! relative humidity value (0.0-1.0)
                                      Operand(3)%Number, & ! pressure (Pa)
                                      'EMS Built-In Function') )
      CASE (FuncPsatFnTemp)
        ReturnValue = SetErlValueNumber( &   ! result=> saturation pressure {Pascals}
                        PsyPsatFnTemp(Operand(1)%Number, & ! drybulb (C)
                                      'EMS Built-In Function') )
      CASE (FuncTsatFnHPb)
        ReturnValue = SetErlValueNumber( &   ! result=> saturation temperature {C}
                        PsyTsatFnHPb(Operand(1)%Number, &  ! enthalpy {J/kg}
                                     Operand(2)%Number, & ! pressure (Pa)
                                     'EMS Built-In Function') )
!      CASE (FuncTsatFnPb)
!        ReturnValue = NumberValue( &   ! result=> saturation temperature {C}
!                        PsyTsatFnPb(Operand(1)%Number, & ! pressure (Pa)
!                                    'EMS Built-In Function') )
      CASE (FuncCpCW)
        ReturnValue = SetErlValueNumber( &   ! result => specific heat of water (J/kg-K) = 4180.d0
                        CPCW(Operand(1)%Number, & ! temperature (C) unused
                            'EMS Built-In Function') )
      CASE (FuncCpHW)
        ReturnValue = SetErlValueNumber( &   ! result => specific heat of water (J/kg-K) = 4180.d0
                        CPHW(Operand(1)%Number, & ! temperature (C) unused
                            'EMS Built-In Function') )
      CASE (FuncRhoH2O)
        ReturnValue = SetErlValueNumber( &   ! result => density of water (kg/m3)
                        RhoH2O(Operand(1)%Number, & ! temperature (C)
                            'EMS Built-In Function') )
      CASE (FuncFatalHaltEp)

        CALL ShowSevereError('EMS user program found serious problem and is halting simulation')
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowFatalError('EMS user program halted simulation with error code = ' &
                                   //Trim(TrimSigDigits(Operand(1)%Number,2 ) ) )
        ReturnValue = SetErlValueNumber(Operand(1)%Number) ! returns back the error code
      CASE (FuncSevereWarnEp)

        CALL ShowSevereError('EMS user program issued severe warning with error code = ' &
                                   //Trim(TrimSigDigits(Operand(1)%Number,2 ) ) )
        CALL ShowContinueErrorTimeStamp(' ')
        ReturnValue = SetErlValueNumber(Operand(1)%Number) ! returns back the error code
      CASE (FuncWarnEp)

        CALL ShowWarningError('EMS user program issued warning with error code = ' &
                                   //Trim(TrimSigDigits(Operand(1)%Number,2 ) ) )
        CALL ShowContinueErrorTimeStamp(' ')
        ReturnValue = SetErlValueNumber(Operand(1)%Number) ! returns back the error code
      CASE (FuncTrendValue)
        ! find TrendVariable , first operand is ErlVariable
        If (Operand(1)%TrendVariable) THEN
          thisTrend = Operand(1)%TrendVarPointer
         !second operand is number for index
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) THEN
            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              ReturnValue = SetErlValueNumber(TrendVariable(thisTrend)%TrendValARR(thisIndex), Operand(1))
            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF
          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF

      CASE (FuncTrendAverage)
        ! find TrendVariable , first operand is ErlVariable
        IF (Operand(1)%TrendVariable) THEN
          thisTrend = Operand(1)%TrendVarPointer
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) THEN
            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              !calculate average
              thisAverage = SUM(TrendVariable(thisTrend)%TrendValARR(1:thisIndex)) &
                          / REAL(thisIndex, r64)
              ReturnValue = SetErlValueNumber( thisAverage , Operand(1))
            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF
          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF
      CASE (FuncTrendMax)
        IF (Operand(1)%TrendVariable) THEN
          thisTrend = Operand(1)%TrendVarPointer
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) THEN
            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              thisMax   = 0.0D0
              IF (thisIndex == 1) THEN
                thisMax = TrendVariable(thisTrend)%TrendValARR(1)
              ELSE
                DO  loop = 2, thisIndex
                  IF (loop == 2) THen
                    thisMax = MAX(TrendVariable(thisTrend)%TrendValARR(1), TrendVariable(thisTrend)%TrendValARR(2))
                  ELSE
                    thisMax = MAX(thisMax, TrendVariable(thisTrend)%TrendValARR(loop))
                  ENDIF
                ENDDO
              ENDIF
              ReturnValue = SetErlValueNumber(thisMax, Operand(1))
            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF
          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF
      CASE (FuncTrendMin)
        IF (Operand(1)%TrendVariable) THEN
          thisTrend = Operand(1)%TrendVarPointer
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) THEN
            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              thisMin   = 0.0D0
              IF (thisIndex == 1) THEN
                thisMin = TrendVariable(thisTrend)%TrendValARR(1)
              ELSE
                DO  loop = 2, thisIndex
                  IF (loop == 2) THen
                    thisMin = MIN(TrendVariable(thisTrend)%TrendValARR(1), TrendVariable(thisTrend)%TrendValARR(2))
                  ELSE
                    thisMin = MIN(thisMin, TrendVariable(thisTrend)%TrendValARR(loop))
                  ENDIF
                ENDDO
              ENDIF
              ReturnValue = SetErlValueNumber(thisMin, Operand(1))

            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF

          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF
      CASE (FuncTrendDirection)
        IF (Operand(1)%TrendVariable) THEN
         ! do a linear least squares fit and get slope of line
          thisTrend = Operand(1)%TrendVarPointer
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) THEN

            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              ! closed form solution for slope of linear least squares fit
              thisSlope = ( Sum(TrendVariable(thisTrend)%TimeARR(1:thisIndex))&
                             *Sum(TrendVariable(thisTrend)%TrendValARR(1:thisIndex)) &
                           - thisIndex * Sum((TrendVariable(thisTrend)%TimeARR(1:thisIndex) &
                                 * TrendVariable(thisTrend)%TrendValARR(1:thisIndex))) ) &
                      / ( Sum(TrendVariable(thisTrend)%TimeARR(1:thisIndex))**2 &
                             -  thisIndex*Sum( TrendVariable(thisTrend)%TimeARR(1:thisIndex)**2) )
              ReturnValue = SetErlValueNumber( thisSlope , Operand(1)) ! rate of change per hour
            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF

          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF
      CASE (FuncTrendSum)
        IF (Operand(1)%TrendVariable) THEN

          thisTrend = Operand(1)%TrendVarPointer
          thisIndex = FLOOR(Operand(2)%Number)
          IF (thisIndex >= 1) Then
            IF (thisIndex <= TrendVariable(thisTrend)%LogDepth) THEN
              ReturnValue = SetErlValueNumber(Sum(TrendVariable(thisTrend)%TrendValARR(1:thisIndex)), Operand(1) )
            ELSE
              ReturnValue%Type = ValueError
              ReturnValue%Error = 'Built-in trend function called with index larger than what is being logged'
            ENDIF
          ELSE
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Built-in trend function called with index less than 1'
          ENDIF
        ELSE !not registered as a trend variable
            ReturnValue%Type = ValueError
            ReturnValue%Error = 'Variable used with built-in trend function is not associated with a registered trend variable'
        ENDIF
      CASE (FuncCurveValue)
        ReturnValue = SetErlValueNumber(CurveValue(    &
                                          FLOOR(Operand(1)%Number) , & ! curve index
                                          Operand(2)%Number , & ! X value
                                         Var2 = Operand(3)%Number , & ! Y value, 2nd independent
                                         Var3 = Operand(4)%Number , & ! Z Value, 3rd independent
                                         Var4 = Operand(5)%Number , & ! 4th independent
                                         Var5 = Operand(6)%Number ) )  ! 5th independent

      CASE DEFAULT
        ! throw Error!
        CALL ShowFatalError('caught unexpected Expression(ExpressionNum)%Operator in EvaluateExpression')
    END SELECT
    DEALLOCATE(Operand)
  END IF


  RETURN

END FUNCTION EvaluateExpression


SUBROUTINE GetRuntimeLanguageUserInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith April 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the runtime language objects from the input file.
          ! GetInput is called from other modules that reference runtime language objects.
          ! The runtime language objects are all loaded in one pass

          ! METHODOLOGY EMPLOYED:
          ! The runtime language objects are all loaded in one step, names registered, etc.  They are parsed in a second step
          ! once all the object names are known.

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: MaxNameLength, TimeStepZone
  USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, &
                            SetupOutputVariable, ShowContinueError
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs, &
                            SameString, FindItemInList

  USE General,        ONLY: TrimSigDigits
  USE CurveManager,   ONLY: GetCurveIndex, GetCurveType
  USE DataHeatBalance,ONLY: Construct, TotConstructs
  USE OutputProcessor,ONLY: UnitsStringLength

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: RoutineName = 'GetRuntimeLanguageUserInput: '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: GlobalNum
  INTEGER    :: StackNum
!unused0909  INTEGER    :: NumPrograms
!unused0909  INTEGER    :: NumFunctions
  INTEGER    :: ErrorNum
  INTEGER    :: NumAlphas ! Number of elements in the alpha array
  INTEGER    :: NumNums   ! Number of elements in the numeric array
  INTEGER    :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL    :: IsNotOK   ! Flag to verify name
  LOGICAL    :: IsBlank   ! Flag for blank name
  LOGICAL    :: ErrorsFound = .FALSE.
  INTEGER    :: VariableNum  ! temporary
  INTEGER    :: RuntimeReportVarNum
!unused0909  INTEGER    :: Pos
!unused0909  CHARACTER(len=MaxNameLength) :: VariableName
  LOGICAL    :: Found
  CHARACTER(len=MaxNameLength) :: FreqString = ' ' ! temporary
  CHARACTER(len=MaxNameLength) :: VarTypeString = ' ' ! temporary
  CHARACTER(len=MaxNameLength) :: ResourceTypeString = ' '
  CHARACTER(len=MaxNameLength) :: GroupTypeString = ' '
  CHARACTER(len=MaxNameLength) :: EndUseTypeString = ' '
  CHARACTER(len=MaxNameLength) :: EndUseSubCatString = ' '

  INTEGER    :: TrendNum
  INTEGER    :: NumTrendSteps
  INTEGER    :: loop
  INTEGER    :: ErlVarLoop
  INTEGER    :: CurveIndexNum
  INTEGER                        :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: TotalArgs = 0 !argument for call to GetObjectDefMaxArgs
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  INTEGER  :: ConstructNum
  LOGICAL :: errFlag
  INTEGER :: lbracket
  CHARACTER(len=UnitsStringLength) :: UnitsA
  CHARACTER(len=UnitsStringLength) :: UnitsB
  INTEGER :: ptr


          ! FLOW:
  IF (GetInput) THEN  ! GetInput check is redundant with the InitializeRuntimeLanguage routine
    GetInput = .FALSE.

  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=NumNums
  MaxNumAlphas=NumAlphas
  cCurrentModuleObject = 'EnergyManagementSystem:Actuator'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:ProgramCallingManager'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:Program'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:Subroutine'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:OutputVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'ExternalInterface:Variable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'ExternalInterface:Actuator'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
!  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
!  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
!  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
!  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:GlobalVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:CurveOrTableIndexVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:ConstructionIndexVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  ALLOCATE(cAlphaFieldNames(MaxNumAlphas))
  cAlphaFieldNames=' '
  ALLOCATE(cAlphaArgs(MaxNumAlphas))
  cAlphaArgs=' '
  ALLOCATE(lAlphaFieldBlanks(MaxNumAlphas))
  lAlphaFieldBlanks=.false.
  ALLOCATE(cNumericFieldNames(MaxNumNumbers))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(MaxNumNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lNumericFieldBlanks(MaxNumNumbers))
  lNumericFieldBlanks=.false.

    cCurrentModuleObject = 'EnergyManagementSystem:GlobalVariable'

    IF (NumUserGlobalVariables + NumExternalInterfaceGlobalVariables > 0) THEN
      DO GlobalNum = 1, NumUserGlobalVariables + NumExternalInterfaceGlobalVariables
         ! If we process the ExternalInterface actuators, all we need to do is to change the
         ! name of the module object, and add an offset for the variable number
         ! This is done in the following IF/THEN section.
         IF ( GlobalNum <= NumUserGlobalVariables ) THEN
            CALL GetObjectItem(cCurrentModuleObject, GlobalNum, cAlphaArgs, NumAlphas, rNumericArgs, &
                 NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
         ELSE
            cCurrentModuleObject = 'ExternalInterface:Variable'
            CALL GetObjectItem(cCurrentModuleObject, GlobalNum-NumUserGlobalVariables, cAlphaArgs, NumAlphas, rNumericArgs,&
                 NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
         END IF

        ! loop over each alpha and register variable named as global Erl variable
        DO ErlVarLoop = 1, NumAlphas
          CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(ErlVarLoop),cAlphaFieldNames(ErlVarLoop),  &
             errFlag,ErrorsFound)
          IF (lAlphaFieldBlanks(ErlVarLoop)) THEN
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject))
            CALL ShowContinueError('Blank '//TRIM(cAlphaFieldNames(1)))
            CALL ShowContinueError('Blank entry will be skipped, and the simulation continues')
          ELSEIF (.not. errFlag) THEN
            VariableNum = FindEMSVariable(cAlphaArgs(ErlVarLoop), 0)
            ! Still need to check for conflicts with program and function names too

            IF (VariableNum > 0) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//', invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(ErlVarLoop))//'='//TRIM(cAlphaArgs(ErlVarLoop)))
              CALL ShowContinueError('Name conflicts with an existing global variable name')
              ErrorsFound = .TRUE.
            ELSE
              VariableNum = NewEMSVariable(cAlphaArgs(ErlVarLoop), 0)
              IF ( GlobalNum > NumUserGlobalVariables ) THEN
                 ! Initialize variables for the ExternalInterface variables.
                 ! This object requires an initial value.
                 CALL ExternalInterfaceInitializeErlVariable( VariableNum, &
                      SetErlValueNumber(rNumericArgs(1)), .false. )
              END IF

            END IF
          END IF
        ENDDO
      END DO
    END IF

    cCurrentModuleObject = 'EnergyManagementSystem:CurveOrTableIndexVariable'
    NumEMSCurveIndices   = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumEMSCurveIndices > 0) THEN
      ALLOCATE(CurveIndexVariableNums(NumEMSCurveIndices))
      CurveIndexVariableNums = 0
      DO Loop =1, NumEMSCurveIndices
        CALL GetObjectItem(cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        ! check if variable name is unique and well formed
        CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),  &
             errFlag,ErrorsFound)
          IF (lAlphaFieldBlanks(1)) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject))
            CALL ShowContinueError('Blank '//TRIM(cAlphaFieldNames(1)))
            CALL ShowContinueError('Blank entry for Erl variable name is not allowed')
            ErrorsFound = .TRUE.
          ELSEIF (.not. errFlag) THEN
            VariableNum = FindEMSVariable(cAlphaArgs(1), 0)
            IF (VariableNum > 0) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(1)))
              CALL ShowContinueError('Name conflicts with an existing variable name')
              ErrorsFound = .TRUE.
            ELSE
              ! create new EMS variable
              VariableNum = NewEMSVariable(cAlphaArgs(1), 0)
              ! store variable num
              CurveIndexVariableNums(Loop) = VariableNum
            ENDIF
          ENDIF

          CurveIndexNum = GetCurveIndex(cAlphaArgs(2))  ! curve name
          IF (CurveIndexNum == 0) THEN
            IF (lAlphaFieldBlanks(2)) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' blank field.')
              CALL ShowContinueError('Blank '//TRIM(cAlphaFieldNames(2)))
              CALL ShowContinueError('Blank entry for curve or table name is not allowed')
            ELSE
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
              CALL ShowContinueError('Curve or table was not found.')
            ENDIF
            ErrorsFound = .TRUE.
          ELSE
            ! fill Erl variable with curve index
            ErlVariable(VariableNum)%Value  = SetErlValueNumber(REAL(CurveIndexNum, r64))
          ENDIF
      ENDDO

    ENDIF ! NumEMSCurveIndices > 0


    cCurrentModuleObject = 'EnergyManagementSystem:ConstructionIndexVariable'
    NumEMSConstructionIndices   = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumEMSConstructionIndices > 0) THEN
      ALLOCATE(ConstructionIndexVariableNums(NumEMSConstructionIndices))
      ConstructionIndexVariableNums = 0
      DO Loop =1, NumEMSConstructionIndices
        CALL GetObjectItem(cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        ! check if variable name is unique and well formed
        CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),  &
             errFlag,ErrorsFound)
          IF (lAlphaFieldBlanks(1)) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject))
            CALL ShowContinueError('Blank '//TRIM(cAlphaFieldNames(1)))
            CALL ShowContinueError('Blank entry for Erl variable name is not allowed')
            ErrorsFound = .TRUE.
          ELSEIF (.not. errFlag) THEN
            VariableNum = FindEMSVariable(cAlphaArgs(1), 0)
            IF (VariableNum > 0) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(1)))
              CALL ShowContinueError('Name conflicts with an existing variable name')
              ErrorsFound = .TRUE.
            ELSE
              ! create new EMS variable
              VariableNum = NewEMSVariable(cAlphaArgs(1), 0)
              ! store variable num
              ConstructionIndexVariableNums(Loop) = VariableNum
            ENDIF
          ELSE
            CYCLE
          ENDIF

          ConstructNum = FindItemInList(cAlphaArgs(2), Construct%Name, TotConstructs)

          IF (ConstructNum == 0) THEN
            IF (lAlphaFieldBlanks(2)) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' blank field.')
              CALL ShowContinueError('Blank '//TRIM(cAlphaFieldNames(2)))
              CALL ShowContinueError('Blank entry for construction name is not allowed')
            ELSE
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
              CALL ShowContinueError('Construction was not found.')
            ENDIF
            ErrorsFound = .TRUE.
          ELSE
            ! fill Erl variable with curve index
            ErlVariable(VariableNum)%Value  = SetErlValueNumber(REAL(ConstructNum, r64))
          ENDIF
      ENDDO

    ENDIF ! NumEMSConstructionIndices > 0

    NumErlStacks = NumErlPrograms + NumErlSubroutines
    ALLOCATE(ErlStack(NumErlStacks))

    IF (NumErlPrograms > 0) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:Program'
      DO StackNum = 1, NumErlPrograms
        CALL GetObjectItem(cCurrentModuleObject, StackNum, cAlphaArgs, NumAlphas, rNumericArgs, &
                      NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1), ErlStack%Name, StackNum - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        CALL ValidateEMSProgramName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),  &
             'Programs',errFlag,ErrorsFound)
        IF (.not. errFlag) THEN
          ErlStack(StackNum)%Name = cAlphaArgs(1)
        END IF

        IF (NumAlphas > 1) THEN
          ALLOCATE(ErlStack(StackNum)%Line(NumAlphas - 1))
          ErlStack(StackNum)%NumLines = NumAlphas - 1
          ErlStack(StackNum)%Line(1:NumAlphas - 1) = cAlphaArgs(2:NumAlphas) ! note array assignment
        END IF

      END DO  ! ProgramNum
    END IF

    IF (NumErlSubroutines > 0) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:Subroutine'
      DO StackNum = NumErlPrograms + 1, NumErlStacks
        CALL GetObjectItem(cCurrentModuleObject, StackNum - NumErlPrograms, cAlphaArgs, NumAlphas, rNumericArgs, &
                      NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1), ErlStack%Name, StackNum - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        CALL ValidateEMSProgramName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),  &
             'Subroutines',errFlag,ErrorsFound)
        IF (.not. errFlag) THEN
          ErlStack(StackNum)%Name = cAlphaArgs(1)
        END IF

        IF (NumAlphas > 1) THEN
          ALLOCATE(ErlStack(StackNum)%Line(NumAlphas - 1))
          ErlStack(StackNum)%NumLines = NumAlphas - 1
          ErlStack(StackNum)%Line(1:NumAlphas - 1) = cAlphaArgs(2:NumAlphas) ! note array assignment
        END IF

      END DO  !
    END IF

    cCurrentModuleObject = 'EnergyManagementSystem:TrendVariable'
    NumErlTrendVariables = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumErlTrendVariables > 0) THEN
      ALLOCATE (TrendVariable(NumErlTrendVariables))
      DO TrendNum = 1, NumErlTrendVariables
        CALL GetObjectItem(cCurrentModuleObject, TrendNum, cAlphaArgs, NumAlphas, rNumericArgs, &
                      NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1), TrendVariable%Name, TrendNum - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),  &
             errFlag,ErrorsFound)
        IF (.not. errFlag) THEN
          TrendVariable(TrendNum)%Name = cAlphaArgs(1)
        END IF

        VariableNum = FindEMSVariable(cAlphaArgs(2), 0)
          ! Still need to check for conflicts with program and function names too
        IF (VariableNum == 0) THEN !did not find it
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Did not find a match with an EMS variable name')
          ErrorsFound = .TRUE.
        ELSE ! found it.
          TrendVariable(TrendNum)%ErlVariablePointer = VariableNum
          ! register the trend pointer in ErlVariable.
          ErlVariable(VariableNum)%Value%TrendVariable = .TRUE.
          ErlVariable(VariableNum)%Value%TrendVarPointer = TrendNum
        ENDIF

        NumTrendSteps = FLOOR(rNumericArgs(1))
        IF (NumTrendSteps > 0) THEN
          TrendVariable(TrendNum)%LogDepth = NumTrendSteps
          !setup data arrays using NumTrendSteps
          ALLOCATE(TrendVariable(TrendNum)%TrendValARR(NumTrendSteps))
          TrendVariable(TrendNum)%TrendValARR = 0.0D0 ! array init
          ALLOCATE(TrendVariable(TrendNum)%tempTrendARR(NumTrendSteps))
          TrendVariable(TrendNum)%tempTrendARR = 0.0D0 ! array init
          ALLOCATE(TrendVariable(TrendNum)%TimeARR(NumTrendSteps))
            !construct time data array for use with other calculations later
            ! current time is zero, each value in trend log array is one zone timestep further back in time
            ! units are hours.  all terms negative, getting increasingly negative the further back in time
            !  further back in time is higher index in array
          DO  loop = 1, NumTrendSteps
            IF (loop == 1) THEN
              TrendVariable(TrendNum)%TimeARR(loop) = - TimeStepZone
              CYCLE
            ELSE
              TrendVariable(TrendNum)%TimeARR(loop) = TrendVariable(TrendNum)%TimeARR(loop - 1)  &
                                                       - TimeStepZone ! fractional hours
            ENDIF
          ENDDO
        ELSE
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//Trim(cNumericFieldNames(1))//'='//TRIM(TrimSigDigits(rNumericArgs(1),2)))
          CALL ShowContinueError('must be greater than zero')
          ErrorsFound = .TRUE.
        ENDIF

      ENDDO ! trendnum
    ENDIF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in getting EMS Runtime Language input. Preceding condition causes termination.')
    END IF

    ! Parse the runtime language code
    DO StackNum = 1, NumErlStacks
      CALL ParseStack(StackNum)

      IF (ErlStack(StackNum)%NumErrors > 0) THEN
        CALL ShowSevereError('Errors found parsing EMS Runtime Language program or subroutine = '//TRIM(ErlStack(StackNum)%Name))
        DO ErrorNum = 1, ErlStack(StackNum)%NumErrors
          CALL ShowContinueError(ErlStack(StackNum)%Error(ErrorNum))
        END DO
        ErrorsFound = .TRUE.
      END IF
    END DO  ! StackNum

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in parsing EMS Runtime Language input. Preceding condition causes termination.')
    END IF

    IF ((NumEMSOutputVariables > 0) .OR. (NumEMSMeteredOutputVariables > 0)) THEN
      ALLOCATE(RuntimeReportVar(NumEMSOutputVariables + NumEMSMeteredOutputVariables))
    ENDIF

    IF (NumEMSOutputVariables > 0) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:OutputVariable'
      DO RuntimeReportVarNum = 1, NumEMSOutputVariables
        CALL GetObjectItem(cCurrentModuleObject, RuntimeReportVarNum, cAlphaArgs, NumAlphas, rNumericArgs, &
                      NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1), RuntimeReportVar%Name, RuntimeReportVarNum - 1,  &
          IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        lbracket=INDEX(cAlphaArgs(1),'[')
        if (lbracket == 0) then
          UnitsA=' '
!          if (lAlphaFieldBlanks(6)) then
!            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' no units indicated.')
!            CALL ShowContinueError('...no units indicated for this variable. [] is assumed.')
!            cAlphaArgs(1)=trim(cAlphaArgs(1))//' []'
!          endif
          UnitsB=cAlphaArgs(6)
          lbracket=INDEX(UnitsB,'[')
          ptr=INDEX(UnitsB,']')
          if (lbracket /= 0) then
            UnitsB(lbracket:lbracket)=' '
            if (ptr /= 0) then
              UnitsB(ptr:ptr)=' '
            endif
            UnitsB=adjustl(UnitsB)
          endif
        else  ! units shown on Name field (7.2 and pre versions)
          ptr=INDEX(cAlphaArgs(1),']')
          if (ptr /= 0) then
            UnitsA=cAlphaArgs(1)(lbracket+1:ptr-1)
          else
            UnitsA=cAlphaArgs(1)(lbracket+1:)
          endif
          cAlphaArgs(1)(lbracket-1:)=' '
          UnitsB=cAlphaArgs(6)
          lbracket=INDEX(UnitsB,'[')
          ptr=INDEX(UnitsB,']')
          if (lbracket /= 0) then
            UnitsB(lbracket:lbracket)=' '
            if (ptr /= 0) then
              UnitsB(ptr:ptr)=' '
            endif
            UnitsB=adjustl(UnitsB)
          endif
          if (UnitsA /= ' ' .and. UnitsB /= ' ') then
            if (UnitsA /= UnitsB) then
              CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
                 ' mismatched units.')
              CALL ShowContinueError('...Units entered in '//trim(cAlphaFieldNames(1))//' (deprecated use)="'//trim(UnitsA)//'"')
              CALL ShowContinueError('...'//trim(cAlphaFieldNames(6))//'="'//trim(UnitsB)//'" (will be used)')
            endif
          elseif (UnitsB == ' ' .and. UnitsA /= ' ') then
            UnitsB=UnitsA
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
               ' using deprecated units designation.')
            CALL ShowContinueError('...Units entered in '//trim(cAlphaFieldNames(1))//' (deprecated use)="'//trim(UnitsA)//'"')
          endif
        endif
        cAlphaArgs(1)=trim(cAlphaArgs(1))//' ['//trim(UnitsB)//']'

        RuntimeReportVar(RuntimeReportVarNum)%Name = cAlphaArgs(1)

        IF (.not. lAlphaFieldBlanks(5)) THEN
          ! Lookup the Runtime Language Context, i.e., PROGRAM, FUNCTION, or global
          Found = .FALSE.
          DO StackNum = 1, NumErlStacks
            IF (ErlStack(StackNum)%Name == cAlphaArgs(5)) THEN
              Found = .TRUE.
              EXIT
            END IF
          END DO
          IF (.NOT. Found) THEN
            StackNum = 0
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
            CALL ShowContinueError('EMS program or subroutine not found.')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          StackNum = 0
        END IF

        VariableNum = FindEMSVariable(cAlphaArgs(2), StackNum)

        IF (VariableNum == 0) THEN
          IF (lAlphaFieldBlanks(5)) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
            CALL ShowContinueError('EMS variable not found among global variables.')
          ELSE IF (StackNum /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
            CALL ShowContinueError('EMS variable not found among local variables in '//TRIM(cAlphaArgs(5) ) )
          END IF
          ErrorsFound = .TRUE.
!        ELSEIF (INDEX('0123456789',cAlphaArgs(2)(1:1)) > 0) THEN
!            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
!            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
!            CALL ShowContinueError('Names used as Erl output variables cannot start with numeric characters.')
!            ErrorsFound = .TRUE.
        ELSE
          RuntimeReportVar(RuntimeReportVarNum)%VariableNum = VariableNum
        END IF

        SELECT CASE (TRIM(cAlphaArgs(3)))

        CASE ('AVERAGED')
          VarTypeString = 'Average'
        CASE ('SUMMED')
          VarTypeString = 'Sum'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('...valid values are Averaged or Summed.')
          ErrorsFound = .TRUE.
        END SELECT

        SELECT CASE (TRIM(cAlphaArgs(4)))

        CASE ('ZONETIMESTEP')
          FreqString = 'Zone'
        CASE ('SYSTEMTIMESTEP')
          FreqString = 'System'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('...valid values are ZoneTimestep or SystemTimestep.')
          ErrorsFound = .TRUE.
        END SELECT

        CALL SetupOutputVariable(TRIM(cAlphaArgs(1)),  &
                                 RuntimeReportVar(RuntimeReportVarNum)%Value, &
                                 FreqString,VarTypeString,'EMS')
            ! Last field is index key, no indexing here so mimic weather output data

      END DO  ! RuntimeReportVarNum
    END IF  ! NumEMSOutputVariables > 0

    IF (NumEMSMeteredOutputVariables > 0) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:MeteredOutputVariable'
      DO Loop = 1, NumEMSMeteredOutputVariables
        RuntimeReportVarNum = NumEMSOutputVariables + Loop
        CALL GetObjectItem(cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, &
                      NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1), RuntimeReportVar%Name, RuntimeReportVarNum - 1,  &
          IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        lbracket=INDEX(cAlphaArgs(1),'[')
        if (lbracket == 0) then
          UnitsA=' '
!          if (lAlphaFieldBlanks(9)) then
!            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' no units indicated.')
!            CALL ShowContinueError('...no units indicated for this variable. [] is assumed.')
!            cAlphaArgs(1)=trim(cAlphaArgs(1))//' []'
!          endif
          UnitsB=cAlphaArgs(9)
          lbracket=INDEX(UnitsB,'[')
          ptr=INDEX(UnitsB,']')
          if (lbracket /= 0) then
            UnitsB(lbracket:lbracket)=' '
            if (ptr /= 0) then
              UnitsB(ptr:ptr)=' '
            endif
            UnitsB=adjustl(UnitsB)
          endif
        else  ! units shown on Name field (7.2 and pre versions)
          ptr=INDEX(cAlphaArgs(1),']')
          if (ptr /= 0) then
            UnitsA=cAlphaArgs(1)(lbracket+1:ptr-1)
          else
            UnitsA=cAlphaArgs(1)(lbracket+1:)
          endif
          cAlphaArgs(1)(lbracket-1:)=' '
          UnitsB=cAlphaArgs(9)
          lbracket=INDEX(UnitsB,'[')
          ptr=INDEX(UnitsB,']')
          if (lbracket /= 0) then
            UnitsB(lbracket:lbracket)=' '
            if (ptr /= 0) then
              UnitsB(ptr:ptr)=' '
            endif
            UnitsB=adjustl(UnitsB)
          endif
          if (UnitsA /= ' ' .and. UnitsB /= ' ') then
            if (UnitsA /= UnitsB) then
              CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
                 ' mismatched units.')
              CALL ShowContinueError('...Units entered in '//trim(cAlphaFieldNames(1))//' (deprecated use)="'//trim(UnitsA)//'"')
              CALL ShowContinueError('...'//trim(cAlphaFieldNames(9))//'="'//trim(UnitsB)//'" (will be used)')
            endif
          elseif (UnitsB == ' ' .and. UnitsA /= ' ') then
            UnitsB=UnitsA
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
               ' using deprecated units designation.')
            CALL ShowContinueError('...Units entered in '//trim(cAlphaFieldNames(1))//' (deprecated use)="'//trim(UnitsA)//'"')
          endif
        endif
        cAlphaArgs(1)=trim(cAlphaArgs(1))//' ['//trim(UnitsB)//']'

        RuntimeReportVar(RuntimeReportVarNum)%Name = cAlphaArgs(1)

        IF (.not. lAlphaFieldBlanks(4)) THEN
          ! Lookup the Runtime Language Context, i.e., PROGRAM, FUNCTION, or global
          Found = .FALSE.
          DO StackNum = 1, NumErlStacks
            IF (ErlStack(StackNum)%Name == cAlphaArgs(4)) THEN
              Found = .TRUE.
              EXIT
            END IF
          END DO
          IF (.NOT. Found) THEN
            StackNum = 0
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
            CALL ShowContinueError('EMS program or subroutine not found.')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          StackNum = 0
        END IF

        VariableNum = FindEMSVariable(cAlphaArgs(2), StackNum)
        IF (VariableNum == 0) THEN
          IF (lAlphaFieldBlanks(4)) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
            CALL ShowContinueError('EMS variable not found among global variables.')
          ELSE IF (StackNum /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
            CALL ShowContinueError('EMS variable not found among local variables in '//TRIM(cAlphaArgs(5) ) )
          END IF
          ErrorsFound = .TRUE.
!        ELSEIF (INDEX('0123456789',cAlphaArgs(2)(1:1)) > 0) THEN
!            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
!            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
!            CALL ShowContinueError('Names used as Erl output variables cannot start with numeric characters.')
!            ErrorsFound = .TRUE.
        ELSE
          RuntimeReportVar(RuntimeReportVarNum)%VariableNum = VariableNum
        END IF

        VarTypeString = 'Sum' ! all metered vars are sum type

        SELECT CASE (TRIM(cAlphaArgs(3)))

        CASE ('ZONETIMESTEP')
          FreqString = 'Zone'
        CASE ('SYSTEMTIMESTEP')
          FreqString = 'System'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('...valid values are ZoneTimestep or SystemTimestep.')
          ErrorsFound = .TRUE.
        END SELECT

        SELECT CASE (TRIM(cAlphaArgs(5)))

        CASE ('ELECTRICITY')
          ResourceTypeString = 'Electricity'
        CASE ('NATURALGAS')
          ResourceTypeString = 'NaturalGas'
        CASE ('GASOLINE')
          ResourceTypeString = 'Gasoline'
        CASE ('DIESEL')
          ResourceTypeString = 'Diesel'
        CASE ('COAL')
          ResourceTypeString = 'Coal'
        CASE ('FUELOIL#1')
          ResourceTypeString = 'FuelOil#1'
        CASE ('FUELOIL#2')
          ResourceTypeString = 'FuelOil#2'
        CASE ('OTHERFUEL1')
          ResourceTypeString = 'OtherFuel1'
        CASE ('OTHERFUEL2')
          ResourceTypeString = 'OtherFuel2'
        CASE ('PROPANE')
          ResourceTypeString = 'Propane'
        CASE ('WATERUSE')
          ResourceTypeString = 'Water'
        CASE ('ONSITEWATERPRODUCED')
          ResourceTypeString = 'OnSiteWater'
        CASE ('MAINSWATERSUPPLY')
          ResourceTypeString = 'MainsWater'
        CASE ('RAINWATERCOLLECTED')
          ResourceTypeString = 'RainWater'
        CASE ('WELLWATERDRAWN')
          ResourceTypeString = 'WellWater'
        CASE ('CONDENSATEWATERCOLLECTED')
          ResourceTypeString = 'Condensate'
        CASE ('ENERGYTRANSFER')
          ResourceTypeString = 'EnergyTransfer'
        CASE ('STEAM')
          ResourceTypeString = 'Steam'
        CASE ('DISTRICTCOOLING')
          ResourceTypeString = 'DistrictCooling'
        CASE ('DISTRICTHEATING')
          ResourceTypeString = 'DistrictHeating'
        CASE ('ELECTRICITYPRODUCEDONSITE')
          ResourceTypeString = 'ElectricityProduced'
        CASE ('SOLARWATERHEATING')
          ResourceTypeString = 'SolarWater'
        CASE ('SOLARAIRHEATING')
          ResourceTypeString = 'SolarAir'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
          ErrorsFound = .TRUE.
        END SELECT

        SELECT CASE (TRIM(cAlphaArgs(6)))

        CASE ('BUILDING')
          GroupTypeString = 'Building'
        CASE ('HVAC')
          GroupTypeString = 'HVAC'
        CASE ('PLANT')
          GroupTypeString = 'Plant'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
          ErrorsFound = .TRUE.
        END SELECT

        SELECT CASE (TRIM(cAlphaArgs(7)))

        CASE ('HEATING')
          EndUseTypeString = 'Heating'
        CASE ('COOLING')
          EndUseTypeString = 'Cooling'
        CASE ('INTERIORLIGHTS')
          EndUseTypeString = 'InteriorLights'
        CASE ('EXTERIORLIGHTS')
          EndUseTypeString = 'ExteriorLights'
        CASE ('INTERIOREQUIPMENT')
          EndUseTypeString = 'InteriorEquipment'
        CASE ('EXTERIOREQUIPMENT')
          EndUseTypeString = 'ExteriorEquipment'
        CASE ('FANS')
          EndUseTypeString = 'Fans'
        CASE ('PUMPS')
          EndUseTypeString = 'Pumps'
        CASE ('HEATREJECTION')
          EndUseTypeString = 'HeatRejection'
        CASE ('HUMIDIFIER')
          EndUseTypeString = 'Humidifier'
        CASE ('HEATRECOVERY')
          EndUseTypeString = 'HeatRecovery'
        CASE ('WATERSYSTEMS')
          EndUseTypeString = 'WaterSystems'
        CASE ('REFRIGERATION')
          EndUseTypeString = 'Refrigeration'
        CASE ('ONSITEGENERATION')
          EndUseTypeString = 'Cogeneration'
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid field.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          ErrorsFound = .TRUE.
        END SELECT

        IF (.NOT. lAlphaFieldBlanks(8)) THEN
          EndUseSubCatString = TRIM(cAlphaArgs(8))

          CALL SetupOutputVariable(TRIM(cAlphaArgs(1)),  &
                                 RuntimeReportVar(RuntimeReportVarNum)%Value, &
                                 FreqString,VarTypeString,'EMS', &
                                 ResourceTypeKey = ResourceTypeString, &
                                 EndUseKey       = EndUseTypeString  , &
                                 GroupKey        = GroupTypeString  , &
                                 EndUseSubKey    = EndUseSubCatString)
        ELSE ! no subcat
          CALL SetupOutputVariable(TRIM(cAlphaArgs(1)),  &
                                 RuntimeReportVar(RuntimeReportVarNum)%Value, &
                                 FreqString,VarTypeString,'EMS', &
                                 ResourceTypeKey = ResourceTypeString, &
                                 EndUseKey       = EndUseTypeString  , &
                                 GroupKey        = GroupTypeString )
        ENDIF


      ENDDO
    ENDIF ! NumEMSMeteredOutputVariables > 0

    DEALLOCATE(cAlphaFieldNames)
    DEALLOCATE(cAlphaArgs)
    DEALLOCATE(lAlphaFieldBlanks)
    DEALLOCATE(cNumericFieldNames)
    DEALLOCATE(rNumericArgs)
    DEALLOCATE(lNumericFieldBlanks)

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in getting EMS Runtime Language input. Preceding condition causes termination.')
    END IF

  END IF  ! GetInput

  RETURN

END SUBROUTINE GetRuntimeLanguageUserInput

SUBROUTINE ReportRuntimeLanguage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RuntimeReportVarNum
  INTEGER :: VariableNum

          ! FLOW:
  DO RuntimeReportVarNum = 1, NumEMSOutputVariables+NumEMSMeteredOutputVariables
    VariableNum = RuntimeReportVar(RuntimeReportVarNum)%VariableNum
    IF (ErlVariable(VariableNum)%Value%Type == ValueNumber) THEN
      RuntimeReportVar(RuntimeReportVarNum)%Value = ErlVariable(VariableNum)%Value%Number
    ELSE
      RuntimeReportVar(RuntimeReportVarNum)%Value = 0.0d0
    END IF
  END DO

  RETURN

END SUBROUTINE ReportRuntimeLanguage

FUNCTION IntegerToString(Number) RESULT(String)
          ! FUNCTION INFORMATION:
          !       AUTHOR         P Ellis
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! convert integer number to a string

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  IMPLICIT NONE ! Enforce explicit typing of all variables

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Number
  CHARACTER(len=25)   :: String
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  WRITE(String, *) Number  ! Could add formatting here
  String=ADJUSTL(String)

  RETURN

END FUNCTION IntegerToString


FUNCTION SetErlValueNumber(Number, OrigValue) RESULT(newValue)
          ! FUNCTION INFORMATION:
          !       AUTHOR         P. Ellis
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
          ! na
  REAL(r64), INTENT(IN) :: Number
  TYPE(ErlValueType), OPTIONAL, Intent(IN) :: OrigValue
  TYPE(ErlValueType)  :: newValue

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  IF (Present(OrigValue)) THEN ! preserve other parts of structure and only updated Value%Number
    newValue = OrigValue
    newValue%Number = Number
  ELSE
    newValue%Type = ValueNumber
    newValue%Number = Number
  ENDIF

  RETURN

END FUNCTION SetErlValueNumber


FUNCTION StringValue(String) RESULT(Value)
          ! FUNCTION INFORMATION:
          !       AUTHOR         P. Ellis
          !       DATE WRITTEN   unkown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! convert string to Erl Value structure

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
          ! na
  CHARACTER(len=*), INTENT(IN) :: String
  TYPE(ErlValueType)  :: Value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
          ! FLOW:

  Value%Type = ValueString
  Value%String = String

  RETURN

END FUNCTION StringValue

FUNCTION ValueToString(Value) RESULT(String)
          ! FUNCTION INFORMATION:
          !       AUTHOR         P. Ellis
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(ErlValueType), INTENT(IN) :: Value
  CHARACTER(len=200) :: String

          ! FLOW:
  String = ''

  SELECT CASE (Value%Type)
    CASE (ValueNumber)
      IF (Value%Number == 0.0d0) THEN
        String = '0.0'
      ELSE
        String=TrimSigDigits(Value%Number, 6) !(String)
      END IF

    CASE (ValueString)
      String = Value%String

    CASE (ValueArray)
      ! TBD

    CASE (ValueError)
      String = ' *** Error: '//TRIM(Value%Error)//' *** '

  END SELECT

  RETURN

END FUNCTION ValueToString


FUNCTION FindEMSVariable(VariableName, StackNum) RESULT(VariableNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUpperCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: VariableName ! variable name in Erl
  INTEGER, INTENT(IN)            :: StackNum
  INTEGER                        :: VariableNum

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: Found
  CHARACTER(len=MaxNameLength)   :: UppercaseName
  INTEGER                        :: TrendVarNum

          ! FLOW:
  Found = .FALSE.
  UppercaseName = MakeUpperCase(VariableName)

  ! check in ErlVariables
  DO VariableNum = 1, NumErlVariables
    IF (ErlVariable(VariableNum)%Name == UppercaseName) THEN
      IF ((ErlVariable(VariableNum)%StackNum == StackNum) .OR. (ErlVariable(VariableNum)%StackNum == 0)) THEN
        Found = .TRUE.
        EXIT
      END IF
    END IF
  END DO

  !check in Trend variables
  DO TrendVarNum = 1, NumErlTrendVariables
    IF (TrendVariable(TrendVarNum)%Name == UppercaseName) THEN
      VariableNum = TrendVariable(TrendVarNum)%ErlVariablePointer
      IF ((ErlVariable(VariableNum)%StackNum == StackNum) .OR. (ErlVariable(VariableNum)%StackNum == 0)) THEN
        Found = .TRUE.
        EXIT
      END IF
    ENDIF
  ENDDO

  IF (.NOT. Found) VariableNum = 0

  RETURN

END FUNCTION FindEMSVariable


FUNCTION NewEMSVariable(VariableName, StackNum, Value) RESULT(VariableNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Creates new variable if it doesn't exist.  If exists, returns existing variable number.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUpperCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)          :: VariableName
  INTEGER, INTENT(IN)                   :: StackNum
  TYPE(ErlValueType), INTENT(IN), OPTIONAL :: Value
  INTEGER                               :: VariableNum

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  TYPE(ErlVariableType), DIMENSION(:), ALLOCATABLE :: TempErlVariable

          ! FLOW:
  VariableNum = FindEMSVariable(VariableName, StackNum)

  IF (VariableNum == 0) THEN  ! Variable does not exist anywhere yet
    IF (NumErlVariables == 0) THEN
      ALLOCATE(ErlVariable(1))
      NumErlVariables = 1
    ELSE
      ! Extend the variable array
      ALLOCATE(TempErlVariable(NumErlVariables))
      TempErlVariable = ErlVariable
      DEALLOCATE(ErlVariable)
      ALLOCATE(ErlVariable(NumErlVariables + 1))
      ErlVariable(1:NumErlVariables) = TempErlVariable(1:NumErlVariables)
      DEALLOCATE(TempErlVariable)
      NumErlVariables = NumErlVariables + 1
    END IF

    ! Add the new variable
    VariableNum = NumErlVariables
    ErlVariable(VariableNum)%Name = MakeUpperCase(VariableName)
    ErlVariable(VariableNum)%StackNum = StackNum
    ErlVariable(VariableNum)%Value%Type = ValueNumber ! ErlVariable values are numbers
  END IF

  IF (PRESENT(Value)) ErlVariable(VariableNum)%Value = Value

  RETURN

END FUNCTION NewEMSVariable


SUBROUTINE SetupPossibleOperators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  setup hard coded list of possible operands

          ! METHODOLOGY EMPLOYED:
          ! Allocate structure and fill basic info on opertors and operands
          !  operators include built-in functions where operands are function arguments

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
  LOGICAL , SAVE :: AlreadyDidOnce = .FALSE.

  IF (AlreadyDidOnce) RETURN

  ALLOCATE (PossibleOperators(NumPossibleOperators))

 ! Build operator table
  ! Order in this table is the order of precedence


  PossibleOperators(OperatorLiteral)%NumOperands = 1
  PossibleOperators(OperatorLiteral)%Code        = OperatorLiteral

! not sure how to distinguish from subtract in parsing of tokens, not yet available
!  PossibleOperators(OperatorNegative)%NumOperands = 1
!  PossibleOperators(OperatorNegative)%Code        = OperatorNegative
!  PossibleOperators(OperatorNegative)%Symbol      = '-'

  PossibleOperators(OperatorDivide)%Symbol       = '/'
  PossibleOperators(OperatorDivide)%NumOperands  = 2
  PossibleOperators(OperatorDivide)%Code         = OperatorDivide

  PossibleOperators(OperatorMultiply)%Symbol      = '*'
  PossibleOperators(OperatorMultiply)%NumOperands = 2
  PossibleOperators(OperatorMultiply)%Code        = OperatorMultiply

  PossibleOperators(OperatorSubtract)%Symbol      = '-'
  PossibleOperators(OperatorSubtract)%NumOperands = 2
  PossibleOperators(OperatorSubtract)%Code        = OperatorSubtract

  PossibleOperators(OperatorAdd)%Symbol          = '+'
  PossibleOperators(OperatorAdd)%NumOperands     = 2
  PossibleOperators(OperatorAdd)%Code            = OperatorAdd

  PossibleOperators(OperatorEqual)%Symbol        = '=='
  PossibleOperators(OperatorEqual)%NumOperands   = 2
  PossibleOperators(OperatorEqual)%Code          = OperatorEqual

  PossibleOperators(OperatorNotEqual)%Symbol      = '<>'
  PossibleOperators(OperatorNotEqual)%NumOperands = 2
  PossibleOperators(OperatorNotEqual)%Code        = OperatorNotEqual

  PossibleOperators(OperatorLessOrEqual)%Symbol      = '<='
  PossibleOperators(OperatorLessOrEqual)%NumOperands = 2
  PossibleOperators(OperatorLessOrEqual)%Code        = OperatorLessOrEqual

  PossibleOperators(OperatorGreaterOrEqual)%Symbol      = '>='
  PossibleOperators(OperatorGreaterOrEqual)%NumOperands = 2
  PossibleOperators(OperatorGreaterOrEqual)%Code        = OperatorGreaterOrEqual

  PossibleOperators(OperatorLessThan)%Symbol      = '<'
  PossibleOperators(OperatorLessThan)%NumOperands = 2
  PossibleOperators(OperatorLessThan)%Code        = OperatorLessThan

  PossibleOperators(OperatorGreaterThan)%Symbol      = '>'
  PossibleOperators(OperatorGreaterThan)%NumOperands = 2
  PossibleOperators(OperatorGreaterThan)%Code        = OperatorGreaterThan

  PossibleOperators(OperatorRaiseToPower)%Symbol        = '^'
  PossibleOperators(OperatorRaiseToPower)%NumOperands   = 2
  PossibleOperators(OperatorRaiseToPower)%Code          = OperatorRaiseToPower

  PossibleOperators(OperatorLogicalAND)%Symbol        = '&&'
  PossibleOperators(OperatorLogicalAND)%NumOperands   = 2
  PossibleOperators(OperatorLogicalAND)%Code          = OperatorLogicalAND

  PossibleOperators(OperatiorLogicalOR)%Symbol       = '||'
  PossibleOperators(OperatiorLogicalOR)%NumOperands   =  2
  PossibleOperators(OperatiorLogicalOR)%Code      = OperatiorLogicalOR

  PossibleOperators(FuncRound)%Symbol       = '@ROUND'
  PossibleOperators(FuncRound)%NumOperands  = 1
  PossibleOperators(FuncRound)%Code         = FuncRound

  PossibleOperators(FuncMod)%Symbol       = '@MOD'
  PossibleOperators(FuncMod)%NumOperands  = 2
  PossibleOperators(FuncMod)%Code         = FuncMod

  PossibleOperators(FuncSin)%Symbol       = '@SIN'
  PossibleOperators(FuncSin)%NumOperands  = 1
  PossibleOperators(FuncSin)%Code         = FuncSin

  PossibleOperators(FuncCos)%Symbol       = '@COS'
  PossibleOperators(FuncCos)%NumOperands  = 1
  PossibleOperators(FuncCos)%Code         = FuncCos

  PossibleOperators(FuncArcSin)%Symbol       = '@ARCSIN'
  PossibleOperators(FuncArcSin)%NumOperands  = 1
  PossibleOperators(FuncArcSin)%Code         = FuncArcSin

  PossibleOperators(FuncArcCos)%Symbol       = '@ARCCOS'
  PossibleOperators(FuncArcCos)%NumOperands  = 1
  PossibleOperators(FuncArcCos)%Code         = FuncArcCos

  PossibleOperators(FuncDegToRad)%Symbol       = '@DEGTORAD'
  PossibleOperators(FuncDegToRad)%NumOperands  = 1
  PossibleOperators(FuncDegToRad)%Code         = FuncDegToRad

  PossibleOperators(FuncRadToDeg)%Symbol       = '@RADTODEG'
  PossibleOperators(FuncRadToDeg)%NumOperands  =  1
  PossibleOperators(FuncRadToDeg)%Code         =  FuncRadToDeg

  PossibleOperators(FuncExp)%Symbol       = '@EXP'
  PossibleOperators(FuncExp)%NumOperands  =  1
  PossibleOperators(FuncExp)%Code         =  FuncExp

  PossibleOperators(FuncLn)%Symbol       = '@LN'
  PossibleOperators(FuncLn)%NumOperands  =  1
  PossibleOperators(FuncLn)%Code         =  FuncLn

  PossibleOperators(FuncMax)%Symbol       = '@MAX'
  PossibleOperators(FuncMax)%NumOperands  = 2
  PossibleOperators(FuncMax)%Code         =  FuncMax

  PossibleOperators(FuncMin)%Symbol       = '@MIN'
  PossibleOperators(FuncMin)%NumOperands  = 2
  PossibleOperators(FuncMin)%Code         = FuncMin

  PossibleOperators(FuncABS)%Symbol       = '@ABS'
  PossibleOperators(FuncABS)%NumOperands  = 1
  PossibleOperators(FuncABS)%Code         = FuncABS

  PossibleOperators(FuncRandU)%Symbol       = '@RANDOMUNIFORM'
  PossibleOperators(FuncRandU)%NumOperands  = 2
  PossibleOperators(FuncRandU)%Code         = FuncRandU

  PossibleOperators(FuncRandG)%Symbol       = '@RANDOMNORMAL'
  PossibleOperators(FuncRandG)%NumOperands  = 4
  PossibleOperators(FuncRandG)%Code         = FuncRandG

  PossibleOperators(FuncRandSeed)%Symbol       = '@SEEDRANDOM'
  PossibleOperators(FuncRandSeed)%NumOperands  = 1
  PossibleOperators(FuncRandSeed)%Code         = FuncRandSeed

  PossibleOperators(FuncRhoAirFnPbTdbW)%Symbol       = '@RHOAIRFNPBTDBW'
  PossibleOperators(FuncRhoAirFnPbTdbW)%NumOperands  =  3
  PossibleOperators(FuncRhoAirFnPbTdbW)%Code         =  FuncRhoAirFnPbTdbW

  PossibleOperators(FuncCpAirFnWTdb)%Symbol       = '@CPAIRFNWTDB'
  PossibleOperators(FuncCpAirFnWTdb)%NumOperands  =  2
  PossibleOperators(FuncCpAirFnWTdb)%Code         =  FuncCpAirFnWTdb

  PossibleOperators(FuncHfgAirFnWTdb)%Symbol       = '@HFGAIRFNWTDB'
  PossibleOperators(FuncHfgAirFnWTdb)%NumOperands  =  2
  PossibleOperators(FuncHfgAirFnWTdb)%Code         =  FuncHfgAirFnWTdb

  PossibleOperators(FuncHgAirFnWTdb)%Symbol       = '@HGAIRFNWTDB'
  PossibleOperators(FuncHgAirFnWTdb)%NumOperands  =  2
  PossibleOperators(FuncHgAirFnWTdb)%Code         =  FuncHgAirFnWTdb

  PossibleOperators(FuncTdpFnTdbTwbPb)%Symbol       = '@TDPFNTDBTWBPB'
  PossibleOperators(FuncTdpFnTdbTwbPb)%NumOperands  = 3
  PossibleOperators(FuncTdpFnTdbTwbPb)%Code         = FuncTdpFnTdbTwbPb

  PossibleOperators(FuncTdpFnWPb)%Symbol       = '@TDPFNWPB'
  PossibleOperators(FuncTdpFnWPb)%NumOperands  =  2
  PossibleOperators(FuncTdpFnWPb)%Code         =  FuncTdpFnWPb

  PossibleOperators(FuncHFnTdbW)%Symbol       = '@HFNTDBW'
  PossibleOperators(FuncHFnTdbW)%NumOperands  =  2
  PossibleOperators(FuncHFnTdbW)%Code         =  FuncHFnTdbW

  PossibleOperators(FuncHFnTdbRhPb)%Symbol       = '@HFNTDBRHPB'
  PossibleOperators(FuncHFnTdbRhPb)%NumOperands  =  3
  PossibleOperators(FuncHFnTdbRhPb)%Code         =  FuncHFnTdbRhPb

  PossibleOperators(FuncTdbFnHW)%Symbol       = '@TDBFNHW'
  PossibleOperators(FuncTdbFnHW)%NumOperands  =  2
  PossibleOperators(FuncTdbFnHW)%Code         =  FuncTdbFnHW

  PossibleOperators(FuncRhovFnTdbRh)%Symbol       = '@RHOVFNTDBR'
  PossibleOperators(FuncRhovFnTdbRh)%NumOperands  =  2
  PossibleOperators(FuncRhovFnTdbRh)%Code         =  FuncRhovFnTdbRh

  PossibleOperators(FuncRhovFnTdbRhLBnd0C)%Symbol       = '@RhovFnTdbRhLBnd0C'
  PossibleOperators(FuncRhovFnTdbRhLBnd0C)%NumOperands  =  2
  PossibleOperators(FuncRhovFnTdbRhLBnd0C)%Code         =  FuncRhovFnTdbRhLBnd0C

  PossibleOperators(FuncRhovFnTdbWPb)%Symbol       = '@RHOVFNTDBWPB'
  PossibleOperators(FuncRhovFnTdbWPb)%NumOperands  =  3
  PossibleOperators(FuncRhovFnTdbWPb)%Code         =  FuncRhovFnTdbWPb

  PossibleOperators(FuncRhFnTdbRhov)%Symbol       = '@RHFNTDBRHOV'
  PossibleOperators(FuncRhFnTdbRhov)%NumOperands  =  2
  PossibleOperators(FuncRhFnTdbRhov)%Code         =  FuncRhFnTdbRhov

  PossibleOperators(FuncRhFnTdbRhovLBnd0C)%Symbol       = '@RHFNTDBRHOVLBND0C'
  PossibleOperators(FuncRhFnTdbRhovLBnd0C)%NumOperands  =  2
  PossibleOperators(FuncRhFnTdbRhovLBnd0C)%Code         =  FuncRhFnTdbRhovLBnd0C

  PossibleOperators(FuncRhFnTdbWPb)%Symbol       = '@RHFNTDBWPB'
  PossibleOperators(FuncRhFnTdbWPb)%NumOperands  =  3
  PossibleOperators(FuncRhFnTdbWPb)%Code         =  FuncRhFnTdbWPb

  PossibleOperators(FuncTwbFnTdbWPb)%Symbol       = '@TWBFNTDBWPB'
  PossibleOperators(FuncTwbFnTdbWPb)%NumOperands  =  3
  PossibleOperators(FuncTwbFnTdbWPb)%Code         =  FuncTwbFnTdbWPb

  PossibleOperators(FuncVFnTdbWPb)%Symbol       = '@VFNTDBWPB'
  PossibleOperators(FuncVFnTdbWPb)%NumOperands  =  3
  PossibleOperators(FuncVFnTdbWPb)%Code         =  FuncVFnTdbWPb

  PossibleOperators(FuncWFnTdpPb)%Symbol       = '@WFNTDPPB'
  PossibleOperators(FuncWFnTdpPb)%NumOperands  =  2
  PossibleOperators(FuncWFnTdpPb)%Code         =  FuncWFnTdpPb

  PossibleOperators(FuncWFnTdbH)%Symbol       = '@WFNTDBH'
  PossibleOperators(FuncWFnTdbH)%NumOperands  =  2
  PossibleOperators(FuncWFnTdbH)%Code         =  FuncWFnTdbH

  PossibleOperators(FuncWFnTdbTwbPb)%Symbol       = '@WFNTDBTWBPB'
  PossibleOperators(FuncWFnTdbTwbPb)%NumOperands  =  3
  PossibleOperators(FuncWFnTdbTwbPb)%Code         =  FuncWFnTdbTwbPb

  PossibleOperators(FuncWFnTdbRhPb)%Symbol       = '@WFNTDBRHPB'
  PossibleOperators(FuncWFnTdbRhPb)%NumOperands  =  4
  PossibleOperators(FuncWFnTdbRhPb)%Code         =  FuncWFnTdbRhPb

  PossibleOperators(FuncPsatFnTemp)%Symbol       = '@PSATFNTEMP'
  PossibleOperators(FuncPsatFnTemp)%NumOperands  =  1
  PossibleOperators(FuncPsatFnTemp)%Code         =  FuncPsatFnTemp

  PossibleOperators(FuncTsatFnHPb)%Symbol       = '@TSATFNHPB'
  PossibleOperators(FuncTsatFnHPb)%NumOperands  =  2
  PossibleOperators(FuncTsatFnHPb)%Code         =  FuncTsatFnHPb

  PossibleOperators(FuncTsatFnPb)%Symbol       = '@TSATFNPB'
  PossibleOperators(FuncTsatFnPb)%NumOperands  =  1
  PossibleOperators(FuncTsatFnPb)%Code         =  FuncTsatFnPb

  PossibleOperators(FuncCpCW)%Symbol       = '@CPCW'
  PossibleOperators(FuncCpCW)%NumOperands  =  1
  PossibleOperators(FuncCpCW)%Code         =  FuncCpCW

  PossibleOperators(FuncCpHW)%Symbol       = '@CPHW'
  PossibleOperators(FuncCpHW)%NumOperands  =  1
  PossibleOperators(FuncCpHW)%Code         =  FuncCpHW

  PossibleOperators(FuncRhoH2O)%Symbol       = '@RHOH2O'
  PossibleOperators(FuncRhoH2O)%NumOperands  =  1
  PossibleOperators(FuncRhoH2O)%Code         =  FuncRhoH2O

  PossibleOperators(FuncFatalHaltEp)%Symbol       = '@FATALHALTEP'
  PossibleOperators(FuncFatalHaltEp)%NumOperands  = 1
  PossibleOperators(FuncFatalHaltEp)%Code         = FuncFatalHaltEp

  PossibleOperators(FuncSevereWarnEp)%Symbol       = '@SEVEREWARNEP'
  PossibleOperators(FuncSevereWarnEp)%NumOperands  = 1
  PossibleOperators(FuncSevereWarnEp)%Code         = FuncSevereWarnEp

  PossibleOperators(FuncWarnEp)%Symbol       = '@WARNEP'
  PossibleOperators(FuncWarnEp)%NumOperands  = 1
  PossibleOperators(FuncWarnEp)%Code         = FuncWarnEp

  PossibleOperators(FuncTrendValue)%Symbol       = '@TRENDVALUE'
  PossibleOperators(FuncTrendValue)%NumOperands  = 2
  PossibleOperators(FuncTrendValue)%Code         = FuncTrendValue

  PossibleOperators(FuncTrendAverage)%Symbol       = '@TRENDAVERAGE'
  PossibleOperators(FuncTrendAverage)%NumOperands  = 2
  PossibleOperators(FuncTrendAverage)%Code         = FuncTrendAverage

  PossibleOperators(FuncTrendMax)%Symbol       = '@TRENDMAX'
  PossibleOperators(FuncTrendMax)%NumOperands  = 2
  PossibleOperators(FuncTrendMax)%Code         = FuncTrendMax

  PossibleOperators(FuncTrendMin)%Symbol       = '@TRENDMIN'
  PossibleOperators(FuncTrendMin)%NumOperands  = 2
  PossibleOperators(FuncTrendMin)%Code         = FuncTrendMin

  PossibleOperators(FuncTrendDirection)%Symbol       = '@TRENDDIRECTION'
  PossibleOperators(FuncTrendDirection)%NumOperands  = 2
  PossibleOperators(FuncTrendDirection)%Code         = FuncTrendDirection

  PossibleOperators(FuncTrendSum)%Symbol       = '@TRENDSUM'
  PossibleOperators(FuncTrendSum)%NumOperands  = 2
  PossibleOperators(FuncTrendSum)%Code         = FuncTrendSum

  PossibleOperators(FuncCurveValue)%Symbol       = '@CURVEVALUE'
  PossibleOperators(FuncCurveValue)%NumOperands  = 6
  PossibleOperators(FuncCurveValue)%Code         = FuncCurveValue

  AlreadyDidOnce = .TRUE.
  RETURN

END SUBROUTINE SetupPossibleOperators

SUBROUTINE ExternalInterfaceSetErlVariable(varNum, value)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rui Zhang
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the ExternalInterface runtime write ErlVariable function

          ! METHODOLOGY EMPLOYED:
          !
          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: varNum  ! The variable index to be written during run time
  REAL(r64), INTENT(IN) :: value   ! The real time value of the vairable to be set

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ErlVariable(varNum)%Value = SetErlValueNumber(value)

  RETURN

END SUBROUTINE ExternalInterfaceSetErlVariable


SUBROUTINE ExternalInterfaceInitializeErlVariable(varNum, initialValue, setToNull)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets flags for ExternalInterface variables

          ! METHODOLOGY EMPLOYED:
          !
          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: varNum  ! The variable index to be written during run time
  TYPE(ErlValueType), INTENT(IN) :: initialValue ! The initial value
  LOGICAL, INTENT(IN)     :: setToNull ! Flag, if true, value will be initialized to Null

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  ! Set initial value
  IF (setToNull) THEN
     ErlVariable(varNum)%Value%Type = ValueNull
  ELSE
     ErlVariable(varNum)%Value = initialValue
  ENDIF

  ! Set variables to read-only as we don't want that other programs write to them
  ErlVariable(varNum)%ReadOnly = .true.
  ! Set flag that it is used by the ExternalInterface. This is needed to make sure that the ExternalInterface
  ! interface writes only to ExternalInterface variables, and not to other ErlVariable
  ErlVariable(varNum)%SetByExternalInterface = .true.

  RETURN

END SUBROUTINE ExternalInterfaceInitializeErlVariable


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION isExternalInterfaceErlVariable(varNum) RESULT(isExternalInterfaceVar)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function checks if an Erl name obtained from the xml file
          ! is indeed specified as a ExternalInterface variable in the idf file

          ! METHODOLOGY EMPLOYED:
          !
          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: varNum  ! The variable index to be written during run time
  LOGICAL               :: isExternalInterfaceVar ! Set to true if the variable is a ExternalInterface variable

  isExternalInterfaceVar = ErlVariable(varNum)%SetByExternalInterface

  RETURN

END FUNCTION isExternalInterfaceErlVariable



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

END MODULE RuntimeLanguageProcessor
