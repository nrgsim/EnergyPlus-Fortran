PROG = EnergyPlus	

SRCS =	AirflowNetworkBalanceManager.f90 AirflowNetworkSolver.f90 \
	BaseboardRadiator.f90 BaseboardRadiatorElectric.f90 \
	BaseboardRadiatorSteam.f90 BaseboardRadiatorWater.f90 \
	BranchInputManager.f90 BranchNodeConnections.f90 \
	ConductionTransferFunctionCalc.f90 CoolTower.f90 \
	CostEstimateManager.f90 CurveManager.f90 DataAirflowNetwork.f90 \
	DataAirLoop.f90 DataAirSystems.f90 DataBranchAirLoopPlant.f90 \
	DataBranchNodeConnections.f90 DataBSDFWindow.f90 \
	DataComplexFenestration.f90 DataContaminantBalance.f90 \
	DataConvergParams.f90 DataConversions.f90 DataCostEstimate.f90 \
	DataDaylighting.f90 DataDaylightingDevices.f90 Datadefineequip.f90 \
	DataDElight.f90 DataEnvironment.f90 DataEquivalentLayerWindow.f90 \
	DataErrorTracking.f90 DataGenerators.f90 DataGlobalConstants.f90 \
	DataGlobals.f90 DataHeatBalance.f90 DataHeatBalFanSys.f90 \
	DataHeatBalSurface.f90 DataHVACControllers.f90 DataHVACGlobals.f90 \
	DataInterfaces.f90 DataIPShortCuts.f90 DataLoopNode.f90 \
	DataMoistureBalance.f90 DataMoistureBalanceEMPD.f90 DataOutputs.f90 \
	DataPhotovoltaics.f90 DataPlant.f90 DataPlantPipingSystems.f90 \
	DataPrecisionGlobals.f90 DataReportingFlags.f90 DataRoomAir.f90 \
	DataRootFinder.f90 DataRuntimeLanguage.f90 \
	DataShadowingCombinations.f90 DataSizing.f90 DataStringGlobals.f90 \
	DataSurfaceColors.f90 DataSurfaceLists.f90 DataSurfaces.f90 \
	DataSystemVariables.f90 DataTimings.f90 DataUCSDSharedData.f90 \
	DataVectorTypes.f90 DataViewFactorInformation.f90 DataWater.f90 \
	DataZoneControls.f90 DataZoneEnergyDemands.f90 DataZoneEquipment.f90 \
	DaylightingDevices.f90 DaylightingManager.f90 DElightManagerF_NO.f90 \
	DemandManager.f90 DesiccantDehumidifiers.f90 DirectAir.f90 \
	DisplayRoutines.f90 DXCoil.f90 EarthTube.f90 \
	EconomicLifeCycleCost.f90 EconomicTariff.f90 EcoRoof.f90 \
	ElectricPowerGenerators.f90 ElectricPowerManager.f90 EMSManager.f90 \
	EnergyPlus.f90 ExteriorEnergyUseManager.f90 ExternalInterface_NO.f90 \
	FanCoilUnits.f90 FaultsManager.f90 FluidProperties.f90 General.f90 \
	GeneralRoutines.f90 GlobalNames.f90 HeatBalanceAirManager.f90 \
	HeatBalanceConvectionCoeffs.f90 HeatBalanceHAMTManager.f90 \
	HeatBalanceInternalHeatGains.f90 HeatBalanceIntRadExchange.f90 \
	HeatBalanceManager.f90 HeatBalanceMovableInsulation.f90 \
	HeatBalanceSurfaceManager.f90 HeatBalFiniteDifferenceManager.f90 \
	HeatRecovery.f90 Humidifiers.f90 HVACControllers.f90 \
	HVACCooledBeam.f90 HVACDualDuctSystem.f90 HVACDuct.f90 \
	HVACDXSystem.f90 HVACEvapComponent.f90 HVACFanComponent.f90 \
	HVACFurnace.f90 HVACHeatingCoils.f90 HVACHXAssistedCoolingCoil.f90 \
	HVACInterfaceManager.f90 HVACManager.f90 HVACMixerComponent.f90 \
	HVACMultiSpeedHeatPump.f90 HVACSingleDuctInduc.f90 \
	HVACSingleDuctSystem.f90 HVACSplitterComponent.f90 \
	HVACStandAloneERV.f90 HVACSteamCoilComponent.f90 \
	HVACTranspiredCollector.f90 HVACUnitaryBypassVAV.f90 \
	HVACUnitarySystem.f90 HVACVariableRefrigerantFlow.f90 \
	HVACWaterCoilComponent.f90 HVACWatertoAir.f90 \
	HVACWatertoAirMultiSpeedHP.f90 InputProcessor.f90 \
	MatrixDataManager.f90 MixedAir.f90 MoistureBalanceEMPDManager.f90 \
	NodeInputManager.f90 NonZoneEquipmentManager.f90 \
	OutAirNodeManager.f90 OutdoorAirUnit.f90 OutputProcessor.f90 \
	OutputReportPredefined.f90 OutputReports.f90 OutputReportTabular.f90 \
	PackagedTerminalHeatPump.f90 PackagedThermalStorageCoil.f90 \
	Photovoltaics.f90 PhotovoltaicThermalCollectors.f90 \
	PlantAbsorptionChillers.f90 PlantBoilers.f90 PlantBoilersSteam.f90 \
	PlantCentralGSHP.f90 PlantChillers.f90 PlantCondLoopOperation.f90 \
	PlantCondLoopTowers.f90 PlantEIRChillers.f90 \
	PlantEvapFluidCoolers.f90 PlantExhaustAbsorptionChiller.f90 \
	PlantFluidCoolers.f90 PlantFreeCoolingHeatExchanger.f90 \
	PlantGasAbsorptionChiller.f90 PlantGroundHeatExchangers.f90 \
	PlantHeatExchanger.f90 PlantIceThermalStorage.f90 \
	PlantLoadProfile.f90 PlantLoopEquipment.f90 PlantLoopSolver.f90 \
	PlantManager.f90 PlantOutsideEnergySources.f90 \
	PlantPipeHeatTransfer.f90 PlantPipes.f90 PlantPipingSystemManager.f90 \
	PlantPlateHeatExchanger.f90 PlantPondGroundHeatExchanger.f90 \
	PlantPressureSystem.f90 PlantPumps.f90 PlantSolarCollectors.f90 \
	PlantSurfaceGroundHeatExchanger.f90 PlantUtilities.f90 \
	PlantValves.f90 PlantWaterSources.f90 PlantWaterThermalTank.f90 \
	PlantWatertoWaterGSHP.f90 PlantWaterUse.f90 \
	PollutionAnalysisModule.f90 PoweredInductionUnits.f90 \
	PsychRoutines.f90 Purchasedairmanager.f90 RadiantSystemHighTemp.f90 \
	RadiantSystemLowTemp.f90 RefrigeratedCase.f90 ReportSizingManager.f90 \
	ReturnAirPath.f90 RoomAirManager.f90 RoomAirModelCrossVent.f90 \
	RoomAirModelDisplacementVent.f90 RoomAirModelMundt.f90 \
	RoomAirModelUFAD.f90 RoomAirModelUserTempPattern.f90 RootFinder.f90 \
	RuntimeLanguageProcessor.f90 ScheduleManager.f90 SetPointManager.f90 \
	SimAirServingZones.f90 SimulationManager.f90 SizingManager.f90 \
	SolarReflectionManager.f90 SolarShading.f90 \
	SortAndStringUtilities.f90 SQLiteFortranRoutines_NO.f90 \
	StandardRatings.f90 SurfaceGeometry.f90 SystemAvailabilityManager.f90 \
	SystemReports.f90 TarcogComplexFenestration.f90 ThermalChimney.f90 \
	ThermalComfort.f90 UnitHeater.f90 UnitVentilator.f90 \
	UserDefinedComponents.f90 UtilityRoutines.f90 VectorUtilities.f90 \
	VentilatedSlab.f90 WaterManager.f90 WeatherManager.f90 WindowAC.f90 \
	WindowComplexManager.f90 WindowEquivalentLayer.f90 WindowManager.f90 \
	WindTurbine.f90 Zoneairloopequipmentmanager.f90 \
	ZoneContaminantPredictorCorrector.f90 ZoneDehumidifier.f90 \
	Zoneequipmentmanager.f90 ZonePlenumComponent.f90 \
	ZoneTempPredictorCorrector.f90

OBJS =	AirflowNetworkBalanceManager.o AirflowNetworkSolver.o \
	BaseboardRadiator.o BaseboardRadiatorElectric.o \
	BaseboardRadiatorSteam.o BaseboardRadiatorWater.o \
	BranchInputManager.o BranchNodeConnections.o \
	ConductionTransferFunctionCalc.o CoolTower.o CostEstimateManager.o \
	CurveManager.o DataAirflowNetwork.o DataAirLoop.o DataAirSystems.o \
	DataBranchAirLoopPlant.o DataBranchNodeConnections.o DataBSDFWindow.o \
	DataComplexFenestration.o DataContaminantBalance.o \
	DataConvergParams.o DataConversions.o DataCostEstimate.o \
	DataDaylighting.o DataDaylightingDevices.o Datadefineequip.o \
	DataDElight.o DataEnvironment.o DataEquivalentLayerWindow.o \
	DataErrorTracking.o DataGenerators.o DataGlobalConstants.o \
	DataGlobals.o DataHeatBalance.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHVACControllers.o DataHVACGlobals.o \
	DataInterfaces.o DataIPShortCuts.o DataLoopNode.o \
	DataMoistureBalance.o DataMoistureBalanceEMPD.o DataOutputs.o \
	DataPhotovoltaics.o DataPlant.o DataPlantPipingSystems.o \
	DataPrecisionGlobals.o DataReportingFlags.o DataRoomAir.o \
	DataRootFinder.o DataRuntimeLanguage.o DataShadowingCombinations.o \
	DataSizing.o DataStringGlobals.o DataSurfaceColors.o \
	DataSurfaceLists.o DataSurfaces.o DataSystemVariables.o DataTimings.o \
	DataUCSDSharedData.o DataVectorTypes.o DataViewFactorInformation.o \
	DataWater.o DataZoneControls.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o DaylightingDevices.o DaylightingManager.o \
	DElightManagerF_NO.o DemandManager.o DesiccantDehumidifiers.o \
	DirectAir.o DisplayRoutines.o DXCoil.o EarthTube.o \
	EconomicLifeCycleCost.o EconomicTariff.o EcoRoof.o \
	ElectricPowerGenerators.o ElectricPowerManager.o EMSManager.o \
	EnergyPlus.o ExteriorEnergyUseManager.o ExternalInterface_NO.o \
	FanCoilUnits.o FaultsManager.o FluidProperties.o General.o \
	GeneralRoutines.o GlobalNames.o HeatBalanceAirManager.o \
	HeatBalanceConvectionCoeffs.o HeatBalanceHAMTManager.o \
	HeatBalanceInternalHeatGains.o HeatBalanceIntRadExchange.o \
	HeatBalanceManager.o HeatBalanceMovableInsulation.o \
	HeatBalanceSurfaceManager.o HeatBalFiniteDifferenceManager.o \
	HeatRecovery.o Humidifiers.o HVACControllers.o HVACCooledBeam.o \
	HVACDualDuctSystem.o HVACDuct.o HVACDXSystem.o HVACEvapComponent.o \
	HVACFanComponent.o HVACFurnace.o HVACHeatingCoils.o \
	HVACHXAssistedCoolingCoil.o HVACInterfaceManager.o HVACManager.o \
	HVACMixerComponent.o HVACMultiSpeedHeatPump.o HVACSingleDuctInduc.o \
	HVACSingleDuctSystem.o HVACSplitterComponent.o HVACStandAloneERV.o \
	HVACSteamCoilComponent.o HVACTranspiredCollector.o \
	HVACUnitaryBypassVAV.o HVACUnitarySystem.o \
	HVACVariableRefrigerantFlow.o HVACWaterCoilComponent.o \
	HVACWatertoAir.o HVACWatertoAirMultiSpeedHP.o InputProcessor.o \
	MatrixDataManager.o MixedAir.o MoistureBalanceEMPDManager.o \
	NodeInputManager.o NonZoneEquipmentManager.o OutAirNodeManager.o \
	OutdoorAirUnit.o OutputProcessor.o OutputReportPredefined.o \
	OutputReports.o OutputReportTabular.o PackagedTerminalHeatPump.o \
	PackagedThermalStorageCoil.o Photovoltaics.o \
	PhotovoltaicThermalCollectors.o PlantAbsorptionChillers.o \
	PlantBoilers.o PlantBoilersSteam.o PlantCentralGSHP.o PlantChillers.o \
	PlantCondLoopOperation.o PlantCondLoopTowers.o PlantEIRChillers.o \
	PlantEvapFluidCoolers.o PlantExhaustAbsorptionChiller.o \
	PlantFluidCoolers.o \
	PlantGasAbsorptionChiller.o PlantGroundHeatExchangers.o \
	PlantHeatExchanger.o PlantIceThermalStorage.o PlantLoadProfile.o \
	PlantLoopEquipment.o PlantLoopSolver.o PlantManager.o \
	PlantOutsideEnergySources.o PlantPipeHeatTransfer.o PlantPipes.o \
	PlantPipingSystemManager.o \
	PlantPondGroundHeatExchanger.o PlantPressureSystem.o PlantPumps.o \
	PlantSolarCollectors.o PlantSurfaceGroundHeatExchanger.o \
	PlantUtilities.o PlantValves.o PlantWaterSources.o \
	PlantWaterThermalTank.o PlantWatertoWaterGSHP.o PlantWaterUse.o \
	PollutionAnalysisModule.o PoweredInductionUnits.o PsychRoutines.o \
	Purchasedairmanager.o RadiantSystemHighTemp.o RadiantSystemLowTemp.o \
	RefrigeratedCase.o ReportSizingManager.o ReturnAirPath.o \
	RoomAirManager.o RoomAirModelCrossVent.o \
	RoomAirModelDisplacementVent.o RoomAirModelMundt.o RoomAirModelUFAD.o \
	RoomAirModelUserTempPattern.o RootFinder.o RuntimeLanguageProcessor.o \
	ScheduleManager.o SetPointManager.o SimAirServingZones.o \
	SimulationManager.o SizingManager.o SolarReflectionManager.o \
	SolarShading.o SortAndStringUtilities.o SQLiteFortranRoutines_NO.o \
	StandardRatings.o SurfaceGeometry.o SystemAvailabilityManager.o \
	SystemReports.o TarcogComplexFenestration.o ThermalChimney.o \
	ThermalComfort.o UnitHeater.o UnitVentilator.o \
	UserDefinedComponents.o UtilityRoutines.o VectorUtilities.o \
	VentilatedSlab.o WaterManager.o WeatherManager.o WindowAC.o \
	WindowComplexManager.o WindowEquivalentLayer.o WindowManager.o \
	WindTurbine.o Zoneairloopequipmentmanager.o \
	ZoneContaminantPredictorCorrector.o ZoneDehumidifier.o \
	Zoneequipmentmanager.o ZonePlenumComponent.o \
	ZoneTempPredictorCorrector.o

LIBS =	

BINDIR = /home/chad/NRGsim/EnergyPlus/EnergyPlus-Fortran/gfortran-test/obj
#BINDIR = f:\gfortran-test\obj
#RUNDIR = f:\gfortran-test
RUNDIR = /home/chad/NRGsim/EnergyPlus/EnergyPlus-Fortran/gfortran-test
CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O
#F90 = f:\gfortran\bin\gfortran
F90 = gfortran
F90FLAGS = -cpp -ffree-line-length-none -Wall -fimplicit-none -Wcharacter-truncation -Wunused-parameter 
LDFLAGS = -s -static

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $(PROG) $(RUNDIR)
	mv *.o $(BINDIR)
	mv *.mod $(BINDIR)

PlantFree: 
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm  $(RUNDIR)$(PROG) $(BINDIR)\$(OBJS) $(BINDIR)\*.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

AirflowNetworkBalanceManager.o: AirflowNetworkSolver.o \
	BranchNodeConnections.o DataAirLoop.o DataAirSystems.o \
	DataAirflowNetwork.o DataBranchNodeConnections.o \
	DataContaminantBalance.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSurfaces.o \
	DataZoneEquipment.o General.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o HVACSingleDuctSystem.o \
	HVACSplitterComponent.o InputProcessor.o MixedAir.o PsychRoutines.o \
	ScheduleManager.o ThermalComfort.o ZoneDehumidifier.o
AirflowNetworkSolver.o: DataAirLoop.o DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSurfaces.o General.o PsychRoutines.o
BaseboardRadiator.o: BranchNodeConnections.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o FluidProperties.o \
	General.o GlobalNames.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
BaseboardRadiatorElectric.o: DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataSurfaces.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o General.o GlobalNames.o \
	InputProcessor.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
BaseboardRadiatorSteam.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataSurfaces.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o GlobalNames.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
BaseboardRadiatorWater.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataSurfaces.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o GlobalNames.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
BranchInputManager.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataErrorTracking.o DataGlobals.o \
	DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o General.o InputProcessor.o \
	NodeInputManager.o
BranchNodeConnections.o: DataBranchNodeConnections.o DataGlobals.o \
	DataInterfaces.o DataLoopNode.o General.o InputProcessor.o
ConductionTransferFunctionCalc.o: DataConversions.o DataGlobals.o \
	DataHeatBalance.o DataInterfaces.o DataPrecisionGlobals.o General.o
CoolTower.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataInterfaces.o \
	DataPrecisionGlobals.o DataWater.o General.o InputProcessor.o \
	PsychRoutines.o ScheduleManager.o WaterManager.o
CostEstimateManager.o: DXCoil.o DataCostEstimate.o DataDaylighting.o \
	DataGlobals.o DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPhotovoltaics.o DataPrecisionGlobals.o DataSurfaces.o \
	HVACHeatingCoils.o InputProcessor.o PlantChillers.o
CurveManager.o: DataBranchAirLoopPlant.o DataGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	DataSystemVariables.o General.o InputProcessor.o
DataAirflowNetwork.o: DataGlobals.o DataPrecisionGlobals.o
DataAirLoop.o: DataGlobals.o DataPrecisionGlobals.o
DataAirSystems.o: DataGlobals.o DataPlant.o DataPrecisionGlobals.o
DataBranchAirLoopPlant.o: DataGlobals.o DataPrecisionGlobals.o
DataBranchNodeConnections.o: DataGlobals.o
DataBSDFWindow.o: DataGlobals.o DataPrecisionGlobals.o DataVectorTypes.o
DataComplexFenestration.o: DataGlobals.o DataPrecisionGlobals.o
DataContaminantBalance.o: DataGlobals.o DataPrecisionGlobals.o DataSurfaces.o
DataConvergParams.o: DataGlobals.o DataPrecisionGlobals.o
DataConversions.o: DataPrecisionGlobals.o
DataCostEstimate.o: DataGlobals.o DataPrecisionGlobals.o
DataDaylighting.o: DataGlobals.o DataPrecisionGlobals.o
DataDaylightingDevices.o: DataGlobals.o DataPrecisionGlobals.o
Datadefineequip.o: DataGlobals.o DataPrecisionGlobals.o
DataDElight.o: DataPrecisionGlobals.o
DataEnvironment.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	General.o
DataEquivalentLayerWindow.o: DataGlobals.o DataPrecisionGlobals.o
DataErrorTracking.o: DataPrecisionGlobals.o
DataGenerators.o: DataGlobals.o DataPrecisionGlobals.o
DataGlobalConstants.o: DataGlobals.o InputProcessor.o
DataGlobals.o: DataPrecisionGlobals.o
DataHeatBalance.o: DataBSDFWindow.o DataComplexFenestration.o \
	DataEnvironment.o DataEquivalentLayerWindow.o DataGlobals.o \
	DataInterfaces.o DataPrecisionGlobals.o DataSurfaces.o \
	DataVectorTypes.o General.o InputProcessor.o
DataHeatBalFanSys.o: DataPrecisionGlobals.o
DataHeatBalSurface.o: DataPrecisionGlobals.o
DataHVACGlobals.o: DataGlobals.o DataPrecisionGlobals.o
DataInterfaces.o: DataPrecisionGlobals.o
DataIPShortCuts.o: DataGlobals.o DataPrecisionGlobals.o
DataLoopNode.o: DataGlobals.o DataPrecisionGlobals.o
DataMoistureBalance.o: DataPrecisionGlobals.o
DataMoistureBalanceEMPD.o: DataPrecisionGlobals.o
DataOutputs.o: DataGlobals.o
DataPhotovoltaics.o: DataGlobals.o DataPrecisionGlobals.o
DataPlant.o: BranchInputManager.o DataGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPrecisionGlobals.o DataSizing.o General.o \
	InputProcessor.o
DataPlantPipingSystems.o: DataGlobals.o DataPrecisionGlobals.o
DataRoomAir.o: DataGlobals.o DataPrecisionGlobals.o
DataRootFinder.o: DataPrecisionGlobals.o
DataRuntimeLanguage.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o
DataSizing.o: DataGlobals.o DataPrecisionGlobals.o
DataSurfaceColors.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	InputProcessor.o
DataSurfaceLists.o: DataGlobals.o DataHeatBalance.o DataInterfaces.o \
	DataPrecisionGlobals.o DataSurfaces.o InputProcessor.o
DataSurfaces.o: DataBSDFWindow.o DataGlobals.o DataPrecisionGlobals.o \
	DataVectorTypes.o
DataSystemVariables.o: DataPrecisionGlobals.o DataStringGlobals.o
DataTimings.o: DataErrorTracking.o DataInterfaces.o DataPrecisionGlobals.o \
	DataSystemVariables.o General.o
DataUCSDSharedData.o: DataPrecisionGlobals.o
DataVectorTypes.o: DataPrecisionGlobals.o
DataViewFactorInformation.o: DataGlobals.o DataPrecisionGlobals.o
DataWater.o: DataGlobals.o DataPrecisionGlobals.o
DataZoneControls.o: DataGlobals.o DataPrecisionGlobals.o
DataZoneEnergyDemands.o: DataPrecisionGlobals.o
DataZoneEquipment.o: BranchNodeConnections.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o General.o InputProcessor.o \
	NodeInputManager.o ScheduleManager.o
DaylightingDevices.o: DataDaylighting.o DataDaylightingDevices.o \
	DataGlobals.o DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataSurfaces.o DataSystemVariables.o \
	FluidProperties.o General.o InputProcessor.o VectorUtilities.o
DaylightingManager.o: DElightManagerF_NO.o DataDaylighting.o \
	DataDaylightingDevices.o DataEnvironment.o DataErrorTracking.o \
	DataGlobals.o DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataStringGlobals.o DataSurfaces.o \
	DataSystemVariables.o DaylightingDevices.o General.o \
	HeatBalanceInternalHeatGains.o InputProcessor.o \
	OutputReportPredefined.o SQLiteFortranRoutines_NO.o ScheduleManager.o \
	SolarReflectionManager.o VectorUtilities.o WindowComplexManager.o
DElightManagerF_NO.o: DataDElight.o DataGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o
DemandManager.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataZoneControls.o \
	ExteriorEnergyUseManager.o General.o InputProcessor.o \
	OutputProcessor.o ScheduleManager.o
DesiccantDehumidifiers.o: BranchNodeConnections.o CurveManager.o DXCoil.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o EMSManager.o FluidProperties.o \
	General.o HVACFanComponent.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o HeatRecovery.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	PlantUtilities.o PsychRoutines.o ScheduleManager.o
DirectAir.o: DataAirflowNetwork.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEquipment.o General.o \
	HVACSplitterComponent.o InputProcessor.o NodeInputManager.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o
DisplayRoutines.o: DataGlobals.o DataSystemVariables.o
DXCoil.o: BranchNodeConnections.o CurveManager.o DataAirLoop.o \
	DataAirSystems.o DataBranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	DataWater.o EMSManager.o General.o GlobalNames.o HVACFanComponent.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	OutputReportPredefined.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o StandardRatings.o WaterManager.o
EarthTube.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataSurfaces.o General.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o
EconomicLifeCycleCost.o: DataCostEstimate.o DataGlobalConstants.o \
	DataGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o EconomicTariff.o InputProcessor.o \
	OutputReportTabular.o SQLiteFortranRoutines_NO.o
EconomicTariff.o: DataCostEstimate.o DataEnvironment.o DataGlobalConstants.o \
	DataGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o General.o InputProcessor.o OutputProcessor.o \
	OutputReportPredefined.o OutputReportTabular.o \
	SQLiteFortranRoutines_NO.o ScheduleManager.o
EcoRoof.o: ConductionTransferFunctionCalc.o DataEnvironment.o DataGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSurfaces.o \
	DataWater.o General.o HeatBalanceConvectionCoeffs.o PsychRoutines.o
ElectricPowerGenerators.o: BranchNodeConnections.o CurveManager.o \
	DataEnvironment.o DataGenerators.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o FluidProperties.o General.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	PlantUtilities.o PsychRoutines.o ScheduleManager.o
ElectricPowerManager.o: CurveManager.o DataEnvironment.o \
	DataGlobalConstants.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o ElectricPowerGenerators.o \
	General.o InputProcessor.o OutputReportPredefined.o Photovoltaics.o \
	ScheduleManager.o WindTurbine.o
EMSManager.o: DataAirLoop.o DataAirSystems.o DataGlobals.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	DataRuntimeLanguage.o DataSurfaces.o DataZoneControls.o General.o \
	InputProcessor.o OutAirNodeManager.o OutputProcessor.o \
	RuntimeLanguageProcessor.o ScheduleManager.o
EnergyPlus.o: DataEnvironment.o DataGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o DataStringGlobals.o DataSystemVariables.o \
	DataTimings.o FluidProperties.o InputProcessor.o PsychRoutines.o \
	ScheduleManager.o SimulationManager.o
ExteriorEnergyUseManager.o: DataEnvironment.o DataGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o General.o InputProcessor.o \
	OutputReportPredefined.o ScheduleManager.o
ExternalInterface_NO.o: DataInterfaces.o InputProcessor.o
FanCoilUnits.o: BranchNodeConnections.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	FluidProperties.o General.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACSingleDuctSystem.o \
	HVACWaterCoilComponent.o InputProcessor.o MixedAir.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
FaultsManager.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	InputProcessor.o ScheduleManager.o
FluidProperties.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	General.o InputProcessor.o
General.o: DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataRuntimeLanguage.o DataStringGlobals.o \
	DataSurfaces.o InputProcessor.o
GeneralRoutines.o: BaseboardRadiator.o BaseboardRadiatorSteam.o \
	BaseboardRadiatorWater.o DataAirLoop.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataLoopNode.o DataPrecisionGlobals.o DataSizing.o DataSurfaces.o \
	DataZoneEquipment.o FanCoilUnits.o General.o HVACMixerComponent.o \
	HVACSingleDuctInduc.o HVACSplitterComponent.o \
	HVACWaterCoilComponent.o HeatBalanceConvectionCoeffs.o \
	InputProcessor.o OutdoorAirUnit.o PlantSolarCollectors.o \
	PlantUtilities.o PoweredInductionUnits.o PsychRoutines.o \
	ScheduleManager.o UnitHeater.o UnitVentilator.o VentilatedSlab.o \
	ZonePlenumComponent.o
GlobalNames.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	InputProcessor.o
HeatalanceAirManager.o: DataEnvironment.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataRoomAir.o DataSurfaces.o \
	DataZoneControls.o General.o HVACManager.o InputProcessor.o \
	PsychRoutines.o ScheduleManager.o SystemAvailabilityManager.o
HeatBalanceConvectionCoeffs.o: CurveManager.o DataEnvironment.o \
	DataErrorTracking.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o \
	DataSurfaces.o DataVectorTypes.o DataZoneEquipment.o General.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o VectorUtilities.o
HeatBalanceHAMTManager.o: DataEnvironment.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataMoistureBalance.o DataSurfaces.o General.o InputProcessor.o \
	PsychRoutines.o
HeatBalanceInternalHeatGains.o: DataContaminantBalance.o DataDaylighting.o \
	DataEnvironment.o DataGlobalConstants.o DataGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataRoomAir.o DataSizing.o \
	DataSurfaces.o DataZoneEquipment.o DaylightingDevices.o \
	ElectricPowerGenerators.o ElectricPowerManager.o General.o \
	InputProcessor.o OutputReportPredefined.o OutputReportTabular.o \
	PlantPipeHeatTransfer.o PlantWaterThermalTank.o PlantWaterUse.o \
	PsychRoutines.o RefrigeratedCase.o ScheduleManager.o \
	ZonePlenumComponent.o
HeatBalanceIntRadExchange.o: DataEnvironment.o DataGlobals.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataSurfaces.o DataTimings.o \
	DataViewFactorInformation.o General.o InputProcessor.o \
	WindowEquivalentLayer.o
HeatBalanceManager.o: ConductionTransferFunctionCalc.o DataBSDFWindow.o \
	DataContaminantBalance.o DataDaylighting.o DataEnvironment.o \
	DataEquivalentLayerWindow.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataReportingFlags.o \
	DataRoomAir.o DataStringGlobals.o DataSurfaces.o \
	DataSystemVariables.o DaylightingDevices.o EMSManager.o \
	EconomicTariff.o General.o HeatBalanceInternalHeatGains.o \
	HeatBalanceSurfaceManager.o InputProcessor.o MatrixDataManager.o \
	NodeInputManager.o OutputReportTabular.o ScheduleManager.o \
	SolarShading.o SurfaceGeometry.o WindowComplexManager.o \
	WindowEquivalentLayer.o WindowManager.o
HeatBalanceMovableInsulation.o: DataHeatBalance.o DataInterfaces.o \
	DataPrecisionGlobals.o DataSurfaces.o ScheduleManager.o
HeatBalanceSurfaceManager.o: BaseboardRadiatorElectric.o \
	BaseboardRadiatorSteam.o BaseboardRadiatorWater.o \
	DElightManagerF_NO.o DataAirflowNetwork.o DataDaylighting.o \
	DataDaylightingDevices.o DataEnvironment.o \
	DataEquivalentLayerWindow.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataLoopNode.o DataMoistureBalance.o DataMoistureBalanceEMPD.o \
	DataPrecisionGlobals.o DataRoomAir.o DataRuntimeLanguage.o \
	DataSizing.o DataSurfaces.o DataSystemVariables.o DataTimings.o \
	DataZoneEquipment.o DaylightingDevices.o DaylightingManager.o \
	EcoRoof.o General.o HeatBalFiniteDifferenceManager.o \
	HeatBalanceAirManager.o HeatBalanceConvectionCoeffs.o \
	HeatBalanceHAMTManager.o HeatBalanceIntRadExchange.o \
	HeatBalanceInternalHeatGains.o HeatBalanceMovableInsulation.o \
	InputProcessor.o MoistureBalanceEMPDManager.o \
	OutputReportPredefined.o OutputReportTabular.o PsychRoutines.o \
	RadiantSystemHighTemp.o RadiantSystemLowTemp.o ScheduleManager.o \
	SolarShading.o ThermalComfort.o WindowEquivalentLayer.o \
	WindowManager.o
HeatBalFiniteDifferenceManager.o: DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataMoistureBalance.o DataPrecisionGlobals.o DataSurfaces.o General.o \
	HeatBalanceMovableInsulation.o InputProcessor.o PsychRoutines.o
HeatRecovery.o: BranchNodeConnections.o DXCoil.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	EMSManager.o General.o InputProcessor.o NodeInputManager.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o
Humidifiers.o: BranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	DataWater.o EMSManager.o FluidProperties.o General.o InputProcessor.o \
	NodeInputManager.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o WaterManager.o
HVACControllers.o: DataAirSystems.o DataConvergParams.o DataEnvironment.o \
	DataGlobals.o DataHVACControllers.o DataHVACGlobals.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataRootFinder.o DataSizing.o DataSystemVariables.o EMSManager.o \
	FluidProperties.o General.o HVACWaterCoilComponent.o InputProcessor.o \
	MixedAir.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o RootFinder.o SetPointManager.o
HVACCooledBeam.o: BranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	Datadefineequip.o FluidProperties.o General.o \
	HVACWaterCoilComponent.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
HVACDualDuctSystem.o: BranchNodeConnections.o DataAirLoop.o \
	DataContaminantBalance.o DataConvergParams.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o Datadefineequip.o General.o InputProcessor.o \
	NodeInputManager.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
HVACDuct.o: BranchNodeConnections.o DataContaminantBalance.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o General.o InputProcessor.o NodeInputManager.o
HVACDXSystem.o: BranchNodeConnections.o DXCoil.o DataAirLoop.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o EMSManager.o General.o \
	HVACHXAssistedCoolingCoil.o HVACWatertoAirMultiSpeedHP.o \
	InputProcessor.o NodeInputManager.o PackagedThermalStorageCoil.o \
	PsychRoutines.o ScheduleManager.o
HVACEvapComponent.o: BranchNodeConnections.o DataAirSystems.o \
	DataContaminantBalance.o DataEnvironment.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	DataWater.o DataZoneEnergyDemands.o DataZoneEquipment.o EMSManager.o \
	General.o HVACFanComponent.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o WaterManager.o
HVACFanComponent.o: BranchNodeConnections.o CurveManager.o DataAirLoop.o \
	DataAirflowNetwork.o DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEquipment.o EMSManager.o \
	General.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
HVACFurnace.o: BranchInputManager.o BranchNodeConnections.o DXCoil.o \
	DataAirLoop.o DataAirSystems.o DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneControls.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o EMSManager.o \
	FluidProperties.o General.o HVACControllers.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o HVACWatertoAir.o \
	HVACWatertoAirMultiSpeedHP.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
HVACHeatingCoils.o: BranchNodeConnections.o CurveManager.o DXCoil.o \
	DataAirLoop.o DataAirSystems.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o EMSManager.o General.o \
	GlobalNames.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PsychRoutines.o RefrigeratedCase.o \
	ReportSizingManager.o ScheduleManager.o
HVACHXAssistedCoolingCoil.o: BranchNodeConnections.o DXCoil.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o General.o HVACWaterCoilComponent.o \
	HeatRecovery.o InputProcessor.o NodeInputManager.o PsychRoutines.o
HVACInterfaceManager.o: DataBranchAirLoopPlant.o DataContaminantBalance.o \
	DataConvergParams.o DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o FluidProperties.o \
	PlantUtilities.o
HVACManager.o: AirflowNetworkBalanceManager.o CoolTower.o DataAirLoop.o \
	DataAirflowNetwork.o DataContaminantBalance.o DataConvergParams.o \
	DataEnvironment.o DataErrorTracking.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataReportingFlags.o DataRoomAir.o \
	DataSurfaces.o DataSystemVariables.o DataZoneEquipment.o \
	DemandManager.o EMSManager.o EarthTube.o ElectricPowerManager.o \
	General.o HVACStandAloneERV.o HeatBalanceInternalHeatGains.o \
	NodeInputManager.o NonZoneEquipmentManager.o OutAirNodeManager.o \
	OutputReportTabular.o PlantCondLoopOperation.o \
	PlantIceThermalStorage.o PlantManager.o PlantUtilities.o \
	PollutionAnalysisModule.o PsychRoutines.o RefrigeratedCase.o \
	ScheduleManager.o SetPointManager.o SimAirServingZones.o \
	SystemAvailabilityManager.o SystemReports.o ThermalChimney.o \
	WaterManager.o ZoneContaminantPredictorCorrector.o \
	ZoneTempPredictorCorrector.o Zoneequipmentmanager.o
HVACMixerComponent.o: DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o General.o InputProcessor.o NodeInputManager.o \
	PsychRoutines.o
HVACMultiSpeedHeatPump.o: BranchNodeConnections.o CurveManager.o DXCoil.o \
	DataAirLoop.o DataAirSystems.o DataBranchNodeConnections.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneControls.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o GlobalNames.o \
	HVACFanComponent.o HVACHeatingCoils.o HVACSteamCoilComponent.o \
	HVACWaterCoilComponent.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
HVACSingleDuctInduc.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o Datadefineequip.o \
	FluidProperties.o General.o HVACHeatingCoils.o HVACMixerComponent.o \
	HVACWaterCoilComponent.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
HVACSingleDuctSystem.o: BranchNodeConnections.o DataAirLoop.o \
	DataAirflowNetwork.o DataContaminantBalance.o DataConvergParams.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o Datadefineequip.o \
	FluidProperties.o General.o HVACFanComponent.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
HVACSplitterComponent.o: DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	General.o InputProcessor.o NodeInputManager.o PsychRoutines.o
HVACStandAloneERV.o: BranchNodeConnections.o CurveManager.o DataAirLoop.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneControls.o \
	DataZoneEquipment.o General.o HVACFanComponent.o HeatRecovery.o \
	InputProcessor.o MixedAir.o NodeInputManager.o OutAirNodeManager.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o
HVACSteamCoilComponent.o: BranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
HVACTranspiredCollector.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalSurface.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSurfaces.o DataVectorTypes.o EMSManager.o \
	General.o HeatBalanceConvectionCoeffs.o InputProcessor.o \
	NodeInputManager.o PsychRoutines.o ScheduleManager.o
HVACUnitaryBypassVAV.o: BranchNodeConnections.o DXCoil.o DataAirLoop.o \
	DataAirSystems.o DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneControls.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o EMSManager.o FluidProperties.o General.o \
	HVACFanComponent.o HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o InputProcessor.o \
	MixedAir.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
HVACUnitarySystem.o: BranchInputManager.o BranchNodeConnections.o \
	CurveManager.o DXCoil.o DataAirLoop.o DataAirSystems.o \
	DataAirflowNetwork.o DataEnvironment.o DataGlobals.o \
	DataHVACControllers.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o DataZoneControls.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o EMSManager.o \
	FluidProperties.o General.o HVACDXSystem.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o HVACWatertoAir.o \
	HVACWatertoAirMultiSpeedHP.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o SetPointManager.o
HVACVariableRefrigerantFlow.o: BranchNodeConnections.o CurveManager.o \
	DXCoil.o DataAirLoop.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	FluidProperties.o General.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o InputProcessor.o \
	MixedAir.o NodeInputManager.o OutAirNodeManager.o PlantUtilities.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o \
	WaterManager.o
HVACWaterCoilComponent.o: BranchNodeConnections.o DataAirSystems.o \
	DataBranchAirLoopPlant.o DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataWater.o EMSManager.o FluidProperties.o General.o GlobalNames.o \
	InputProcessor.o NodeInputManager.o OutputReportPredefined.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o SetPointManager.o WaterManager.o
HVACWatertoAir.o: BranchNodeConnections.o CurveManager.o DataAirSystems.o \
	DataContaminantBalance.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o FluidProperties.o General.o \
	GlobalNames.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o
HVACWatertoAirMultiSpeedHP.o: BranchNodeConnections.o CurveManager.o \
	DataAirSystems.o DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o DataWater.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutAirNodeManager.o OutputReportPredefined.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o WaterManager.o
InputProcessor.o: DataGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataOutputs.o DataPrecisionGlobals.o DataSizing.o DataStringGlobals.o \
	DataSystemVariables.o SortAndStringUtilities.o
MatrixDataManager.o: DataGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o General.o InputProcessor.o
MixedAir.o: BranchNodeConnections.o CurveManager.o DataAirLoop.o \
	DataAirSystems.o DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneControls.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o Datadefineequip.o \
	DesiccantDehumidifiers.o EMSManager.o FaultsManager.o General.o \
	HVACDXSystem.o HVACEvapComponent.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACTranspiredCollector.o \
	HVACUnitarySystem.o HVACWaterCoilComponent.o HeatRecovery.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	OutputReportPredefined.o PhotovoltaicThermalCollectors.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o \
	UserDefinedComponents.o
MoistureBalanceEMPDManager.o: DataEnvironment.o DataGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataMoistureBalanceEMPD.o DataPrecisionGlobals.o \
	DataSurfaces.o General.o InputProcessor.o PsychRoutines.o
NodeInputManager.o: BranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataErrorTracking.o DataGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPrecisionGlobals.o FluidProperties.o General.o \
	InputProcessor.o OutputProcessor.o PsychRoutines.o ScheduleManager.o
NonZoneEquipmentManager.o: DataGlobals.o InputProcessor.o \
	PlantWaterThermalTank.o PlantWaterUse.o
OutAirNodeManager.o: DataContaminantBalance.o DataEnvironment.o DataGlobals.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	InputProcessor.o NodeInputManager.o PsychRoutines.o
OutdoorAirUnit.o: BranchNodeConnections.o DXCoil.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataSizing.o \
	DataSurfaceLists.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	DesiccantDehumidifiers.o FluidProperties.o General.o HVACDXSystem.o \
	HVACFanComponent.o HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACUnitarySystem.o HVACWaterCoilComponent.o \
	HeatRecovery.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
OutputProcessor.o: DataEnvironment.o DataGlobalConstants.o DataGlobals.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataOutputs.o \
	DataPrecisionGlobals.o DataStringGlobals.o DataSystemVariables.o \
	General.o InputProcessor.o OutputReportPredefined.o \
	SQLiteFortranRoutines_NO.o ScheduleManager.o SortAndStringUtilities.o
OutputReportPredefined.o: DataGlobals.o DataPrecisionGlobals.o
OutputReports.o: DataDaylighting.o DataErrorTracking.o DataGlobals.o \
	DataHeatBalance.o DataInterfaces.o DataPrecisionGlobals.o \
	DataStringGlobals.o DataSurfaceColors.o DataSurfaces.o General.o \
	ScheduleManager.o VectorUtilities.o
OutputReportTabular.o: DataAirflowNetwork.o DataCostEstimate.o \
	DataEnvironment.o DataErrorTracking.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataOutputs.o DataPrecisionGlobals.o \
	DataShadowingCombinations.o DataSizing.o DataStringGlobals.o \
	DataSurfaces.o DataWater.o DataZoneEquipment.o ElectricPowerManager.o \
	ExteriorEnergyUseManager.o General.o InputProcessor.o \
	OutputProcessor.o OutputReportPredefined.o PollutionAnalysisModule.o \
	PsychRoutines.o RadiantSystemLowTemp.o SQLiteFortranRoutines_NO.o \
	ScheduleManager.o VentilatedSlab.o ZonePlenumComponent.o
PackagedTerminalHeatPump.o: BranchNodeConnections.o DXCoil.o DataAirLoop.o \
	DataAirSystems.o DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o \
	HVACSteamCoilComponent.o HVACWaterCoilComponent.o HVACWatertoAir.o \
	HVACWatertoAirMultiSpeedHP.o InputProcessor.o MixedAir.o \
	NodeInputManager.o OutAirNodeManager.o PlantUtilities.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o
PackagedThermalStorageCoil.o: BranchNodeConnections.o CurveManager.o \
	DataAirSystems.o DataBranchAirLoopPlant.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataWater.o DataZoneEquipment.o FluidProperties.o \
	General.o GlobalNames.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o OutputReportPredefined.o PlantWaterThermalTank.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o \
	WaterManager.o
Photovoltaics.o: DataEnvironment.o DataGlobalConstants.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPhotovoltaics.o DataPrecisionGlobals.o DataSurfaces.o General.o \
	HVACTranspiredCollector.o InputProcessor.o \
	PhotovoltaicThermalCollectors.o ScheduleManager.o
PhotovoltaicThermalCollectors.o: BranchNodeConnections.o DataAirLoop.o \
	DataAirSystems.o DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPhotovoltaics.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataSurfaces.o EMSManager.o FluidProperties.o General.o \
	HeatBalanceConvectionCoeffs.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
PlantAbsorptionChillers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutputReportPredefined.o PlantUtilities.o \
	ReportSizingManager.o
PlantBoilers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataGlobalConstants.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutputReportPredefined.o PlantUtilities.o \
	ReportSizingManager.o
PlantBoilersSteam.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataGlobalConstants.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o EMSManager.o FluidProperties.o \
	General.o GlobalNames.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PlantUtilities.o ReportSizingManager.o
PlantCentralGSHP.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	OutputReportPredefined.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
PlantChillers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutAirNodeManager.o OutputReportPredefined.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
PlantCondLoopOperation.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataRuntimeLanguage.o DataSizing.o \
	EMSManager.o FluidProperties.o General.o InputProcessor.o \
	NodeInputManager.o ReportSizingManager.o ScheduleManager.o
PlantCondLoopTowers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o DataWater.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o OutputReportPredefined.o PlantUtilities.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o \
	WaterManager.o
PlantEIRChillers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutAirNodeManager.o OutputReportPredefined.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o StandardRatings.o
PlantEvapFluidCoolers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o DataWater.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o OutputReportPredefined.o PlantUtilities.o \
	PsychRoutines.o ReportSizingManager.o ScheduleManager.o \
	WaterManager.o
PlantExhaustAbsorptionChiller.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	EMSManager.o ElectricPowerGenerators.o FluidProperties.o General.o \
	GlobalNames.o InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	OutputReportPredefined.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o
PlantFluidCoolers.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o FluidProperties.o \
	General.o InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	OutputReportPredefined.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
PlantFreeCoolingHeatExchanger.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o FluidProperties.o \
	General.o InputProcessor.o NodeInputManager.o PlantUtilities.o \
	ScheduleManager.o
PlantGasAbsorptionChiller.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataSizing.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o OutAirNodeManager.o OutputReportPredefined.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o
PlantGroundHeatExchangers.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o FluidProperties.o \
	General.o InputProcessor.o NodeInputManager.o PlantUtilities.o
PlantHeatExchanger.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	EMSManager.o FluidProperties.o General.o InputProcessor.o \
	NodeInputManager.o OutputReportPredefined.o PlantUtilities.o \
	ReportSizingManager.o ScheduleManager.o
PlantIceThermalStorage.o: BranchNodeConnections.o CurveManager.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o FluidProperties.o General.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ScheduleManager.o
PlantLoadProfile.o: BranchNodeConnections.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o FluidProperties.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o ScheduleManager.o
PlantLoopEquipment.o: BaseboardRadiator.o BaseboardRadiatorSteam.o \
	BaseboardRadiatorWater.o DataGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	ElectricPowerGenerators.o HVACVariableRefrigerantFlow.o \
	HVACWaterCoilComponent.o PhotovoltaicThermalCollectors.o \
	PlantAbsorptionChillers.o PlantBoilers.o PlantBoilersSteam.o \
	PlantCentralGSHP.o PlantChillers.o PlantCondLoopTowers.o \
	PlantEIRChillers.o PlantEvapFluidCoolers.o \
	PlantExhaustAbsorptionChiller.o PlantFluidCoolers.o \
	PlantGasAbsorptionChiller.o PlantGroundHeatExchangers.o \
	PlantHeatExchanger.o PlantIceThermalStorage.o PlantLoadProfile.o \
	PlantOutsideEnergySources.o PlantPipeHeatTransfer.o PlantPipes.o \
	PlantPipingSystemManager.o PlantPondGroundHeatExchanger.o \
	PlantPumps.o PlantSolarCollectors.o PlantSurfaceGroundHeatExchanger.o \
	PlantValves.o PlantWaterSources.o PlantWaterThermalTank.o \
	PlantWaterUse.o PlantWatertoWaterGSHP.o RefrigeratedCase.o \
	ScheduleManager.o UserDefinedComponents.o
PlantLoopSolver.o: DataBranchAirLoopPlant.o DataGlobals.o DataHVACGlobals.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	FluidProperties.o General.o HVACInterfaceManager.o \
	PlantCondLoopOperation.o PlantLoopEquipment.o PlantPressureSystem.o \
	PlantPumps.o PlantUtilities.o
PlantManager.o: BranchInputManager.o DataBranchAirLoopPlant.o \
	DataConvergParams.o DataEnvironment.o DataErrorTracking.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	EMSManager.o FluidProperties.o General.o HVACInterfaceManager.o \
	InputProcessor.o NodeInputManager.o PlantLoopEquipment.o \
	PlantLoopSolver.o PlantPipeHeatTransfer.o PlantPipes.o \
	PlantUtilities.o ReportSizingManager.o ScheduleManager.o \
	SetPointManager.o SystemAvailabilityManager.o
PlantOutsideEnergySources.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o FluidProperties.o \
	General.o InputProcessor.o NodeInputManager.o PlantUtilities.o \
	ScheduleManager.o
PlantPipeHeatTransfer.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o FluidProperties.o General.o \
	HeatBalanceConvectionCoeffs.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o ScheduleManager.o
PlantPipes.o: BranchNodeConnections.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o General.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o
PlantPipingSystemManager.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalSurface.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPlantPipingSystems.o DataPrecisionGlobals.o DataSurfaces.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o
PlantPlateHeatExchanger.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ScheduleManager.o
PlantPondGroundHeatExchanger.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	FluidProperties.o General.o HeatBalanceConvectionCoeffs.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o PsychRoutines.o
PlantPressureSystem.o: CurveManager.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o FluidProperties.o General.o
PlantPumps.o: BranchNodeConnections.o CurveManager.o DataBranchAirLoopPlant.o \
	DataConvergParams.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o FluidProperties.o General.o \
	InputProcessor.o NodeInputManager.o OutputReportPredefined.o \
	PlantPressureSystem.o PlantUtilities.o ReportSizingManager.o \
	ScheduleManager.o
PlantSolarCollectors.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSurfaces.o FluidProperties.o General.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o
PlantSurfaceGroundHeatExchanger.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	FluidProperties.o General.o HeatBalanceConvectionCoeffs.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o
PlantUtilities.o: DataBranchAirLoopPlant.o DataGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	FluidProperties.o General.o
PlantValves.o: BranchNodeConnections.o DataBranchAirLoopPlant.o DataGlobals.o \
	DataHVACGlobals.o DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o General.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o
PlantWaterSources.o: BranchNodeConnections.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o FluidProperties.o General.o \
	InputProcessor.o NodeInputManager.o OutputReportPredefined.o \
	PlantUtilities.o ReportSizingManager.o ScheduleManager.o
PlantWaterThermalTank.o: BranchNodeConnections.o CurveManager.o DXCoil.o \
	DataBranchAirLoopPlant.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o DataSurfaces.o \
	DataZoneEquipment.o FluidProperties.o General.o GlobalNames.o \
	HVACFanComponent.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o OutputReportPredefined.o PlantSolarCollectors.o \
	PlantUtilities.o PsychRoutines.o RefrigeratedCase.o \
	ReportSizingManager.o ScheduleManager.o
PlantWatertoWaterGSHP.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o FluidProperties.o \
	General.o InputProcessor.o NodeInputManager.o PlantUtilities.o
PlantWaterUse.o: BranchNodeConnections.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataWater.o General.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o ScheduleManager.o \
	WaterManager.o
PollutionAnalysisModule.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataPrecisionGlobals.o \
	InputProcessor.o ScheduleManager.o
PoweredInductionUnits.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataSizing.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	Datadefineequip.o FluidProperties.o General.o HVACFanComponent.o \
	HVACHeatingCoils.o HVACMixerComponent.o HVACSteamCoilComponent.o \
	HVACWaterCoilComponent.o InputProcessor.o NodeInputManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
PsychRoutines.o: DataEnvironment.o DataGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o General.o
Purchasedairmanager.o: DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o General.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
RadiantSystemHighTemp.o: DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	DataSurfaces.o DataZoneEnergyDemands.o DataZoneEquipment.o General.o \
	InputProcessor.o ReportSizingManager.o ScheduleManager.o
RadiantSystemLowTemp.o: BranchNodeConnections.o DataBranchAirLoopPlant.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataSurfaceLists.o DataSurfaces.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
RefrigeratedCase.o: BranchNodeConnections.o CurveManager.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataWater.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	FluidProperties.o General.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PlantUtilities.o PsychRoutines.o \
	ScheduleManager.o WaterManager.o
ReportSizingManager.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	General.o OutputReportPredefined.o SQLiteFortranRoutines_NO.o
ReturnAirPath.o: DataAirflowNetwork.o DataGlobals.o DataHVACGlobals.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataZoneEquipment.o HVACMixerComponent.o \
	InputProcessor.o NodeInputManager.o ZonePlenumComponent.o
RoomAirManager.o: DataAirflowNetwork.o DataEnvironment.o DataErrorTracking.o \
	DataGlobals.o DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataRoomAir.o DataSurfaces.o \
	DataUCSDSharedData.o DataZoneEquipment.o General.o InputProcessor.o \
	PsychRoutines.o RoomAirModelCrossVent.o \
	RoomAirModelDisplacementVent.o RoomAirModelMundt.o RoomAirModelUFAD.o \
	RoomAirModelUserTempPattern.o ScheduleManager.o
RoomAirModelCrossVent.o: DataAirflowNetwork.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataRoomAir.o DataSurfaces.o \
	DataUCSDSharedData.o DataZoneEquipment.o \
	HeatBalanceConvectionCoeffs.o HeatBalanceInternalHeatGains.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o
RoomAirModelDisplacementVent.o: DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o DataSurfaces.o \
	DataUCSDSharedData.o DataZoneEquipment.o \
	HeatBalanceConvectionCoeffs.o HeatBalanceInternalHeatGains.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o
RoomAirModelMundt.o: DataEnvironment.o DataGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataInterfaces.o \
	DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o DataSurfaces.o \
	DataZoneEquipment.o HeatBalanceInternalHeatGains.o InputProcessor.o \
	PsychRoutines.o
RoomAirModelUFAD.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o \
	DataSizing.o DataSurfaces.o DataUCSDSharedData.o DataZoneEquipment.o \
	General.o HeatBalanceConvectionCoeffs.o \
	HeatBalanceInternalHeatGains.o InputProcessor.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
RoomAirModelUserTempPattern.o: DataEnvironment.o DataErrorTracking.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o \
	DataSurfaces.o DataZoneEquipment.o FluidProperties.o General.o \
	HeatBalanceInternalHeatGains.o InputProcessor.o OutputReportTabular.o \
	PsychRoutines.o ScheduleManager.o
RootFinder.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	DataRootFinder.o General.o
RuntimeLanguageProcessor.o: CurveManager.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalance.o DataInterfaces.o \
	DataPrecisionGlobals.o DataRuntimeLanguage.o DataSystemVariables.o \
	General.o InputProcessor.o OutputProcessor.o PsychRoutines.o
ScheduleManager.o: DataEnvironment.o DataGlobals.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataStringGlobals.o \
	DataSystemVariables.o General.o InputProcessor.o
SetPointManager.o: CurveManager.o DataAirLoop.o DataAirSystems.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPlant.o DataPrecisionGlobals.o \
	DataZoneControls.o DataZoneEnergyDemands.o DataZoneEquipment.o \
	EMSManager.o General.o InputProcessor.o NodeInputManager.o \
	OutAirNodeManager.o PsychRoutines.o ScheduleManager.o
SimAirServingZones.o: BranchInputManager.o DataAirLoop.o DataAirSystems.o \
	DataContaminantBalance.o DataConvergParams.o DataEnvironment.o \
	DataGlobals.o DataHVACControllers.o DataHVACGlobals.o \
	DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataSystemVariables.o \
	DataZoneEquipment.o Datadefineequip.o DesiccantDehumidifiers.o \
	EMSManager.o General.o HVACControllers.o HVACDXSystem.o HVACDuct.o \
	HVACEvapComponent.o HVACFanComponent.o HVACFurnace.o \
	HVACHXAssistedCoolingCoil.o HVACHeatingCoils.o HVACInterfaceManager.o \
	HVACMultiSpeedHeatPump.o HVACSplitterComponent.o \
	HVACSteamCoilComponent.o HVACUnitaryBypassVAV.o HVACUnitarySystem.o \
	HVACWaterCoilComponent.o HeatRecovery.o Humidifiers.o \
	InputProcessor.o MixedAir.o NodeInputManager.o \
	OutputReportPredefined.o PsychRoutines.o ReportSizingManager.o \
	SystemAvailabilityManager.o UserDefinedComponents.o \
	ZonePlenumComponent.o
SimulationManager.o: BranchInputManager.o BranchNodeConnections.o \
	CostEstimateManager.o CurveManager.o DataAirLoop.o \
	DataBranchNodeConnections.o DataContaminantBalance.o \
	DataConvergParams.o DataEnvironment.o DataErrorTracking.o \
	DataGlobalConstants.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataOutputs.o DataPlant.o \
	DataPrecisionGlobals.o DataReportingFlags.o DataRuntimeLanguage.o \
	DataSizing.o DataStringGlobals.o DataSurfaces.o DataSystemVariables.o \
	DataTimings.o DataZoneEquipment.o DemandManager.o EMSManager.o \
	EconomicLifeCycleCost.o EconomicTariff.o ElectricPowerManager.o \
	ExteriorEnergyUseManager.o ExternalInterface_NO.o FaultsManager.o \
	FluidProperties.o General.o HVACControllers.o HVACDualDuctSystem.o \
	HVACManager.o HeatBalanceAirManager.o HeatBalanceManager.o \
	HeatBalanceSurfaceManager.o InputProcessor.o MixedAir.o \
	NodeInputManager.o OutAirNodeManager.o OutputProcessor.o \
	OutputReportPredefined.o OutputReportTabular.o PlantManager.o \
	PollutionAnalysisModule.o PsychRoutines.o RefrigeratedCase.o \
	SQLiteFortranRoutines_NO.o SetPointManager.o SizingManager.o \
	SolarShading.o SystemReports.o WeatherManager.o \
	ZoneContaminantPredictorCorrector.o ZoneTempPredictorCorrector.o
SizingManager.o: CostEstimateManager.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataPrecisionGlobals.o DataSizing.o \
	DataStringGlobals.o DataZoneEquipment.o General.o \
	HeatBalanceManager.o InputProcessor.o OutputReportPredefined.o \
	OutputReportTabular.o SQLiteFortranRoutines_NO.o ScheduleManager.o \
	SimAirServingZones.o WeatherManager.o Zoneequipmentmanager.o
SolarReflectionManager.o: DataEnvironment.o DataGlobals.o DataHeatBalance.o \
	DataInterfaces.o DataPrecisionGlobals.o DataSurfaces.o \
	DataSystemVariables.o DataVectorTypes.o General.o ScheduleManager.o \
	VectorUtilities.o
SolarShading.o: DataBSDFWindow.o DataDaylighting.o DataDaylightingDevices.o \
	DataEnvironment.o DataEquivalentLayerWindow.o DataErrorTracking.o \
	DataGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataReportingFlags.o \
	DataShadowingCombinations.o DataSurfaces.o DataSystemVariables.o \
	DataTimings.o DataVectorTypes.o DataViewFactorInformation.o \
	DaylightingDevices.o DaylightingManager.o General.o InputProcessor.o \
	OutputReportPredefined.o ScheduleManager.o SolarReflectionManager.o \
	VectorUtilities.o WindowComplexManager.o WindowEquivalentLayer.o
SortAndStringUtilities.o: DataGlobals.o
SQLiteFortranRoutines_NO.o: DataGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o InputProcessor.o
StandardRatings.o: CurveManager.o DataBranchAirLoopPlant.o DataGlobals.o \
	DataHVACGlobals.o DataInterfaces.o DataPlant.o DataPrecisionGlobals.o \
	FluidProperties.o General.o OutputReportPredefined.o
SurfaceGeometry.o: DataEnvironment.o DataEquivalentLayerWindow.o \
	DataErrorTracking.o DataGlobals.o DataHeatBalSurface.o \
	DataHeatBalance.o DataIPShortCuts.o DataInterfaces.o \
	DataPrecisionGlobals.o DataReportingFlags.o DataSurfaces.o \
	DataVectorTypes.o General.o InputProcessor.o OutputReportPredefined.o \
	ScheduleManager.o VectorUtilities.o
SystemAvailabilityManager.o: AirflowNetworkBalanceManager.o CurveManager.o \
	DataAirLoop.o DataAirSystems.o DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataZoneControls.o DataZoneEquipment.o \
	General.o InputProcessor.o NodeInputManager.o PsychRoutines.o \
	ScheduleManager.o
SystemReports.o: BranchNodeConnections.o DataAirLoop.o DataAirSystems.o \
	DataConvergParams.o DataEnvironment.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSizing.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o FanCoilUnits.o \
	HVACSplitterComponent.o HVACStandAloneERV.o InputProcessor.o \
	PackagedTerminalHeatPump.o PsychRoutines.o Purchasedairmanager.o \
	UnitVentilator.o WindowAC.o ZonePlenumComponent.o
TarcogComplexFenestration.o: DataGlobals.o DataPrecisionGlobals.o
ThermalChimney.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataSurfaces.o General.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o
ThermalComfort.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataPrecisionGlobals.o \
	DataRoomAir.o DataSurfaces.o DataZoneEnergyDemands.o General.o \
	InputProcessor.o OutputReportPredefined.o OutputReportTabular.o \
	PsychRoutines.o ScheduleManager.o
UnitHeater.o: BranchNodeConnections.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o FluidProperties.o General.o HVACFanComponent.o \
	HVACHeatingCoils.o HVACSteamCoilComponent.o HVACWaterCoilComponent.o \
	InputProcessor.o NodeInputManager.o PlantUtilities.o PsychRoutines.o \
	ReportSizingManager.o ScheduleManager.o
UnitVentilator.o: BranchNodeConnections.o DataContaminantBalance.o \
	DataEnvironment.o DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataLoopNode.o DataPlant.o DataPrecisionGlobals.o DataSizing.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o FluidProperties.o \
	General.o HVACFanComponent.o HVACHXAssistedCoolingCoil.o \
	HVACHeatingCoils.o HVACSteamCoilComponent.o HVACWaterCoilComponent.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
UserDefinedComponents.o: BranchNodeConnections.o DataEnvironment.o \
	DataGlobals.o DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPlant.o DataPrecisionGlobals.o DataRuntimeLanguage.o DataWater.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o EMSManager.o \
	FluidProperties.o General.o GlobalNames.o InputProcessor.o \
	NodeInputManager.o PlantUtilities.o PsychRoutines.o WaterManager.o
UtilityRoutines.o: BranchInputManager.o BranchNodeConnections.o \
	DataEnvironment.o DataErrorTracking.o DataGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o DataReportingFlags.o DataStringGlobals.o \
	DataSystemVariables.o DataTimings.o DaylightingManager.o \
	ExternalInterface_NO.o General.o NodeInputManager.o PlantManager.o \
	SQLiteFortranRoutines_NO.o SimulationManager.o SolarShading.o \
	SystemReports.o
VectorUtilities.o: DataGlobals.o DataInterfaces.o DataPrecisionGlobals.o \
	DataSurfaces.o DataVectorTypes.o General.o
VentilatedSlab.o: BranchNodeConnections.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataInterfaces.o DataLoopNode.o DataPlant.o \
	DataSizing.o DataSurfaceLists.o DataSurfaces.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o FluidProperties.o \
	General.o HVACFanComponent.o HVACHXAssistedCoolingCoil.o \
	HVACHeatingCoils.o HVACSteamCoilComponent.o HVACWaterCoilComponent.o \
	InputProcessor.o NodeInputManager.o OutAirNodeManager.o \
	PlantUtilities.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
WaterManager.o: DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalance.o DataInterfaces.o DataPrecisionGlobals.o \
	DataSurfaces.o DataWater.o General.o InputProcessor.o \
	ScheduleManager.o
WeatherManager.o: DataEnvironment.o DataGlobals.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataPrecisionGlobals.o \
	DataReportingFlags.o DataSystemVariables.o General.o InputProcessor.o \
	OutputProcessor.o OutputReportPredefined.o PsychRoutines.o \
	ScheduleManager.o ThermalComfort.o
WindowAC.o: BranchNodeConnections.o DXCoil.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSizing.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o General.o HVACFanComponent.o \
	HVACHXAssistedCoolingCoil.o InputProcessor.o MixedAir.o \
	NodeInputManager.o PsychRoutines.o ReportSizingManager.o \
	ScheduleManager.o
WindowComplexManager.o: DataBSDFWindow.o DataEnvironment.o DataGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	DataShadowingCombinations.o DataSurfaces.o DataSystemVariables.o \
	DataVectorTypes.o DataZoneEquipment.o General.o InputProcessor.o \
	PsychRoutines.o TarcogComplexFenestration.o VectorUtilities.o
WindowEquivalentLayer.o: DataEnvironment.o DataEquivalentLayerWindow.o \
	DataGlobals.o DataHeatBalFanSys.o DataHeatBalSurface.o \
	DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSurfaces.o DataZoneEquipment.o \
	DaylightingManager.o General.o InputProcessor.o PsychRoutines.o
WindowManager.o: DataBSDFWindow.o DataEnvironment.o DataGlobals.o \
	DataHeatBalFanSys.o DataHeatBalSurface.o DataHeatBalance.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataSurfaces.o \
	DataZoneEquipment.o General.o HeatBalanceConvectionCoeffs.o \
	InputProcessor.o PsychRoutines.o ScheduleManager.o VectorUtilities.o \
	WindowComplexManager.o WindowEquivalentLayer.o
WindTurbine.o: DataEnvironment.o DataGenerators.o DataGlobalConstants.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o \
	DataPrecisionGlobals.o General.o InputProcessor.o PsychRoutines.o \
	ScheduleManager.o
Zoneairloopequipmentmanager.o: BranchNodeConnections.o DataAirLoop.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataZoneEquipment.o Datadefineequip.o \
	General.o HVACCooledBeam.o HVACDualDuctSystem.o HVACSingleDuctInduc.o \
	HVACSingleDuctSystem.o InputProcessor.o NodeInputManager.o \
	PoweredInductionUnits.o PsychRoutines.o UserDefinedComponents.o
ZoneContaminantPredictorCorrector.o: DataAirflowNetwork.o \
	DataContaminantBalance.o DataEnvironment.o DataGlobals.o \
	DataHVACGlobals.o DataHeatBalFanSys.o DataHeatBalance.o \
	DataIPShortCuts.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataSurfaces.o DataZoneControls.o \
	DataZoneEquipment.o Datadefineequip.o General.o \
	HeatBalanceInternalHeatGains.o InputProcessor.o PsychRoutines.o \
	ScheduleManager.o ZonePlenumComponent.o ZoneTempPredictorCorrector.o
ZoneDehumidifier.o: CurveManager.o DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataWater.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o General.o InputProcessor.o NodeInputManager.o \
	PsychRoutines.o ScheduleManager.o WaterManager.o
Zoneequipmentmanager.o: BaseboardRadiator.o BaseboardRadiatorElectric.o \
	BaseboardRadiatorSteam.o BaseboardRadiatorWater.o DataAirLoop.o \
	DataAirSystems.o DataAirflowNetwork.o DataContaminantBalance.o \
	DataConvergParams.o DataEnvironment.o DataGlobals.o DataHVACGlobals.o \
	DataHeatBalFanSys.o DataHeatBalance.o DataInterfaces.o DataLoopNode.o \
	DataPrecisionGlobals.o DataRoomAir.o DataSizing.o DataSurfaces.o \
	DataZoneEnergyDemands.o DataZoneEquipment.o DirectAir.o EMSManager.o \
	FanCoilUnits.o General.o HVACEvapComponent.o HVACFanComponent.o \
	HVACInterfaceManager.o HVACSplitterComponent.o HVACStandAloneERV.o \
	HVACUnitarySystem.o HVACVariableRefrigerantFlow.o \
	HeatBalanceInternalHeatGains.o HeatRecovery.o InputProcessor.o \
	OutdoorAirUnit.o PackagedTerminalHeatPump.o PlantWaterThermalTank.o \
	PsychRoutines.o Purchasedairmanager.o RadiantSystemHighTemp.o \
	RadiantSystemLowTemp.o RefrigeratedCase.o ReturnAirPath.o \
	ScheduleManager.o SystemAvailabilityManager.o UnitHeater.o \
	UnitVentilator.o UserDefinedComponents.o VentilatedSlab.o WindowAC.o \
	ZoneDehumidifier.o ZonePlenumComponent.o ZoneTempPredictorCorrector.o \
	Zoneairloopequipmentmanager.o
ZonePlenumComponent.o: DataContaminantBalance.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o \
	DataZoneEquipment.o Datadefineequip.o General.o InputProcessor.o \
	NodeInputManager.o PoweredInductionUnits.o PsychRoutines.o
ZoneTempPredictorCorrector.o: DataAirflowNetwork.o DataEnvironment.o \
	DataGlobals.o DataHVACGlobals.o DataHeatBalFanSys.o \
	DataHeatBalSurface.o DataHeatBalance.o DataIPShortCuts.o \
	DataInterfaces.o DataLoopNode.o DataPrecisionGlobals.o DataRoomAir.o \
	DataSurfaces.o DataZoneControls.o DataZoneEnergyDemands.o \
	DataZoneEquipment.o Datadefineequip.o General.o \
	HeatBalanceInternalHeatGains.o InputProcessor.o PsychRoutines.o \
	RoomAirManager.o ScheduleManager.o ThermalComfort.o \
	ZonePlenumComponent.o
