<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">

  <xsl:import href="CheckLibrary.xsl"/>
  
  <xsl:output method="xml" indent="yes" />

  <xsl:variable name="CheckLinkedDocuments"
   select="$CheckParameters/Parameter[@name='CheckLinkedDocuments'] = 'true'"/>
  <xsl:variable name="CheckFormat"
   select="$CheckParameters/Parameter[@name='CheckFormat'] = 'true'"/>
  <xsl:variable name="CheckQuality"
   select="$CheckParameters/Parameter[@name='CheckQuality'] = 'true'"/>
  <xsl:variable name="CheckSemantic"
   select="$CheckParameters/Parameter[@name='CheckSemantic'] = 'true'"/>
  <xsl:variable name="MaxRecursionLevel"
   select="number($CheckParameters/Parameter[@name='MaxRecursionLevel'])"/>
  
  <xsl:template name="processing_document">
    <xsl:param name="level"/>
    <xsl:apply-templates/>
    <xsl:if test="$CheckFormat">
      <xsl:element name="CheckFormat">
        <xsl:apply-templates mode="Format"/>
      </xsl:element>
    </xsl:if>
    <xsl:if test="$CheckQuality">
      <xsl:element name="CheckQuality">
        <xsl:apply-templates mode="Quality"/>
      </xsl:element>
    </xsl:if>
    <xsl:if test="$CheckSemantic">
      <xsl:element name="CheckSemantic">
        <xsl:apply-templates mode="Semantic"/>
      </xsl:element>
    </xsl:if>
    <xsl:if test="$CheckLinkedDocuments">
      <xsl:call-template name="processing_linked_documents">
        <xsl:with-param name="level" select="$level"/>
        <xsl:with-param name="caller" select="."/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <!-- Processing linked documents -->
  <xsl:template name="processing_linked_documents">
    <xsl:param name="level"/>
    <xsl:param name="caller"/>
    <xsl:for-each select="t:ExternalQIFReferences/t:ExternalQIFDocument">
      <xsl:element name="CheckLinkedDocument">
        <xsl:attribute name="uri">
          <xsl:value-of select="t:URI"/>
        </xsl:attribute>
        <xsl:variable name="URI" select="t:URI"/>
        <xsl:variable name="extDocId" select="@id"/>
        <xsl:choose>
          <xsl:when test="$level = $MaxRecursionLevel">
            <xsl:call-template name="error_node">
              <xsl:with-param name="report">
                Maximum recursion level exceeded: Level
		(<xsl:value-of select="$level + 1"/>) &gt;
		MaxRecursionLevel
		(<xsl:value-of select="$MaxRecursionLevel"/>).
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="document($URI)/t:QIFDocument">
              <xsl:call-template name="processing_document">
                <xsl:with-param name="level" select="$level + 1"/>
              </xsl:call-template>
              
              <xsl:variable name="measi"
                select="$caller/t:MeasurementsResults"/>
              <xsl:variable name="charItems"
                select="$caller/t:Characteristics/t:CharacteristicItems"/>
              <xsl:variable name="charNominals"
                select="$caller/t:Characteristics/t:CharacteristicNominals"/>
              <xsl:variable name="featItems"
                select="$caller/t:Features/t:FeatureItems"/>
              <xsl:variable name="featNominals"
                select="$caller/t:Features/t:FeatureNominals"/>

<!-- START CALLING MEASUREMENT TO ITEM ID CHECKS FOR CHARACTERISTICS -->
              <xsl:call-template
                name="checkAngleBetweenCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleFromCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularCoordinateCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkChordCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularRunoutCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCoaxialityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConcentricityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalTaperCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCurveLengthCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDepthCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDiameterCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceBetweenCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceFromCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatTaperCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatnessCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkHeightCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLengthCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineProfileCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLinearCoordinateCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherFormCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkParallelismCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPerpendicularityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointProfileCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPositionCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkRadiusCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalDiameterCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalRadiusCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSquareCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkStraightnessCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileNonUniformCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceTextureCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSymmetryCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThicknessCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidicityCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTotalRunoutCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAngularCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAreaCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAttributeCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedForceCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedLinearCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedMassCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedPressureCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedSpeedCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTemperatureCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTimeCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedUnitCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldBevelCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldCompoundCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldEdgeCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFilletCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareBevelCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareVCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldJCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldPlugCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldScarfCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSeamCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSlotCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSpotCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSquareCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldStudCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSurfacingCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldUCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldVCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWidthCharacteristicMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING MEASUREMENT TO ITEM ID CHECKS FOR CHARACTERISTICS -->

<!-- START CALLING ITEM TO NOMINAL ID CHECKS FOR CHARACTERISTICS -->
              <xsl:call-template
                name="checkAngleBetweenCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleFromCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularCoordinateCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkChordCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularRunoutCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCoaxialityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConcentricityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalTaperCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCurveLengthCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDepthCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDiameterCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceBetweenCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceFromCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatTaperCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatnessCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkHeightCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLengthCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineProfileCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLinearCoordinateCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherFormCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkParallelismCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPerpendicularityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointProfileCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPositionCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkRadiusCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalDiameterCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalRadiusCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSquareCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkStraightnessCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileNonUniformCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceTextureCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSymmetryCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThicknessCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidicityCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTotalRunoutCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAngularCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAreaCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAttributeCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedForceCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedLinearCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedMassCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedPressureCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedSpeedCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTemperatureCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTimeCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedUnitCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldBevelCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldCompoundCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldEdgeCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFilletCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareBevelCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareVCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldJCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldPlugCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldScarfCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSeamCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSlotCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSpotCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSquareCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldStudCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSurfacingCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldUCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldVCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWidthCharacteristicItemNominalId">
	        <xsl:with-param name="charItems" select="$charItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING ITEM TO NOMINAL ID CHECKS FOR CHARACTERISTICS -->
 
<!-- START CALLING NOMINAL TO DEFINITION ID CHECKS FOR CHARACTERISTICS -->
              <xsl:call-template
                name="checkAngleBetweenCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngleFromCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularCoordinateCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkAngularityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkChordCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularRunoutCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCoaxialityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConcentricityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalTaperCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCurveLengthCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDepthCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDiameterCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceBetweenCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkDistanceFromCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatTaperCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkFlatnessCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkHeightCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLengthCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineProfileCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLinearCoordinateCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherFormCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkParallelismCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPerpendicularityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointProfileCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPositionCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkRadiusCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalDiameterCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalRadiusCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSquareCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkStraightnessCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceProfileNonUniformCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceTextureCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSymmetryCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThicknessCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidicityCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTotalRunoutCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAngularCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAreaCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedAttributeCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedForceCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedLinearCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedMassCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedPressureCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedSpeedCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTemperatureCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedTimeCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkUserDefinedUnitCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldBevelCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldCompoundCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldEdgeCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFilletCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareBevelCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldFlareVCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldJCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldPlugCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldScarfCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSeamCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSlotCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSpotCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSquareCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldStudCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldSurfacingCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldUCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWeldVCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkWidthCharacteristicNominalDefinitionId">
	        <xsl:with-param name="charNominals" select="$charNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING NOMINAL TO DEFINITION ID CHECKS FOR CHARACTERISTICS -->

<!-- START CALLING MEASUREMENT TO ITEM ID CHECKS FOR FEATURES -->
              <xsl:call-template
                name="checkCircleFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularArcFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConeFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalSegmentFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylinderFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricalSegmentFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEdgePointFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipseFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticalArcFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCircleFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCylinderFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkExtrudedCrossSectionFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkGroupFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkMarkingFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledLinesFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledPlanesFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelLinesFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelPlanesFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherCurveFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherNonShapeFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherShapeFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherSurfaceFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPlaneFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedCurveFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedSurfaceFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphereFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalSegmentFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceOfRevolutionFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadedFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidalSegmentFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTorusFeatureMeasurementItemId">
	        <xsl:with-param name="measi" select="$measi"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING MEASUREMENT TO ITEM ID CHECKS FOR FEATURES -->

<!-- START CALLING ITEM TO NOMINAL ID CHECKS FOR FEATURES -->
              <xsl:call-template
                name="checkCircleFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularArcFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConeFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalSegmentFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylinderFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricalSegmentFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEdgePointFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipseFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticalArcFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCircleFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCylinderFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkExtrudedCrossSectionFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkGroupFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkMarkingFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledLinesFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledPlanesFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelLinesFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelPlanesFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherCurveFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherNonShapeFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherShapeFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherSurfaceFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureCircleItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureCircularArcItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureLinearItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureParallelogramItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPlaneFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedCurveFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedSurfaceFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphereFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalSegmentFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceOfRevolutionFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadedFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidalSegmentFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTorusFeatureItemNominalId">
	        <xsl:with-param name="featItems" select="$featItems"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING ITEM TO NOMINAL ID CHECKS FOR FEATURES -->

<!-- START CALLING NOMINAL TO DEFINITION ID CHECKS FOR FEATURES -->
              <xsl:call-template
                name="checkCircleFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCircularArcFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConeFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkConicalSegmentFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylinderFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkCylindricalSegmentFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEdgePointFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipseFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkEllipticalArcFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCircleFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkElongatedCylinderFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkExtrudedCrossSectionFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkGroupFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkLineFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkMarkingFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledLinesFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeAngledPlanesFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelLinesFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOppositeParallelPlanesFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherCurveFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherNonShapeFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherShapeFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkOtherSurfaceFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureCircleNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureCircularArcNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureLinearNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPatternFeatureParallelogramNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPlaneFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedCurveFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointDefinedSurfaceFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkPointFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphereFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSphericalSegmentFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkSurfaceOfRevolutionFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkThreadedFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkToroidalSegmentFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
              <xsl:call-template
                name="checkTorusFeatureNominalDefinitionId">
	        <xsl:with-param name="featNominals" select="$featNominals"/>
	        <xsl:with-param name="extDoc" select="."/>
	        <xsl:with-param name="extDocId" select="$extDocId"/>
              </xsl:call-template>
<!-- END CALLING NOMINAL TO DEFINITION ID CHECKS FOR FEATURES -->

            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:element>
    </xsl:for-each>
  </xsl:template>

<!-- START MEASUREMENT TO EXTERNAL ITEM ID CHECKS FOR CHARACTERISTICS
For each XXXCharacteristicMeasurement in the measi (MeasurementsResults) in
which the CharacteristicItemId is the id of the given extDoc (which is the
contents of a document external to the caller), this template checks that
the xId attribute of the CharacteristicItemId is the id of exactly one
XXXCharacteristicItem in the extDoc. There is one of these for each type of
characteristic. There are 73 XXX types.
-->

  <xsl:template name="checkAngleBetweenCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:AngleBetweenCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:AngleBetweenCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      AngleBetweenCharacteristicMeasurement does not
              identify an AngleBetweenCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementsResults/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredCharacteristics
             /t:CharacteristicMeasurements
             /t:AngleCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:AngleCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      AngleCharacteristicMeasurement does not
              identify an AngleCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleFromCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:AngleFromCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:AngleFromCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      AngleFromCharacteristicMeasurement does not
              identify an AngleFromCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularCoordinateCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:AngularCoordinateCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:AngularCoordinateCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      AngularCoordinateCharacteristicMeasurement does not
              identify an AngularCoordinateCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:AngularityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:AngularityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      AngularityCharacteristicMeasurement does not
              identify an AngularityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkChordCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ChordCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ChordCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ChordCharacteristicMeasurement does not
              identify a ChordCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularRunoutCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:CircularRunoutCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:CircularRunoutCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      CircularRunoutCharacteristicMeasurement does not
              identify a CircularRunoutCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:CircularityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:CircularityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      CircularityCharacteristicMeasurement does not
              identify a CircularityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCoaxialityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:CoaxialityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:CoaxialityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      CoaxialityCharacteristicMeasurement does not
              identify a CoaxialityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConcentricityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ConcentricityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ConcentricityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ConcentricityCharacteristicMeasurement does not
              identify a ConcentricityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalTaperCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ConicalTaperCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ConicalTaperCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ConicalTaperCharacteristicMeasurement does not
              identify a ConicalTaperCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ConicityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ConicityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ConicityCharacteristicMeasurement does not
              identify a ConicityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCurveLengthCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:CurveLengthCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:CurveLengthCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      CurveLengthCharacteristicMeasurement does not
              identify a CurveLengthCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:CylindricityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:CylindricityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      CylindricityCharacteristicMeasurement does not
              identify a CylindricityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDepthCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:DepthCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:DepthCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      DepthCharacteristicMeasurement does not
              identify a DepthCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDiameterCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:DiameterCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:DiameterCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      DiameterCharacteristicMeasurement does not
              identify a DiameterCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceBetweenCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:DistanceBetweenCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:DistanceBetweenCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      DistanceBetweenCharacteristicMeasurement does not
              identify a DistanceBetweenCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceFromCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:DistanceFromCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:DistanceFromCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      DistanceFromCharacteristicMeasurement does not
              identify a DistanceFromCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:EllipticityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:EllipticityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      EllipticityCharacteristicMeasurement does not
              identify an EllipticityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatTaperCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:FlatTaperCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:FlatTaperCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      FlatTaperCharacteristicMeasurement does not
              identify a FlatTaperCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatnessCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:FlatnessCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:FlatnessCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      FlatnessCharacteristicMeasurement does not
              identify a FlatnessCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkHeightCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:HeightCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:HeightCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      HeightCharacteristicMeasurement does not
              identify a HeightCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLengthCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:LengthCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:LengthCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      LengthCharacteristicMeasurement does not
              identify a LengthCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineProfileCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:LineProfileCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:LineProfileCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      LineProfileCharacteristicMeasurement does not
              identify a LineProfileCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLinearCoordinateCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:LinearCoordinateCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:LinearCoordinateCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      LinearCoordinateCharacteristicMeasurement does not
              identify a LinearCoordinateCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherFormCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:OtherFormCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:OtherFormCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of an
	      OtherFormCharacteristicMeasurement does not
              identify an OtherFormCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkParallelismCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ParallelismCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ParallelismCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ParallelismCharacteristicMeasurement does not
              identify a ParallelismCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPerpendicularityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:PerpendicularityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:PerpendicularityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      PerpendicularityCharacteristicMeasurement does not
              identify a PerpendicularityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointProfileCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:PointProfileCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:PointProfileCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      PointProfileCharacteristicMeasurement does not
              identify a PointProfileCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPositionCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:PositionCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:PositionCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      PositionCharacteristicMeasurement does not
              identify a PositionCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkRadiusCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:RadiusCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:RadiusCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      RadiusCharacteristicMeasurement does not
              identify a RadiusCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalDiameterCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SphericalDiameterCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SphericalDiameterCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SphericalDiameterCharacteristicMeasurement does not
              identify a SphericalDiameterCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalRadiusCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SphericalRadiusCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SphericalRadiusCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SphericalRadiusCharacteristicMeasurement does not
              identify a SphericalRadiusCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SphericityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SphericityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SphericityCharacteristicMeasurement does not
              identify a SphericityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSquareCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SquareCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SquareCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SquareCharacteristicMeasurement does not
              identify a SquareCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkStraightnessCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:StraightnessCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:StraightnessCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      StraightnessCharacteristicMeasurement does not
              identify a StraightnessCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceProfileCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SurfaceProfileCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SurfaceProfileCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SurfaceProfileCharacteristicMeasurement does not
              identify a SurfaceProfileCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template
    name="checkSurfaceProfileNonUniformCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SurfaceProfileNonUniformCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
             /t:SurfaceProfileNonUniformCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SurfaceProfileNonUniformCharacteristicMeasurement does not
              identify a SurfaceProfileNonUniformCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceTextureCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SurfaceTextureCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SurfaceTextureCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SurfaceTextureCharacteristicMeasurement does not
              identify a SurfaceTextureCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSymmetryCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:SymmetryCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:SymmetryCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      SymmetryCharacteristicMeasurement does not
              identify a SymmetryCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThicknessCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ThicknessCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ThicknessCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ThicknessCharacteristicMeasurement does not
              identify a ThicknessCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ThreadCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ThreadCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ThreadCharacteristicMeasurement does not
              identify a ThreadCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidicityCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:ToroidicityCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:ToroidicityCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      ToroidicityCharacteristicMeasurement does not
              identify a ToroidicityCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTotalRunoutCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:TotalRunoutCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:TotalRunoutCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      TotalRunoutCharacteristicMeasurement does not
              identify a TotalRunoutCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAngularCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedAngularCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedAngularCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedAngularCharacteristicMeasurement does not
              identify a UserDefinedAngularCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAreaCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedAreaCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedAreaCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedAreaCharacteristicMeasurement does not
              identify a UserDefinedAreaCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAttributeCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedAttributeCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedAttributeCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedAttributeCharacteristicMeasurement does not
              identify a UserDefinedAttributeCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedForceCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedForceCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedForceCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedForceCharacteristicMeasurement does not
              identify a UserDefinedForceCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedLinearCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedLinearCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedLinearCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedLinearCharacteristicMeasurement does not
              identify a UserDefinedLinearCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedMassCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedMassCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedMassCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedMassCharacteristicMeasurement does not
              identify a UserDefinedMassCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedPressureCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedPressureCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedPressureCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedPressureCharacteristicMeasurement does not
              identify a UserDefinedPressureCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedSpeedCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedSpeedCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedSpeedCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedSpeedCharacteristicMeasurement does not
              identify a UserDefinedSpeedCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTemperatureCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedTemperatureCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedTemperatureCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedTemperatureCharacteristicMeasurement does not
              identify a UserDefinedTemperatureCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTimeCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedTimeCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedTimeCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedTimeCharacteristicMeasurement does not
              identify a UserDefinedTimeCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedUnitCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:UserDefinedUnitCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:UserDefinedUnitCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      UserDefinedUnitCharacteristicMeasurement does not
              identify a UserDefinedUnitCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldBevelCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldBevelCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldBevelCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldBevelCharacteristicMeasurement does not
              identify a WeldBevelCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldCompoundCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldCompoundCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldCompoundCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldCompoundCharacteristicMeasurement does not
              identify a WeldCompoundCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldEdgeCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldEdgeCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldEdgeCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldEdgeCharacteristicMeasurement does not
              identify a WeldEdgeCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFilletCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldFilletCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldFilletCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldFilletCharacteristicMeasurement does not
              identify a WeldFilletCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareBevelCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldFlareBevelCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldFlareBevelCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldFlareBevelCharacteristicMeasurement does not
              identify a WeldFlareBevelCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareVCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldFlareVCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldFlareVCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldFlareVCharacteristicMeasurement does not
              identify a WeldFlareVCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldJCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldJCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldJCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldJCharacteristicMeasurement does not
              identify a WeldJCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldPlugCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldPlugCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldPlugCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldPlugCharacteristicMeasurement does not
              identify a WeldPlugCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldScarfCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldScarfCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldScarfCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldScarfCharacteristicMeasurement does not
              identify a WeldScarfCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSeamCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldSeamCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldSeamCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldSeamCharacteristicMeasurement does not
              identify a WeldSeamCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSlotCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldSlotCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldSlotCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldSlotCharacteristicMeasurement does not
              identify a WeldSlotCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSpotCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldSpotCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldSpotCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldSpotCharacteristicMeasurement does not
              identify a WeldSpotCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSquareCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldSquareCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldSquareCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldSquareCharacteristicMeasurement does not
              identify a WeldSquareCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldStudCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldStudCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldStudCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldStudCharacteristicMeasurement does not
              identify a WeldStudCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSurfacingCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldSurfacingCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldSurfacingCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldSurfacingCharacteristicMeasurement does not
              identify a WeldSurfacingCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldUCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldUCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldUCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldUCharacteristicMeasurement does not
              identify a WeldUCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldVCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WeldVCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WeldVCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WeldVCharacteristicMeasurement does not
              identify a WeldVCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWidthCharacteristicMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet/t:MeasurementResults
             /t:MeasuredCharacteristics/t:CharacteristicMeasurements
             /t:WidthCharacteristicMeasurement
             /t:CharacteristicItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicItems
	    /t:WidthCharacteristicItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicItemId (<xsl:value-of select="."/>) of a
	      WidthCharacteristicMeasurement does not
              identify a WidthCharacteristicItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END MEASUREMENT TO EXTERNAL ITEM ID CHECKS FOR CHARACTERISTICS -->

<!-- START ITEM TO EXTERNAL NOMINAL ID CHECKS FOR CHARACTERISTICS
For each XXXCharacteristicItem in the charItems (CharacteristicItems) in
which the CharacteristicNominalId is the id of the given extDoc (which is
the contents of a document external to the caller), this template checks
that the xId attribute of the CharacteristicNominalId is the id of exactly
one XXXCharacteristicNominal in the extDoc. There is one of these for each
type of characteristic. There are 73 XXX types.
-->

  <xsl:template name="checkAngleBetweenCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:AngleBetweenCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:AngleBetweenCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      AngleBetweenCharacteristicItem does not
              identify an AngleBetweenCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:AngleCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:AngleCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      AngleCharacteristicItem does not
              identify an AngleCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleFromCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:AngleFromCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:AngleFromCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      AngleFromCharacteristicItem does not
              identify an AngleFromCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularCoordinateCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:AngularCoordinateCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:AngularCoordinateCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      AngularCoordinateCharacteristicItem does not
              identify an AngularCoordinateCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:AngularityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:AngularityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      AngularityCharacteristicItem does not
              identify an AngularityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkChordCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ChordCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ChordCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ChordCharacteristicItem does not
              identify a ChordCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularRunoutCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:CircularRunoutCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:CircularRunoutCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      CircularRunoutCharacteristicItem does not
              identify a CircularRunoutCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:CircularityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:CircularityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      CircularityCharacteristicItem does not
              identify a CircularityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCoaxialityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:CoaxialityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:CoaxialityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      CoaxialityCharacteristicItem does not
              identify a CoaxialityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConcentricityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ConcentricityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ConcentricityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ConcentricityCharacteristicItem does not
              identify a ConcentricityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalTaperCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ConicalTaperCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ConicalTaperCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ConicalTaperCharacteristicItem does not
              identify a ConicalTaperCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ConicityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ConicityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ConicityCharacteristicItem does not
              identify a ConicityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCurveLengthCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:CurveLengthCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:CurveLengthCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      CurveLengthCharacteristicItem does not
              identify a CurveLengthCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:CylindricityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:CylindricityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      CylindricityCharacteristicItem does not
              identify a CylindricityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDepthCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:DepthCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:DepthCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      DepthCharacteristicItem does not
              identify a DepthCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDiameterCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:DiameterCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:DiameterCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      DiameterCharacteristicItem does not
              identify a DiameterCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceBetweenCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:DistanceBetweenCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:DistanceBetweenCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      DistanceBetweenCharacteristicItem does not
              identify a DistanceBetweenCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceFromCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:DistanceFromCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:DistanceFromCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      DistanceFromCharacteristicItem does not
              identify a DistanceFromCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:EllipticityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:EllipticityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      EllipticityCharacteristicItem does not
              identify an EllipticityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatTaperCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:FlatTaperCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:FlatTaperCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      FlatTaperCharacteristicItem does not
              identify a FlatTaperCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatnessCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:FlatnessCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:FlatnessCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      FlatnessCharacteristicItem does not
              identify a FlatnessCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkHeightCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:HeightCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:HeightCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      HeightCharacteristicItem does not
              identify a HeightCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLengthCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:LengthCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:LengthCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      LengthCharacteristicItem does not
              identify a LengthCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineProfileCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:LineProfileCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:LineProfileCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      LineProfileCharacteristicItem does not
              identify a LineProfileCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLinearCoordinateCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:LinearCoordinateCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:LinearCoordinateCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      LinearCoordinateCharacteristicItem does not
              identify a LinearCoordinateCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherFormCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:OtherFormCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:OtherFormCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of an
	      OtherFormCharacteristicItem does not
              identify an OtherFormCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkParallelismCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ParallelismCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ParallelismCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ParallelismCharacteristicItem does not
              identify a ParallelismCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPerpendicularityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:PerpendicularityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:PerpendicularityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      PerpendicularityCharacteristicItem does not
              identify a PerpendicularityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointProfileCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:PointProfileCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:PointProfileCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      PointProfileCharacteristicItem does not
              identify a PointProfileCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPositionCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:PositionCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:PositionCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      PositionCharacteristicItem does not
              identify a PositionCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkRadiusCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:RadiusCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:RadiusCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      RadiusCharacteristicItem does not
              identify a RadiusCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalDiameterCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SphericalDiameterCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SphericalDiameterCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SphericalDiameterCharacteristicItem does not
              identify a SphericalDiameterCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalRadiusCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SphericalRadiusCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SphericalRadiusCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SphericalRadiusCharacteristicItem does not
              identify a SphericalRadiusCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SphericityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SphericityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SphericityCharacteristicItem does not
              identify a SphericityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSquareCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SquareCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SquareCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SquareCharacteristicItem does not
              identify a SquareCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkStraightnessCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:StraightnessCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:StraightnessCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      StraightnessCharacteristicItem does not
              identify a StraightnessCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceProfileCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SurfaceProfileCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SurfaceProfileCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SurfaceProfileCharacteristicItem does not
              identify a SurfaceProfileCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template
    name="checkSurfaceProfileNonUniformCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SurfaceProfileNonUniformCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
             /t:SurfaceProfileNonUniformCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SurfaceProfileNonUniformCharacteristicItem does not
              identify a SurfaceProfileNonUniformCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceTextureCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SurfaceTextureCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SurfaceTextureCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SurfaceTextureCharacteristicItem does not
              identify a SurfaceTextureCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSymmetryCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:SymmetryCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:SymmetryCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      SymmetryCharacteristicItem does not
              identify a SymmetryCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThicknessCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ThicknessCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ThicknessCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ThicknessCharacteristicItem does not
              identify a ThicknessCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ThreadCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ThreadCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ThreadCharacteristicItem does not
              identify a ThreadCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidicityCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:ToroidicityCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:ToroidicityCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      ToroidicityCharacteristicItem does not
              identify a ToroidicityCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTotalRunoutCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:TotalRunoutCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:TotalRunoutCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      TotalRunoutCharacteristicItem does not
              identify a TotalRunoutCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAngularCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedAngularCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedAngularCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedAngularCharacteristicItem does not
              identify a UserDefinedAngularCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAreaCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedAreaCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedAreaCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedAreaCharacteristicItem does not
              identify a UserDefinedAreaCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAttributeCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedAttributeCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedAttributeCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedAttributeCharacteristicItem does not
              identify a UserDefinedAttributeCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedForceCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedForceCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedForceCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedForceCharacteristicItem does not
              identify a UserDefinedForceCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedLinearCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedLinearCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedLinearCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedLinearCharacteristicItem does not
              identify a UserDefinedLinearCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedMassCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedMassCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedMassCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedMassCharacteristicItem does not
              identify a UserDefinedMassCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedPressureCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedPressureCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedPressureCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedPressureCharacteristicItem does not
              identify a UserDefinedPressureCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedSpeedCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedSpeedCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedSpeedCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedSpeedCharacteristicItem does not
              identify a UserDefinedSpeedCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTemperatureCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedTemperatureCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedTemperatureCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedTemperatureCharacteristicItem does not
              identify a UserDefinedTemperatureCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTimeCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedTimeCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedTimeCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedTimeCharacteristicItem does not
              identify a UserDefinedTimeCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedUnitCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:UserDefinedUnitCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:UserDefinedUnitCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      UserDefinedUnitCharacteristicItem does not
              identify a UserDefinedUnitCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldBevelCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldBevelCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldBevelCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldBevelCharacteristicItem does not
              identify a WeldBevelCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldCompoundCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldCompoundCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldCompoundCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldCompoundCharacteristicItem does not
              identify a WeldCompoundCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldEdgeCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldEdgeCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldEdgeCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldEdgeCharacteristicItem does not
              identify a WeldEdgeCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFilletCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldFilletCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldFilletCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldFilletCharacteristicItem does not
              identify a WeldFilletCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareBevelCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldFlareBevelCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldFlareBevelCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldFlareBevelCharacteristicItem does not
              identify a WeldFlareBevelCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareVCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldFlareVCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldFlareVCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldFlareVCharacteristicItem does not
              identify a WeldFlareVCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldJCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldJCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldJCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldJCharacteristicItem does not
              identify a WeldJCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldPlugCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldPlugCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldPlugCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldPlugCharacteristicItem does not
              identify a WeldPlugCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldScarfCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldScarfCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldScarfCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldScarfCharacteristicItem does not
              identify a WeldScarfCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSeamCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldSeamCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldSeamCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldSeamCharacteristicItem does not
              identify a WeldSeamCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSlotCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldSlotCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldSlotCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldSlotCharacteristicItem does not
              identify a WeldSlotCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSpotCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldSpotCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldSpotCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldSpotCharacteristicItem does not
              identify a WeldSpotCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSquareCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldSquareCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldSquareCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldSquareCharacteristicItem does not
              identify a WeldSquareCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldStudCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldStudCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldStudCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldStudCharacteristicItem does not
              identify a WeldStudCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSurfacingCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldSurfacingCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldSurfacingCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldSurfacingCharacteristicItem does not
              identify a WeldSurfacingCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldUCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldUCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldUCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldUCharacteristicItem does not
              identify a WeldUCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldVCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WeldVCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WeldVCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WeldVCharacteristicItem does not
              identify a WeldVCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWidthCharacteristicItemNominalId">
    <xsl:param name="charItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charItems/t:WidthCharacteristicItem
             /t:CharacteristicNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicNominals
	    /t:WidthCharacteristicNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicNominalId (<xsl:value-of select="."/>) of a
	      WidthCharacteristicItem does not
              identify a WidthCharacteristicNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END ITEM TO EXTERNAL NOMINAL ID CHECKS FOR CHARACTERISTICS -->

<!-- START NOMINAL TO EXTERNAL DEFINITION ID CHECKS FOR CHARACTERISTICS
For each XXXCharacteristicNominal in the charNominals
(CharacteristicNominals) in which the CharacteristicDefinitionId is the id
of the given extDoc (which is the contents of a document external to the
caller), this template checks that the xId attribute of the
CharacteristicDefinitionId is the id of exactly one
XXXCharacteristicDefinition in the extDoc. There is one of these for each
type of characteristic. There are 73 XXX types.
-->

  <xsl:template name="checkAngleBetweenCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:AngleBetweenCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
		 /t:AngleBetweenCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      AngleBetweenCharacteristicNominal does not
              identify an AngleBetweenCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:AngleCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:AngleCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      AngleCharacteristicNominal does not
              identify an AngleCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngleFromCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:AngleFromCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:AngleFromCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      AngleFromCharacteristicNominal does not
              identify an AngleFromCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularCoordinateCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:AngularCoordinateCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:AngularCoordinateCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      AngularCoordinateCharacteristicNominal does not
              identify an AngularCoordinateCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkAngularityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:AngularityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:AngularityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      AngularityCharacteristicNominal does not
              identify an AngularityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkChordCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ChordCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ChordCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ChordCharacteristicNominal does not
              identify a ChordCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularRunoutCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:CircularRunoutCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:CircularRunoutCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      CircularRunoutCharacteristicNominal does not
              identify a CircularRunoutCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:CircularityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:CircularityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      CircularityCharacteristicNominal does not
              identify a CircularityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCoaxialityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:CoaxialityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:CoaxialityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      CoaxialityCharacteristicNominal does not
              identify a CoaxialityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConcentricityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ConcentricityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ConcentricityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ConcentricityCharacteristicNominal does not
              identify a ConcentricityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalTaperCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ConicalTaperCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ConicalTaperCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ConicalTaperCharacteristicNominal does not
              identify a ConicalTaperCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ConicityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ConicityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ConicityCharacteristicNominal does not
              identify a ConicityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCurveLengthCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:CurveLengthCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:CurveLengthCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      CurveLengthCharacteristicNominal does not
              identify a CurveLengthCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:CylindricityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:CylindricityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      CylindricityCharacteristicNominal does not
              identify a CylindricityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDepthCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:DepthCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:DepthCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      DepthCharacteristicNominal does not
              identify a DepthCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDiameterCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:DiameterCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:DiameterCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      DiameterCharacteristicNominal does not
              identify a DiameterCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceBetweenCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:DistanceBetweenCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:DistanceBetweenCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      DistanceBetweenCharacteristicNominal does not
              identify a DistanceBetweenCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkDistanceFromCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:DistanceFromCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:DistanceFromCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      DistanceFromCharacteristicNominal does not
              identify a DistanceFromCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:EllipticityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:EllipticityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      EllipticityCharacteristicNominal does not
              identify an EllipticityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatTaperCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:FlatTaperCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:FlatTaperCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      FlatTaperCharacteristicNominal does not
              identify a FlatTaperCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkFlatnessCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:FlatnessCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:FlatnessCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      FlatnessCharacteristicNominal does not
              identify a FlatnessCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkHeightCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:HeightCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:HeightCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      HeightCharacteristicNominal does not
              identify a HeightCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLengthCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:LengthCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:LengthCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      LengthCharacteristicNominal does not
              identify a LengthCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineProfileCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:LineProfileCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:LineProfileCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      LineProfileCharacteristicNominal does not
              identify a LineProfileCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLinearCoordinateCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:LinearCoordinateCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:LinearCoordinateCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      LinearCoordinateCharacteristicNominal does not
              identify a LinearCoordinateCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherFormCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:OtherFormCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:OtherFormCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of an
	      OtherFormCharacteristicNominal does not
              identify an OtherFormCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkParallelismCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ParallelismCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ParallelismCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ParallelismCharacteristicNominal does not
              identify a ParallelismCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPerpendicularityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:PerpendicularityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:PerpendicularityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      PerpendicularityCharacteristicNominal does not
              identify a PerpendicularityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointProfileCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:PointProfileCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:PointProfileCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      PointProfileCharacteristicNominal does not
              identify a PointProfileCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPositionCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:PositionCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:PositionCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      PositionCharacteristicNominal does not
              identify a PositionCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkRadiusCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:RadiusCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:RadiusCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      RadiusCharacteristicNominal does not
              identify a RadiusCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalDiameterCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SphericalDiameterCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SphericalDiameterCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SphericalDiameterCharacteristicNominal does not
              identify a SphericalDiameterCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalRadiusCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SphericalRadiusCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SphericalRadiusCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SphericalRadiusCharacteristicNominal does not
              identify a SphericalRadiusCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SphericityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SphericityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SphericityCharacteristicNominal does not
              identify a SphericityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSquareCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SquareCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SquareCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SquareCharacteristicNominal does not
              identify a SquareCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkStraightnessCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:StraightnessCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:StraightnessCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      StraightnessCharacteristicNominal does not
              identify a StraightnessCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceProfileCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SurfaceProfileCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SurfaceProfileCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SurfaceProfileCharacteristicNominal does not
              identify a SurfaceProfileCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template
    name="checkSurfaceProfileNonUniformCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SurfaceProfileNonUniformCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
             /t:SurfaceProfileNonUniformCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SurfaceProfileNonUniformCharacteristicNominal does not
              identify a SurfaceProfileNonUniformCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceTextureCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SurfaceTextureCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SurfaceTextureCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SurfaceTextureCharacteristicNominal does not
              identify a SurfaceTextureCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSymmetryCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:SymmetryCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:SymmetryCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      SymmetryCharacteristicNominal does not
              identify a SymmetryCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThicknessCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ThicknessCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ThicknessCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ThicknessCharacteristicNominal does not
              identify a ThicknessCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ThreadCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ThreadCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ThreadCharacteristicNominal does not
              identify a ThreadCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidicityCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:ToroidicityCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:ToroidicityCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      ToroidicityCharacteristicNominal does not
              identify a ToroidicityCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTotalRunoutCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:TotalRunoutCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:TotalRunoutCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      TotalRunoutCharacteristicNominal does not
              identify a TotalRunoutCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAngularCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedAngularCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedAngularCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedAngularCharacteristicNominal does not
              identify a UserDefinedAngularCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAreaCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedAreaCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedAreaCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedAreaCharacteristicNominal does not
              identify a UserDefinedAreaCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedAttributeCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedAttributeCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedAttributeCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedAttributeCharacteristicNominal does not
              identify a UserDefinedAttributeCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedForceCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedForceCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedForceCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedForceCharacteristicNominal does not
              identify a UserDefinedForceCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedLinearCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedLinearCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedLinearCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedLinearCharacteristicNominal does not
              identify a UserDefinedLinearCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedMassCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedMassCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedMassCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedMassCharacteristicNominal does not
              identify a UserDefinedMassCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedPressureCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedPressureCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedPressureCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedPressureCharacteristicNominal does not
              identify a UserDefinedPressureCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedSpeedCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedSpeedCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedSpeedCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedSpeedCharacteristicNominal does not
              identify a UserDefinedSpeedCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTemperatureCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedTemperatureCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedTemperatureCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedTemperatureCharacteristicNominal does not
              identify a UserDefinedTemperatureCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedTimeCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedTimeCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedTimeCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedTimeCharacteristicNominal does not
              identify a UserDefinedTimeCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkUserDefinedUnitCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:UserDefinedUnitCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:UserDefinedUnitCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      UserDefinedUnitCharacteristicNominal does not
              identify a UserDefinedUnitCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldBevelCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldBevelCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldBevelCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldBevelCharacteristicNominal does not
              identify a WeldBevelCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldCompoundCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldCompoundCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldCompoundCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldCompoundCharacteristicNominal does not
              identify a WeldCompoundCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldEdgeCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldEdgeCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldEdgeCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldEdgeCharacteristicNominal does not
              identify a WeldEdgeCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFilletCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldFilletCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldFilletCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldFilletCharacteristicNominal does not
              identify a WeldFilletCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareBevelCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldFlareBevelCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldFlareBevelCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldFlareBevelCharacteristicNominal does not
              identify a WeldFlareBevelCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldFlareVCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldFlareVCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldFlareVCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldFlareVCharacteristicNominal does not
              identify a WeldFlareVCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldJCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldJCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldJCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldJCharacteristicNominal does not
              identify a WeldJCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldPlugCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldPlugCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldPlugCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldPlugCharacteristicNominal does not
              identify a WeldPlugCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldScarfCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldScarfCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldScarfCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldScarfCharacteristicNominal does not
              identify a WeldScarfCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSeamCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldSeamCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldSeamCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldSeamCharacteristicNominal does not
              identify a WeldSeamCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSlotCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldSlotCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldSlotCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldSlotCharacteristicNominal does not
              identify a WeldSlotCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSpotCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldSpotCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldSpotCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldSpotCharacteristicNominal does not
              identify a WeldSpotCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSquareCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldSquareCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldSquareCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldSquareCharacteristicNominal does not
              identify a WeldSquareCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldStudCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldStudCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldStudCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldStudCharacteristicNominal does not
              identify a WeldStudCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldSurfacingCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldSurfacingCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldSurfacingCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldSurfacingCharacteristicNominal does not
              identify a WeldSurfacingCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldUCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldUCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldUCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldUCharacteristicNominal does not
              identify a WeldUCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWeldVCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WeldVCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WeldVCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WeldVCharacteristicNominal does not
              identify a WeldVCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkWidthCharacteristicNominalDefinitionId">
    <xsl:param name="charNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$charNominals/t:WidthCharacteristicNominal
             /t:CharacteristicDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Characteristics/t:CharacteristicDefinitions
	    /t:WidthCharacteristicDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      CharacteristicDefinitionId (<xsl:value-of select="."/>) of a
	      WidthCharacteristicNominal does not
              identify a WidthCharacteristicDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END NOMINAL TO EXTERNAL DEFINITION ID CHECKS FOR CHARACTERISTICS -->

<!-- START MEASUREMENT TO EXTERNAL ITEM ID CHECKS FOR FEATURES
For each XXXFeatureMeasurement in the measi (MeasurementsResults) in which
the FeatureItemId is the id of the given extDoc (which is the contents of a
document external to the caller), this template checks that the xId
attribute of the FeatureItemId is the id of exactly one XXXFeatureItem in
the extDoc. There is one of these for each type of feature. There are 33
XXX types (no pattern feature measurements).
-->


  <xsl:template name="checkCircleFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:CircleFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:CircleFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      CircleFeatureMeasurementMeasurement does not
              identify a CircleFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularArcFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:CircularArcFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:CircularArcFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      CircularArcFeatureMeasurementMeasurement does not
              identify a CircularArcFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConeFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ConeFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ConeFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ConeFeatureMeasurementMeasurement does not
              identify a ConeFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalSegmentFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ConicalSegmentFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ConicalSegmentFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ConicalSegmentFeatureMeasurementMeasurement does not
              identify a ConicalSegmentFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylinderFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:CylinderFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:CylinderFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      CylinderFeatureMeasurementMeasurement does not
              identify a CylinderFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricalSegmentFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:CylindricalSegmentFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:CylindricalSegmentFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      CylindricalSegmentFeatureMeasurementMeasurement does not
              identify a CylindricalSegmentFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEdgePointFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:EdgePointFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:EdgePointFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      EdgePointFeatureMeasurementMeasurement does not
              identify a EdgePointFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipseFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:EllipseFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:EllipseFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      EllipseFeatureMeasurementMeasurement does not
              identify a EllipseFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticalArcFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:EllipticalArcFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:EllipticalArcFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      EllipticalArcFeatureMeasurementMeasurement does not
              identify a EllipticalArcFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCircleFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ElongatedCircleFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ElongatedCircleFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ElongatedCircleFeatureMeasurementMeasurement does not
              identify a ElongatedCircleFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCylinderFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ElongatedCylinderFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ElongatedCylinderFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ElongatedCylinderFeatureMeasurementMeasurement does not
              identify a ElongatedCylinderFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkExtrudedCrossSectionFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ExtrudedCrossSectionFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ExtrudedCrossSectionFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ExtrudedCrossSectionFeatureMeasurementMeasurement does not
              identify a ExtrudedCrossSectionFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkGroupFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:GroupFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:GroupFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      GroupFeatureMeasurementMeasurement does not
              identify a GroupFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:LineFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:LineFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      LineFeatureMeasurementMeasurement does not
              identify a LineFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkMarkingFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:MarkingFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:MarkingFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      MarkingFeatureMeasurementMeasurement does not
              identify a MarkingFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledLinesFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OppositeAngledLinesFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OppositeAngledLinesFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OppositeAngledLinesFeatureMeasurementMeasurement does not
              identify a OppositeAngledLinesFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledPlanesFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OppositeAngledPlanesFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OppositeAngledPlanesFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OppositeAngledPlanesFeatureMeasurementMeasurement does not
              identify a OppositeAngledPlanesFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelLinesFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OppositeParallelLinesFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OppositeParallelLinesFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OppositeParallelLinesFeatureMeasurementMeasurement does not
              identify a OppositeParallelLinesFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelPlanesFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OppositeParallelPlanesFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OppositeParallelPlanesFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OppositeParallelPlanesFeatureMeasurementMeasurement does not
              identify a OppositeParallelPlanesFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherCurveFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OtherCurveFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OtherCurveFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OtherCurveFeatureMeasurementMeasurement does not
              identify a OtherCurveFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherNonShapeFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OtherNonShapeFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OtherNonShapeFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OtherNonShapeFeatureMeasurementMeasurement does not
              identify a OtherNonShapeFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

   <xsl:template name="checkOtherShapeFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OtherShapeFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OtherShapeFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OtherShapeFeatureMeasurementMeasurement does not
              identify a OtherShapeFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherSurfaceFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:OtherSurfaceFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:OtherSurfaceFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      OtherSurfaceFeatureMeasurementMeasurement does not
              identify a OtherSurfaceFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

 <xsl:template name="checkPlaneFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:PlaneFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:PlaneFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      PlaneFeatureMeasurementMeasurement does not
              identify a PlaneFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedCurveFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:PointDefinedCurveFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:PointDefinedCurveFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      PointDefinedCurveFeatureMeasurementMeasurement does not
              identify a PointDefinedCurveFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedSurfaceFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:PointDefinedSurfaceFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:PointDefinedSurfaceFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      PointDefinedSurfaceFeatureMeasurementMeasurement does not
              identify a PointDefinedSurfaceFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:PointFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:PointFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      PointFeatureMeasurementMeasurement does not
              identify a PointFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphereFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:SphereFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:SphereFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      SphereFeatureMeasurementMeasurement does not
              identify a SphereFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalSegmentFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:SphericalSegmentFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:SphericalSegmentFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      SphericalSegmentFeatureMeasurementMeasurement does not
              identify a SphericalSegmentFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceOfRevolutionFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:SurfaceOfRevolutionFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:SurfaceOfRevolutionFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      SurfaceOfRevolutionFeatureMeasurementMeasurement does not
              identify a SurfaceOfRevolutionFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadedFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ThreadedFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ThreadedFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ThreadedFeatureMeasurementMeasurement does not
              identify a ThreadedFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidalSegmentFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:ToroidalSegmentFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:ToroidalSegmentFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      ToroidalSegmentFeatureMeasurementMeasurement does not
              identify a ToroidalSegmentFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTorusFeatureMeasurementItemId">
    <xsl:param name="measi"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$measi/t:MeasurementResultsSet
             /t:MeasurementResults/t:MeasuredFeatures
             /t:TorusFeatureMeasurement/t:FeatureItemId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureItems
	    /t:TorusFeatureItem[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureItemId (<xsl:value-of select="."/>) of a
	      TorusFeatureMeasurementMeasurement does not
              identify a TorusFeatureItem in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END MEASUREMENT TO EXTERNAL ITEM ID CHECKS FOR FEATURES -->

<!-- START ITEM TO EXTERNAL NOMINAL ID CHECKS FOR FEATURES
For each XXXFeatureItem in the featItems (FeatureItems) in which the
FeatureNominalId is the id of the given extDoc (which is the contents of a
document external to the caller), this template checks that the xId
attribute of the FeatureNominalId is the id of exactly one
XXXFeatureNominal in the extDoc. There is one of these for each type of
feature. There are 37 XXX types (patterns included).
-->

  <xsl:template name="checkCircleFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:CircleFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:CircleFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      CircleFeatureItem does not
              identify a CircleFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularArcFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:CircularArcFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:CircularArcFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      CircularArcFeatureItem does not
              identify a CircularArcFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConeFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ConeFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ConeFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ConeFeatureItem does not
              identify a ConeFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalSegmentFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ConicalSegmentFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ConicalSegmentFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ConicalSegmentFeatureItem does not
              identify a ConicalSegmentFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylinderFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:CylinderFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:CylinderFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      CylinderFeatureItem does not
              identify a CylinderFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricalSegmentFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:CylindricalSegmentFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:CylindricalSegmentFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      CylindricalSegmentFeatureItem does not
              identify a CylindricalSegmentFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEdgePointFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:EdgePointFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:EdgePointFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      EdgePointFeatureItem does not
              identify a EdgePointFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipseFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:EllipseFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:EllipseFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      EllipseFeatureItem does not
              identify a EllipseFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticalArcFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:EllipticalArcFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:EllipticalArcFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      EllipticalArcFeatureItem does not
              identify a EllipticalArcFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCircleFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ElongatedCircleFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ElongatedCircleFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ElongatedCircleFeatureItem does not
              identify a ElongatedCircleFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCylinderFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ElongatedCylinderFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ElongatedCylinderFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ElongatedCylinderFeatureItem does not
              identify a ElongatedCylinderFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkExtrudedCrossSectionFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ExtrudedCrossSectionFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ExtrudedCrossSectionFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ExtrudedCrossSectionFeatureItem does not
              identify a ExtrudedCrossSectionFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkGroupFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:GroupFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:GroupFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      GroupFeatureItem does not
              identify a GroupFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:LineFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:LineFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      LineFeatureItem does not
              identify a LineFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkMarkingFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:MarkingFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:MarkingFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      MarkingFeatureItem does not
              identify a MarkingFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledLinesFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OppositeAngledLinesFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OppositeAngledLinesFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OppositeAngledLinesFeatureItem does not
              identify a OppositeAngledLinesFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledPlanesFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OppositeAngledPlanesFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OppositeAngledPlanesFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OppositeAngledPlanesFeatureItem does not
              identify a OppositeAngledPlanesFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelLinesFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OppositeParallelLinesFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OppositeParallelLinesFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OppositeParallelLinesFeatureItem does not
              identify a OppositeParallelLinesFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelPlanesFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OppositeParallelPlanesFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OppositeParallelPlanesFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OppositeParallelPlanesFeatureItem does not
              identify a OppositeParallelPlanesFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherCurveFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OtherCurveFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OtherCurveFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OtherCurveFeatureItem does not
              identify a OtherCurveFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherNonShapeFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OtherNonShapeFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OtherNonShapeFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OtherNonShapeFeatureItem does not
              identify a OtherNonShapeFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherShapeFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OtherShapeFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OtherShapeFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OtherShapeFeatureItem does not
              identify a OtherShapeFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherSurfaceFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:OtherSurfaceFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:OtherSurfaceFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      OtherSurfaceFeatureItem does not
              identify a OtherSurfaceFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureCircleItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PatternFeatureCircleItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PatternFeatureCircleNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PatternFeatureCircleItem does not
              identify a PatternFeatureCircleNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureCircularArcItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PatternFeatureCircularArcItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PatternFeatureCircularArcNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PatternFeatureCircularArcItem does not
              identify a PatternFeatureCircularArcNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureLinearItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PatternFeatureLinearItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PatternFeatureLinearNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PatternFeatureLinearItem does not
              identify a PatternFeatureLinearNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureParallelogramItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PatternFeatureParallelogramItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PatternFeatureParallelogramNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PatternFeatureParallelogramItem does not
              identify a PatternFeatureParallelogramNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPlaneFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PlaneFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PlaneFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PlaneFeatureItem does not
              identify a PlaneFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedCurveFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PointDefinedCurveFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PointDefinedCurveFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PointDefinedCurveFeatureItem does not
              identify a PointDefinedCurveFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedSurfaceFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PointDefinedSurfaceFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PointDefinedSurfaceFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PointDefinedSurfaceFeatureItem does not
              identify a PointDefinedSurfaceFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:PointFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:PointFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      PointFeatureItem does not
              identify a PointFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphereFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:SphereFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:SphereFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      SphereFeatureItem does not
              identify a SphereFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalSegmentFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:SphericalSegmentFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:SphericalSegmentFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      SphericalSegmentFeatureItem does not
              identify a SphericalSegmentFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceOfRevolutionFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:SurfaceOfRevolutionFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:SurfaceOfRevolutionFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      SurfaceOfRevolutionFeatureItem does not
              identify a SurfaceOfRevolutionFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadedFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ThreadedFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ThreadedFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ThreadedFeatureItem does not
              identify a ThreadedFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidalSegmentFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:ToroidalSegmentFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:ToroidalSegmentFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      ToroidalSegmentFeatureItem does not
              identify a ToroidalSegmentFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTorusFeatureItemNominalId">
    <xsl:param name="featItems"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featItems/t:TorusFeatureItem
             /t:FeatureNominalId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureNominals
	       /t:TorusFeatureNominal[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureNominalId (<xsl:value-of select="."/>) of a
	      TorusFeatureItem does not
              identify a TorusFeatureNominal in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END ITEM TO EXTERNAL NOMINAL ID CHECKS FOR FEATURES -->

<!-- START NOMINAL TO EXTERNAL DEFINITION ID CHECKS FOR FEATURES
For each XXXFeatureNominal in the featNominals (FeatureNominals) in which
the FeatureDefinitionId is the id of the given extDoc (which is the
contents of a document external to the caller), this template checks that
the xId attribute of the FeatureDefinitionId is the id of exactly one
XXXFeatureDefinition in the extDoc. There is one of these for each type of
feature. There are 37 XXX types (patterns included).
-->

  <xsl:template name="checkCircleFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:CircleFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:CircleFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      CircleFeatureNominal does not
              identify a CircleFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCircularArcFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:CircularArcFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:CircularArcFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      CircularArcFeatureNominal does not
              identify a CircularArcFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConeFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ConeFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ConeFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ConeFeatureNominal does not
              identify a ConeFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkConicalSegmentFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ConicalSegmentFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ConicalSegmentFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ConicalSegmentFeatureNominal does not
              identify a ConicalSegmentFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylinderFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:CylinderFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:CylinderFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      CylinderFeatureNominal does not
              identify a CylinderFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkCylindricalSegmentFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:CylindricalSegmentFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:CylindricalSegmentFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      CylindricalSegmentFeatureNominal does not
              identify a CylindricalSegmentFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEdgePointFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:EdgePointFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:EdgePointFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      EdgePointFeatureNominal does not
              identify a EdgePointFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipseFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:EllipseFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:EllipseFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      EllipseFeatureNominal does not
              identify a EllipseFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkEllipticalArcFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:EllipticalArcFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:EllipticalArcFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      EllipticalArcFeatureNominal does not
              identify a EllipticalArcFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCircleFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ElongatedCircleFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ElongatedCircleFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ElongatedCircleFeatureNominal does not
              identify a ElongatedCircleFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkElongatedCylinderFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ElongatedCylinderFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ElongatedCylinderFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ElongatedCylinderFeatureNominal does not
              identify a ElongatedCylinderFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkExtrudedCrossSectionFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ExtrudedCrossSectionFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ExtrudedCrossSectionFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ExtrudedCrossSectionFeatureNominal does not
              identify a ExtrudedCrossSectionFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkGroupFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:GroupFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:GroupFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      GroupFeatureNominal does not
              identify a GroupFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkLineFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:LineFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:LineFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      LineFeatureNominal does not
              identify a LineFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkMarkingFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:MarkingFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:MarkingFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      MarkingFeatureNominal does not
              identify a MarkingFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledLinesFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OppositeAngledLinesFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OppositeAngledLinesFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OppositeAngledLinesFeatureNominal does not
              identify a OppositeAngledLinesFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeAngledPlanesFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OppositeAngledPlanesFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OppositeAngledPlanesFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OppositeAngledPlanesFeatureNominal does not
              identify a OppositeAngledPlanesFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelLinesFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OppositeParallelLinesFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OppositeParallelLinesFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OppositeParallelLinesFeatureNominal does not
              identify a OppositeParallelLinesFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOppositeParallelPlanesFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OppositeParallelPlanesFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OppositeParallelPlanesFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OppositeParallelPlanesFeatureNominal does not
              identify a OppositeParallelPlanesFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherCurveFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OtherCurveFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OtherCurveFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OtherCurveFeatureNominal does not
              identify a OtherCurveFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherNonShapeFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OtherNonShapeFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OtherNonShapeFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OtherNonShapeFeatureNominal does not
              identify a OtherNonShapeFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherShapeFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OtherShapeFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OtherShapeFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OtherShapeFeatureNominal does not
              identify a OtherShapeFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkOtherSurfaceFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:OtherSurfaceFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:OtherSurfaceFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      OtherSurfaceFeatureNominal does not
              identify a OtherSurfaceFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureCircleNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PatternFeatureCircleNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PatternFeatureCircleDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PatternFeatureCircleNominal does not
              identify a PatternFeatureCircleDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureCircularArcNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PatternFeatureCircularArcNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PatternFeatureCircularArcDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PatternFeatureCircularArcNominal does not
              identify a PatternFeatureCircularArcDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureLinearNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PatternFeatureLinearNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PatternFeatureLinearDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PatternFeatureLinearNominal does not
              identify a PatternFeatureLinearDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPatternFeatureParallelogramNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PatternFeatureParallelogramNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PatternFeatureParallelogramDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PatternFeatureParallelogramNominal does not
              identify a PatternFeatureParallelogramDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPlaneFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PlaneFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PlaneFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PlaneFeatureNominal does not
              identify a PlaneFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedCurveFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PointDefinedCurveFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PointDefinedCurveFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PointDefinedCurveFeatureNominal does not
              identify a PointDefinedCurveFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointDefinedSurfaceFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PointDefinedSurfaceFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PointDefinedSurfaceFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PointDefinedSurfaceFeatureNominal does not
              identify a PointDefinedSurfaceFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkPointFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:PointFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:PointFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      PointFeatureNominal does not
              identify a PointFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphereFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:SphereFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:SphereFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      SphereFeatureNominal does not
              identify a SphereFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSphericalSegmentFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:SphericalSegmentFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:SphericalSegmentFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      SphericalSegmentFeatureNominal does not
              identify a SphericalSegmentFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkSurfaceOfRevolutionFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:SurfaceOfRevolutionFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:SurfaceOfRevolutionFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      SurfaceOfRevolutionFeatureNominal does not
              identify a SurfaceOfRevolutionFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkThreadedFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ThreadedFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ThreadedFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ThreadedFeatureNominal does not
              identify a ThreadedFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkToroidalSegmentFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:ToroidalSegmentFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:ToroidalSegmentFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      ToroidalSegmentFeatureNominal does not
              identify a ToroidalSegmentFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="checkTorusFeatureNominalDefinitionId">
    <xsl:param name="featNominals"/>
    <xsl:param name="extDoc"/>
    <xsl:param name="extDocId"/>
    <xsl:for-each
      select="$featNominals/t:TorusFeatureNominal
             /t:FeatureDefinitionId">
      <xsl:if test=".= $extDocId">
	<xsl:variable name="xId" select="@xId"/>
	<xsl:if
	  test="count($extDoc/t:Features/t:FeatureDefinitions
	       /t:TorusFeatureDefinition[@id = $xId]) != 1">
	  <xsl:call-template name="error_node">
            <xsl:with-param name="report">
	      The xId (<xsl:value-of select="$xId"/>) of a
	      FeatureDefinitionId (<xsl:value-of select="."/>) of a
	      TorusFeatureNominal does not
              identify a TorusFeatureDefinition in the
	      referenced external document.
            </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- END NOMINAL TO EXTERNAL DEFINITION ID CHECKS FOR FEATURES -->

</xsl:stylesheet>
