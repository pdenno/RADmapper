<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">

  <xsl:import href="CheckLibrary.xsl"/>

  <xsl:output method="xml" indent="yes" />

  <!-- skip text content -->
  <xsl:template match="text()" mode="Semantic" />

  <!-- FeatureNominal, the check: must be connected to a topology entity -->
  <!-- This is currently not enabled in the CheckParameters and should  -->
  <!-- not be enabled because entities numbered by the nTopo variable  -->
  <!-- are not necessarily topology entities. Also, nTopo will always   -->
  <!-- be zero in 'select="$nTopo"', so it is pointless to evaluate it. -->
  <xsl:variable
      name="FeatureNominalTopologyLink"
      select="$CheckParameters/CheckSemanticParameters
	      /Check[@name='FeatureNominalTopologyLink']/@active = 'true'"/>
  <xsl:template match="/t:QIFDocument/t:Features/t:FeatureNominals/t:*"
		mode="Semantic">
    <xsl:if test="$FeatureNominalTopologyLink">
      <xsl:variable
	  name="nTopo"
	  select="count(t:EntityInternalIds/t:Id | t:EntityExternalIds/t:Id)"/>
      <xsl:if test="not($nTopo &gt; 0)">
        <xsl:for-each select=".">
          <xsl:call-template name="error_node">
            <xsl:with-param name="report">
              FeatureNominal not connected with topology:
	      id=<xsl:value-of select="@id"/>, 
	      nTopo(<xsl:value-of select="$nTopo"/>)
            </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:if>
    </xsl:if>
    <xsl:apply-templates mode="Semantic"/>
  </xsl:template>

  <!-- PositionCharacteristicDefinition, the check:                   -->
  <!-- if ToleranceValue = 0, then MaterialCondition must be MAXIMUM  -->
  <xsl:template
      match="/t:QIFDocument/t:Characteristics/t:CharacteristicDefinitions
	     /t:PositionCharacteristicDefinition" mode="Semantic">
    <xsl:if test="not((t:ToleranceValue != 0) or 
		      (t:MaterialCondition = 'MAXIMUM'))">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            PositionCharacteristicDefinition: 
	    id=<xsl:value-of select="@id"/>, ToleranceValue=0 and 
	    MaterialCondition(<xsl:value-of select="t:MaterialCondition"/>)
	    != 'MAXIMUM'
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Semantic"/>
  </xsl:template>
  
</xsl:stylesheet>
