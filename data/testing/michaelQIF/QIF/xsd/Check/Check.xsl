<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">

  <xsl:import href="CheckFormat.xsl"/>
  <xsl:import href="CheckQuality.xsl"/>
  <xsl:import href="CheckSemantic.xsl"/>
  <xsl:import href="CheckDocuments.xsl"/>

  <xsl:output method="xml" indent="yes" />

  <!-- skip text content -->
  <xsl:template match="text()"/>

  <!-- apply templates -->
  <xsl:template match="/t:QIFDocument">
    <xsl:element name="CheckReport">
      <xsl:call-template name="processing_document">
        <xsl:with-param name="level" select="0"/>
        <xsl:with-param name="caller" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
