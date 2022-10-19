<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">
  
  <xsl:output method="xml" indent="yes" />

  <xsl:variable name="CheckParameters" select="document('CheckParameters.xml')/CheckParameters" />
  
  <!-- <Node>/root/.../node</Node> -->
  <xsl:template name="full_path_node">
    <xsl:element name="Node">
      <xsl:call-template name="full_path_imp" />
    </xsl:element>
  </xsl:template>

  <xsl:template name="full_path_imp">
    <xsl:param name="val"></xsl:param>
    <xsl:variable name="pos" select="count(preceding-sibling::*[name()=name(current())]) + 1"/>
    <xsl:variable name="str">
      <xsl:choose>
        <xsl:when test="$pos = 1">
          <xsl:value-of select="concat('/', name(), $val)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('/', name(), '[', $pos, ']', $val)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not(parent::*)">
      <xsl:value-of select="$str"/>
    </xsl:if>
    <xsl:if test="parent::*">
      <xsl:for-each select="parent::*">
        <xsl:call-template name="full_path_imp">
          <xsl:with-param name="val" select="$str"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <!-- /<xsl:value-of select="name()"/> -->
  </xsl:template>

  <!-- Report error for current node:
       <Error>
          <Node>...<Node>
          <Report>$report</Report>
       </Error>
  -->
  <xsl:template name="error_node">
    <xsl:param name="report"></xsl:param>
    <xsl:element name="Error">
      <xsl:element name="Report">
        <xsl:value-of select="normalize-space($report)"/>
      </xsl:element>
      <xsl:call-template name="full_path_node" />
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
