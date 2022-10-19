<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">

  <xsl:import href="CheckLibrary.xsl"/>
  
  <xsl:output method="xml" indent="yes" />

  <!-- skip text content -->
  <xsl:template match="text()" mode="Quality" />

  <!-- Excessively high-degree Surface (G-SU-HD) -->
  <xsl:template match="/t:QIFDocument/t:Product/t:GeometrySet/t:SurfaceSet
                       /t:Nurbs23/t:Nurbs23Core"
		mode="Quality">
    <xsl:variable
	name="MaxDegree"
	select="$CheckParameters/CheckQualityParameters
		/Check[@name='G-SU-HD']/Parameter[@name='MaxDegree']"/>
    <xsl:if test="not(((t:OrderU - 1) &lt;= $MaxDegree) and 
		      ((t:OrderV - 1) &lt;= $MaxDegree))">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Excessively high-degree Surface (G-SU-HD):
	    id=<xsl:value-of select="../@id"/>,
	    ordU(<xsl:value-of select="t:OrderU"/>),
	    ordV(<xsl:value-of select="t:OrderV"/>),
	    MaxDegree(<xsl:value-of select="$MaxDegree"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Quality"/>
  </xsl:template>

  <!-- Free edge (G-SH-FR) -->
  <!-- Over-used edge (G-SH-NM) -->
  <xsl:template match="/t:QIFDocument/t:Product/t:TopologySet/t:EdgeSet
		       /t:Edge" mode="Quality">
    <xsl:variable name="idEdge" select="@id"/>
    <xsl:variable
	name="nEdgeRef"
	select="count(//t:QIFDocument/t:Product/t:TopologySet/t:LoopSet
		/t:Loop/t:CoEdges/t:CoEdge[t:EdgeOriented/t:Id = $idEdge])"/>
    <xsl:if test="$nEdgeRef = 1">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Free edge (G-SH-FR): id=<xsl:value-of select="@id"/>,
	    nEdgeRef(<xsl:value-of select="$nEdgeRef"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:if test="$nEdgeRef &gt; 2">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Over-used edge (G-SH-NM): id=<xsl:value-of select="@id"/>,
	    nEdgeRef(<xsl:value-of select="$nEdgeRef"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Quality"/>
  </xsl:template>

  <!-- Fragmented curve (G-CU-FG) -->
  <xsl:template match="//t:Polyline13Core
                     | //t:Polyline12Core"
                mode="Quality">
    <xsl:variable
	name="MaxNumSegments"
	select="$CheckParameters/CheckQualityParameters
		/Check[@name='G-CU-FG']/Parameter[@name='MaxNumSegments']"/>
    <xsl:variable name="nSeg">
      <xsl:if test="t:Points">
        <xsl:value-of select="t:Points/@count"/>
      </xsl:if>
      <xsl:if test="t:PointsBinary">
        <xsl:value-of select="t:PointsBinary/@count"/>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="not($nSeg &lt;= $MaxNumSegments)">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Fragmented curve (G-CU-FG): id=<xsl:value-of select="../@id"/>
	    nSeg(<xsl:value-of select="$nSeg"/>) greater than
	    MaxNumSegments(<xsl:value-of select="$MaxNumSegments"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Quality"/>
  </xsl:template>

</xsl:stylesheet>
