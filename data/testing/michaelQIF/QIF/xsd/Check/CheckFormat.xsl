<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:t="http://qifstandards.org/xsd/qif3">
  
  <xsl:import href="CheckLibrary.xsl"/>
  
  <xsl:output method="xml" indent="yes" />

  <!-- skip text content -->
  <xsl:template match="text()" mode="Format" />

  <!-- idMax -->
  <xsl:template match="//t:*[@id &gt; /t:QIFDocument/@idMax]" mode="Format">
    <xsl:call-template name="error_node">
      <xsl:with-param name="report">
        The id of element is greater than idMax.
	id(<xsl:value-of select="@id"/>) greater than
	idMax(<xsl:value-of select="/t:QIFDocument/@idMax"/>).
      </xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>

  <!-- Links with external documents -->
  <!-- Warning: some XSLT 1.0 processors (e.g. the one in XMLSpy) signal -->
  <!-- an error if the document function does not find what it is        -->
  <!-- looking for and no output file is produced. Other XSLT processors -->
  <!-- (e.g. xsltproc) return an empty node list that can be tested,     -->
  <!-- and will produce an output file if the document function fails.   -->

  <xsl:template
    match="t:ExternalQIFReferences/t:ExternalQIFDocument"
    mode="Format">
    <xsl:variable name="id" select="@id"/>
    <xsl:variable name="QPId" select="t:QPId"/>
    <xsl:variable name="URI" select="t:URI"/>
    <xsl:choose>
      <xsl:when test="document($URI)">
	<xsl:variable name="docExt" select="document($URI)/t:QIFDocument"/>
	<xsl:variable name="QPIdExt" select="$docExt/t:QPId"/>
	<xsl:choose>
	  <xsl:when test="not($QPId = $QPIdExt)">
            <xsl:call-template name="error_node">
              <xsl:with-param name="report">
		The external document has a different QPId.
		URI = <xsl:value-of select="$URI"/>,
		QPId(<xsl:value-of select="$QPId"/>)
		!= QPIdExt(<xsl:value-of select="$QPIdExt"/>).
              </xsl:with-param>
            </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
            <xsl:for-each select="//t:*[@xId][. = $id]">
              <xsl:variable name="xId" select="@xId"/>
              <xsl:if test="not(count($docExt//t:*[@id = $xId]) = 1)">
		<xsl:call-template name="error_node">
		  <xsl:with-param name="report">
                    The external entity with this id was not found.
		    URI = <xsl:value-of select="$URI"/>,
		    idDoc(<xsl:value-of select="$id"/>),
		    xId(<xsl:value-of select="$xId"/>).
		  </xsl:with-param>
		</xsl:call-template>
              </xsl:if>
            </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
	    The external document was not found
	    URI = <xsl:value-of select="$URI"/>.
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>

<!--

2D/3D NURBS curves, the check: the number of control points = the number of
knots - the curve order

-->

  <xsl:template
    match="/t:QIFDocument/t:Product/t:GeometrySet/t:Curve12Set/t:Nurbs12
           /t:Nurbs12Core
         | /t:QIFDocument/t:Product/t:GeometrySet/t:Curve13Set/t:Nurbs13
           /t:Nurbs13Core"
                mode="Format">
    <xsl:variable name="nCP">
      <xsl:if test="t:CPs">
        <xsl:value-of select="t:CPs/@count"/>
      </xsl:if>
      <xsl:if test="t:CPsBinary">
        <xsl:value-of select="t:CPsBinary/@count"/>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="not($nCP = t:Knots/@count - t:Order)">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Nurbs12Core/Nurbs13Core: id=<xsl:value-of select="../@id"/>,
	    nCP(<xsl:value-of select="$nCP"/>)
	    != nKt(<xsl:value-of select="t:Knots/@count"/>) - 
	    order(<xsl:value-of select="t:Order"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>

<!-- NURBS surface, the check:
   the number of control points = ('nKtU' - 'ordU')*('nKtV' - 'ordV') -->

  <xsl:template
    match="/t:QIFDocument/t:Product/t:GeometrySet/t:SurfaceSet/t:Nurbs23
           /t:Nurbs23Core"
    mode="Format">
    <xsl:variable name="nCP">
      <xsl:if test="t:CPs">
        <xsl:value-of select="t:CPs/@count"/>
      </xsl:if>
      <xsl:if test="t:CPsBinary">
        <xsl:value-of select="t:CPsBinary/@count"/>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="not($nCP = (t:KnotsU/@count - t:OrderU) *
		             (t:KnotsV/@count - t:OrderV))">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            Nurbs23Core: id=<xsl:value-of select="../@id"/>,
	    nCP(<xsl:value-of select="$nCP"/>) !=
	    (nKtU(<xsl:value-of select="t:KnotsU/@count"/>) -
	    ordU(<xsl:value-of select="t:OrderU"/>)) *
	    (nKtV(<xsl:value-of select="t:KnotsV/@count"/>) -
	    ordV(<xsl:value-of select="t:OrderV"/>))
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>
  
  <!-- Array of elements, the check: number of elements must be equal to
       a value specified in the attribute 'n' -->

  <xsl:template match="//t:*[@n]" mode="Format">
    <xsl:if test="not(@n = count(t:*))">
      <xsl:for-each select=".">
        <xsl:call-template name="error_node">
          <xsl:with-param name="report">
            The number of array elements doesn't correspond to the
	    specified "n" attribute: n(<xsl:value-of select="@n"/>) != 
	    nElem(<xsl:value-of select="count(t:*)"/>)
          </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>

<!--

template for the square of the length of a 3D vector represented as an
xs:list of three numerical values.

-->

  <xsl:template name="lengthSquare3D">
    <xsl:param name="vec3D"/>
    <xsl:variable name="norm">
      <xsl:value-of select="normalize-space($vec3D)"/>
    </xsl:variable>
    <xsl:variable name="first">
      <xsl:value-of select='substring-before($norm, " ")'/>
    </xsl:variable>
    <xsl:variable name="rest">
      <xsl:value-of select='substring-after($norm, " ")'/>
    </xsl:variable>
    <xsl:variable name="second">
      <xsl:value-of select='substring-before($rest, " ")'/>
    </xsl:variable>
    <xsl:variable name="third">
      <xsl:value-of select='substring-after($rest, " ")'/>
    </xsl:variable>
    <xsl:variable name="x">
      <xsl:value-of select="number($first)"/>
    </xsl:variable>
    <xsl:variable name="y">
      <xsl:value-of select="number($second)"/>
    </xsl:variable>
    <xsl:variable name="z">
      <xsl:value-of select="number($third)"/>
    </xsl:variable>
    <xsl:value-of select="($x*$x) + ($y*$y) + ($z*$z)"/>
  </xsl:template>

<!--

Unit vector, the check: the sum of the squares of the components must
not be less than MinLength squared and must not be greater than MaxLength
squared. MinLength and MaxLength are parameters from CheckParameters.xml.

This is checking only 3D unit vectors. 

See the file unitVectors.txt for an analysis of where all elements are
found whose type is a 3D unit vector type; there are four 3D unit vector
types.
-->


 <xsl:template
     match="//t:AnalysisVector
          | //t:AxisDirection
          | //t:AxisVector
          | //t:DatumTargetTranslationDirection
          | //t:DepthVector
          | //t:DirMeridianPrime
          | //t:DirNorthPole
          | //t:LengthDirection
          | //t:LengthVector
          | //t:NominalDirection
          | //t:Normal
          | //t:NormalSpecial
          | //t:OriginDirection
          | //t:PrimaryAxis
          | //t:RectangularUnitAreaOrientation
          | //t:RotationAxis
          | //t:SecondaryAxis
          | //t:Vector
          | //t:WidthDirection
          | //t:XDirection
          | //t:YDirection
          | //t:ZDirection
          | //t:ZoneDirection
          | //t:ZoneOrientationVector
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:AdjacentNormal 
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:Direction
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:DraftVector
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:FeatureDirection
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:LineDirection
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:PlaneNormal
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:StartDirection
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:Axis/t:Direction
          | /t:QIFDocument/t:Features/t:FeatureNominals/t:*/t:Sweep/t:DirBeg
          | /t:QIFDocument/t:FeatureZones/t:FeatureZoneAreaCylindrical
            /t:Cylinder/t:Axis
          | /t:QIFDocument/t:Product/t:GeometrySet/t:Curve13Set
            /t:ArcCircular13/t:ArcCircular13Core/t:DirBeg
          | /t:QIFDocument/t:Product/t:GeometrySet/t:Curve13Set
            /t:ArcConic13/t:ArcConic13Core/t:DirBeg
          | /t:QIFDocument/t:Product/t:VisualizationSet/t:PMIDisplaySet
            /t:PMIDisplay/t:Plane/t:Direction
          | /t:QIFDocument/t:Product/t:ViewSet/t:AnnotationViewSet
            /t:AnnotationView/t:Direction
          | /t:QIFDocument/t:Product/t:ViewSet/t:ExplodedViewSet
            /t:ExplodedView/t:MoveGroups/t:MoveGroup/t:Translate/t:Direction
          | /t:QIFDocument/t:Product/t:ViewSet/t:ExplodedViewSet
            /t:ExplodedView/t:MoveGroups/t:MoveGroup/t:Rotate/t:Axis
          | /t:QIFDocument/t:Product/t:ZoneSectionSet/t:ZoneSection
            /t:SectionPlanes/t:SectionPlane/t:Plane/t:Direction
          | /t:QIFDocument/t:MeasurementsResults/t:MeasurementResultsSet
            /t:MeasurementResults/t:MeasuredFeatures/t:*
            /t:SweepFull/t:DirBeg
          | /t:QIFDocument/t:MeasurementsResults/t:MeasurementResultsSet
            /t:MeasurementResults/t:MeasuredFeatures/t:*
            /t:SweepMeasurementRange/t:DirBeg
          | /t:QIFDocument/t:MeasurementsResults/t:MeasurementResultsSet/
             t:MeasurementResults/t:MeasuredFeatures/t:*/t:Axis/t:Direction"
     mode="Format">
    <xsl:variable
      name="minLength"
      select="$CheckParameters/CheckFormatParameters
             /Check[@name='unitVectorLength']/Parameter[@name='MinLength']"/>
    <xsl:variable
      name="maxLength"
      select="$CheckParameters/CheckFormatParameters
             /Check[@name='unitVectorLength']/Parameter[@name='MaxLength']"/>
    <xsl:variable name="minSquare" select="$minLength*$minLength"/>
    <xsl:variable name="maxSquare" select="$maxLength*$maxLength"/>
    <xsl:variable name="lengthSquare">
      <xsl:call-template name="lengthSquare3D">
        <xsl:with-param name="vec3D" select="." />
      </xsl:call-template>
    </xsl:variable>
    <xsl:if test="$lengthSquare &gt; $maxSquare">
      <xsl:call-template name="error_node">
        <xsl:with-param name="report">
          Unit vector too long: length of (<xsl:value-of select="."/>)
	  greater than <xsl:value-of select="$maxLength"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="$lengthSquare &lt; $minSquare">
      <xsl:call-template name="error_node">
        <xsl:with-param name="report">
          Unit vector too short: length of (<xsl:value-of select="."/>)
          less than <xsl:value-of select="$minLength"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:apply-templates mode="Format"/>
  </xsl:template>

</xsl:stylesheet>
