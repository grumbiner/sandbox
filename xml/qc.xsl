<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>

<!-- Note that we're passing in the parameter for defining 'big' difference by
     command line:
Xalan -p big '5.0'  (then the rest of the usual command -->

<xsl:param name="big" select="3.3"/>

<xsl:template match="qc">
  <xsl:text>hello low resolution ob  </xsl:text>
<!-- Note that this is a full path spec to the data we're interested in -->
<!-- pring out the value of the lores ob: -->
  <xsl:value-of select="/qc/low"/>

<!-- playing with line number in output 
  <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="position()"/>
  <xsl:text>&#10;</xsl:text>

  <xsl:if test="position() = 1">
    <xsl:value-of select="position()"/>
1 </xsl:if>

-->

  <xsl:if test="low > 20.0">
    <xsl:text>font color = red</xsl:text>
  </xsl:if>
  <xsl:text>&#10;</xsl:text>


  <xsl:text>High vs. low difference is:  </xsl:text>
  <xsl:value-of select="(/qc/low - /qc/high)"/>
  <xsl:text>&#10;</xsl:text>

  <xsl:text>big is </xsl:text>
  <xsl:value-of select="$big"/>
  <xsl:text>&#10;</xsl:text>


  <xsl:if test=" (/qc/high - /qc/low)  &gt; $big " > 
  <!-- <xsl:if test=" (/qc/high - /qc/low)  != 0 " > -->
    <xsl:text>big difference  </xsl:text>
    <xsl:value-of select="(/qc/high - /qc/low)"/>
    <xsl:text> vs big of </xsl:text>
    <xsl:value-of select="$big"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:if>


</xsl:template>

</xsl:stylesheet>
