<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="directories">
  <HTML>
  <HEAD>
    <title> testing stuff</title>
  </HEAD>
  <BODY>
  <HR/>
  <xsl:value-of select="document('2003_101_00/indexall.xml')"/>
  <HR/>
  <xsl:copy-of select="document('file:///usr1/rmg3/xml/20040525/2003_101_00/indexall.xml')/parmset/figure2d[1]"/>
  <HR/>
  There are <xsl:value-of select="count(/*/*)"/> graphics in this tree<BR/>
  There are <xsl:value-of select="count(figure2d)"/> plan view figures<BR/>
  There are <xsl:value-of select="count(section)"/> cross-sections figures<BR/>
  </BODY>
  </HTML>
</xsl:template>

</xsl:stylesheet>
