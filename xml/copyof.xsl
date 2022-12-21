<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>

<xsl:template match="directories">
  <xsl:copy-of select="document('2003_101_00/indexall.xml')"/>
  <xsl:copy-of select="document('2003_098_00/indexall.xml')"/>
  <xsl:copy-of select="document('2003_095_00/indexall.xml')"/>
</xsl:template>

</xsl:stylesheet>
