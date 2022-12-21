<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="parmset">
  <HTML>
  <HEAD>
    <title> testing stuff</title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </HEAD>
  <BODY>
  There are <xsl:value-of select="count(/*/*)"/> graphics in this tree<BR/>
  There are <xsl:value-of select="count(figure2d)"/> plan view figures<BR/>
  There are <xsl:value-of select="count(section)"/> cross-sections figures<BR/>
  <xsl:copy-of select="document('plan.index.xml')/index/parmline[1]"/><BR/>
  </BODY>
  </HTML>
</xsl:template>

</xsl:stylesheet>
