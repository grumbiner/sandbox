<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>

<xsl:template match="parmset">
  <index>
  <title>Product Index</title>
  <status>Developmental</status>
  <contact>NCEP.EMC.Coastal.Ocean@noaa.gov</contact>
  <xsl:apply-templates select="figure2d"/> 
  <xsl:apply-templates select="section"/> 
  </index>
</xsl:template>

<xsl:template match="figure2d">
  <parmline>
    <parm> <xsl:apply-templates select="parameters/parm"/> </parm>
    <depth> <xsl:apply-templates select="depth"/> </depth>
    <valid> <xsl:apply-templates select="date/valid"/> </valid>
  </parmline>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="section">
  <parmline>
    <parm> <xsl:apply-templates select="parameters/parm"/> </parm>
    <name> <xsl:apply-templates select="domain_section/name"/></name>
    <valid> <xsl:apply-templates select="date/valid"/> </valid>
  </parmline>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

</xsl:stylesheet>
