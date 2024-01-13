<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>

<xsl:template match="parmset">
  <index>
  <title>Ocean Forecast System Cross Section View Product Index</title>
  <topic>cross-section salinity, temperature, velocity, height, elevation, sea, sea surface</topic>
  <status>Developmental</status>
  <contact>NCEP.EMC.Coastal.Ocean@noaa.gov</contact>
  <xsl:apply-templates select="section"/>
  </index>
</xsl:template>

<xsl:template match="section">
  <parmline>
    <parm> <xsl:apply-templates select="parameters/parm"/> </parm>
    <valid> <xsl:apply-templates select="date/valid"/> </valid>
    <name> <xsl:apply-templates select="domain_section/name"/></name>
    <full> <xsl:apply-templates select="full"/></full>
    <xsl:apply-templates select="graphic"/>
  </parmline>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="graphic">
  <graphic> <xsl:value-of select="@source"/> </graphic>
</xsl:template>

</xsl:stylesheet>
