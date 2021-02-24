<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>

<xsl:template match="parmset">
<!--
  <xsl:text><?xml-stylesheet href="index.xsl" type="text/xsl" ?></xsl:text>
-->
  <index>
  <title>Plan View Product Index</title>
  <topic>salinity, temperature, velocity, height, elevation, sea, sea surface</topic>
  <status>Developmental</status>
  <contact>NCEP.EMC.Coastal.Ocean@noaa.gov</contact>
  <date>
<!-- Want only 1 of the construction dates to be copied.  Ensure that all do match -->
    <constructed><xsl:apply-templates select="figure2d[1]/date/constructed"/>
      </constructed>
    <valid><xsl:apply-templates select="figure2d[1]/date/valid"/> </valid>
  </date>

  <xsl:variable name="constructed_ref" select="figure2d[1]/date/constructed"/>
  <xsl:variable name="valid_ref" select="figure2d[1]/date/valid"/>
<!--   <xsl:apply-templates select="figure2d"/> --> 
  <xsl:for-each select="figure2d">
    <parmline>
      <parm>  <xsl:apply-templates select="parameters/parm"/> 
      <!-- Ensure that the constructed and valid date really are consistent.  
           If they are, we can rely on only the above outputting of the date -->
      <xsl:if test="date/constructed != $constructed_ref">
         illegal constructed date!
      </xsl:if>
      <xsl:if test="date/valid != $valid_ref">
         illegal valid date!
      </xsl:if>
      </parm>
      <depth> <xsl:apply-templates select="depth"/> </depth>
      <full>  <xsl:apply-templates select="full"/> </full>
      <xsl:apply-templates select="graphic"/>

    </parmline>
    <xsl:text>
    </xsl:text>
  </xsl:for-each>

<!-- How to echo the xml along with node content? 
  <xsl:apply-templates select="figure2d[1]/domain2d"/>
-->
  </index>
</xsl:template>


<xsl:template match="graphic">
  <graphic> <xsl:value-of select="@source"/> </graphic>
</xsl:template>


</xsl:stylesheet>
