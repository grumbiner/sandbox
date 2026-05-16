<!-- Robert Grumbine 22 Mar 2013 -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>


<xsl:template match="/model">
<HTML>
<HEAD>
  <title><xsl:apply-templates select="name"/><xsl:text>  </xsl:text>  
<xsl:apply-templates select="version"/> </title>
</HEAD>
<BODY>
  <H3>Model <xsl:apply-templates select="name"/><xsl:text>  </xsl:text>
  Version<xsl:apply-templates select="version"/></H3>

  Implementation Operational on: <xsl:apply-templates select="impdate"/><br/>
  <br/>
  Model output fields are: <xsl:apply-templates select="fields"/><br/>
  <br/>
  Physics -- Model Evolution<br/><ul>
  <li>Dynamics       : <xsl:apply-templates select="dynamics"/></li>
  <li>Concentration  : <xsl:apply-templates select="concentration"/></li>
  <li>Thickness      : <xsl:apply-templates select="thickness"/></li>
  <li>Thermodynamics : <xsl:apply-templates select="thermo"/></li>
  </ul>
  <br/>
  Model Grid : <xsl:apply-templates select="grid/spacing"/> on <xsl:apply-templates select="grid/type"/> <br/>
  Model Output Frequency : <xsl:apply-templates select="time/increment"/> to <xsl:apply-templates select="time/lead"/><br/>

  <xsl:apply-templates select="narrative"/>



</BODY>
</HTML>
<!-- Done with constructing the html page for a given model -->

<!-- Deal with elementary bits down here -->
<!--
<xsl:template match="name">
</xsl:template>
<xsl:template match="version">
</xsl:template>
-->

</xsl:template>
</xsl:stylesheet>
