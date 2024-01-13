<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/experiment">
  <HTML> <head>
    <title>MMAB: <xsl:apply-templates select="title"/></title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <BODY>
    <CENTER>
    <B><xsl:apply-templates select="status"/><BR/></B>
    <xsl:apply-templates select="title"/><BR/>
    Experiment begun: <xsl:apply-templates select="date/begin"/><BR/>
    </CENTER>
    <BR/>
    <xsl:apply-templates select="purpose"/><BR/>
    <xsl:apply-templates select="contact"/> for more information about the experiment.<BR/>
    <HR/>
    Parameters Displayed:<BR/>
    <xsl:apply-templates select="parameters"/>
    <HR/>
    <xsl:apply-templates select="directories"/>
    <HR/>
    <xsl:apply-templates select="domain2d"/>
    <HR/>
    Page display last modified 19 March 2004
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="parameters">
  <xsl:apply-templates select="parm"/>
</xsl:template>
<xsl:template match="parm">
  <LI><xsl:apply-templates/></LI>
</xsl:template>

<xsl:template match="directories">
  <LI><A>
      <xsl:attribute name="href"><xsl:value-of select="location"/>plan.index.html</xsl:attribute>
      <xsl:apply-templates select="title"/> Plan view products
  </A></LI>
  <LI><A>
      <xsl:attribute name="href"><xsl:value-of select="location"/>xsec.index.html</xsl:attribute>
      <xsl:apply-templates select="title"/> Cross-Section products
  </A></LI>
</xsl:template>

     
  
<xsl:template match="domain2d">
    Domain: <xsl:apply-templates select="name"/><BR/>
    Map Projection: <xsl:apply-templates select="projection"/><BR/>
    Size (approximate): <xsl:apply-templates select="area"/> <BR/>
    Bounding points: <xsl:apply-templates select="bounds"/> <BR/>
    <xsl:apply-templates select="contact"/>for more information about the grid.<BR/>
</xsl:template>

<xsl:template match="bounds">
  <table>
  <xsl:apply-templates select="loc"/>
  </table>
</xsl:template>

<xsl:template match="loc">
<tr>
  <td width="60" align="right">
    <xsl:value-of select="@lat"/>
    <xsl:text> N </xsl:text>
  </td>
  <td width="60" align="right">
    <xsl:value-of select="@lon"/>
    <xsl:text> E </xsl:text>
  </td>
</tr>
</xsl:template>


<xsl:template match="contact">
    Please contact <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates/>
    </xsl:attribute>
    <xsl:apply-templates/>
    </A>
</xsl:template>

</xsl:stylesheet>
