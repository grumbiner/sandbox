<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/section">
  <HTML> <head>
    <title>MMAB: <xsl:apply-templates select="title"/></title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <BODY>
    <CENTER>
    <B>
    <xsl:apply-templates select="status"/><BR/> </B>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="date"/><BR/>
    </CENTER>
    <xsl:apply-templates select="contact"/> for more information about the product displayed.<BR/>
    <HR/>
    <xsl:apply-templates select="graphic"/><BR/>
    <HR/>
    <xsl:apply-templates select="domain_section"/><BR/>
    <HR/>
    Page display last modified 10 March 2004
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="graphic">
  <IMG>
  <xsl:attribute name="src">
    <xsl:value-of select="@source"/>
  </xsl:attribute>
  </IMG>
</xsl:template>

<xsl:template match="date">
  Figure constructed <xsl:apply-templates select="constructed"/> <xsl:value-of select="@cycle"/> UTC cycle<BR/>
  <xsl:apply-templates select="valid"/>
</xsl:template>
<xsl:template match="valid">
  Valid Time <xsl:apply-templates/><xsl:text>  </xsl:text><xsl:value-of select="@time"/> UTC <BR/>
</xsl:template>

<xsl:template match="parameters">
  <xsl:apply-templates select="parm"/>
</xsl:template>
<xsl:template match="parm">
  <LI><xsl:apply-templates/></LI>
</xsl:template>

  
<xsl:template match="domain_section">
    Domain: <xsl:apply-templates select="name"/><BR/>
    Size: <xsl:apply-templates select="length"/> <BR/>
    Bounding points:<xsl:apply-templates select="bounds"/>
    <BR/>
    Map Projection:<xsl:apply-templates select="projection"/><BR/>
    <xsl:apply-templates select="notes"/><BR/> 
    <xsl:apply-templates select="contact"/>for more information about the grid<BR/>
</xsl:template>

<xsl:template match="bounds">
  <table>
  <xsl:apply-templates select="loc"/>
  </table>
</xsl:template>

<xsl:template match="loc">
<tr>
  <td  width="90" align="right">
    <xsl:value-of select="@lat"/>
    <xsl:text> N </xsl:text>
  </td>
  <td  width="90" align="right">
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
