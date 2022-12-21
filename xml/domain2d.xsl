<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/figure2d">
  <HTML> <head>
    <title>MMAB: <xsl:apply-templates select="title"/></title>
    <link href="main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <BODY>
    <CENTER>
    <B> 
    <xsl:apply-templates select="status"/><BR/> </B>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="date"/><BR/>
    </CENTER>
    <HR/>
2d Domain description:
    <xsl:apply-templates select="domain2d"/><BR/>
    <HR/>
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="domain2d"/>
    Domain: <xsl:apply-templates select="name"/><BR/>
    Size: <xsl:apply-templates select="area"/> <BR/>
    Bounding points:<xsl:apply-templates select="bounds"/>
    <BR/>
    Map Projection:<xsl:apply-templates select="projection"/><BR/>
    <xsl:apply-templates select="notes"/><BR/> 
    <HR/>
    Please contact <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates select="contact"/>
    </xsl:attribute>
    <xsl:apply-templates select="contact"/>
    </A>
    for more information<BR/>
</xsl:template>

<xsl:template match="bounds">
  <table>
  <xsl:apply-templates select="loc"/>
  </table>
</xsl:template>

<xsl:template match="loc">
<tr>
  <td>
    <xsl:value-of select="@lat"/>
    <xsl:text> N </xsl:text>
  </td>
  <td>
    <xsl:value-of select="@lon"/>
    <xsl:text> E </xsl:text>
  </td>
</tr>
</xsl:template>


</xsl:stylesheet>
