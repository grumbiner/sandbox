<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>
<xsl:output doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>
<!--
<xsl:output doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"/>
<xsl:output doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
-->

<xsl:template match="/figure2d">
  <HTML> <head>
    <title>MMAB: <xsl:apply-templates select="title"/></title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <BODY background="/mmab/images/dirbkgnd.gif">
    <CENTER>
    <B> <xsl:apply-templates select="status"/><BR/> </B>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="date"/><BR/>
    </CENTER>
    <A>
      <xsl:attribute name="href">/seaice/support/ssmi.advice.shtml</xsl:attribute>
      For More Information
    </A><BR/>
    <xsl:apply-templates select="contact"/> for even more information about the product displayed.<BR/>
    <HR/>
<!--
    Figure shows:<BR/>
    <xsl:apply-templates select="parameters"/><BR/>
-->
    <xsl:apply-templates select="graphic"/><BR/>
    <HR/>
    <xsl:apply-templates select="domain2d"/><BR/>
    <HR/>
    Page display last modified 23 March 2005<BR/>
    <xsl:apply-templates select="return"/>
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="graphic">
  <IMG>
  <xsl:attribute name="src">
    <xsl:value-of select="@source"/>
  </xsl:attribute>
  <xsl:attribute name="alt">
    <xsl:value-of select="@alt"/>
  </xsl:attribute>
  </IMG>
  <IMG> 
    <xsl:attribute name="src">cbar.gif</xsl:attribute>
    <xsl:attribute name="alt">Color Bar </xsl:attribute>
  </IMG>  

</xsl:template>


<xsl:template match="date">
  <xsl:apply-templates select="constructed"/>
  <xsl:apply-templates select="valid"/>
</xsl:template>

<xsl:template match="constructed">
  Figure Constructed <xsl:apply-templates/><xsl:text>  </xsl:text><xsl:value-of select="@cycle"/> UTC cycle<BR/>
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

  

<xsl:template match="domain2d">
    Domain: <xsl:apply-templates select="name"/><BR/>
    Map Projection: <xsl:apply-templates select="projection"/><BR/>
    Size (approximate): <xsl:apply-templates select="area"/> <BR/>
    Grid Cell Size: <xsl:apply-templates select="resolution"/> <BR/>
    Bounding points: <xsl:apply-templates select="bounds"/>
    <xsl:apply-templates select="notes"/><BR/> 
    <xsl:apply-templates select="contact"/> for more information about the grid.
</xsl:template>

<xsl:template match="projection">
    <xsl:value-of select="." />
    <xsl:if test=".='Polar Stereographic'">
        <BR/><xsl:text>True at 60 degree latitude</xsl:text><BR/>
    </xsl:if>
</xsl:template>

<xsl:template match="bounds">
  <table>
  <tbody>
  <xsl:apply-templates select="loc"/>
  </tbody>
  </table>
</xsl:template>

<xsl:template match="loc">
<tr>
  <td  align="right">
    <xsl:value-of select="@lat"/>
    <xsl:text> N </xsl:text>
  </td>
  <td  align="right">
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

<xsl:template match="return">
  <A>
  <xsl:attribute name="href">/seaice/<xsl:apply-templates select="page"/>
  </xsl:attribute>
  Return to <xsl:apply-templates select="text"/>
  </A><BR/>
</xsl:template>

</xsl:stylesheet>
