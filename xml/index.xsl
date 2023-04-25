<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="index">
  <HTML>
  <HEAD>
    <TITLE><xsl:apply-templates select="title"/></TITLE>
    <link href="main.css" type="text/css" rel="STYLESHEET"/>
  </HEAD>
  <BODY>
    <CENTER>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="status"/><BR/>
    </CENTER>
    <BR/>
    The brief figures are simply the .jpg images of the figure.  More detail
figures include more detail about the date and time of construction, and of
the map projection used.<BR/>
    <xsl:apply-templates select="contact"/> for more information<BR/>
  <HR/>
  <table border="2">
  <tr>
    <td>Brief figure</td>
    <td>More detail</td>
  </tr>
  <xsl:apply-templates select="parmline"/> 
  </table>
  <HR/>
<!-- Want only 1 of the 2d domains to be copied! -->
<!--  <xsl:apply-templates select="figure2d/domain2d"/> -->
  <HR/>
  <A HREF="/ofs/Welcome.html">Return to main Ocean Forecast System page</A>
  <BR/>
  Presentation Last Modified 15 March 2004<BR/>
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="parmline">
  <tr>
    <td> 
      <A> 
         <xsl:attribute name="href"><xsl:value-of select="graphic"/>
         </xsl:attribute>
        <xsl:apply-templates select="parm"/><xsl:text>  </xsl:text> 
        <xsl:apply-templates select="valid"/><xsl:text>  </xsl:text> 
        <xsl:apply-templates select="name"/> 
      </A>
    </td>
    <td> 
      <A> 
        <xsl:attribute name="href"><xsl:value-of select="full"/>
          </xsl:attribute>
        <xsl:apply-templates select="parm"/> <xsl:text>  </xsl:text>
        <xsl:apply-templates select="name"/> 
      </A>
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
