<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>

<xsl:template match="/welcome">
  <HTML> <head>
    <title><xsl:apply-templates select="title"/></title>
<!--
<authors:>
    <meta http-equiv="author" context="value">
</authors:>
<keywords:>
    <meta http-equiv="keywords" context="value">
</keywords:>
-->
    <meta http-equiv="description">
        <xsl:attribute name="content"> <xsl:apply-templates select="description"/>
        </xsl:attribute>
    </meta>
    <link href="main.css" type="text/css" rel="STYLESHEET"/>
  </head>

  <BODY background="/mmab/images/dirbkgnd.gif">

  <H1><xsl:apply-templates select="top"/></H1>
  <A HREF="/">Marine Modeling and Analysis Branch</A>

  <UL>
  <xsl:for-each select="section">
    <LI><A>
      <xsl:attribute name="href">#<xsl:apply-templates select="name"/>
      </xsl:attribute>
      <xsl:apply-templates select="name"/>
    </A>
    </LI>
  </xsl:for-each>
  </UL>

  <HR/>

  <xsl:for-each select="section">
    <H2>
    <A>
    <xsl:attribute name="NAME"><xsl:apply-templates select="name"/>
      </xsl:attribute>
    <xsl:apply-templates select="name"/>
    </A>
    </H2> 
    <xsl:apply-templates select="text"/>
    
    <div style="text-align: right;">
    <div align="right">
    <font face="VERDANA, ARIAL, HELVETICA, SANS-SERIF"><font size="-2">
    <a href="#top">Back to top</a></font></font>
    <a href="#top">
     <img SRC="/mmab/images/up_arrow13x10.gif" ALT="^" BORDER="0" height="10" width="13"/>
    </a>
    </div>
    </div>

  </xsl:for-each>

  <HR/>

  <xsl:apply-templates select="contact"/><BR/>
  Last Modified: <xsl:apply-templates select="date/modified"/><BR/>
  Last Reviewed: <xsl:apply-templates select="date/reviewed"/><BR/>
  Initial: <xsl:apply-templates select="date/initial"/><BR/>

  <HR/>

<CENTER>
<P><A HREF="http://www.noaa.gov">NOAA</A> | <A HREF="http://www.nws.noaa.gov">NWS
</A>| <A HREF="http://www.ncep.noaa.gov">NCEP</A> | <A
HREF="/Welcome.html">MMAB</A> </P>
<P><A HREF="http://www.nws.noaa.gov/disclaimer1.html">NWS web distribution disclaimer</A></P>
</CENTER>

  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="contact">
  <A><xsl:attribute name="href"><xsl:apply-templates/></xsl:attribute>
  <xsl:apply-templates/>
  </A>
</xsl:template>


<!-- Items to preserve some inlined markup -->
<xsl:template match="a|A">
  <A>
  <xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
  <xsl:apply-templates/>
  </A>
</xsl:template>
<xsl:template match="UL">
  <UL>
  <xsl:apply-templates/>
  </UL>
</xsl:template>
<xsl:template match="BR">
  <BR/>
</xsl:template>

</xsl:stylesheet>
