<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>

<xsl:template match="/tool">
  <HTML> <head>
    <title>OFS: <xsl:apply-templates select="title"/></title>
    <link href="main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <BODY>
    <CENTER>
    <B>
    <xsl:apply-templates select="status"/><BR/>
    </B>
    <xsl:apply-templates select="date"/><BR/>

    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="authors"/>
    <BR/>
    </CENTER>
    Abstract:<xsl:apply-templates select="abstract"/>
    <P/>
    <HR/>

    Full documentation is at <A>
    <xsl:attribute name="href">ftp://polar.ncep.noaa.gov/pub/<xsl:apply-templates select="fulldoc"/>
    </xsl:attribute>
    <xsl:apply-templates select="fulldoc"/>
    </A>
    <BR/>
    Tool package is at <A>
    <xsl:attribute name="href">ftp://polar.ncep.noaa.gov/pub/<xsl:apply-templates select="code"/>
    </xsl:attribute>
    <xsl:apply-templates select="code"/>
    </A>
    <BR/>

    Please contact <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates select="contact"/>
    </xsl:attribute>
    <xsl:apply-templates select="contact"/>
    </A>
    for more information<BR/>
    <HR/>
    <xsl:apply-templates select="illustration"/>
  </BODY>
  </HTML>
</xsl:template>

<xsl:template match="authors">
  <xsl:apply-templates select="person"/> 
</xsl:template>
<xsl:template match="person">
  <xsl:apply-templates select="name"/><xsl:text>  </xsl:text>
  <xsl:apply-templates select="employer"/><BR/>
</xsl:template>

<xsl:template match="illustration">
  <xsl:apply-templates/>
  <img align="top" width="100%"> 
    <xsl:attribute name="src">
      <xsl:value-of select="@fileref"/>
    </xsl:attribute>
  </img>
</xsl:template>

</xsl:stylesheet>
