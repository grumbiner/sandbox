<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/paper">
  <HTML> <head>
    <title><xsl:apply-templates select="title"/></title>
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

    Full document is at <A>
    <xsl:attribute name="href"><xsl:apply-templates select="fulldoc"/>
    </xsl:attribute>
    <xsl:apply-templates select="fulldoc"/><BR/>
    </A>

    Please contact <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates select="contact"/>
    </xsl:attribute>
    <xsl:apply-templates select="contact"/>
    </A>
    for more information<BR/>


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

</xsl:stylesheet>
