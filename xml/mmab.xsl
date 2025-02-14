<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/person">
  <html>
  <head>
    <title><xsl:apply-templates select="name"/></title>
  </head>
  <body>
    We're in xslt processing<BR/>
    <xsl:apply-templates select="name"/><BR/>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="extension"/><BR/>
    <xsl:apply-templates select="email"/><BR/>
    
  </body>
  </html>
</xsl:template>

<xsl:template match="email">
  <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates/>
    </xsl:attribute>
    <xsl:apply-templates/>
  </A>
</xsl:template>

<xsl:template match="title">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="extension">
  Phone 301-763-8133 ext:<xsl:apply-templates/>
</xsl:template>

<xsl:template match="name">
  <xsl:apply-templates/>
</xsl:template>

</xsl:stylesheet>
