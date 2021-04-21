<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/paper">
    <LI>
    <xsl:apply-templates select="authors"/>
    <A>
    <xsl:attribute name="href">
    <xsl:apply-templates select="fulldoc"/>
    </xsl:attribute>
    <xsl:apply-templates select="title"/>
    </A>
    <xsl:apply-templates select="date"/>.
    </LI>
</xsl:template>

<xsl:template match="authors">
  <xsl:apply-templates select="person"/> 
</xsl:template>
<xsl:template match="person">
  <A>
    <xsl:attribute name="href">mailto:<xsl:apply-templates select="email"/>
    </xsl:attribute>
    <xsl:apply-templates select="name"/>,
  </A>
</xsl:template>

</xsl:stylesheet>
