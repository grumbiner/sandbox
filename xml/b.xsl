<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>

<xsl:template match="directory">
  <directory>
  <xsl:for-each select="link">
    <link>
      <text><xsl:apply-templates select="text"/></text>
      <ref>
      <xsl:if test="./@type = 'plan'">
         <xsl:value-of select="ref"/>
      </xsl:if>
      <xsl:if test="@type = 'xsec'">
         <xsl:value-of select="ref"/>
      </xsl:if>
      </ref>
    </link>
    <xsl:text>
</xsl:text>
  </xsl:for-each>
  </directory>

</xsl:template>
   
  



</xsl:stylesheet>
