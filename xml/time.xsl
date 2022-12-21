<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="yes"/>

<xsl:template match="race">
  <HTML><BODY>
  <table border="1">
  <xsl:apply-templates select="time"/>
  </table>
  <xsl:value-of select="round(1000*distance div 1.60934)div 1000"/>
  </BODY> </HTML>
</xsl:template>

<xsl:template match="time">
  <tr>
    <td>
      <xsl:value-of select="position()"/>
    </td>
    <td>
      <xsl:apply-templates select="minutes"/>:<xsl:apply-templates select="seconds"/>
    </td>
    <td>
      <xsl:value-of select="round( (seconds + 60*minutes) div 3.107) "/>
    </td>
    <td>
      <xsl:value-of select="(seconds + 60*minutes) div /race/distance "/>
    </td>
  </tr>
</xsl:template>

The following is to appened a 0 in front of times in seconds that are less than 10
<xsl:template match="seconds">
  <xsl:apply-templates select="seconds[ (. &lt; 10) ]"/>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="seconds[ (. &lt; 10) ]">
  <xsl:text>0</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

  

</xsl:stylesheet>
