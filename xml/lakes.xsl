<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>

<xsl:template match="/Folder">
  <HTML> <head>
    <title>MMAB: <xsl:apply-templates select="/Folder/name"/></title>
  </head>
  <BODY>
  <table>
    <tr>
    <td>Name</td>
    <td>Description</td>
    <td>Coordinates</td>
    </tr>
    <xsl:apply-templates select="Placemark"/>
  </table>

  </BODY>
  </HTML>
</xsl:template>


<xsl:template match="Placemark">
  <tr> 
    <td><xsl:apply-templates select="name"/> </td>
    <td><xsl:apply-templates select="description"/> </td>
    <td><xsl:apply-templates select="Point/coordinates"/> </td>
  </tr>
</xsl:template>


</xsl:stylesheet>
