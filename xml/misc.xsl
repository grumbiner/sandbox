<!-- Some example usages -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml"/>
<!-- alternatively: -->
<xsl:output method="html"/>

<!-- some straightforward applications: 20040225 -->
<xsl:template match="/person">
  <html>
  <head>
    <title><xsl:apply-templates select="name"/></title>
  </head>
  <body>
    We're in xslt processing<BR/>
    <xsl:apply-templates select="name"/><BR/>
    <i>
    <xsl:apply-templates select="employer"/>
    <xsl:apply-templates select="site"/><BR/>
    </i>

  </body>
  </html>
</xsl:template>
<xsl:template match="email">
  <A>
    <xsl:attribute name="href">
    <xsl:apply-templates/>
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
<!-- -------------------------------------   -->

<!-- table-making 20040315 -->
<xsl:template match="bounds">
  <table>
  <xsl:apply-templates select="loc"/>
  </table>
</xsl:template>

<xsl:template match="loc">
<tr>
  <td>
    <xsl:value-of select="@lat"/>
    <xsl:text> N </xsl:text>
  </td>
  <td>
    <xsl:value-of select="@lon"/>
    <xsl:text> E </xsl:text>
  </td>
</tr>

<!-- figure, IMG type -->
<xsl:template match="graphic">
  <IMG>
  <xsl:attribute name="src">
    <xsl:value-of select="@source"/>
  </xsl:attribute>
  </IMG>
</xsl:template>


<!-- copy-of 20040526 -->
<xsl:template match="directories">
  <xsl:copy-of select="document('2003_101_00/indexall.xml')"/>
  <xsl:copy-of select="document('2003_098_00/indexall.xml')"/>
  <xsl:copy-of select="document('2003_095_00/indexall.xml')"/>
</xsl:template>

<!-- counting 20040526 -->
<xsl:template match="parmset">
  <HTML>
  <HEAD>
    <title> testing stuff</title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </HEAD>
  <BODY>
  There are <xsl:value-of select="count(/*/*)"/> graphics in this tree<BR/>
  There are <xsl:value-of select="count(figure2d)"/> plan view figures<BR/>
  There are <xsl:value-of select="count(section)"/> cross-sections figures<BR/>
  <xsl:copy-of select="document('plan.index.xml')/index/parmline[1]"/><BR/>
  </BODY>
  </HTML>
</xsl:template>

<!-- Pulling in a file 20040526 -->
<xsl:template match="directories">
  <HTML>
  <HEAD>
    <title> testing stuff</title>
  </HEAD>
  <BODY>
  <HR/>
  <xsl:value-of select="document('2003_101_00/indexall.xml')"/>
  <HR/>
  <xsl:copy-of select="document('file:///usr1/rmg3/xml/20040525/2003_101_00/indexall.xml')/parmset/figure2d[1]"/>
  <HR/>
  There are <xsl:value-of select="count(/*/*)"/> graphics in this tree<BR/>
  There are <xsl:value-of select="count(figure2d)"/> plan view figures<BR/>
  There are <xsl:value-of select="count(section)"/> cross-sections figures<BR/>
  </BODY>
  </HTML>
</xsl:template>

<!-- If testing, selective presentation 20040526 -->
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
    <xsl:text>     </xsl:text>
    <xsl:text>&#10;</xsl:text>
  </xsl:for-each>
  </directory>

</xsl:template>




</xsl:stylesheet>
