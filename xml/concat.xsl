<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN  http://www.w3.org/TR/html4/loose.dtd"/>

<xsl:template match="directory">
  <HTML>
  <HEAD>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
    <TITLE>MMAB Ocean Forecast System Output</TITLE>
  </HEAD>
  <BODY background="/mmab/images/dirbkgnd.gif">
  boilerplate goes here<BR/>
  Run made on
    <xsl:value-of select="document(concat(link[1]/ref,'.xml'))/index/date/constructed"/><BR/>
  <HR/>
  <TABLE>
    <TR>
       <TD align="center">Valid Date</TD>
       <TD>Graphic Type</TD>
    </TR>

  <xsl:for-each select="link">
    <TR>
       <TD>
         <xsl:value-of select="document(concat(ref,'.xml'))/index/date/valid"/>
       </TD>
       <TD>
        <A> 
         <xsl:attribute name="href"><xsl:value-of select="ref"/>.html</xsl:attribute>
         <xsl:apply-templates select="text"/>
        </A>
       </TD>
    </TR>
  </xsl:for-each>

  </TABLE>
  <HR/>

  </BODY>
  </HTML>
</xsl:template>
   
</xsl:stylesheet>
