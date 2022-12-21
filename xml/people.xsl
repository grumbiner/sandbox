<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.01" indent="yes"/>
<xsl:output encoding="UTF-8"/>
<xsl:output doctype-public="-//W3C//DTD HTML 4.01 transitional//EN"/>

<xsl:template match="/person">
  <html>
  <head>
    <title><xsl:apply-templates select="name"/></title>
    <link href="/mmab/images/main.css" type="text/css" rel="STYLESHEET"/>
  </head>
  <body>

    <center>
    <h2>
    <xsl:apply-templates select="name"/><BR/>
    <xsl:apply-templates select="title"/><BR/>
    <xsl:apply-templates select="employer"/><BR/>
    NOAA/Marine Modeling and Analysis Branch <BR/> 
    <xsl:apply-templates select="site"/><BR/>
    </h2>
    <xsl:apply-templates select="extension"/><BR/>
    Fax: 301-763-8545  <BR/>  
    <xsl:apply-templates select="email"/><BR/>
    </center>

    <hr/>
    <xsl:apply-templates select="blurb"/><BR/>
    <xsl:apply-templates select="more"/><BR/>
    <center>
    <xsl:apply-templates select="graphic"/><BR/> 
    </center>

    <hr/>
    <a href="/mmab/groups.html">Return to MMAB Personnel Page</a><br/>
    <a href="/Welcome.html">Return to MMAB Main Page</a><br/>
    <hr/>
    
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

<xsl:template match="title|employer|site|name|blurb">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="extension">
  Phone 301-763-8000 ext:<xsl:apply-templates/>
</xsl:template>

<xsl:template match="more"><br/>
For more information on Branch products related to this page, see: 
  <xsl:apply-templates select="group"/> <br/>
For biographical information, see:
  <xsl:apply-templates select="personal"/> <br/>
</xsl:template>
  
<xsl:template match="personal|group">
<A>
  <xsl:attribute name="href"><xsl:apply-templates select="link"/>
  </xsl:attribute>
  <xsl:apply-templates select="text"/>
</A> <BR/>
</xsl:template>

<xsl:template match="graphic">
   <HR/>
   <IMG>
   <xsl:attribute name="src">
   <xsl:value-of select="@source"/>
   </xsl:attribute>
   </IMG>
</xsl:template>  

</xsl:stylesheet>
