<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:my="http://www.okada.jp.org/schema/config2rdf#"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:val="http://www.opencdisc.org/schema/validator" 
    xmlns:odm="http://www.cdisc.org/ns/odm/v1.3"
    exclude-result-prefixes="xs"
    version="2.0">
    <xsl:output method="xml" encoding="UTF-8" />
    <xsl:template match="/">
        <xsl:element name="rdf:RDF">
        <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>
    <xsl:template match="odm:ODM/odm:Study/odm:MetaDataVersion">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="odm:ItemGroupDef">
        <xsl:text>
                </xsl:text>
        <xsl:element name="my:Domain">
            <xsl:attribute name="rdf:ID">domain_<xsl:value-of select="@Name"/></xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>
    <xsl:template match="val:ValidationRuleRef">
        <xsl:if test="@Active='Yes'">
            <xsl:text>
                </xsl:text>
            <xsl:element name="my:hasRule">
                <xsl:attribute name="rdf:datatype">http://www.w3.org/2001/XMLSchema#string</xsl:attribute>
                <xsl:value-of select="@RuleID"/>
            </xsl:element>            
        </xsl:if>
    </xsl:template>
    <xsl:template match="val:ValidationRules/val:*">
        <xsl:text>
                </xsl:text>
        <xsl:element name="my:Rule">
            <xsl:attribute name="rdf:ID">rule_<xsl:value-of select="@ID"/></xsl:attribute>
            <xsl:text>
                </xsl:text>
                <xsl:if test="contains(@Variable,'%Domain%')">
                    <xsl:variable name="vname" select="substring-after(@Variable, '%Domain%')"/>
                    <xsl:for-each select="//odm:ItemGroupDef/@Name">
                        <xsl:element name="my:target">
                            <xsl:attribute name="rdf:datatype">http://www.w3.org/2001/XMLSchema#string</xsl:attribute>
                            <xsl:value-of select="concat(.,$vname)"/>
                        </xsl:element>
                        <xsl:text>
                        </xsl:text>
                    </xsl:for-each>
                </xsl:if>
                <xsl:if test="not(contains(@Variable,'%Domain%'))">
                    <xsl:element name="my:target">
                        <xsl:attribute name="rdf:datatype">http://www.w3.org/2001/XMLSchema#string</xsl:attribute>
                        <xsl:value-of select="@Variable"/>
                    </xsl:element>
                </xsl:if>
            <xsl:text>
                </xsl:text>
            <xsl:element name="my:description">
                <xsl:attribute name="rdf:datatype">http://www.w3.org/2001/XMLSchema#string</xsl:attribute>
                <xsl:value-of select="@Description"/>
            </xsl:element>
            <xsl:text>
                </xsl:text>
            <xsl:element name="my:message">
                <xsl:attribute name="rdf:datatype">http://www.w3.org/2001/XMLSchema#string</xsl:attribute>
                <xsl:value-of select="@Message"/>
            </xsl:element>
        </xsl:element>        
    </xsl:template>
    <xsl:template match="text()"/>
</xsl:stylesheet>