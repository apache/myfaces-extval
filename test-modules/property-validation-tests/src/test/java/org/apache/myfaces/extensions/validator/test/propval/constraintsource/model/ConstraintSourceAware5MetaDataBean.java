package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

public class ConstraintSourceAware5MetaDataBean
{
    @Required
    @Target3
    private String property3;

    private String property4;

    @Required
    @Target4
    public String getProperty4()
    {
        return property4;
    }

    public void setProperty4(String property4)
    {
        this.property4 = property4;
    }
}
