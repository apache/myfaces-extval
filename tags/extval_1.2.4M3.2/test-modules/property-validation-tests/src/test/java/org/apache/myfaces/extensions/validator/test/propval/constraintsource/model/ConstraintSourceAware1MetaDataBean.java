package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;


public class ConstraintSourceAware1MetaDataBean
{
    @Required
    private String property1;

    private String property2;

    @Required
    public String getProperty2()
    {
        return property2;
    }

    public void setProperty2(String property2)
    {
        this.property2 = property2;
    }
}
