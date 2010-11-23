package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

public class ConstraintSourceAware2MetaDataBean
{
    @Required
    private String property1;

    private String property3;

    @Required
    public String getProperty3()
    {
        return property3;
    }

    public void setProperty3(String property3)
    {
        this.property3 = property3;
    }

}
