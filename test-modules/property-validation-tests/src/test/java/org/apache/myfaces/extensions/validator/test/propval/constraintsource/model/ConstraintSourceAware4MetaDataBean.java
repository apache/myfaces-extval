package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

public class ConstraintSourceAware4MetaDataBean
{
    private String property;

    @Required
    public String getProperty()
    {
        return property;
    }

}
