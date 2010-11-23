package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;

public class ConstraintSourceAware4Bean
{
    private String property1;

    private String property2;

    @TargetProperty("property")
    @ConstraintSource(ConstraintSourceAware4MetaDataBean.class)
    public String getProperty1()
    {
        return property1;
    }

    public void setProperty1(String property1)
    {
        this.property1 = property1;
    }

    public String getProperty2()
    {
        return property2;
    }

    public void setProperty2(String property2)
    {
        this.property2 = property2;
    }
}
